#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table)
{
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table *table)
{
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

/*
    Core of the has table.
    Takes a key and array of buckets and figures out which entry the key belongs in.
    Used to LOOKUP existing entries AND decide where to INSERT new ones
    Returns an entry to either read from or insert into.
*/
static Entry *findEntry(Entry *entries, int capacity, ObjString *key)
{
    uint32_t index = key->hash % capacity; // ideal bucket to find/place entry
    Entry *tombstone = NULL;
    for (;;)
    {
        Entry *entry = &entries[index];
        if (entry->key == NULL)
        {
            if (IS_NIL(entry->value))
            {
                // Empty entry.
                // Returns an empty entry for both lookup and insert
                // but allows insert to reuse an empty tombstone entry
                return tombstone != NULL ? tombstone : entry;
            }
            else
            {
                // We found a tombstone
                // this is fine for insert, but lookup needs to check for a properly empty entry
                // to know for sure the key is not in the table
                // so we keep going with the search
                if (tombstone == NULL)
                    tombstone = entry;
            }
        }
        else if (entry->key == key)
        {
            // We found the key
            return entry;
        }

        // else
        // buckets has entry but for different key (collision)
        // advance to next element and check there (linear probing)
        index = (index + 1) % capacity;
    }
}

/*
    Lookup a key in the table
    Returns true if it finds a key, false otherwise
    If it finds a key, makes value output parameter point to the resulting value
*/
bool tableGet(Table *table, ObjString *key, Value *value)
{
    // table empty
    if (table->count == 0)
        return false;

    Entry *entry = findEntry(table->entries, table->capacity, key);
    // bucket empty
    if (entry->key == NULL)
        return false;

    *value = entry->value;
    return true;
}

static void adjustCapacity(Table *table, int capacity)
{
    Entry *entries = ALLOCATE(Entry, capacity);

    // old values will be copied over to new array
    // but we use the array size to determine which bucket entries go in, so there will be new collisions we need to deal with
    // so we rebuild from scratch...
    for (int i = 0; i < capacity; i++)
    {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // ...and re-insert all the old entries
    table->count = 0;
    for (int i = 0; i < table->capacity; i++)
    {
        Entry *entry = &table->entries[i];

        //////// empty slot or tombstone in old array ///////
        if (entry->key == NULL)
            continue;
        ///////// skip over rest ////////////////////////////

        // find the entry in the NEW array
        Entry *dest = findEntry(entries, capacity, entries->key);
        dest->key = entry->key;
        dest->value = entry->value;
        // recount to adjust for removed tombstones
        table->count++;
    }

    // release memory from old array
    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

/*
    Adds given key/value pair to the hash table
    If an entry for that key is present, overwrites the old value
    Returns true if a new entry was added
*/
bool tableSet(Table *table, ObjString *key, Value value)
{
    // make sure we have an entries array and its big enough
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD)
    {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    // figure out which bucket in the array it should go and return a pointer to it
    Entry *entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    // only increment count for an entrely new bucket, not a tombstone
    // tombstones have already been counted
    if (isNewKey && IS_NIL(entry->value))
        table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

/*
    Delete an entry in the table
    Replaces with a 'tombstone' so a probe sequence doesn't find an empty entry
    and stop the lookup prematurely
*/
bool tableDelete(Table *table, ObjString *key)
{
    if (table->count == 0)
        return false;

    // Find the entry.
    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL)
        return false;

    // Place a tombstone in the entry.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

// copy entries from one table to another
void tableAddAll(Table *from, Table *to)
{
    for (int i = 0; i < from->capacity; i++)
    {
        Entry *entry = &from->entries[i];
        if (entry->key != NULL)
        {
            tableSet(to, entry->key, entry->value);
        }
    }
}

/*
    Used to find duplicate strings for string interning
    Similar to findEntry() but that uses tableGet(), which does a string compare with memory location
    We need to do a deep character-by-character comparison
    (with string interning we do this only once in the VM, everything else can do a regular memory compare)
*/
ObjString *tableFindString(Table *table, const char *chars, int length, uint32_t hash)
{
    if (table->count == 0)
        return NULL;

    uint32_t index = hash % table->capacity;
    for (;;)
    {
        Entry *entry = &table->entries[index];
        if (entry->key == NULL)
        {
            // Stop if we find an empty non-tombstone entry
            if (IS_NIL(entry->value))
                return NULL;
        }
        // check length and hash (fast) and if not the same definately not same string
        // then if they are a hash collision we do a final character-by-character compare
        else if (entry->key->length == length && entry->key->hash == hash && memcmp(entry->key->chars, chars, length) == 0)
        {
            // We found it.
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}