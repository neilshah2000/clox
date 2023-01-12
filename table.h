#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

// Bucket in a hash table
typedef struct
{
    ObjString *key; /* since the key is always a string, we store the ObjString pointer directly, instead of wrapping it in a Value*/
    Value value;
} Entry;

// Hash Table
typedef struct
{
    int count;      /* number of key/values pairs currently stored */
    int capacity;   /* same as dynamic array, we keep track of allocated size of array */
    Entry *entries; /* hash table is an array of entries */
} Table;

void initTable(Table *table);
void freeTable(Table *table);
bool tableGet(Table *table, ObjString *key, Value *value);
bool tableSet(Table *table, ObjString *key, Value value);
bool tableDelete(Table *table, ObjString *key);
void tableAddAll(Table *from, Table *to);
ObjString *tableFindString(Table *table, const char *chars, int length, uint32_t hash);

#endif