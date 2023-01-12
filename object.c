#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) (type *)allocateObject(sizeof(type), objectType)

// allocates an object of the given size on the heap.
// size is not just the size of the Obj itself, caller passes number of bytes so there is room for extra payload fields.
static Obj *allocateObject(size_t size, ObjType type)
{
    Obj *object = (Obj *)reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;
    return object;
}

static ObjString *allocateString(char *chars, int length, uint32_t hash)
{
    ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    // intern the string
    // add it to the vm strings table to ensure there are no duplicates
    // ie we use table as a hash set (only care about keys so set value to nil)
    tableSet(&vm.strings, string, NIL_VAL);
    return string;
}

/*
    Lox hash function
    FNV-1a algorithm
*/
static uint32_t hashString(const char *key, int length)
{
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++)
    {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

// takes ownership of the characters you pass in.
// ie uses and stores the original pointer passed in
ObjString *takeString(char *chars, int length)
{
    uint32_t hash = hashString(chars, length);

    // interned
    ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL)
    {
        // as ownership is passed, free original duplicate string
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }

    return allocateString(chars, length, hash);
}

// takes a copy of the string you pass in
// ie allocates more memory and uses the new char pointer, leaving the original untouched
ObjString *copyString(const char *chars, int length)
{
    uint32_t hash = hashString(chars, length);

    // check if we already have the string in the interned table
    // if we find it, return a reference to existing string instead of copying
    ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL)
    {
        printf("\nfound interned string\n");
        return interned;
    }

    char *heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

void printObject(Value value)
{
    switch (OBJ_TYPE(value))
    {
    case OBJ_STRING:
        printf("%s", AS_CSTRING(value));
        break;
    }
}