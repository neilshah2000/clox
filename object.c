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
    object->isMarked = false;

    object->next = vm.objects;
    vm.objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for %d\n", (void *)object, size, type);
#endif
    return object;
}

/*
    create a class
*/
ObjClass *newClass(ObjString *name)
{
    ObjClass *klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    klass->name = name;
    return klass;
}

/*
    takes a pointer to the ObjFunction it wraps
    also initializes the type field to the new type;
*/
ObjClosure *newClosure(ObjFunction *function)
{
    // allocate an array of upvalues and initialize to null
    ObjUpvalue **upvalues = ALLOCATE(ObjUpvalue *, function->upvalueCount);
    for (int i = 0; i < function->upvalueCount; i++)
    {
        upvalues[i] = NULL;
    }

    ObjClosure *closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;
    return closure;
}

/*
    Create a function object
    All fields set as empty, filled in later
*/
ObjFunction *newFunction()
{
    ObjFunction *function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjInstance *newInstance(ObjClass *klass)
{
    ObjInstance *instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->klass = klass;
    initTable(&instance->fields); // initialize the field table to an empty hash table
    return instance;
}

/*
    Takes a C function pointer to wrap in an ObjNative
*/
ObjNative *newNative(NativeFn function)
{
    ObjNative *native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
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
    push(OBJ_VAL(string)); // GC push and pop
    tableSet(&vm.strings, string, NIL_VAL);
    pop(); // GC push and pop
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

/*
    create an Upvalue and store the Value reference in it
*/
ObjUpvalue *newUpvalue(Value *slot)
{
    ObjUpvalue *upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->closed = NIL_VAL;
    upvalue->location = slot;
    upvalue->next = NULL;
    return upvalue;
}

static void printFunction(ObjFunction *function)
{
    if (function->name == NULL)
    {
        printf("<script>");
        return;
    }
    printf("<fn %s>", function->name->chars);
}

void printObject(Value value)
{
    switch (OBJ_TYPE(value))
    {
    case OBJ_CLASS:
    {
        printf("%s", AS_CLASS(value)->name->chars);
        break;
    }
    case OBJ_CLOSURE:
    {
        printFunction(AS_CLOSURE(value)->function);
        break;
    }
    case OBJ_FUNCTION:
    {
        printFunction(AS_FUNCTION(value));
        break;
    }
    case OBJ_INSTANCE:
    {
        printf("%s instance", AS_INSTANCE(value)->klass->name->chars);
        break;
    }
    case OBJ_NATIVE:
    {
        printf("<native fn>");
        break;
    }
    case OBJ_STRING:
    {
        printf("%s", AS_CSTRING(value));
        break;
    }
    case OBJ_UPVALUE:
    {
        // users will never see this anyway because upvalues are internal to the vm only
        printf("upvalue");
        break;
    }
    }
}