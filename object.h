#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

typedef enum
{
    OBJ_FUNCTION,
    OBJ_STRING,
} ObjType;

// struct that contains the state shared across all object types.
// sort of base class for objects
struct Obj
{
    ObjType type;
    struct Obj *next; /* make it a linked list so we can reclaim all the memory later */
};

/*
    Each function has its own chunk
*/
typedef struct
{
    Obj obj;   // first class functions so same Obj header all object types in Lox share
    int arity; // number of parameters
    Chunk chunk;
    ObjString *name;
} ObjFunction;

/*
    Object of type string, living on the heap
*/
struct ObjString
{
    Obj obj;    /* nested Obj struct means the state all Objs share is stored right here */
    int length; /* tells us the number of bytes in the array without walking the character string for the null terminator */
    char *chars;
    uint32_t hash; /* cache the hash value (can do because strings are immutable in Lox) */
};

ObjFunction *newFunction();
ObjString *takeString(char *chars, int length);
ObjString *copyString(const char *chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type)
{
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif