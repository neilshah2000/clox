#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

typedef enum
{
    OBJ_STRING,
} ObjType;

// struct that contains the state shared across all object types.
// sort of base class for objects
struct Obj
{
    ObjType type;
    struct Obj *next;
};

struct ObjString
{
    Obj obj;    // nested Obj struct means the state all Objs share is stored right here
    int length; // tells us the number of bytes in the array without walking the character string for the null terminator
    char *chars;
};

ObjString *takeString(char *chars, int length);
ObjString *copyString(const char *chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type)
{
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif