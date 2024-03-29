#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "table.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_BOUND_METHOD(value) ((ObjBoundMethod *)AS_OBJ(value))
#define AS_CLASS(value) ((ObjClass *)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure *)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance *)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative *)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

/*
    Runtime types
*/
typedef enum
{
    OBJ_BOUND_METHOD,
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_INSTANCE,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE,
} ObjType;

// struct that contains the state shared across all object types.
// sort of base class for objects
struct Obj
{
    ObjType type;
    bool isMarked;    // for GC
    struct Obj *next; /* make it a linked list so we can reclaim all the memory later */
};

/*
    Each function has its own chunk
*/
typedef struct
{
    Obj obj;          // first class functions so same Obj header all object types in Lox share
    int arity;        // number of parameters
    int upvalueCount; // count of closed over variables
    Chunk chunk;
    ObjString *name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value *args);

/*
    Native functions are merely an Obj header and pointer to C function implementation
*/
typedef struct
{
    Obj obj;
    NativeFn function;
} ObjNative;

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

/*
    upvalues manages closed-over variables that no longer live in the stack
    this is the runtime representation of upvalues
*/
typedef struct ObjUpvalue
{
    Obj obj;         // base class for objects
    Value *location; // pointer to closed over variable. allows variable in outer function to be assigned to
    Value closed;
    struct ObjUpvalue *next; // make upvalues a linked list
} ObjUpvalue;

/*
    Wrapper around ObjFunction. This the compile time representation of closures
    Also contains the runtime state for the variables the function closes over
*/
typedef struct
{
    Obj obj;
    ObjFunction *function;
    ObjUpvalue **upvalues; // pointer to a dynamically allocated array of pointers to upvalues
    int upvalueCount;
} ObjClosure;

/*
    Classes
*/
typedef struct
{
    Obj obj;
    ObjString *name;
    Table methods;
} ObjClass;

typedef struct
{
    Obj obj;
    ObjClass *klass; // instances know thier class
    Table fields;
} ObjInstance;

/*
    Class methods
    Used to bind reciever instances to method closures, so 'this' points to the correct instance
*/
typedef struct
{
    Obj obj;
    Value receiver; // type Value even though the methods can only be called on ObjInstances
    ObjClosure *method;
} ObjBoundMethod;

ObjBoundMethod *newBoundMethod(Value receiver, ObjClosure *method);
ObjClass *newClass(ObjString *name);
ObjClosure *newClosure(ObjFunction *function);
ObjFunction *newFunction();
ObjInstance *newInstance(ObjClass *klass);
ObjNative *newNative(NativeFn function);
ObjString *takeString(char *chars, int length);
ObjString *copyString(const char *chars, int length);
ObjUpvalue *newUpvalue(Value *slot);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type)
{
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif