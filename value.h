#ifndef clox_value_h
#define clox_value_h

#include "common.h"

// why define the type here and not directly with the struct?
// because of cyclic dependencies between values and objects we forward declare it here
typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum
{
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ,
} ValueType;


/*
    Every Lox value that you can store in a variable or return from an expression will be a value.
    For small fixed size types, like numbers, the payload is stored directly inside the Value struct itself.
    If the object is larger, it lives on the heap, Then the Value's payload is a pointer to that blob of memory
*/
typedef struct
{
    ValueType type;
    union
    {
        bool boolean;   /* small fixed-size types store directly */ 
        double number;
        Obj *obj;       /* pointer to memory on the heap */
    } as;               /* 'as' so it reads nicely when you pull various values out */ 
} Value;

///////////// helpful macros for working with objects //////////////////

// guard to check the value is the RIGHT TYPE
#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

// given a value of the RIGHT TYPE, unwrap it and return the corresponding raw c value
#define AS_OBJ(value) ((value).as.obj)
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)

// c value to lox value conversion
// produces a value with the correct type tag and the underlying value
#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object) ((Value){VAL_OBJ, {.obj = (Obj *)object}})

// The constants pool is an array of values
typedef struct
{
    int capacity;
    int count;
    Value *values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
void printValue(Value value);

#endif