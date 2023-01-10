#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef enum
{
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
} ValueType;

typedef struct
{
    ValueType type;
    union
    {
        bool boolean;
        double number;
    } as; // 'as' so it reads nicely when you pull various values out
} Value;

// guard to check the value is the RIGHT TYPE
#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

// given a value of the RIGHT TYPE, unwrap it and return the corresponding raw c value
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)

// c value to lox value conversion
// produces a value with the correct type tag and the underlying value
#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})

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