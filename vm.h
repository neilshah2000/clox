#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef struct
{
    Chunk *chunk; /* chunk is a sequence of bytecode */
    uint8_t *ip;  /* instruction pointer */

    Value stack[STACK_MAX];

    /* points one past the top ie where the next value will go */
    Value *stackTop; /* direct pointer faster than index */
    Obj *objects;    /* linked list of all the objects on the heap */
} VM;

typedef enum
{
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

// object module uses the global VM to update linked list of all the programs objects
// so expose it here
extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char *source);
void push(Value value);
Value pop();

#endif