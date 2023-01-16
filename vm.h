#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "table.h"
#include "value.h"

#define STACK_MAX 256

typedef struct
{
    Chunk *chunk; /* chunk is a sequence of bytecode */
    uint8_t *ip;  /* instruction pointer */

    // run() function is not recursive
    // nested expression tree is flattened into a linear series of instructions
    // the stack is where we store these temporary values
    // the stack array is declared directly inline in the VM struct, so we dont need to allocate it
    Value stack[STACK_MAX];

    /* points one past the top ie where the next value will go */
    Value *stackTop; /* direct pointer faster than index */
    Table globals;   /* hash table of all global variables*/
    Table strings;   /* hash table of all strings. To support string interning */
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