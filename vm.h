#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

/*
    Frame within the stack where the functions local variables will live
    Represents a single ongoing function call
*/
typedef struct
{
    ObjClosure *closure; // used to looks up constants and other stuff
    // caller stores its own instruction pointer.
    // When we return from a funciton, the VM will jump to the ip of the callers CallFrame and resume from there
    uint8_t *ip;
    Value *slots; // points to the VMs value stack at the first slot this function can use
} CallFrame;

typedef struct
{
    // each CallFrame has its own ip and pointer to object function which contains the chunk
    CallFrame frames[FRAMES_MAX]; // create array of CallFrames up front and treat it as a stack
    int frameCount;               // current height of the callframe stack, number of ongoing function calls

    // run() function is not recursive
    // nested expression tree is flattened into a linear series of instructions
    // the stack is where we store these temporary values
    // the stack array is declared directly inline in the VM struct, so we dont need to allocate it
    // used to store local variables and temporaries
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