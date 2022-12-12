
#include <stdio.h>
#include "common.h"
#include "vm.h"
#include "debug.h"

VM vm;

static void resetStack()
{
    vm.stackTop = vm.stack;
}

void initVM()
{
    resetStack();
}

void freeVM()
{
}

void push(Value value)
{
    // TODO: do we need to check if the stack is full
    // int index = (int)(vm.stackTop - vm.stack);
    // if (index < STACK_MAX)

    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop()
{
    vm.stackTop--;
    return *vm.stackTop;
}

/* beating heart of the VM */
/* 90% of program execution spent here */
static InterpretResult run()
{
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// binary operator takes 2 operands so it pops twice
// first pop is second operand
#define BINARY_OP(op)     \
    do                    \
    {                     \
        double b = pop(); \
        double a = pop(); \
        push(a op b);     \
    } while (false)

    //////////// loop /////////////

    for (;;)
    {

/* debugging */
#ifdef DEBUG_TRACE_EXECUTION
        printf("            ");
        for (Value *slot = vm.stack; slot < vm.stackTop; slot++)
        {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        /* with pointer math to convert ip back to a relative offset from beginning of bytecode */
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif

        uint8_t instruction;

        /* each turn of the loop we read and execute a single bytecode instruction */
        switch (instruction = READ_BYTE())
        {
        case OP_CONSTANT:
        {
            Value constant = READ_CONSTANT();
            push(constant);
            break;
        }
        case OP_ADD:
        {
            BINARY_OP(+);
            break;
        }
        case OP_SUBTRACT:
        {
            BINARY_OP(-);
            break;
        }
        case OP_MULTIPLY:
        {
            BINARY_OP(*);
            break;
        }
        case OP_DIVIDE:
        {
            BINARY_OP(/);
            break;
        }
        case OP_NEGATE:
        {
            push(-pop());
            break;
        }
        case OP_RETURN:
        {
            printValue(pop());
            printf("\n");
            return INTERPRET_OK;
        }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(Chunk *chunk)
{
    vm.chunk = chunk;
    vm.ip = vm.chunk->code; /* initialize ip by pointing to first byte of code in the chunk */
    return run();
}
