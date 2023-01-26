#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "vm.h"
#include "object.h"
#include "memory.h"
#include "debug.h"

// single top level VM so we dont have to pass around to all the functions
VM vm;

static Value clockNative(int argCount, Value *args)
{
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack()
{
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
}

// variadic function
static void runtimeError(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    // walk the call stack from most top of call stack (most recent call) to the bottom (top-level code)
    // for each frame, print the line number corresponding to the ip, and the function name
    for (int i = vm.frameCount - 1; i >= 0; i--)
    {
        CallFrame *frame = &vm.frames[i];
        ObjFunction *function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1; // -1 because the ip has already been incremented, so decrement to the failed instruction
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL)
        {
            fprintf(stderr, "script\n");
        }
        else
        {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

/*
    Takes a pointer to a C function and a name it will be known as in Lox
    Wrap the function in an ObjNative and store it in a global variable with the given name
*/
static void defineNative(const char *name, NativeFn function)
{
    // copyString() and newNative() allocate memory
    // so the GC doesnt collect them, we push and pop them off the stack so the GC knows we are using them
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initVM()
{
    resetStack();
    vm.objects = NULL;
    initTable(&vm.globals);
    initTable(&vm.strings);

    defineNative("clock", clockNative);
}

void freeVM()
{
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    freeObjects();
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

static Value peek(int distance)
{
    return vm.stackTop[-1 - distance];
}

/*
    Initializes the next call frame on the stack
*/
static bool call(ObjClosure *closure, int argCount)
{
    // check argument count
    if (argCount != closure->function->arity)
    {
        runtimeError("Expected %d arguments but got %d", closure->function->arity, argCount);
        return false;
    }

    // check call stack size
    if (vm.frameCount == FRAMES_MAX)
    {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame *frame = &vm.frames[vm.frameCount++];
    // stores a pointer to the function being called
    frame->closure = closure;
    // points the frames ip to the beginning of the functions bytecode
    frame->ip = closure->function->chunk.code;
    // sets the slots pointer to give the frame its window into the stack
    // the arithmatic ensures the arguments already on the stack lign up with the functions parameters
    frame->slots = vm.stackTop - argCount - 1; // -1 is to account for stack slot zero which the compiler set asside for methods
    return true;
}

/*
    Only allow calls to functions and other callable values
*/
static bool callValue(Value callee, int argCount)
{
    if (IS_OBJ(callee))
    {
        switch (OBJ_TYPE(callee))
        {
        case OBJ_CLOSURE:
        {
            return call(AS_CLOSURE(callee), argCount);
        }
        case OBJ_NATIVE:
        {
            // get the C function from the bytecode chunk
            NativeFn native = AS_NATIVE(callee);
            // call the native C function here (no need for CallFrames or anything)
            Value result = native(argCount, vm.stackTop - argCount);
            vm.stackTop -= argCount + 1;
            // put result back on stack
            push(result);
            return true;
        }
        default:
        {
            break; // Non-callable object type
        }
        }
    }
    runtimeError("Can only call functions and classes");
    return false;
}

static bool isFalsey(Value value)
{
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate()
{
    ObjString *b = AS_STRING(pop());
    ObjString *a = AS_STRING(pop());

    int length = a->length + b->length;
    char *chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString *result = takeString(chars, length);
    push(OBJ_VAL(result));
}

/*
    beating heart of the VM
    90% of program execution spent here
    Controls how instructions are executed at runtime
*/
static InterpretResult run()
{
    // cached pointer to current frame
    // store as local var to encourage c compiler to keep the pointer in a register
    CallFrame *frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
// grabs the next 2 bytes from the chunk and builds a 16-bit unsigned integer from them
#define READ_SHORT() (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
// reads a one byte operand from the bytecode chunk. Treats as an index into the chunks constants table and reads the string at that index
#define READ_STRING() AS_STRING(READ_CONSTANT())

// binary operator takes 2 operands so it pops twice
// first pop is second operand
#define BINARY_OP(valueType, op)                        \
    do                                                  \
    {                                                   \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) \
        {                                               \
            runtimeError("Operands must be numbers.");  \
            return INTERPRET_RUNTIME_ERROR;             \
        }                                               \
        double b = AS_NUMBER(pop());                    \
        double a = AS_NUMBER(pop());                    \
        push(valueType(a op b));                        \
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
        disassembleInstruction(&frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code));
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
        case OP_NIL:
        {
            push(NIL_VAL);
            break;
        }
        case OP_TRUE:
        {
            push(BOOL_VAL(true));
            break;
        }
        case OP_FALSE:
        {
            push(BOOL_VAL(false));
            break;
        }
        case OP_POP:
        {
            pop();
            break;
        }
        case OP_GET_LOCAL:
        {
            // we got the slot from the locals array at compile time and stored in the single-byte operand
            uint8_t slot = READ_BYTE();
            push(frame->slots[slot]);
            break;
        }
        case OP_SET_LOCAL:
        {
            // we got the slot from the locals array at compile time and stored in the single-byte operand
            uint8_t slot = READ_BYTE();
            // take the assigned value from top of stack and store it in the slot of teh local variable
            // does not pop the stack. assignment is an expression, and expressions always produce a value
            // so leaves that value on the top of the stack
            frame->slots[slot] = peek(0);
            break;
        }
        case OP_GET_GLOBAL:
        {
            // pulls the constant table index from the instructions operand and gets the variable name
            ObjString *name = READ_STRING();
            Value value;
            if (!tableGet(&vm.globals, name, &value))
            {
                runtimeError("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(value); // take the value and push it onto the stack
            break;
        }
        case OP_DEFINE_GLOBAL:
        {
            ObjString *name = READ_STRING();
            tableSet(&vm.globals, name, peek(0));
            pop();
            break;
        }
        case OP_SET_GLOBAL:
        {
            ObjString *name = READ_STRING();
            if (tableSet(&vm.globals, name, peek(0)))
            {
                // tableSet stores the global variable even if not previously defined
                // so we take care to delete the zombie value
                tableDelete(&vm.globals, name);
                runtimeError("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            // does not pop a value off the stack
            // assigment is an expression so it needs to leave that value there in case
            // the assignment is nested in some larger expression
            break;
        }
        case OP_EQUAL:
        {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(valuesEqual(a, b)));
            break;
        }
        case OP_GREATER:
        {
            BINARY_OP(BOOL_VAL, >);
            break;
        }
        case OP_LESS:
        {
            BINARY_OP(BOOL_VAL, <);
            break;
        }
        case OP_ADD:
        {
            if (IS_STRING(peek(0)) && IS_STRING(peek(1)))
            {
                concatenate();
            }
            else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1)))
            {
                double b = AS_NUMBER(pop());
                double a = AS_NUMBER(pop());
                push(NUMBER_VAL(a + b));
            }
            else
            {
                runtimeError("Operands must be two numbers or two strings.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SUBTRACT:
        {
            BINARY_OP(NUMBER_VAL, -);
            break;
        }
        case OP_MULTIPLY:
        {
            BINARY_OP(NUMBER_VAL, *);
            break;
        }
        case OP_DIVIDE:
        {
            BINARY_OP(NUMBER_VAL, /);
            break;
        }
        case OP_NOT:
        {
            push(BOOL_VAL(isFalsey(pop())));
            break;
        }
        case OP_NEGATE:
        {
            if (!IS_NUMBER(peek(0)))
            {
                runtimeError("Operand must be a number.");
                return INTERPRET_RUNTIME_ERROR;
            }
            push(NUMBER_VAL(-AS_NUMBER(pop())));
            break;
        }
        case OP_PRINT:
        {
            // when the interpreter reaches this instruction it has already executed the code for the expression,
            // leaving the result on top of the stack
            printValue(pop());
            printf("\n");
            printf("\n----- printed value-----\n");
            break;
        }
        case OP_JUMP:
        {
            uint16_t offset = READ_SHORT();
            // no condition, always offset
            frame->ip += offset;
            break;
        }
        case OP_JUMP_IF_FALSE:
        {
            // takes 16-bit operand
            uint16_t offset = READ_SHORT();
            // checks condition at runtime and jumps if false
            if (isFalsey(peek(0)))
                frame->ip += offset;
            break;
        }
        case OP_LOOP:
        {
            uint16_t offset = READ_SHORT();
            frame->ip -= offset;
            break;
        }
        case OP_CALL:
        {
            int argCount = READ_BYTE(); // number of arguments passed to the function is stored in teh instructions operand
            if (!callValue(peek(argCount), argCount))
            {
                return INTERPRET_RUNTIME_ERROR;
            }
            // else, is successful so there will be a new frame on the CallFrame stack for the called function
            // so update our local cached variable
            frame = &vm.frames[vm.frameCount - 1];
            break;
        }
        case OP_CLOSURE:
        {
            ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
            ObjClosure *closure = newClosure(function); // wrap all functions in a closure and push it onto the stack
            push(OBJ_VAL(closure));
            break;
        }
        case OP_RETURN:
        {
            // when a function returns a value, that value will be on top of the stack.
            // We're about to disgard the called functions entire stack window so we pop that return value off and hang onto it
            Value result = pop();
            // disgard the CallFrame for the returning function
            vm.frameCount--;
            // if last CallFrame we have finished executing top-level code
            // program has finished so exit interpreter
            if (vm.frameCount == 0)
            {
                pop();
                return INTERPRET_OK;
            }

            // otherwise disgard slots callee was using for parameters and local variables
            // this means the top of the stack ends up right at the beginning of the returning functions stack window
            vm.stackTop = frame->slots;
            // push the return value onto the stack at the new lower location
            push(result);
            // update run() functions cached pointer to the current frame
            frame = &vm.frames[vm.frameCount - 1];
        }
        }
    }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char *source)
{
    ObjFunction *function = compile(source); // pass source code to compiler
    if (function == NULL)
        return INTERPRET_COMPILE_ERROR;
    printf("\nconverting the function to closure\n");
    push(OBJ_VAL(function)); // store the function on the stack
    // set up first frame for executing top level code
    ObjClosure *closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);

    printf("\nstart the VM and run the bytecode chunk\n");
    return run();
}
