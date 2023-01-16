#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum
{
    OP_CONSTANT, // takes a single byte operand the specifies whihch constant to load from the chunks constant array
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,        // instruction pops the top value off the stack and forgets it
    OP_GET_LOCAL,  // getting local variables
    OP_SET_LOCAL,  // settign local variables
    OP_GET_GLOBAL, // getting global variables
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_RETURN,
} OpCode;

/* chunk is a sequence of bytecode */
typedef struct
{
    int count;
    int capacity;
    uint8_t *code; /* Bytecode is a series of instructions. This is a dynamic array. Each instruction has a 1-byte opcode, most have another 1-byte constants index */
    int *lines;
    ValueArray constants; /* values are stored here. We use the OP_CONSTANT instruction with contains an index to grab from this array */
} Chunk;

void initChunk(Chunk *chuck);
void freeChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, uint8_t byte, int line);
int addConstant(Chunk *chunk, Value value);

#endif