#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

/*
    Single pass compiler
    No intermediate AST produced by the parser for the code generator to traverse and output target code

    Its single pass, so includes parser stuff like consuming tokens, matching expected token types
    and, code gen stuff like emitting bytecode and adding constants to the desitnation chunk
*/

typedef struct
{
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum
{
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY
} Precedence;

// hide syntax for function pointer types behind typedef
typedef void (*ParseFn)(bool canAssign);

typedef struct
{
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

/*
    Local variables
*/
typedef struct
{
    Token name;      // variable name to compare against lexeme when we are resolving identifiers
    int depth;       // level of nesting where they appear. Zero is global, one first top-level block...
    bool isCaptured; // true if the local is captured by any leter function declaration
} Local;

/*
    references closed over variables in outer scopes
*/
typedef struct
{
    uint8_t index; // stores which local slot the upvalue is capturing
    bool isLocal;  // does it capture a local variable or an upvalue from the surrounding function
} Upvalue;

/*
    top-level code vs function body
*/
typedef enum
{
    TYPE_FUNCTION,
    TYPE_SCRIPT
} FunctionType;

/*
    Flat array of all locals in scope during each point of the compilation process
    Create a compiler for each function being compiled
    treat a series of nested Compiler structs as a stack
    But we dont use an array, we use a linked list
*/
typedef struct Compiler
{
    struct Compiler *enclosing; // each Compiler points back to the function the encloses it (all the way back to root Compiler for top-level code)
    ObjFunction *function;      // all code including top-level is within a function body
    FunctionType type;

    Local locals[UINT8_COUNT];     // local variables
    int localCount;                // how many locals are in scope
    Upvalue upvalues[UINT8_COUNT]; // references closed over variables in surrounding scopes
    int scopeDepth;                // number of blocks surrounding the CURRENT bit of code we're compiling
} Compiler;

// single global parser variable so dont have to pass around different functions
Parser parser;
// global Compiler pointer but Compiler itself is stored on the stack in compiler() function
Compiler *current = NULL;

/*
    The current chunk is always the chunk owned by the function we're in the middle of compiling
*/
static Chunk *currentChunk()
{
    return &current->function->chunk;
}

//////////////// front end (parser) ////////////////////////

static void errorAt(Token *token, const char *message)
{
    if (parser.panicMode)
        return;
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF)
    {
        fprintf(stderr, " at end");
    }
    else if (token->type == TOKEN_ERROR)
    {
        // nothing
    }
    else
    {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char *message)
{
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char *message)
{
    errorAt(&parser.current, message);
}

/*
    step forward through the token stream
*/
static void advance()
{
    // printf("advance \n");
    // printf("%s", parser.current.start);
    // printf("\n");
    parser.previous = parser.current;

    for (;;)
    {
        parser.current = scanToken();
        // printf("current \n");
        // printf("%s", parser.current.start);
        // printf("\n");
        if (parser.current.type != TOKEN_ERROR)
            break;

        // clox scanner doesn't report lexical errors. Instead it creates special error tokens
        // and leaves it to the parser to report them. We do that here
        errorAtCurrent(parser.current.start);
    }
}

/*
    Similar to advance(), but checks token type as well
*/
static void consume(TokenType type, const char *message)
{
    if (parser.current.type == type)
    {
        advance();
        return;
    }

    errorAtCurrent(message);
}

// return true if the current token has the given type
static bool check(TokenType type)
{
    return parser.current.type == type;
}

/*
    If the current token has the given type,
    we consume the token and return true,
    Otherwise leave alone and return false.
*/
static bool match(TokenType type)
{
    if (!check(type))
        return false;
    advance();
    return true;
}

//////////////// code gen, back end (compiler) /////////////////

static void emitByte(uint8_t byte)
{
    writeChunk(currentChunk(), byte, parser.previous.line);
}

// used for writing an opcode followed by a one-byte operand
static void emitBytes(uint8_t byte1, uint8_t byte2)
{
    emitByte(byte1);
    emitByte(byte2);
}

/*
    Emits a new loop instruction which unconditionally jumps backwards by a given offset
*/
static void emitLoop(int loopStart)
{
    emitByte(OP_LOOP);

    // calculate the offset from the instructionwe're currently at,
    // to the loopStart point we want to jumpback to
    // +2 to jump over the OP_LOOP instructions own operands
    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX)
        error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

/*
    Used for control flow like if statements
    Condition evaluated ar runtime
*/
static int emitJump(uint8_t instruction)
{
    emitByte(instruction);
    // backpatching - put 2 placeholder bytes (16-bits)
    // after we compile the then branch we will know how many bytes to jump (depending on condition)
    emitByte(0xff);
    emitByte(0xff);
    // return offset of emitted instruction so we can come back and fill the placeholder bytes
    return currentChunk()->count - 2;
}

static void emitReturn()
{
    emitByte(OP_NIL);
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value)
{
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX)
    {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value)
{
    emitBytes(OP_CONSTANT, makeConstant(value));
}

/*
    Goes back into the bytecode and replaces the operand at the given location
    with the calculated jump offset
    Call this right before we emit the next instruction we want the jump to land on.
*/
static void patchJump(int offset)
{
    // -2 to adjust for the bytecode for the jump offset itself.
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX)
    {
        error("Too much code to jump over");
    }

    // fill in the 2 bytes of operand placeholders where the jump instruction is
    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler *compiler, FunctionType type)
{
    // capture the about-to-no-longer-be-current one in that pointer (set up nested compiler linked list/stack)
    compiler->enclosing = current;

    compiler->function = NULL; // garbage collection related paranoia
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;
    if (type != TYPE_SCRIPT)
    {
        // function object created in the compiler outlives the compiler and persists until runtime
        // so we can't reference the string from the source code, we have to copy it
        current->function->name = copyString(parser.previous.start, parser.previous.length);
    }

    // locals array keeps track of which stack slots are associated with which local variables or temporaries
    // compiler implicitly claims stack slot 0 for the VMs own internal use
    Local *local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    local->name.start = "";
    local->name.length = 0;
}

/*
    Return the function the chunk is compiled into
*/
static ObjFunction *endCompiler()
{
    emitReturn();
    ObjFunction *function = current->function;

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError)
    {
        printf("\ndisassembling chunk\n");
        disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>"); // check top-level or not
    }
#endif

    // when the compiler finishes, it pops off the stack by restoring the previous compiler to be the new current one
    current = current->enclosing;
    return function;
}

/*
    Before we compile the body of a block, we call this to enter a new scope
*/
static void beginScope()
{
    current->scopeDepth++;
}

static void endScope()
{
    current->scopeDepth--;

    // clean up all local variables
    while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth)
    {
        if (current->locals[current->localCount - 1].isCaptured)
        {
            // compiler emits code to free the stack slots for the locals, we can tell which ones need to be hoisted onto the heap
            emitByte(OP_CLOSE_UPVALUE);
        }
        else
        {
            // clean locals off stack by poping each one
            emitByte(OP_POP);
        }
        current->localCount--;
    }
}

// forward declarations.
// functions eg binary can not access the rules table directly
// because the rules table needs a reference to the binary function.
// So the rules table comes after and we use these forward declarations for access
static void expression();
static void statement();
static void declaration();
static ParseRule *getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

/*
    Takes the given token and adds its lexeme to the chunks constant table as a string.
    Returns the index of the constant in the constant table
*/
static uint8_t identifierConstant(Token *name)
{
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token *a, Token *b)
{
    if (a->length != b->length)
        return false;
    // tokens are not full lox strings so we can not check thier hashes
    return memcmp(a->start, b->start, a->length) == 0;
}

/*
    To find a local variable, we walk the locals array backwards to ensure inner variables correctly shadow outer scope variables
    Return the index of the locals array, because the locals array (at compile time) will have the exact same layout as the VM stack at runtime.
*/
static int resolveLocal(Compiler *compiler, Token *name)
{
    for (int i = compiler->localCount - 1; i >= 0; i--)
    {
        Local *local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name))
        {
            if (local->depth == -1)
            {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    // local not found so assume global variable
    return -1;
}

static int addUpvalue(Compiler *compiler, uint8_t index, bool isLocal)
{
    int upvalueCount = compiler->function->upvalueCount;

    // check if there is already an upvalue that closes over this variable
    for (int i = 0; i < upvalueCount; i++)
    {
        Upvalue *upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal)
        {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT)
    {
        error("Too many colsure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

/*
    resolve a local variable with the surrounding scopes
    check if it declared there and is closed over
*/
static int resolveUpvalue(Compiler *compiler, Token *name)
{
    // reached top level so not closed over variables
    if (compiler->enclosing == NULL)
        return -1;

    // check in enclosing scope for variable declaration
    // if found, create an upvalue
    // (base case of recursive call)
    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1)
    {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    // otherwise look for local variable beyond immediately enclosing function
    // by calling resolveUpvalue on ENCLOSING function
    int upvalue = resolveUpvalue(compiler->enclosing, name); // recursive call
    if (upvalue != -1)
    {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

/*
    compiler needs to remember that a local exists
    so we add to the compilers list of variables in the current scope
*/
static void addLocal(Token name)
{
    if (current->localCount == UINT8_COUNT)
    {
        error("Too many local variables in function.");
        return;
    }

    // initiallize the next available Local in the compilers array of variables
    Local *local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

/*
    The point where the compiler records the existence of the variable
    Only do this for locals
    'declaring' is when a variable is added to scope
*/
static void declareVariable()
{
    // exit if global
    if (current->scopeDepth == 0)
        return;

    Token *name = &parser.previous;
    // check backwards from end of the array as thats where the current scope is
    for (int i = current->localCount - 1; i >= 0; i--)
    {
        Local *local = &current->locals[i];
        // exit if we are not in the current scope
        if (local->depth != -1 && local->depth < current->scopeDepth)
            break;

        if (identifiersEqual(name, &local->name))
        {
            error("Already a variable with this name in this scope.");
        }
    }
    addLocal(*name);
}

static uint8_t parseVariable(const char *errorMessage)
{
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    // exit the function if we are in a local scope (ie not global)
    // at runtime locals arent looked up by name, so no need to add variable name to constatnts table
    // return a dummy table index instead
    if (current->scopeDepth > 0)
        return 0;

    return identifierConstant(&parser.previous);
}

/*
    For variables (local scope) and functions (top-level and local scope)
*/
static void markInitialized()
{
    // top-level functions have no local variable to mark initialized
    if (current->scopeDepth == 0)
        return;

    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/*
    Outputs the bytecode instruction that defines the new variable and stores its initial value
    The index of the variables name in the constant table is the instructions operand
    'defining' is when the variable is available for use (initialized)
*/
static void defineVariable(uint8_t global)
{
    // if its a local variable the value is sitting right at the top of our value stack (as a temporary)
    // so no need to do anything, this temporary becomes the local variable
    if (current->scopeDepth > 0)
    {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

/*
    Each argument expression generates code that leaves its value on the stack in preparation for the call
*/
static uint8_t argumentList()
{
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN))
    {
        do
        {
            expression();
            if (argCount == 255)
            {
                error("Can't have more than 255 arguments");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

/*
    left hand side expression has already been called
    its value is on top of the stack.
    if its false, the entire AND must be false, so skip the right and leave value on stack
    if its true, disgard it and evaluate the right hand side which becomes the value of the entire AND
*/
static void and_(bool canAssign)
{
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static void binary(bool canAssign)
{
    TokenType operatorType = parser.previous.type;
    ParseRule *rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType)
    {
    case TOKEN_BANG_EQUAL:
    {
        emitBytes(OP_EQUAL, OP_NOT);
        break;
    }
    case TOKEN_EQUAL_EQUAL:
    {
        emitByte(OP_EQUAL);
        break;
    }
    case TOKEN_GREATER:
    {
        emitByte(OP_GREATER);
        break;
    }
    case TOKEN_GREATER_EQUAL:
    {
        emitBytes(OP_LESS, OP_NOT);
        break;
    }
    case TOKEN_LESS:
    {
        emitByte(OP_LESS);
        break;
    }
    case TOKEN_LESS_EQUAL:
    {
        emitBytes(OP_GREATER, OP_NOT);
        break;
    }
    case TOKEN_PLUS:
        emitByte(OP_ADD);
        break;
    case TOKEN_MINUS:
        emitByte(OP_SUBTRACT);
        break;
    case TOKEN_STAR:
        emitByte(OP_MULTIPLY);
        break;
    case TOKEN_SLASH:
        emitByte(OP_DIVIDE);
        break;
    default:
        return; // Unreachable.
    }
}

/*
    Dispatched when the parser encounters a left paranthesis followed by an expression
*/
static void call(bool canAssign)
{
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

static void literal(bool canAssign)
{
    switch (parser.previous.type)
    {
    case TOKEN_FALSE:
        emitByte(OP_FALSE);
        break;
    case TOKEN_NIL:
        emitByte(OP_NIL);
        break;
    case TOKEN_TRUE:
        emitByte(OP_TRUE);
        break;
    default:
        return; // unreachable.
    }
}

static void grouping(bool canAssign)
{
    // recursively call back into expression() to compile the expression between the parenthesis
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression");
}

static void number(bool canAssign)
{
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

/*
    executed in terms of the same instructions as an AND operator
*/
static void or_(bool canAssign)
{
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

static void string(bool canAssign)
{
    // + 1 and - 2 trim leading and trailing quotation marks
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

/*
    Emit bytecode for variables. Both reading variables and assignment
*/
static void namedVariable(Token name, bool canAssign)
{
    // take the given identifier and add its lexeme to the chunks constant table
    // uint8_t arg = identifierConstant(&name);

    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1)
    {
        // try and find a local variable with that name, if we find it use there ops
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    }
    else if ((arg = resolveUpvalue(current, &name)) != -1)
    {
        // try looking in the surrounding scopes for closed over variables
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    }
    else
    {
        // if we cant find a local, use global variabley bytecode
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    // right before compiling an expression that can also be used as an assignment target,
    // we look for a subsequent = token

    //  look for and consume the = only if its in the context of a low precedence expression
    if (canAssign && match(TOKEN_EQUAL))
    {
        // of we see and =, we compile it as an assignment or setter
        expression(); // compmile the assigned value
        emitBytes(setOp, (uint8_t)arg);
    }
    else
    {
        // if no =, we compile as a variable access or getter
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign)
{
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign)
{
    // the leading '-' token has been consumed and is sitting in previous
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction. (note: this is after operand)
    switch (operatorType)
    {
    case TOKEN_BANG:
        emitByte(OP_NOT);
        break;
    case TOKEN_MINUS:
        emitByte(OP_NEGATE);
        break;
    default:
        return; // Unreachable.
    }
}
/*
    Column 1 - A function to compile a prefix expression starting with a token of that type
    Column 2 - A function to compile an infix expression whose left operand is followed by a token of that type
    Column 3 - The precedence of the infix expression that uses that token as an operator

*/
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, and_, PREC_AND},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_OR},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

// orchestrates all the parsing functions
static void parsePrecedence(Precedence precedence)
{
    advance();
    // first token is always going to be some kind of prefix expression
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL)
    {
        error("Expect expression.");
        return;
    }

    //  allow variable() to take into account the precedence of the surrounding expression that contains the variable
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence)
    {
        advance();
        // parsed expression from previous recursive call (infix or prefix)
        // gets input to this gets input to this operator.
        // It consumes whatever token it needs (usually the right operand)
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    // if the assignment variable is nested inside some expression with a higher precedence
    // and the = doesnt get consumed as part of the expression, nothing else is going to consume it
    // its an error and needs reporting
    if (canAssign && match(TOKEN_EQUAL))
    {
        error("Invalid assignment target.");
    }

    // if the next token is too low precedence, or isnt an infix operator at all, we are done.
}

static ParseRule *getRule(TokenType type)
{
    return &rules[type];
}

/*
    uses a Pratt parser - top-down operator precedence parsing
    When you add the stack effects of a series of instructions compiled from any complete expression, it will total one
    Each expression leaves one result value on the stack
*/
static void expression()
{
    // parse the lowest precidence level which subsumes all the higher-precidence expressions too
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block()
{
    // blocks can have multiple declarations (and statements)
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF))
    {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

/*
    Compile function parameters and body
*/
static void function(FunctionType type)
{
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");

    // function parameters
    if (!check(TOKEN_RIGHT_PAREN))
    {
        do
        {
            current->function->arity++;
            if (current->function->arity > 255)
            {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction *function = endCompiler(); // end compiler so we dont need to bother with endScope
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    // OP_CLOSURE ins unique in that it has variable sized encoding
    // for each upvalue the closure captures there are 2 single byte operands
    // 1st byte is local or enclosing function, 2nd byte is local slot or upvalue index to capture
    for (int i = 0; i < function->upvalueCount; i++)
    {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

/*
    class declarations are statements
*/
static void classDeclaration()
{
    consume(TOKEN_IDENTIFIER, "Expect class name");
    uint8_t nameConstant = identifierConstant(&parser.previous); // take name and add to surrounding functions constants table
    declareVariable();                                           // bind the class object to a variable of the same name

    emitBytes(OP_CLASS, nameConstant); // instruction to create class at runtime
    defineVariable(nameConstant);      // cant user a variable until its defined

    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
}

/*
    Functions are first class values
    Store function in a newly declared variable
*/
static void funDeclaration()
{
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();       // to ensure name can be referenced in the body without generating an error
    function(TYPE_FUNCTION); // compile the function parameter list and body
    defineVariable(global);  // store the function back into the variable we defined for it
}

/*
    Production for the declaration grammer
*/
static void varDeclaration()
{
    // variable name
    uint8_t global = parseVariable("Expect variable name.");

    // variable initialisation
    if (match(TOKEN_EQUAL))
    {
        expression();
    }
    else
    {
        // compiler implicitly initializes to nil
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    // as usual in a stack based VM we emit this instruction last
    defineVariable(global);
}

/*
    An 'expression statement' is simply an expression followed by a statement
    They're how you write an expression ina a context where a statement is expected.
    Usually so you can call a function or evaluate an assignment for its side effect eg

    brunch = "quiche";
    each(brunch);

    Semantically, an expression statement evaluates the expression and discards the result.
    The compiler directly encodes that behaviour. It compiles the expression then emits the OP_POP instruction

    They are essential when we add functions
*/
static void expressionStatement()
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void forStatement()
{
    // if for statement declares a variable, scope it to loop body
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    // first clause (initializer)
    if (match(TOKEN_SEMICOLON))
    {
        // No initializer.
    }
    else if (match(TOKEN_VAR))
    {
        varDeclaration();
    }
    else
    {
        // call expressionStatement() instead of expression() to consume the semicolon
        // and emit OP_POP to disgard the value as we dont want the initializer to leave anything on the stack
        expressionStatement();
    }

    int loopStart = currentChunk()->count;

    // second clause (condition)
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) // optional clause, so check if its present
    {
        expression(); // compile clause
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of loop if condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP); // discard condition
    }

    // third clause (increment)
    if (!match(TOKEN_RIGHT_PAREN)) // optional clause, so check if its present
    {
        // compile now, but dont execute until after body.
        // so unconditional jump over this clause to body
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;

        expression();     // compile increment expression itself
        emitByte(OP_POP); // only execute condition for side effect, so emit pop to discard value
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clause");

        emitLoop(loopStart);        // main loop that takes us back to top of for loop
        loopStart = incrementStart; // change loop start to offset to where the increment expression begins
        patchJump(bodyJump);
    }

    statement(); // compile body
    emitLoop(loopStart);

    if (exitJump != -1)
    {
        patchJump(exitJump);
        emitByte(OP_POP); // descard condition
    }

    endScope();
}

/*
    Jumps over certain amount of bytecode depending on the condition (runtime evaluated)
*/
static void ifStatement()
{
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    // at runtime leave the condition value at the top of the stack
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    // OP_JUMP_FALSE has an operand for how much to offset the ip - how many bytes of code to skip
    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // clean the condition off the stack
    statement();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP); // clean the condition off the stack

    if (match(TOKEN_ELSE))
        statement();
    patchJump(elseJump);
}

/*
    Evaluates an expressiona nd prints it
*/
static void printStatement()
{
    expression();
    // grammer expects a semi colon after so we consume it
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void returnStatement()
{
    if (current->type == TYPE_SCRIPT)
    {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON))
    {
        // return OP_NIL if no return value
        emitReturn();
    }
    else
    {
        // otherwise compile the return value expression
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

/*

*/
static void whileStatement()
{
    // mark the start of the loop before the condition to re-evaluate it
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    // emit a loop instruction
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
}

/*
    Skip token indiscriminately until we reach somehting that looks like a statement boundary
    We recognise a boundary by looking for a semi colon that ends a statement.
    Or we look for a subsequent token that begins a statement like control flow for declaration keywords
*/
static void synchronize()
{
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF)
    {
        if (parser.previous.type == TOKEN_SEMICOLON)
            return;
        switch (parser.current.type)
        {
        case TOKEN_CLASS:
        case TOKEN_FUN:
        case TOKEN_VAR:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_PRINT:
        case TOKEN_RETURN:
            return;

        default:; // Do nothing.
        }

        advance();
    }
}

/*
    Declaration type statements that bind a name to a variable
    Declaration rule (parent) contains statement, so all statements are declarations

    clox uses panic mode error recovery to minimise the number of cascaded compile errors it reports
    The compiler exits panic mode when it reaches a synchronisation point. For clox we choose statement boundaries
*/
static void declaration()
{
    if (match(TOKEN_CLASS))
    {
        classDeclaration();
    }
    else if (match(TOKEN_FUN))
    {
        funDeclaration();
    }
    else if (match(TOKEN_VAR))
    {
        varDeclaration();
    }
    else
    {
        statement();
    }

    if (parser.panicMode)
        synchronize();
}

/*
    Other type of statements, not declarations that bind a name to a variable
    eg control flow, print etc
    The bytecode for an entire statement has a total stack effect of zero.
    Since a statement produces no values, it ultimately leaves the stack unchanged
*/
static void statement()
{
    if (match(TOKEN_PRINT))
    {
        printStatement();
    }
    else if (match(TOKEN_FOR))
    {
        forStatement();
    }
    else if (match(TOKEN_IF))
    {
        ifStatement();
    }
    else if (match(TOKEN_RETURN))
    {
        returnStatement();
    }
    else if (match(TOKEN_WHILE))
    {
        whileStatement();
    }
    else if (match(TOKEN_LEFT_BRACE))
    {
        // Blocks are a kind of statement so the rule for them is here in the statement() production
        beginScope();
        block();
        endScope();
    }
    else
    {
        expressionStatement();
    }
}

/*
    Single pass compilation.
    Many languages split into 2 passes.
    Parser - generate an AST (consuming tokens, matching expected token types etc).
    Code Generator - produces target bytecode (and adding constants to the destination chunk).
    We do all this in one pass.

    the compiler will create and return a function that contains the compiled top-level code
*/
ObjFunction *compile(const char *source)
{
    initScanner(source);
    Compiler compiler; // compiler is stored on the stack in this compile() function
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance(); /* primes the pump on the scanner */

    // a program is a sequence of declarations
    while (!match(TOKEN_EOF))
    {
        printf("\ncompiling declaration\n");
        declaration();
    }

    printf("\ncompiler end reached\n");
    ObjFunction *function = endCompiler();    // emits an OP_RETURN
    return parser.hadError ? NULL : function; // only return function if there are no errors, so VM doesn't try and execute invalid bytecode
}

/*
    the only object the compiler uses is the ObjFunction it is compiling into
*/
void markCompilerRoots()
{
    Compiler *compiler = current;
    while (compiler != NULL)
    {
        markObject((Obj *)compiler->function);
        compiler = compiler->enclosing;
    }
}