#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

static void repl()
{
    char line[1024];
    for (;;)
    {
        printf("> ");

        if (!fgets(line, sizeof(line), stdin))
        {
            printf("\n");
            break;
        }

        interpret(line);
    }
}

// we also need to free the source code string
// because readFile passes ownership to its caller
static char *readFile(const char *path)
{
    // file pointer of type FILE
    // "r" - read
    // "b" - binary
    // returns NULL on error
    FILE *file = fopen(path, "rb");
    if (file == NULL)
    {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    // seek to end to get file size so we know how much memory to allocate
    fseek(file, 0L, SEEK_END);

    // how many bytes we are from the start of the file
    size_t fileSize = ftell(file);

    // rewind to beginning, allocate string that size and read the whole file in a single batch
    rewind(file);

    char *buffer = (char *)malloc(fileSize + 1);
    if (buffer == NULL)
    {
        fprintf(stderr, "Not enough mempry to read \"%s\".\n", path);
    }
    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
    if (bytesRead < fileSize)
    {
        fprintf(stderr, "Could not read file \"%s\".\n", path);
    }
    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
}

static void runFile(const char *path)
{
    char *source = readFile(path);
    printf("\nsource code\n");
    printf("%s\n", source);
    printf("\nstarting interpret\n");
    InterpretResult result = interpret(source);

    // only free after interpret has run as all tokens refer to this string so we need to keep it around
    free(source);

    if (result == INTERPRET_COMPILE_ERROR)
        exit(65);
    if (result == INTERPRET_RUNTIME_ERROR)
        exit(70);
}

int main(int argc, const char *argv[])
{
    initVM();

    if (argc == 1)
    {
        repl();
    }
    else if (argc == 2)
    {
        runFile(argv[1]);
    }
    else
    {
        fprintf(stderr, "Usage: clox [path]\n");
        exit(64);
    }

    freeVM();
    return 0;
}