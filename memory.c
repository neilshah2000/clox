#include <stdlib.h>

#include "memory.h"
#include "vm.h"

void *reallocate(void *pointer, size_t oldSize, size_t newSize)
{
    if (newSize == 0)
    {
        free(pointer);
        return NULL;
    }

    void *result = realloc(pointer, newSize);
    if (result == NULL)
        exit(1);
    return result;
}

static void freeObject(Obj *object)
{
    switch (object->type)
    {
    case OBJ_CLOSURE:
    {
        // free upvalue array
        ObjClosure *closure = (ObjClosure *)object;
        FREE_ARRAY(ObjUpvalue *, closure->upvalues, closure->upvalueCount);

        // free only the ObjClosure itself, not the ObjFunction. Because the closure doesnt own the function.
        // other closures may reference it
        FREE(ObjClosure, object);
        break;
    }
    case OBJ_FUNCTION:
    {
        ObjFunction *function = (ObjFunction *)object;
        freeChunk(&function->chunk);
        FREE(ObjFunction, object);
        break;
    }
    case OBJ_NATIVE:
    {
        FREE(ObjNative, object);
        break;
    }
    case OBJ_STRING:
    {
        ObjString *string = (ObjString *)object;
        FREE_ARRAY(char, string->chars, string->length + 1);
        FREE(ObjString, object);
        break;
    }
    case OBJ_UPVALUE:
    {
        // free only the upvalue object wrapper, not the object itself, so other upvalues can keep the reference
        FREE(ObjUpvalue, object);
        break;
    }
    }
}

void freeObjects()
{
    Obj *object = vm.objects;
    while (object != NULL)
    {
        Obj *next = object->next;
        freeObject(object);
        object = next;
    }
}