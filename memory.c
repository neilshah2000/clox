#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void *reallocate(void *pointer, size_t oldSize, size_t newSize)
{
    vm.bytesAllocated += newSize - oldSize;
    // if we are allocating more memory, trigger garbage collection
    if (newSize > oldSize)
    {
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif

        if (vm.bytesAllocated > vm.nextGC)
        {
            // collectGarbage();
        }
    }

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

/*
    Set the objects mark bit
    Means we have a reference to it in our program (either direct or indirect)
*/
void markObject(Obj *object)
{
    if (object == NULL)
        return;
    if (object->isMarked)
        return;

#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void *)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    object->isMarked = true;

    /*
        Keep track of the grey objects
        These are objects we have visited and marked, but we haven't yet visited any object they reference in turn
    */
    if (vm.grayCapacity < vm.grayCount + 1)
    {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        // does not use our reallocate() wrapper, but c system realloc()
        // otherwise the GC will allocate memory which will trigger a GC which will allocate memory... and get into a loop
        vm.grayStack = (Obj **)realloc(vm.grayStack, sizeof(Obj *) * vm.grayCapacity);

        // if we can not allocate any more memory for the gray stack, then exit
        if (vm.grayStack == NULL)
            exit(1);
    }

    vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value)
{
    // check its an actual heap object
    if (IS_OBJ(value))
        markObject(AS_OBJ(value));
}

static void markArray(ValueArray *array)
{
    for (int i = 0; i < array->count; i++)
    {
        markValue(array->values[i]);
    }
}

/*
    Visit all the referenced objects that this object contains
    and mark them so the GC doesn't collect them.
    Once that's done this object becomes 'black' (from grey)
*/
static void blackenObject(Obj *object)
{
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void *)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    switch (object->type)
    {
    case OBJ_CLASS:
    {
        // mark the name to keep that string alive
        ObjClass *klass = (ObjClass *)object;
        markObject((Obj *)klass->name);
        break;
    }
    case OBJ_CLOSURE:
    {
        // mark references to the bare function and upvalues it captures
        ObjClosure *closure = (ObjClosure *)object;
        markObject((Obj *)closure->function);
        for (int i = 0; i < closure->upvalueCount; i++)
        {
            markObject((Obj *)closure->upvalues[i]);
        }
        break;
    }
    case OBJ_FUNCTION:
    {
        // functions reference their name
        // and also all their constants containing references to other objects
        ObjFunction *function = (ObjFunction *)object;
        markObject((Obj *)function->name);
        markArray(&function->chunk.constants);
    }
    case OBJ_UPVALUE:
    {
        // reference to the closed over value in upvalue
        markValue(((ObjUpvalue *)object)->closed);
        break;
    }
    // doesnt reference any other objects
    case OBJ_NATIVE:
    case OBJ_STRING:
        break;
    }
}

static void freeObject(Obj *object)
{

#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void *)object, object->type);
#endif

    switch (object->type)
    {
    case OBJ_CLASS:
    {
        FREE(ObjClass, object);
        break;
    }
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

/*
    Mark all the reachable objects
    Roots are those objects that can be reached directly withough going through some other object
    After this, all these objects have their mark bit set
*/
static void markRoots()
{
    // most roots are local variables and temporaries sitting right on the VMs stack
    for (Value *slot = vm.stack; slot < vm.stackTop; slot++)
    {
        markValue(*slot);
    }

    // mark constants and upvalues in the call frame closures
    for (int i = 0; i < vm.frameCount; i++)
    {
        markObject((Obj *)vm.frames[i].closure);
    }

    // open upvalue list is another set of values the VM can directly reach
    for (ObjUpvalue *upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next)
    {
        markObject((Obj *)upvalue);
    }

    // global variables that live in a hashtable owned by the vm
    markTable(&vm.globals);
    // compiler itself periodically grabs memory from the heap for literals and constants table
    markCompilerRoots();
}

/*
    Go through the list of marked objects (gray ones)
    For each, mark any object it references (white to grey),
    and then mark the object itself (grey to black)
    by the end grey stack is empty
*/
static void traceReferences()
{
    while (vm.grayCount > 0)
    {
        Obj *object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

/*
    reclaim any unreachable objects
*/
static void sweep()
{
    Obj *previous = NULL;
    Obj *object = vm.objects;
    // walk the linked list of every object in the heap
    while (object != NULL)
    {
        if (object->isMarked)
        {
            // unmark for nect GC cycle
            object->isMarked = false;
            // continue past black objects (reachable)
            previous = object;
            object = object->next;
        }
        else
        {
            // free any white objects (unmarked)
            Obj *unreached = object;
            object = object->next;
            // re-link the previous part of the list to next
            if (previous != NULL)
            {
                previous->next = object;
            }
            else
            {
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}

void collectGarbage()
{
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    size_t before = vm.bytesAllocated;
#endif

    markRoots();
    // process grey objects
    traceReferences();
    tableRemoveWhite(&vm.strings);
    // nothing grey left, all objects either black (reachable) or white (garbage)
    // reclaim white objects (unreferenced garbage)
    sweep();

    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("-- gc end \n");
    printf("        collected %zu bytes (from %zu to %zu) next at %zu\n", before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
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

    free(vm.grayStack);
}