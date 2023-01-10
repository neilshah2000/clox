#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "object.h"

// low-level macro the allocates an array with a given element type and count
#define ALLOCATE(type, count) (type *)reallocate(NULL, 0, sizeof(type) * (count))

// go though reallocate() instead of free() so everything goes through there making it easier to keep track of how much memory is in use
#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)

#define GROW_ARRAY(type, pointer, oldCount, newCount) (type *)reallocate(pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) reallocate(pointer, sizeof(type) * oldCount, 0)

void *reallocate(void *pointer, size_t oldSize, size_t newSize);
void freeObjects();

#endif