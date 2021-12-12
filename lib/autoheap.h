#pragma once

#include "array.h"

struct AutoHeap
{
    struct Array arr;
};

__forceinline void autoheap_init(struct AutoHeap* heap) { array_init(&heap->arr); }
__forceinline size_t autoheap_size(struct AutoHeap* heap) { return heap->arr.sz / sizeof(void*); }
__forceinline void** autoheap_data(struct AutoHeap* heap) { return (void**)heap->arr.data; }
void* autoheap_alloc(struct AutoHeap* heap, size_t alloc_size);
void autoheap_destroy(struct AutoHeap* heap);
