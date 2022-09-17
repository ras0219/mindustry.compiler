#include "autoheap.h"

#include "stdlibe.h"

void* autoheap_alloc(struct AutoHeap* heap, size_t alloc_size)
{
    void* const p = alloc_size ? my_malloc(alloc_size) : 0;
    arrptr_push(&heap->arr, p);
    return p;
}
void autoheap_pop(struct AutoHeap* heap) { my_free(arrptr_pop(&heap->arr)); }
void autoheap_destroy(struct AutoHeap* heap)
{
    void** const data = (void**)heap->arr.data;
    for (size_t i = 0; i < heap->arr.sz / sizeof(void*); ++i)
    {
        my_free(data[i]);
#ifndef NDEBUG
        data[i] = (void*)0xCC;
#endif
    }
    array_destroy(&heap->arr);
}
