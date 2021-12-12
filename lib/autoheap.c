#include "autoheap.h"

#include "stdlibe.h"

void* autoheap_alloc(struct AutoHeap* heap, size_t alloc_size)
{
    void* const p = my_malloc(alloc_size);
    array_push_ptr(&heap->arr, p);
    return p;
}
void autoheap_destroy(struct AutoHeap* heap)
{
    void** const data = (void**)heap->arr.data;
    for (size_t i = 0; i < heap->arr.sz / sizeof(void*); ++i)
    {
        my_free(data[i]);
    }
    array_destroy(&heap->arr);
}
