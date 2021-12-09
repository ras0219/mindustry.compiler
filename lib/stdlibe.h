#pragma once

#include <stdlib.h>

__attribute__((__malloc__)) __attribute__((__warn_unused_result__, returns_nonnull))
__attribute__((__alloc_size__(1))) __forceinline void*
my_malloc(size_t __size)
{
    void* r = malloc(__size);
    if (!r) abort();
    return r;
}
__attribute__((__warn_unused_result__, returns_nonnull)) __attribute__((__alloc_size__(2))) __forceinline void*
my_realloc(void* __ptr, size_t __size)
{
    void* r = realloc(__ptr, __size);
    if (!r) abort();
    return r;
}
__forceinline void my_free(void* __ptr) { free(__ptr); }
