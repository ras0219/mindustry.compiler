#pragma once

#include "stddef.h"

#if defined(__cplusplus)
extern "C"
{
#endif

    void memscan();

    void* memcpy(void* dst, const void* src, size_t size);
    void* memset(void* dst, int ch, size_t count);

    extern void* malloc(size_t __size) __attribute__((__malloc__)) __attribute__((__alloc_size__(1)))
    __attribute__((__warn_unused_result__));
    extern void* realloc(void* __ptr, size_t __size) __attribute__((__warn_unused_result__))
    __attribute__((__alloc_size__(2)));

    extern void free(void* buffer);

    void abort();

#if defined(__cplusplus)
}
#endif
