#pragma once

#include "Exports.h"
#include "stddef.h"

#if defined(__cplusplus)
extern "C"
{
#endif

    extern void* memcpy(void* __restrict __dest, const void* __restrict __src, size_t __n)
        __attribute__((__nonnull__(1, 2)));
    extern void* memmove(void* __dest, const void* __src, size_t __n) __attribute__((__nonnull__(1, 2)));
    extern void* memset(void* __s, int __c, size_t __n) __attribute__((__nonnull__(1)));
    extern int memcmp(const void* __s1, const void* __s2, size_t __n) __attribute__((pure))
    __attribute__((__nonnull__(1, 2)));
    extern void* memchr(const void* __s, int __c, size_t __n) __attribute__((pure)) __attribute__((__nonnull__(1)));
    extern char* strcpy(char* __restrict __dest, const char* __restrict __src) __attribute__((__nonnull__(1, 2)));
    extern char* strncpy(char* __restrict __dest, const char* __restrict __src, size_t __n)
        __attribute__((__nonnull__(1, 2)));

    extern char* strcat(char* __restrict __dest, const char* __restrict __src) __attribute__((__nonnull__(1, 2)));
    extern char* strncat(char* __restrict __dest, const char* __restrict __src, size_t __n)
        __attribute__((__nonnull__(1, 2)));

    extern int strcmp(const char* __s1, const char* __s2) __attribute__((pure)) __attribute__((__nonnull__(1, 2)));
    extern int strncmp(const char* __s1, const char* __s2, size_t __n) __attribute__((pure))
    __attribute__((__nonnull__(1, 2)));

    extern size_t strlen(const char* __s) __attribute__((pure)) __attribute__((__nonnull__(1)));

#if defined(__cplusplus)
}
#endif
