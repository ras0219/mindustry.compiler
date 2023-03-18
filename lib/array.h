#pragma once

#include <stdarg.h>
#include <stddef.h>

#include "compilermacros.h"

/// <summary>
/// Resizable array
/// </summary>
typedef struct Array
{
    // size in bytes
    size_t sz;
    // capacity in bytes
    size_t cap;
    // allocated buffer
    void* data;
} Array;

void array_init(struct Array* arr);
__forceinline void* array_back(const struct Array* arr, size_t sz) { return (char*)arr->data + arr->sz - sz; }
__forceinline size_t array_size(const struct Array* arr, size_t sz) { return arr->sz / sz; }
void* array_alloc(struct Array* arr, size_t sz) __attribute__((alloc_size(2), returns_nonnull, ));
void* array_push(struct Array* arr, const void* src, size_t sz) __attribute__((alloc_size(3), returns_nonnull));
void* array_push_byte(struct Array* arr, char ch) __attribute__((returns_nonnull));
void* array_push_zeroes(struct Array* arr, size_t sz) __attribute__((alloc_size(2), returns_nonnull));
__forceinline void array_clear(struct Array* arr) { arr->sz = 0; }
// UB if n > previous size
__forceinline void array_shrink(struct Array* arr, size_t n, size_t sz) { arr->sz = n * sz; }
/// postcondition: arr->cap >= cap
void array_reserve(struct Array* arr, size_t cap);
/// postcondition: arr->sz == size
void array_resize(struct Array* arr, size_t size);
/// postcondition: arr->sz == size
void array_assign_zeroes(struct Array* arr, size_t size);
/// postcondition: arr->sz == size
void array_assign(struct Array* arr, const void* data, size_t size);
__forceinline void array_copy(struct Array* arr, const struct Array* other)
{
    array_assign(arr, other->data, other->sz);
}
void array_pop(struct Array* arr, size_t sz);
void array_destroy(struct Array* arr);

// Does not append null byte
void array_appendf(struct Array* arr, const char* fmt, ...);
void array_appendv(struct Array* arr, const char* fmt, va_list argp);

/// Does not append null byte
/// @return bytes appended
size_t array_appends(struct Array* arr, const char* s);

__forceinline void* array_end(const struct Array* arr) { return arr->data + arr->sz; }

__forceinline void** arrptr_back(const struct Array* arr)
{
    return (void**)((char*)arr->data + arr->sz - sizeof(void*));
}
__forceinline size_t arrptr_size(const struct Array* arr) { return arr->sz / sizeof(void*); }
__forceinline void** arrptr_push(struct Array* arr, const void* data) { return array_push(arr, &data, sizeof(data)); }
__forceinline void* arrptr_pop(struct Array* arr)
{
    arr->sz -= sizeof(void*);
    return ((void**)arr->data)[arr->sz / sizeof(void*)];
}
size_t arrptr_find(const struct Array* arr, const void* p);

__forceinline size_t arrsz_at(const struct Array* arr, size_t i) { return ((size_t*)arr->data)[i]; }
__forceinline size_t arrsz_size(const struct Array* arr) { return arr->sz / sizeof(size_t); }
__forceinline size_t arrsz_back(const struct Array* arr) { return ((size_t*)arr->data)[arr->sz / sizeof(size_t) - 1]; }
__forceinline size_t arrsz_pop(struct Array* arr)
{
    arr->sz -= sizeof(size_t);
    return ((size_t*)arr->data)[arr->sz / sizeof(size_t)];
}
__forceinline void arrsz_shrink(struct Array* arr, size_t n) { arr->sz = n * sizeof(size_t); }
__forceinline size_t* arrsz_push(struct Array* arr, size_t data) { return array_push(arr, &data, sizeof(data)); }

#define ARRAY_FOREACH(type, i, arr) for (type* i = (arr)->data, *__end##i = array_end((arr)); i != __end##i; ++i)
#define ARRPTR_FOREACH(type, i, arr) for (type** i = (arr)->data, ** __end##i = array_end((arr)); i != __end##i; ++i)
