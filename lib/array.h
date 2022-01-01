#pragma once

#include <stddef.h>

/// <summary>
/// Resizable array
/// </summary>
struct Array
{
    // size in bytes
    size_t sz;
    // capacity in bytes
    size_t cap;
    // allocated buffer
    void* data;
};

void array_init(struct Array* arr);
__forceinline size_t array_size(const struct Array* arr, size_t sz) { return arr->sz / sz; }
__forceinline void* array_back(const struct Array* arr, size_t sz) { return (char*)arr->data + arr->sz - sz; }
void* array_alloc(struct Array* arr, size_t sz) __attribute__((alloc_size(2), returns_nonnull));
void* array_push(struct Array* arr, const void* src, size_t sz) __attribute__((alloc_size(3), returns_nonnull));
void* array_push_byte(struct Array* arr, char ch) __attribute__((returns_nonnull));
void* array_push_zeroes(struct Array* arr, size_t sz) __attribute__((alloc_size(2), returns_nonnull));
__forceinline void array_clear(struct Array* arr) { arr->sz = 0; }
// UB if n > previous size
__forceinline void array_shrink(struct Array* arr, size_t n, size_t sz) { arr->sz = n * sz; }

/// postcondition: arr->cap >= cap
void array_reserve(struct Array* arr, size_t cap);
size_t* array_push_size_t(struct Array* arr, size_t data);
void* array_push_ptr(struct Array* arr, void* data);
void* array_pop_ptr(struct Array* arr);
void array_pop(struct Array* arr, size_t sz);
void array_destroy(struct Array* arr);

// Does not append null byte
int array_appendf(struct Array* arr, const char* fmt, ...);

/// Does not append null byte
/// @return bytes appended
size_t array_appends(struct Array* arr, const char* s);
