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
void* array_alloc(struct Array* arr, size_t sz) __attribute__((alloc_size(2), returns_nonnull));
void* array_push(struct Array* arr, const void* src, size_t sz) __attribute__((alloc_size(3), returns_nonnull));
void* array_push_zeroes(struct Array* arr, size_t sz) __attribute__((alloc_size(2), returns_nonnull));
void array_clear(struct Array* arr);
void array_reserve(struct Array* arr, size_t sz);
void* array_push_ptr(struct Array* arr, void* data);
void* array_pop_ptr(struct Array* arr);
void array_pop(struct Array* arr, size_t sz);
void array_destroy(struct Array* arr);
