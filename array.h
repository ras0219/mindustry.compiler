#pragma once

#include <stddef.h>

struct Array
{
    size_t sz;
    size_t cap;
    void* data;
};

void array_init(struct Array* arr);
void* array_alloc(struct Array* arr, size_t sz);
void* array_push(struct Array* arr, const void* src, size_t sz);
void array_pop(struct Array* arr, size_t sz);
void array_destroy(struct Array* arr);
