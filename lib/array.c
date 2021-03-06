#include "array.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "stdlibe.h"

void array_init(struct Array* arr) { memset(arr, 0, sizeof(struct Array)); }
void* array_alloc(struct Array* arr, size_t sz)
{
    arr->sz += sz;
    if (arr->sz > arr->cap)
    {
        do
        {
            arr->cap = arr->cap ? arr->cap * 2 : sz * 4;
        } while (arr->sz > arr->cap);
        arr->data = my_realloc(arr->data, arr->cap);
    }
    return arr->data + arr->sz - sz;
}
void* array_push(struct Array* arr, const void* src, size_t sz)
{
    void* dst = array_alloc(arr, sz);
    memcpy(dst, src, sz);
    return dst;
}
void* array_push_zeroes(struct Array* arr, size_t sz)
{
    void* dst = array_alloc(arr, sz);
    memset(dst, 0, sz);
    return dst;
}
void array_clear(struct Array* arr) { arr->sz = 0; }
void array_reserve(struct Array* arr, size_t cap)
{
    if (cap > arr->cap)
    {
        arr->cap = cap;
        arr->data = my_realloc(arr->data, arr->cap);
    }
}
void* array_push_ptr(struct Array* arr, void* data) { return array_push(arr, &data, sizeof(data)); }
void array_pop(struct Array* arr, size_t sz) { arr->sz -= sz; }
void* array_pop_ptr(struct Array* arr)
{
    arr->sz -= sizeof(void*);
    return ((void**)arr->data)[arr->sz / sizeof(void*)];
}
void array_destroy(struct Array* arr) { free(arr->data); }

size_t array_appends(struct Array* arr, const char* s)
{
    const size_t n = strlen(s);
    array_push(arr, s, n);
    return n;
}

int array_appendf(struct Array* arr, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);

    const size_t start_offset = arr->sz;
    const size_t remain = arr->cap - arr->sz;

    int size_req = vsnprintf((char*)arr->data + start_offset, remain, fmt, argp);

    if (size_req + 1 > remain)
    {
        array_alloc(arr, size_req + 1);
        size_req = vsnprintf((char*)arr->data + start_offset, size_req + 1, fmt, argp);
        // pop null byte
        --arr->sz;
    }
    else if (size_req >= 0)
    {
        arr->sz += size_req;
    }

    va_end(argp);

    return size_req;
}
