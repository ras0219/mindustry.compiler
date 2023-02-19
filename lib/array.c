#include "array.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "stdlibe.h"

void array_init(struct Array* arr) { memset(arr, 0, sizeof(struct Array)); }
void* array_alloc(struct Array* arr, size_t sz)
{
    if (sz == 0) return array_reserve(arr, 1), arr->data;
    arr->sz += sz;
    if (arr->sz > arr->cap)
    {
        do
        {
            arr->cap = arr->cap ? arr->cap * 2 : 32;
        } while (arr->sz > arr->cap);
        arr->data = my_realloc(arr->data, arr->cap);
    }
    void* ret = (char*)arr->data + arr->sz - sz;
    memset(ret, 0, sz);
    return ret;
}
void* array_push(struct Array* arr, const void* src, size_t sz)
{
    void* dst = array_alloc(arr, sz);
    if (sz > 0) memcpy(dst, src, sz);
    return dst;
}
void* array_push_byte(struct Array* arr, char ch)
{
    char* dst = array_alloc(arr, 1);
    *dst = ch;
    return dst;
}
void* array_push_zeroes(struct Array* arr, size_t sz)
{
    void* dst = array_alloc(arr, sz);
    return dst;
}
void array_reserve(struct Array* arr, size_t cap)
{
    if (cap > arr->cap)
    {
        arr->cap = cap;
        arr->data = my_realloc(arr->data, arr->cap);
    }
}
void array_resize(struct Array* arr, size_t size)
{
    array_reserve(arr, size);
    arr->sz = size;
}
void array_assign_zeroes(struct Array* arr, size_t size)
{
    array_resize(arr, size);
    memset(arr->data, 0, size);
}
void array_pop(struct Array* arr, size_t sz) { arr->sz -= sz; }
void array_destroy(struct Array* arr) { free(arr->data); }

size_t array_appends(struct Array* arr, const char* s)
{
    const size_t n = strlen(s);
    array_push(arr, s, n);
    return n;
}

void array_appendf(struct Array* arr, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    array_appendv(arr, fmt, argp);
    va_end(argp);
}
void array_appendv(struct Array* arr, const char* fmt, va_list argp)
{
    va_list args2;
    va_copy(args2, argp);
    const int size_req = vsnprintf(NULL, 0, fmt, argp);
    vsnprintf(array_alloc(arr, size_req + 1), size_req + 1, fmt, args2);
    // pop null byte
    --arr->sz;
    va_end(args2);
}

size_t arrptr_find(const struct Array* arr, const void* p)
{
    const void** data = arr->data;
    size_t i = 0;
    const size_t n = arrptr_size(arr);
    for (; i < n; ++i)
    {
        if (data[i] == p) return i;
    }
    return i;
}
