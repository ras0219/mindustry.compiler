#include "array.h"

#include "malloc.h"
#include "string.h"

void array_init(struct Array* arr)
{
    arr->sz = 0;
    arr->cap = 0;
    arr->data = 0;
}
void* array_alloc(struct Array* arr, size_t sz)
{
    arr->sz += sz;
    if (arr->sz > arr->cap)
    {
        do
        {
            arr->cap = arr->cap ? arr->cap * 2 : sz * 4;
        } while (arr->sz > arr->cap);
        arr->data = realloc(arr->data, arr->cap);
    }
    return arr->data + arr->sz - sz;
}
void* array_push(struct Array* arr, const void* src, size_t sz)
{
    void* dst = array_alloc(arr, sz);
    memcpy(dst, src, sz);
    return dst;
}
void array_pop(struct Array* arr, size_t sz) { arr->sz -= sz; }
void array_destroy(struct Array* arr) { free(arr->data); }
