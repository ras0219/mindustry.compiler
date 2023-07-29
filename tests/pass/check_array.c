typedef unsigned long long size_t;
#define __forceinline
typedef __builtin_va_list va_list;

#define va_start(ap, param) __builtin_va_start(ap, param)
#define va_end(ap) __builtin_va_end(ap)
#define va_arg(ap, type) __builtin_va_arg(ap, type)
#define va_copy(dest, src) __builtin_va_copy(dest, src)

#define NULL (void*)0

#define INT32_MAX 0x7FFFFFFF

__attribute__((noreturn)) int abort(void);

void* memset(void* __b, int __c, size_t __len);

int vsnprintf(char* __restrict __str, size_t __size, const char* __restrict __format, va_list)
    __attribute__((__format__(__printf__, 3, 0)));

__attribute__((__warn_unused_result__, returns_nonnull)) __attribute__((__alloc_size__(2))) __forceinline void*
my_realloc(void* __ptr, size_t __size);

__attribute__((__malloc__)) __attribute__((__warn_unused_result__, returns_nonnull))
__attribute__((__alloc_size__(1))) __forceinline void*
my_malloc(size_t __size);

__forceinline void my_free(void* __ptr);

void* memcpy(void* __dst, const void* __src, size_t __n);

size_t strlen(const char* __s);

typedef void ArrayElem;

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
    ArrayElem* data;
} Array;

__attribute__((nonnull)) void array_init(struct Array* arr);
__attribute__((nonnull)) __forceinline void* array_back(const struct Array* arr, size_t sz)
{
    void* data = arr->data;
    if (!data)
        abort();
    else
        __prove(data);
    return (char*)data + arr->sz - sz;
}
__forceinline size_t array_size(const struct Array* arr, size_t sz) __attribute__((nonnull, pre(sz > 0)))
{
    return arr->sz / sz;
}
__attribute__((nonnull)) void* array_alloc(struct Array* arr, size_t sz)
    __attribute__((alloc_size(2), returns_nonnull));
__attribute__((nonnull)) void* array_push(struct Array* arr, const void* src, size_t sz)
    __attribute__((alloc_size(3), returns_nonnull));
__attribute__((nonnull)) void* array_push_byte(struct Array* arr, char ch) __attribute__((returns_nonnull));
__attribute__((nonnull)) void* array_push_zeroes(struct Array* arr, size_t sz)
    __attribute__((alloc_size(2), returns_nonnull));
__attribute__((nonnull)) __forceinline void array_concat(struct Array* arr, const struct Array* arr2)
{
    void* data = arr2->data;
    if (data) array_push(arr, data, arr2->sz);
}
__attribute__((nonnull)) __forceinline void array_clear(struct Array* arr) { arr->sz = 0; }
// UB if n > previous size
__attribute__((nonnull)) __forceinline void array_shrink(struct Array* arr, size_t n, size_t sz) { arr->sz = n * sz; }
/// postcondition: arr->cap >= cap
__attribute__((nonnull)) void array_reserve(struct Array* arr, size_t cap);
__attribute__((nonnull)) __forceinline void array_commit(struct Array* arr, size_t bytes) { arr->sz += bytes; }
/// postcondition: arr->sz == size
__attribute__((nonnull)) void array_resize(struct Array* arr, size_t size);
/// postcondition: arr->sz == size
__attribute__((nonnull)) void array_assign_zeroes(struct Array* arr, size_t size);
/// postcondition: arr->sz == size
__attribute__((nonnull)) void array_assign(struct Array* arr, const void* data, size_t size);
__attribute__((nonnull)) __forceinline void array_copy(struct Array* arr, const struct Array* other)
{
    void* data = other->data;
    if (data) array_assign(arr, data, other->sz);
}
__attribute__((nonnull)) void array_pop(struct Array* arr, size_t sz);
__attribute__((nonnull)) void array_destroy(struct Array* arr);

// Does not append null byte
__attribute__((nonnull)) void array_appendf(struct Array* arr, const char* fmt, ...);
__attribute__((nonnull)) void array_appendv(struct Array* arr, const char* fmt, va_list argp);

/// Does not append null byte
/// @return bytes appended
__attribute__((nonnull)) size_t array_appends(struct Array* arr, const char* s);

__attribute__((nonnull)) __forceinline void* array_end(const struct Array* arr)
{
    char* data = arr->data;
    return data ? data + arr->sz : NULL;
}

__attribute__((nonnull)) __forceinline void** arrptr_back(const struct Array* arr)
{
    char* data = arr->data;
    if (!data) abort();
    return (void**)(data + arr->sz - sizeof(void*));
}
__attribute__((nonnull)) __forceinline size_t arrptr_size(const struct Array* arr) { return arr->sz / sizeof(void*); }
__attribute__((nonnull)) __forceinline void** arrptr_push(struct Array* arr, const void* data)
{
    return array_push(arr, &data, sizeof(data));
}
__attribute__((nonnull)) __forceinline void* arrptr_pop(struct Array* arr)
{
    void** data = arr->data;
    if (!data) abort();
    arr->sz -= sizeof(void*);
    return data[arr->sz / sizeof(void*)];
}
__attribute__((nonnull)) size_t arrptr_find(const struct Array* arr, const void* p);

__attribute__((nonnull)) __forceinline size_t arrsz_at(const struct Array* arr, size_t i)
{
    size_t* data = arr->data;
    if (!data) abort();
    return data[i];
}
__attribute__((nonnull)) __forceinline size_t arrsz_size(const struct Array* arr) { return arr->sz / sizeof(size_t); }
__attribute__((nonnull)) __forceinline size_t arrsz_back(const struct Array* arr)
{
    size_t* data = arr->data;
    if (!data) abort();
    return data[arr->sz / sizeof(size_t) - 1];
}
__attribute__((nonnull)) __forceinline size_t arrsz_pop(struct Array* arr)
{
    size_t* data = arr->data;
    if (!data) abort();
    arr->sz -= sizeof(size_t);
    return data[arr->sz / sizeof(size_t)];
}
__attribute__((nonnull)) __forceinline void arrsz_shrink(struct Array* arr, size_t n) { arr->sz = n * sizeof(size_t); }
__attribute__((nonnull)) __forceinline size_t* arrsz_push(struct Array* arr, size_t data)
{
    return array_push(arr, &data, sizeof(data));
}

#define ARRAY_FOREACH(type, i, arr) for (type* i = (arr)->data, *__end##i = array_end((arr)); i != __end##i; ++i)
#define ARRPTR_FOREACH(type, i, arr) for (type** i = (arr)->data, ** __end##i = array_end((arr)); i != __end##i; ++i)

__attribute__((nonnull)) void array_init(struct Array* arr) { memset(arr, 0, sizeof(struct Array)); }
__attribute__((nonnull)) void* array_alloc(struct Array* arr, size_t sz) __attribute__((alloc_size(2), returns_nonnull))
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
    char* data = arr->data;
    if (!data) abort();
    void* ret = (char*)data + arr->sz - sz;
    memset(ret, 0, sz);
    return ret;
}
__attribute__((nonnull)) void* array_push(struct Array* arr, const void* src, size_t sz)
{
    void* dst = array_alloc(arr, sz);
    if (sz > 0) memcpy(dst, src, sz);
    return dst;
}
__attribute__((nonnull)) void* array_push_byte(struct Array* arr, char ch)
{
    char* dst = array_alloc(arr, 1);
    *dst = ch;
    return dst;
}
__attribute__((nonnull)) void* array_push_zeroes(struct Array* arr, size_t sz)
{
    void* dst = array_alloc(arr, sz);
    return dst;
}
__attribute__((nonnull)) void array_reserve(struct Array* arr, size_t cap)
{
    if (cap > arr->cap)
    {
        arr->cap = cap;
        arr->data = my_realloc(arr->data, arr->cap);
    }
}
__attribute__((nonnull)) void array_resize(struct Array* arr, size_t size)
{
    array_reserve(arr, size);
    arr->sz = size;
}
__attribute__((nonnull)) void array_assign_zeroes(struct Array* arr, size_t size)
{
    array_resize(arr, size);
    memset(arr->data, 0, size);
}
__attribute__((nonnull)) void array_assign(struct Array* arr, const void* src, size_t n)
{
    arr->sz = 0;
    array_push(arr, src, n);
}
__attribute__((nonnull)) void array_pop(struct Array* arr, size_t sz) { arr->sz -= sz; }
__attribute__((nonnull)) void array_destroy(struct Array* arr) { my_free(arr->data); }

__attribute__((nonnull)) size_t array_appends(struct Array* arr, const char* s)
{
    const size_t n = strlen(s);
    array_push(arr, s, n);
    return n;
}

__attribute__((nonnull)) void array_appendf(struct Array* arr, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    array_appendv(arr, fmt, argp);
    va_end(argp);
}
__attribute__((nonnull)) void array_appendv(struct Array* arr, const char* fmt, va_list argp)
{
    va_list args2;
    va_copy(args2, argp);
    const int size_req = vsnprintf(NULL, 0, fmt, argp);
    if (size_req >= INT32_MAX) abort();
    vsnprintf(array_alloc(arr, size_req + 1), size_req + 1, fmt, args2);
    // pop null byte
    --arr->sz;
    va_end(args2);
}

__attribute__((nonnull)) size_t arrptr_find(const struct Array* arr, const void* p)
{
    const void** data = arr->data;
    size_t i = 0;
    const size_t n = arrptr_size(arr);
    if (n && !data) abort();
    for (; i < n; ++i)
    {
        if (data[i] == p) return i;
    }
    return i;
}
