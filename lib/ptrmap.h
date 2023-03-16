#pragma once

#include "array.h"

typedef struct PtrMap
{
    // Array<void*>
    struct Array ptrs;
    // Array<size_t>
    struct Array data;
} PtrMap;

/// @returns NULL on not found
size_t* ptrmap_find(const PtrMap* m, const void* k);

void ptrmap_copy(PtrMap* m, const PtrMap* o);

size_t* ptrmap_set(PtrMap* m, const void* k, size_t v);

void ptrmap_insert_all(PtrMap* m, const PtrMap* other);

static __forceinline size_t ptrmap_size(const PtrMap* m) { return arrptr_size(&m->ptrs); }
static __forceinline const void* ptrmap_nth_ptr(const PtrMap* m, size_t n) { return ((const void**)m->ptrs.data)[n]; }
static __forceinline size_t* ptrmap_nth_val(const PtrMap* m, size_t n) { return (size_t*)m->data.data + n; }

static __forceinline void ptrmap_destroy(PtrMap* m)
{
    array_destroy(&m->ptrs);
    array_destroy(&m->data);
}
