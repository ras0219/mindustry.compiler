#pragma once

#include "array.h"

typedef size_t zuset_index;

typedef struct ZUSet
{
    // Array<size_t>
    struct Array data;
} ZUSet;

zuset_index zuset_find(ZUSet* s, size_t n);
/// @returns index of added element
zuset_index zuset_insert(ZUSet* s, size_t n);
/// @returns index of removed element
zuset_index zuset_remove(ZUSet* s, size_t n);
static __forceinline size_t zuset_size(const ZUSet* s) { return arrsz_size(&s->data); }
static __forceinline size_t zuset_nth(const ZUSet* s, zuset_index n) { return arrsz_at(&s->data, n); }
void zuset_insert_all(ZUSet* s, const ZUSet* o);
void zuset_copy(ZUSet* s, const ZUSet* o);
static __forceinline void zuset_destroy(ZUSet* s) { array_destroy(&s->data); }

typedef struct PtrMap
{
    // Array<void*>
    struct Array ptrs;
    // Array<size_t>
    struct Array data;
} PtrMap;

static __forceinline void ptrmap_clear(PtrMap* m)
{
    array_clear(&m->ptrs);
    array_clear(&m->data);
}

/// @returns NULL on not found
size_t* ptrmap_find(const PtrMap* m, const void* k);

void ptrmap_copy(PtrMap* m, const PtrMap* o);

size_t* ptrmap_set(PtrMap* m, const void* k, size_t v);

void ptrmap_insert_all(PtrMap* m, const PtrMap* other);

size_t* ptrmap_insert_if_missing(PtrMap* m, const void* k, size_t v);

static __forceinline size_t ptrmap_size(const PtrMap* m) { return arrptr_size(&m->ptrs); }
static __forceinline const void* ptrmap_nth_ptr(const PtrMap* m, size_t n) { return ((const void**)m->ptrs.data)[n]; }
static __forceinline size_t* ptrmap_nth_val(const PtrMap* m, size_t n) { return (size_t*)m->data.data + n; }

static __forceinline void ptrmap_destroy(PtrMap* m)
{
    array_destroy(&m->ptrs);
    array_destroy(&m->data);
}
