#pragma once

#include <stddef.h>

/// <summary>
/// Address-stable, resizable element pool.
/// </summary>
struct Pool
{
    size_t sz;
    size_t sz_buckets;
    void** data;
};

void pool_init(struct Pool*);
void pool_destroy(struct Pool*);

// sz must be held constant for life of p
void* pool_alloc(struct Pool* p, size_t sz) __attribute__((alloc_size(2), returns_nonnull));
// sz must be held constant for life of p
void* pool_alloc_zeroes(struct Pool* p, size_t sz) __attribute__((alloc_size(2), returns_nonnull));
// sz must be held constant for life of p
void* pool_push(struct Pool* arr, const void* src, size_t sz) __attribute__((alloc_size(3), returns_nonnull));
void pool_shrink(struct Pool* p, size_t count);

typedef void* (*pool_foreach_cb_t)(void* userp, void* bucket, size_t bucket_sz);

// sz must be held constant for life of p
// if cb returns non-null, exit foreach early with value
void* pool_foreach_bucket(struct Pool* p, size_t sz, pool_foreach_cb_t cb, void* userp);
