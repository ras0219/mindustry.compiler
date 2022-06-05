#pragma once

#include <stddef.h>

/// <summary>
/// Address-stable, resizable element pool.
/// </summary>
struct Pool
{
    /// count of stored elements
    size_t sz;
    /// count of buckets
    size_t sz_buckets;
    /// T**, pointer to buckets
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

/// \param userp User provided data pointer
/// \param bucket Pointer to beginning of bucket span
/// \param n Number of active elements in bucket
typedef void* (*pool_foreach_cb_t)(void* userp, void* bucket, size_t n);

// if cb returns non-null, exit foreach early with value
void* pool_foreach_bucket(struct Pool* p, pool_foreach_cb_t cb, void* userp);
