#pragma once

#include "array.h"

struct Pool
{
    size_t sz;
    size_t sz_buckets;
    void** data;
};

void pool_init(struct Pool*);
void pool_destroy(struct Pool*);

// sz >= sizeof(void*), sz must be held constant for life of p
void* pool_alloc(struct Pool* p, size_t sz);
void pool_clear(struct Pool* p);
