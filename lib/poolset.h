#pragma once

#include "compilermacros.h"
#include "pool.h"

struct PoolSet
{
    struct Pool p;
};

void* poolset_emplace(struct PoolSet* p, const void* obj, size_t bytes);
__forceinline void poolset_destroy(struct PoolSet* p) { pool_destroy(&p->p); }
