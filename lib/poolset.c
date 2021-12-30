#include "poolset.h"

#include <string.h>

#include "stdlibe.h"

struct PoolSetFind
{
    const void* obj;
    size_t sz;
};

static void* poolset_find_obj(void* userp, void* bucket, size_t bucket_sz)
{
    char* const data = bucket;
    const struct PoolSetFind user = *(struct PoolSetFind*)userp;
    for (size_t i = 0; i < bucket_sz; i += user.sz)
    {
        if (memcmp(user.obj, data + i, user.sz) == 0) return data + i;
    }
    return NULL;
}

void* poolset_emplace(struct PoolSet* p, const void* obj, size_t sz)
{
    struct PoolSetFind user = {.obj = obj, .sz = sz};
    void* found = pool_foreach_bucket(&p->p, sz, poolset_find_obj, &user);
    if (found) return found;
    return pool_push(&p->p, obj, sz);
}
