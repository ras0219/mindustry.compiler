#include "pool.h"

#include <string.h>

#include "stdlibe.h"

void pool_init(struct Pool* p)
{
    p->data = NULL;
    p->sz = 0;
    p->sz_buckets = 0;
}
void pool_destroy(struct Pool* p)
{
    for (size_t i = 0; i < p->sz_buckets; ++i)
    {
        free(p->data[i]);
    }
    free(p->data);
}

void* pool_alloc(struct Pool* p, size_t sz)
{
    size_t n = p->sz++;
    for (size_t i = 0; i < p->sz_buckets; ++i)
    {
        size_t cur_bucket_end = (16 << i) - 8; // 8, 24, 56
        if (n < cur_bucket_end)
        {
            size_t cur_bucket_start = (8 << i) - 8; // 0, 8, 24
            return p->data[i] + (n - cur_bucket_start) * sz;
        }
    }
    const size_t new_bucket = p->sz_buckets++;
    p->data = (void**)my_realloc(p->data, p->sz_buckets * sizeof(void*));
    return p->data[new_bucket] = my_malloc((8 << new_bucket) * sz);
}
void* pool_alloc_zeroes(struct Pool* p, size_t sz)
{
    void* r = pool_alloc(p, sz);
    memset(r, 0, sz);
    return r;
}
void* pool_push(struct Pool* p, const void* src, size_t sz)
{
    void* r = pool_alloc(p, sz);
    memcpy(r, src, sz);
    return r;
}
void pool_shrink(struct Pool* p, size_t count) { p->sz = count; }

void* pool_foreach_bucket(struct Pool* p, pool_foreach_cb_t cb, void* userp)
{
    void* r = NULL;
    if (p->sz_buckets > 0)
    {
        int i = 0;
        for (; i < p->sz_buckets - 1; ++i)
        {
            if (r = cb(userp, p->data[i], (8 << i))) return r;
        }
        r = cb(userp, p->data[i], p->sz - (8 << i) + 8);
    }
    return r;
}
