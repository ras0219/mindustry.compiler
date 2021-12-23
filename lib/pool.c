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
        size_t cur_bucket_end = (8 << i) - 4;
        if (n < cur_bucket_end)
        {
            size_t cur_bucket_start = (4 << i) - 4;
            return p->data[i] + (n - cur_bucket_start) * sz;
        }
    }
    ++p->sz_buckets;
    p->data = (void**)my_realloc(p->data, p->sz_buckets * sizeof(void*));
    return p->data[p->sz_buckets - 1] = my_malloc((4 << p->sz_buckets) * sz);
}
void* pool_push(struct Pool* p, const void* src, size_t sz)
{
    void* r = pool_alloc(p, sz);
    memcpy(r, src, sz);
    return r;
}
void pool_shrink(struct Pool* p, size_t count) { p->sz = count; }

void* pool_foreach_bucket(struct Pool* p, size_t sz, pool_foreach_cb_t cb, void* userp)
{
    void* r = NULL;
    for (size_t i = 0; i < p->sz_buckets; ++i)
    {
        if (r = cb(userp, p->data[i], (8 << i) * sz)) break;
    }
    return r;
}
