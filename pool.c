#include "pool.h"

#include <malloc.h>
#include <stdlib.h>

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

// sz >= sizeof(void*)
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
    p->data = realloc(p->data, p->sz_buckets * sizeof(void*));
    return p->data[p->sz_buckets - 1] = malloc((4 << p->sz_buckets) * sz);
}

void pool_clear(struct Pool* p) { p->sz = 0; }
