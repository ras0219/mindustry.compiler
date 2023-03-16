#include "ptrmap.h"

size_t* ptrmap_find(const PtrMap* m, const void* k)
{
    const void** data = m->ptrs.data;
    for (size_t i = 0, j = arrptr_size(&m->ptrs); i < j; ++i)
    {
        if (data[i] == k) return (size_t*)m->data.data + i;
    }
    return NULL;
}

void ptrmap_copy(PtrMap* m, const PtrMap* o)
{
    array_assign(&m->data, o->data.data, o->data.sz);
    array_assign(&m->ptrs, o->ptrs.data, o->ptrs.sz);
}

size_t* ptrmap_set(PtrMap* m, const void* k, size_t v)
{
    size_t* p = ptrmap_find(m, k);
    if (p)
        *p = v;
    else
    {
        arrptr_push(&m->ptrs, k);
        p = arrsz_push(&m->data, v);
    }
    return p;
}

void ptrmap_insert_all(PtrMap* m, const PtrMap* o)
{
    const void** keys = o->ptrs.data;
    size_t* values = o->data.data;
    for (size_t i = 0, j = arrptr_size(&o->ptrs); i < j; ++i)
    {
        ptrmap_set(m, keys[i], values[i]);
    }
}
