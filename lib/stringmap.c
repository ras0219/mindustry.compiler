#include "stringmap.h"

#include <string.h>

#include "stdlibe.h"

size_t* bsm_get(const BStringMap* map, const void* key, size_t len)
{
    char** keys = (char**)map->keys.arr.data;
    size_t* key_lens = map->key_lens.data;
    size_t keys_sz = arrsz_size(&map->key_lens);
    for (size_t i = 0; i < keys_sz; ++i)
    {
        if (key_lens[i] == len)
        {
            if (memcmp(key, keys[i], len) == 0) return (size_t*)map->values.data + i;
        }
    }
    return NULL;
}
size_t* sm_get(const StringMap* map, const char* ntbs_key) { return bsm_get(&map->m, ntbs_key, strlen(ntbs_key) + 1); }
void bsm_insert(BStringMap* map, const void* key, size_t len, size_t value)
{
    size_t* p = bsm_get(map, key, len);
    if (p)
        *p = value;
    else
    {
        char* new_key = autoheap_alloc(&map->keys, len);
        memcpy(new_key, key, len);
        arrsz_push(&map->key_lens, len);
        arrsz_push(&map->values, value);
    }
}
void sm_insert(StringMap* map, const char* ntbs_key, size_t value)
{
    bsm_insert(&map->m, ntbs_key, strlen(ntbs_key) + 1, value);
}
void bsm_remove(BStringMap* map, const void* key, size_t len)
{
    size_t* v = bsm_get(map, key, len);
    if (v)
    {
        size_t offset = v - (size_t*)map->values.data;

        char** key = (char**)map->keys.arr.data + offset;
        my_free(*key);

        *key = arrptr_pop(&map->keys.arr);
        *v = arrsz_pop(&map->values);
        ((size_t*)map->key_lens.data)[offset] = arrsz_pop(&map->key_lens);
    }
}
void sm_remove(StringMap* map, const char* ntbs_key) { bsm_remove(&map->m, ntbs_key, strlen(ntbs_key) + 1); }
void bsm_destroy(BStringMap* map)
{
    autoheap_destroy(&map->keys);
    array_destroy(&map->key_lens);
    array_destroy(&map->values);
}
void sm_destroy(StringMap* map) { bsm_destroy(&map->m); }

int sm_foreach(const StringMap* map, sm_foreach_cb cb, void* userp)
{
    int r = 0;
    const size_t sz = arrsz_size(&map->m.values);
    for (int i = 0; i < sz; ++i)
    {
        if (r = cb(userp, ((const char**)map->m.keys.arr.data)[i], ((size_t*)map->m.values.data)[i])) break;
    }
    return r;
}
