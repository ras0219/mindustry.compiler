#include "stringmap.h"

#include <string.h>

#include "stdlibe.h"

size_t strset_get(const StringSet* set, const void* key, size_t len)
{
    const void* const* const keys = (const void**)set->keys.arr.data;
    const size_t* const key_lens = set->lens.data;
    const size_t keys_sz = arrsz_size(&set->lens);
    for (size_t i = 0; i < keys_sz; ++i)
    {
        if (key_lens[i] == len)
        {
            if (memcmp(key, keys[i], len) == 0) return i;
        }
    }
    return SIZE_MAX;
}
size_t strset_insert(StringSet* set, const void* key, size_t len)
{
    size_t i = strset_get(set, key, len);
    if (i == SIZE_MAX)
    {
        i = arrsz_size(&set->lens);
        void* new_key = autoheap_alloc(&set->keys, len);
        memcpy(new_key, key, len);
        arrsz_push(&set->lens, len);
    }
    return i;
}
void strset_remove(StringSet* set, size_t index)
{
    const size_t keys_sz = arrsz_size(&set->lens);
    if (index >= keys_sz) abort();
    void** const keys = set->keys.arr.data;
    my_free(keys[index]);
    void* const key = arrptr_pop(&set->keys.arr);
    size_t len = arrsz_pop(&set->lens);
    if (index < keys_sz - 1)
    {
        size_t* const key_lens = set->lens.data;
        keys[index] = key;
        key_lens[index] = len;
    }
}
void strset_destroy(StringSet* set)
{
    autoheap_destroy(&set->keys);
    array_destroy(&set->lens);
}

#if 0
int strset_foreach(const StringSet* set, strset_foreach_cb cb, void* userp)
{
    ;
    ;
}
#endif

size_t* bsm_get(const BStringMap* map, const void* key, size_t len)
{
    const size_t i = strset_get(&map->keys, key, len);
    if (i == SIZE_MAX) return NULL;
    return (size_t*)map->values.data + i;
}
size_t* sm_get(const StringMap* map, const char* ntbs_key) { return bsm_get(&map->m, ntbs_key, strlen(ntbs_key) + 1); }
void bsm_insert(BStringMap* map, const void* key, size_t len, size_t value)
{
    size_t j = strset_insert(&map->keys, key, len);
    if (j == arrsz_size(&map->values)) array_alloc(&map->values, sizeof(size_t));
    ((size_t*)map->values.data)[j] = value;
}
void sm_insert(StringMap* map, const char* ntbs_key, size_t value)
{
    bsm_insert(&map->m, ntbs_key, strlen(ntbs_key) + 1, value);
}
void bsm_remove(BStringMap* map, const void* key, size_t len)
{
    size_t i = strset_get(&map->keys, key, len);
    if (i != SIZE_MAX)
    {
        strset_remove(&map->keys, i);
        ((size_t*)map->values.data)[i] = arrsz_pop(&map->values);
    }
}
void sm_remove(StringMap* map, const char* ntbs_key) { bsm_remove(&map->m, ntbs_key, strlen(ntbs_key) + 1); }
void bsm_destroy(BStringMap* map)
{
    strset_destroy(&map->keys);
    array_destroy(&map->values);
}
void sm_destroy(StringMap* map) { bsm_destroy(&map->m); }

int sm_foreach(const StringMap* map, sm_foreach_cb cb, void* userp)
{
    const StringSet* const set = &map->m.keys;
    const void* const* const keys = (const void**)set->keys.arr.data;
    const size_t keys_sz = arrsz_size(&set->lens);
    const size_t* const values = map->m.values.data;
    int i = 0;
    for (size_t i = 0; i < keys_sz; ++i)
    {
        if (i = cb(userp, (const char*)keys[i], values[i])) break;
    }
    return i;
}

const char* ssm_get(const StrStrMap* map, const void* key, size_t len)
{
    size_t i = strset_get(&map->keys, key, len);
    return i == SIZE_MAX ? NULL : ((void**)map->values.arr.data)[i];
}
void ssm_insert(StrStrMap* map, const void* key, size_t len, const char* data)
{
    if (data == NULL) abort();
    size_t data_len = strlen(data);
    size_t i = strset_insert(&map->keys, key, len);
    if (i == autoheap_size(&map->values))
    {
        memcpy(autoheap_alloc(&map->values, data_len + 1), data, data_len + 1);
    }
    else
    {
        void** datas = autoheap_data(&map->values);
        datas[i] = my_realloc(datas[i], data_len + 1);
        memcpy(datas[i], data, data_len + 1);
    }
}
void ssm_remove(StrStrMap* map, const void* key, size_t len)
{
    size_t i = strset_get(&map->keys, key, len);
    if (i != SIZE_MAX)
    {
        strset_remove(&map->keys, i);
        void** data = autoheap_data(&map->values);
        size_t j = autoheap_size(&map->values) - 1;
        void* tmp = data[i];
        data[i] = data[j];
        data[j] = tmp;
        autoheap_pop(&map->values);
    }
}
void ssm_destroy(StrStrMap* map)
{
    strset_destroy(&map->keys);
    autoheap_destroy(&map->values);
}
