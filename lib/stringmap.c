#include "stringmap.h"

#include <string.h>

#include "stdlibe.h"

static size_t* sm_get_from(const struct StringMap* map, size_t start, const char* ntbs_key)
{
    char** keys = (char**)map->keys.arr.data;
    size_t keys_sz = array_size(&map->keys.arr, sizeof(char*));
    for (size_t i = start; i < keys_sz; ++i)
    {
        if (keys[i] && strcmp(ntbs_key, keys[i]) == 0)
        {
            return (size_t*)map->values.data + i;
        }
    }
    return NULL;
}

size_t* sm_get(const struct StringMap* map, const char* ntbs_key) { return sm_get_from(map, 0, ntbs_key); }
void sm_insert(struct StringMap* map, const char* ntbs_key, size_t value)
{
    char** keys = (char**)map->keys.arr.data;
    size_t keys_sz = array_size(&map->keys.arr, sizeof(char*));
    for (size_t i = 0; i < keys_sz; ++i)
    {
        if (keys[i])
        {
            if (strcmp(ntbs_key, keys[i]) == 0)
            {
                ((size_t*)map->values.data)[i] = value;
                return;
            }
        }
        else
        {
            // found a null
            size_t* found = sm_get_from(map, i + 1, ntbs_key);
            if (found)
            {
                *found = value;
                return;
            }
            else
            {
                size_t len = strlen(ntbs_key);
                keys[i] = my_malloc(len + 1);
                memcpy(keys[i], ntbs_key, len + 1);
                ((size_t*)map->values.data)[i] = value;
                return;
            }
        }
    }
    size_t len = strlen(ntbs_key);
    char* new_key = autoheap_alloc(&map->keys, len + 1);
    memcpy(new_key, ntbs_key, len + 1);
    arrsz_push(&map->values, value);
}
void sm_remove(struct StringMap* map, const char* ntbs_key)
{
    size_t* v = sm_get(map, ntbs_key);
    if (v)
    {
        size_t offset = v - (size_t*)map->values.data;
        char** key = ((char**)map->keys.arr.data) + offset;
        my_free(*key);
        *key = NULL;
    }
}
void sm_destroy(struct StringMap* map)
{
    autoheap_destroy(&map->keys);
    array_destroy(&map->values);
}
