#pragma once

#include "array.h"
#include "autoheap.h"

typedef struct BStringMap
{
    struct AutoHeap keys;
    // Array<size_t>
    struct Array key_lens;
    // Array<size_t>
    struct Array values;
} BStringMap;

typedef struct StringMap
{
    BStringMap m;
} StringMap;

size_t* sm_get(const StringMap* map, const char* ntbs_key);
void sm_insert(StringMap* map, const char* ntbs_key, size_t value);
void sm_remove(StringMap* map, const char* ntbs_key);
void sm_destroy(StringMap* map);

size_t* bsm_get(const BStringMap* map, const void* key, size_t len);
void bsm_insert(BStringMap* map, const void* key, size_t len, size_t value);
void bsm_remove(BStringMap* map, const void* key, size_t len);
void bsm_destroy(BStringMap* map);

typedef int (*sm_foreach_cb)(void* userp, const char* ntbs_key, size_t value);

int sm_foreach(const StringMap* map, sm_foreach_cb cb, void* userp);
