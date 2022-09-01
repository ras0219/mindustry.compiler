#pragma once

#include "array.h"
#include "autoheap.h"
#include "fwd.h"

typedef struct StringSet
{
    struct AutoHeap keys;
    // Array<size_t>
    struct Array lens;
} StringSet;

typedef struct BStringMap
{
    StringSet keys;
    // Array<size_t>
    struct Array values;
} BStringMap;

typedef struct StringMap
{
    BStringMap m;
} StringMap;

typedef struct StrStrMap
{
    StringSet keys;
    // Array<void*>
    struct AutoHeap values;
} StrStrMap;

/// \returns SIZE_MAX on failure, index on success
size_t strset_get(const StringSet* set, const void* key, size_t key_len);
size_t strset_insert(StringSet* set, const void* key, size_t key_len);
/// Swaps last element into removed slot.
void strset_remove(StringSet* set, size_t index);
void strset_destroy(StringSet* set);
typedef int (*strset_foreach_cb)(void* userp, const void* key, size_t key_len, size_t index);
int strset_foreach(const StringSet* set, strset_foreach_cb cb, void* userp);

size_t* sm_get(const StringMap* map, const char* ntbs_key);
void sm_insert(StringMap* map, const char* ntbs_key, size_t value);
void sm_remove(StringMap* map, const char* ntbs_key);
void sm_destroy(StringMap* map);

typedef int (*sm_foreach_cb)(void* userp, const char* ntbs_key, size_t value);
int sm_foreach(const StringMap* map, sm_foreach_cb cb, void* userp);

size_t* bsm_get(const BStringMap* map, const void* key, size_t len);
void bsm_insert(BStringMap* map, const void* key, size_t len, size_t value);
void bsm_remove(BStringMap* map, const void* key, size_t len);
void bsm_destroy(BStringMap* map);

const char* ssm_get(const StrStrMap* map, const void* key, size_t len);
void ssm_insert(StrStrMap* map, const void* key, size_t len, const char* data);
void ssm_remove(StrStrMap* map, const void* key, size_t len);
void ssm_destroy(StrStrMap* map);
