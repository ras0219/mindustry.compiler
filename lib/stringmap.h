#pragma once

#include "array.h"
#include "autoheap.h"

struct StringMap
{
    struct AutoHeap keys;
    struct Array values;
};

size_t* sm_get(const struct StringMap* map, const char* ntbs_key);
void sm_insert(struct StringMap* map, const char* ntbs_key, size_t value);
void sm_remove(struct StringMap* map, const char* ntbs_key);
void sm_destroy(struct StringMap* map);
