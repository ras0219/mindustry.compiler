#pragma once

#include "array.h"

struct StringPool
{
    struct Array ends;
    struct Array data;
};

void sp_init(struct StringPool* sp);
size_t sp_insert(struct StringPool* sp, const char* s, size_t len);
void sp_destroy(struct StringPool* sp);
