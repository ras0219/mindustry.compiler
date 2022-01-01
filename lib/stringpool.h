#pragma once

#include "array.h"

struct StringPool
{
    struct Array data;
};

size_t sp_insert(struct StringPool* sp, const char* s, size_t len);
