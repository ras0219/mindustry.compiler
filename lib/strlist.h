#pragma once

#include <stdint.h>

#include "array.h"
#include "fwd.h"

typedef struct StrListV
{
    const void* data;
    size_t sz;
} StrListV;
typedef size_t StrListIterator;

#define STRLIST_NPOS SIZE_MAX

static __forceinline StrListV strlistv(const StrList* sl)
{
    StrListV r = {.data = sl->data, .sz = sl->sz};
    return r;
}

void strlist_init(StrList* sl);
void strlist_destroy(StrList* sl);

void strlist_append(StrList* sl, const char* s, size_t n);
void strlist_appendz(StrList* sl, const char* z);

StrListIterator strlistv_next(StrListV sl, StrListIterator it, const char** s, size_t* n);
int strlistv_at_end(StrListV sl, StrListIterator it);

typedef int (*strlist_foreach_cb_t)(void* userp, const char* s, size_t n, StrListIterator it);
int strlistv_foreach(StrListV sl, StrListIterator start, strlist_foreach_cb_t cb, void* userp);

/// \returns index of found string, STRLIST_NPOS if not found
StrListIterator strlistv_find(StrListV sl, StrListIterator start, const char* s, size_t sz);
