#pragma once

#include <stdint.h>

#include "array.h"
#include "fwd.h"

void strlist_init(StrList* sl);
void strlist_destroy(StrList* sl);

void strlist_append(StrList* sl, const char* s, size_t n);
void strlist_appendz(StrList* sl, const char* z);

typedef int (*strlist_foreach_cb_t)(void* userp, char* s, size_t n);
int strlistv_foreach(char* sl_data, size_t sl_sz, strlist_foreach_cb_t cb, void* userp);
int strlist_foreach(const StrList* sl, strlist_foreach_cb_t cb, void* userp);
/// \returns index of found string, -1 if not found
int strlist_find(const StrList* sl, const char* s, size_t sz);
