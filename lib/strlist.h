#pragma once

#include <array.h>
#include <stdint.h>

typedef struct Array StrList;

void strlist_init(StrList* sl);
void strlist_append(StrList* sl, const char* s, size_t n);
void strlist_appendz(StrList* sl, const char* z);
typedef int (*strlist_foreach_cb)(void* userp, char* s, size_t n);
int strlist_foreach(StrList* sl, strlist_foreach_cb cb, void* userp);
void strlist_destroy(StrList* sl);
