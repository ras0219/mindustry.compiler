#include <stdlib.h>

#include "string.h"
#include "strlist.h"

void strlist_init(StrList* sl) { array_init(sl); }

void strlist_append(StrList* sl, const char* s, size_t n)
{
    array_push(sl, &n, sizeof(size_t));
    array_push(sl, s, n);
    array_push_byte(sl, 0);
}

void strlist_appendz(StrList* sl, const char* z) { strlist_append(sl, z, strlen(z)); }

StrListIterator strlistv_next(StrListV sl, StrListIterator it, const char** s, size_t* n)
{
    memcpy(n, sl.data + it, sizeof(size_t));
    *s = (char*)sl.data + it + sizeof(size_t);
    return it + sizeof(size_t) + 1 + *n;
}
int strlistv_at_end(StrListV sl, StrListIterator it) { return it == sl.sz; }

/// \param cb int(*)(void* userp, char* s, size_t sz)
int strlistv_foreach(StrListV sl, StrListIterator it, strlist_foreach_cb_t cb, void* userp)
{
    if (it > sl.sz) abort();
    while (it != sl.sz)
    {
        size_t n;
        const char* s;
        it = strlistv_next(sl, it, &s, &n);
        const int r = cb(userp, s, n, it);
        if (r) return r;
    }
    return 0;
}

StrListIterator strlistv_find(StrListV sl, StrListIterator it, const char* s, size_t sz)
{
    if (it > sl.sz) abort();
    while (it != sl.sz)
    {
        size_t n;
        const char* t;
        it = strlistv_next(sl, it, &t, &n);
        if (n == sz && 0 == memcmp(s, t, sz)) return it;
    }
    return STRLIST_NPOS;
}

void strlist_destroy(StrList* sl) { array_destroy(sl); }
