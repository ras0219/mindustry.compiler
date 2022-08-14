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

int strlist_foreach(StrList* sl, strlist_foreach_cb cb, void* userp)
{
    char* const data = sl->data;
    size_t i = 0;
    while (i != sl->sz)
    {
        int r;
        size_t n;
        memcpy(&n, data + i, sizeof(size_t));
        char* const s = data + i + sizeof(size_t);
        if (r = cb(userp, s, n)) return r;
        i += sizeof(size_t) + 1 + n;
    }
    return 0;
}

void strlist_destroy(StrList* sl) { array_destroy(sl); }
