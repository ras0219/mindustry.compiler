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

/// \param cb int(*)(void* userp, char* s, size_t sz)
int strlistv_foreach(char* const data, size_t const sz, strlist_foreach_cb_t cb, void* userp)
{
    size_t i = 0;
    while (i != sz)
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

int strlist_foreach(const StrList* sl, strlist_foreach_cb_t cb, void* userp)
{
    return strlistv_foreach(sl->data, sl->sz, cb, userp);
}

struct A
{
    int n;
    const size_t sz;
    const char* const s;
};

static int strlist_find_cb(void* userp, char* s, size_t sz)
{
    struct A* const a = userp;
    if (sz == a->sz && memcmp(a->s, s, sz) == 0) return 1;
    ++a->n;
    return 0;
}

int strlist_find(const StrList* sl, const char* s, size_t sz)
{
    struct A a = {
        .sz = sz,
        .s = s,
    };
    return strlist_foreach(sl, strlist_find_cb, &a) ? a.n : -1;
}

void strlist_destroy(StrList* sl) { array_destroy(sl); }
