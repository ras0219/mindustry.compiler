#include "stringpool.h"

#include <string.h>

#include "stringpoolview.h"

void sp_destroy(struct StringPool* sp)
{
    array_destroy(&sp->ends);
    array_destroy(&sp->data);
}

struct StringPoolView sp_view(struct StringPool* sp)
{
    struct StringPoolView r = {
        .data = sp->data.data,
    };
    return r;
}

void sp_init(struct StringPool* sp)
{
    for (size_t i = 0; i < 128; ++i)
    {
        array_push_size_t(&sp->ends, 2 + i * 2);
        array_push_byte(&sp->data, i);
        array_push_byte(&sp->data, 0);
    }
}

size_t sp_insert(struct StringPool* sp, const char* s, size_t len)
{
    if (len == 0) return 0;
    if (len == 2 && (unsigned int)s[0] < 128 && s[1] == '\0')
    {
        return s[0] * 2;
    }
    size_t prev_end = 0;
    char* data = sp->data.data;
    size_t* ends = sp->ends.data;
    size_t ends_sz = array_size(&sp->ends, sizeof(size_t));
    for (size_t i = 0; i < ends_sz; ++i)
    {
        if (ends[i] - prev_end == len && memcmp(data + prev_end, s, len) == 0)
        {
            return prev_end;
        }
        prev_end = ends[i];
    }

    void* idx = array_push(&sp->data, s, len);
    size_t r = idx - sp->data.data;
    array_push_size_t(&sp->ends, sp->data.sz);
    return r;
}
