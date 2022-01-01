#include "stringpool.h"

#include "stringpoolview.h"

struct StringPoolView sp_view(struct StringPool* sp)
{
    struct StringPoolView r = {
        .data = sp->data.data,
    };
    return r;
}

size_t sp_insert(struct StringPool* sp, const char* s, size_t len)
{
    void* idx = array_push(&sp->data, s, len);
    size_t r = idx - sp->data.data;
    array_push_byte(&sp->data, 0);
    return r;
}
