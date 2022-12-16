#include "stream.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int lstream_next(LinesStream* s)
{
    if (s->buf.sz)
    {
        size_t n = s->buf.sz - s->len - 1;
        if (n) memmove(s->buf.data, (char*)s->buf.data + s->len + 1, n);
        s->buf.sz = n;
    }
    while (1)
    {
        if (ferror(s->f)) perror("lstream_next"), abort();
        if (feof(s->f) && s->buf.sz == 0) return 0;
        char* const nl = memchr(s->buf.data, '\n', s->buf.sz);
        if (nl)
        {
            ++s->row;
            *nl = '\0';
            s->len = nl - (char*)s->buf.data;
            return 1;
        }
        else if (feof(s->f))
        {
            ++s->row;
            s->len = s->buf.sz;
            array_push_byte(&s->buf, '\0');
            return 1;
        }
        if (s->buf.cap - s->buf.sz < 1024) array_reserve(&s->buf, s->buf.sz + 1024);
        size_t n = fread(s->buf.data, 1, s->buf.cap - s->buf.sz, s->f);
        s->buf.sz += n;
    }
}
