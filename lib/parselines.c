#include "parselines.h"

#include "array.h"
#include "seqview.h"

void split_lines(const char* s, size_t n, Array* buf)
{
    SplitLineInfo i = {.row = 1};
    for (; i.end < n;)
    {
        char ch = s[i.end];
        if (ch == '\r' || ch == '\n')
        {
            array_push(buf, &i, sizeof(i));
            ++i.row;
            ++i.end;
            if (ch == '\r' && i.end < n && s[i.end] == '\n') ++i.end;
            i.start = i.end;
        }
        else
            ++i.end;
    }
    array_push(buf, &i, sizeof(i));
}

void remove_empty_lines(Array* buf)
{
    const size_t sz = array_size(buf, sizeof(SplitLineInfo));
    SplitLineInfo* p = buf->data;
    size_t i;
    for (i = 0; i < sz; ++i)
    {
        if (p[i].end == p[i].start) goto start_remove;
    }
    return;
start_remove:
    for (size_t j = i + 1; j < sz; ++j)
    {
        if (p[j].end != p[j].start)
        {
            p[i] = p[j];
            ++i;
        }
    }
    array_resize(buf, i * sizeof(SplitLineInfo));
}
