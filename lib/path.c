#include "path.h"

void maybe_append_pathsep(Array* buf)
{
    if (buf->sz && ((char*)buf->data)[buf->sz - 1] != '/') array_push_byte(buf, '/');
}

void path_combine(Array* buf, const char* e1, size_t s1)
{
    maybe_append_pathsep(buf);
    array_push(buf, e1, s1);
}

// Null-terminates
void assign_path_join(Array* buf, const char* e1, size_t s1, const char* e2, size_t s2)
{
    array_clear(buf);
    array_push(buf, e1, s1);
    path_combine(buf, e2, s2);
    array_push_byte(buf, '\0');
}

const char* last_path_element(const char* p)
{
    const char* r = p;
    char ch;
    while (ch = *p++)
    {
        if (ch == '/') r = p;
    }
    return r;
}
