#include "scope.h"

#include <string.h>

#include "stdlib.h"
#include "symbol.h"

void scope_init(struct Scope* s)
{
    array_init(&s->binds);
    array_init(&s->strings);
    array_init(&s->subscopes);
}
void scope_destroy(struct Scope* s)
{
    array_destroy(&s->binds);
    array_destroy(&s->strings);
    array_destroy(&s->subscopes);
}
static size_t scope_size(struct Scope* s) { return array_size(&s->binds, sizeof(Binding)); }
static struct Binding* scope_data(struct Scope* s) { return s->binds.data; }
void scope_push_subscope(Scope* s) { arrsz_push(&s->subscopes, scope_size(s)); }
void scope_pop_subscope(Scope* s)
{
    if (!s->subscopes.sz) abort();
    size_t i = arrsz_pop(&s->subscopes);
    if (i == scope_size(s)) return;
    s->strings.sz = scope_data(s)[i].ident_offset;
    array_shrink(&s->binds, i, sizeof(Binding));
}
size_t scope_insert(struct Scope* s, void* sym, const char* name)
{
    const size_t sz = scope_size(s);
    struct Binding* const e = array_alloc(&s->binds, sizeof(struct Binding));
    e->ident_offset = s->strings.sz;
    e->sym = sym;
    array_push(&s->strings, name, strlen(name) + 1);
    return sz;
}
struct Binding* scope_find(struct Scope* s, const char* id)
{
    struct Binding* const begin = scope_data(s);
    const size_t sz = scope_size(s);
    for (size_t i = 0; i < sz; ++i)
    {
        struct Binding* const e = begin + sz - i - 1;
        const char* const e_id = (char*)s->strings.data + e->ident_offset;
        if (strcmp(e_id, id) == 0)
        {
            return e;
        }
    }
    return NULL;
}
struct Binding* scope_find_subscope(struct Scope* s, const char* id)
{
    if (!s->subscopes.sz) return scope_find(s, id);
    struct Binding* const begin = scope_data(s);
    const size_t sz = scope_size(s);
    for (size_t i = arrsz_back(&s->subscopes); i < sz; ++i)
    {
        struct Binding* const e = begin + sz - i - 1;
        const char* const e_id = (char*)s->strings.data + e->ident_offset;
        if (strcmp(e_id, id) == 0)
        {
            return e;
        }
    }
    return NULL;
}
