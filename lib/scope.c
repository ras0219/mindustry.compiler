#include "scope.h"

#include <string.h>

#include "stdio.h"
#include "stdlib.h"
#include "symbol.h"

void scope_init(struct Scope* s) { memset(s, 0, sizeof(Scope)); }
void scope_destroy(struct Scope* s)
{
    array_destroy(&s->binds);
    array_destroy(&s->strings);
    array_destroy(&s->subscopes);
}
static __forceinline size_t scope_size(struct Scope* s) { return array_size(&s->binds, sizeof(Binding)); }
static __forceinline struct Binding* scope_data(struct Scope* s) { return s->binds.data; }
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
static struct Binding* scope_find_impl(Scope* s, const char* id, size_t start)
{
    struct Binding* const begin = scope_data(s);
    const size_t sz = scope_size(s);
    for (size_t i = sz; i > start; --i)
    {
        struct Binding* const e = begin + i - 1;
        const char* const e_id = (char*)s->strings.data + e->ident_offset;
        if (strcmp(e_id, id) == 0)
        {
#if defined(TRACING_SCOPES)
            fprintf(stderr, "scope hit (from %zu): %zu: %s\n", start, i - 1, id);
#endif
            return e;
        }
    }
#if defined(TRACING_SCOPES)
    fprintf(stderr, "scope miss (from %zu): %zu: %s\n", start, sz, id);
#endif
    return NULL;
}

struct Binding* scope_find(struct Scope* s, const char* id) { return scope_find_impl(s, id, 0); }
struct Binding* scope_find_subscope(struct Scope* s, const char* id)
{
    return scope_find_impl(s, id, s->subscopes.sz ? arrsz_back(&s->subscopes) : 0);
}
