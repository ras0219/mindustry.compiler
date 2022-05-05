#pragma once

#include <stddef.h>

#include "array.h"

typedef struct Binding
{
    size_t ident_offset;
    struct Symbol* sym;
} Binding;

typedef struct Scope
{
    /// Array<char>
    struct Array strings;
    /// Array<Binding>
    struct Array binds;
    /// Array<size_t>
    struct Array subscopes;
} Scope;

void scope_init(struct Scope* s);
void scope_destroy(struct Scope* s);
void scope_push_subscope(struct Scope* s);
void scope_pop_subscope(struct Scope* s);
size_t scope_insert(struct Scope* s, struct Symbol* sym);
struct Binding* scope_find_subscope(struct Scope* s, const char* id);
struct Binding* scope_find(struct Scope* s, const char* id);
