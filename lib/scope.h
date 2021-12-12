#pragma once

#include <stddef.h>

#include "array.h"

struct Binding
{
    size_t ident_offset;
    struct Symbol* sym;
};

struct Scope
{
    struct Array strings;
    struct Array binds;
};

void scope_init(struct Scope* s);
void scope_destroy(struct Scope* s);
size_t scope_size(struct Scope* s);
struct Binding* scope_data(struct Scope* s);
void scope_shrink(struct Scope* s, size_t sz);
size_t scope_insert(struct Scope* s, const char* ident, struct Symbol* sym);
struct Binding* scope_find(struct Scope* s, const char* id);
