#pragma once

#include <stdint.h>
#include <stdio.h>

#include "fwd.h"

struct TypeStr;

typedef struct Elaborator
{
    struct Parser* p;

    struct TypeTable* types;

    FILE* fdebug;
} Elaborator;

struct ElaborateDeclCtx
{
    struct Decl* decl;
};

void typestr_fmt(const struct TypeTable* tt, const struct TypeStr* ts, struct Array* buf);
void elaborator_init(struct Elaborator* elab, struct Parser* p);
int elaborate(struct Elaborator* elab);
void elaborator_destroy(struct Elaborator* elab);
