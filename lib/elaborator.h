#pragma once
#pragma once

#include <stdio.h>

#include "array.h"
#include "fwd.h"

struct Elaborator
{
    struct Parser* p;

    struct TypeTable* types;

    FILE* fdebug;
};

struct ArrSpan
{
    size_t offset;
    size_t extent;
};

struct ElaborateDeclCtx
{
    struct Decl* decl;
};

void elaborator_init(struct Elaborator* elab, struct Parser* p);
int elaborate(struct Elaborator* elab);
void elaborator_destroy(struct Elaborator* elab);
