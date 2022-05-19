#pragma once
#pragma once

#include <stdint.h>
#include <stdio.h>

#include "array.h"
#include "fwd.h"

struct ParamConversion
{
    int32_t sizing;
    int32_t align;
};

typedef struct Elaborator
{
    struct Parser* p;

    struct TypeTable* types;

    FILE* fdebug;
} Elaborator;

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
