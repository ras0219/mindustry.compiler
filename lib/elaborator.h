#pragma once

#include <stdio.h>

#include "array.h"
#include "fwd.h"

struct Elaborator
{
    struct Parser* p;
    int all_stackless : 1;
    struct Decl* main;
    struct Array fns;
    struct Array callees_spans;
    struct Array callees_seqs;

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
    int calls_non_builtins;
    struct ArrSpan callees_span;
};

void elaborator_init(struct Elaborator* elab, struct Parser* p);
int elaborate(struct Elaborator* elab);
void elaborator_destroy(struct Elaborator* elab);
