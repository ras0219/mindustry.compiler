#pragma once

#include <stdint.h>
#include <stdio.h>

#include "fwd.h"

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

void elaborator_init(struct Elaborator* elab, struct Parser* p);
int elaborate(struct Elaborator* elab);
void elaborator_destroy(struct Elaborator* elab);
