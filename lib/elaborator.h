#pragma once

#include <stdint.h>
#include <stdio.h>

#include "array.h"
#include "fwd.h"

typedef struct Elaborator
{
    struct Parser* p;

    struct TypeTable* types;

    struct Array constinit;
    // Array<char | Symbol*>, unaligned
    struct Array constinit_bases;

    // Array<AstElabInfo>
    struct Array ast_info;

    struct Decl* cur_decl;

    FILE* fdebug;
} Elaborator;

struct ElaborateDeclCtx
{
    struct Decl* decl;
};

struct AstElabInfo* elab_info(Elaborator* e, struct Expr* a);

void typestr_fmt(const struct TypeTable* tt, const struct TypeStr* ts, struct Array* buf);
void elaborator_init(struct Elaborator* elab, struct Parser* p);
int elaborate(struct Elaborator* elab);
void elaborator_destroy(struct Elaborator* elab);
