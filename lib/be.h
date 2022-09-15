#pragma once

#include "array.h"
#include "autoheap.h"
#include "fwd.h"
#include "scope.h"

typedef struct BackEnd
{
    struct Parser* parser;
    struct Elaborator* elab;
    struct CodeGen* cg;

    struct AutoHeap sym_renames;

    struct Array code;
    struct Array switch_cases;
    struct Scope scope;

    struct Symbol* cur_sym;
    struct DeclFn* cur_fn;
    size_t continue_label;
    size_t break_label;
    size_t default_label;

    size_t frame_size;
    size_t max_frame_size;
    size_t next_label;

    size_t next_constant;

    unsigned debug_taces : 1;
} BackEnd;

void be_init(struct BackEnd* be, struct Parser* p, struct Elaborator* e, struct CodeGen* cg);
int be_compile_toplevel_decl(struct BackEnd* be, struct Decl* decl);
int be_compile(struct BackEnd* be);
void be_destroy(struct BackEnd* be);
