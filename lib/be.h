#pragma once

#include "array.h"
#include "autoheap.h"
#include "fwd.h"
#include "scope.h"

struct BackEnd
{
    Parser* parser;
    Elaborator* elab;
    CodeGen* cg;

    // Set of program string literals for coalescing. Does not own their memory.
    struct Array aszConstants;
    struct AutoHeap const_ref;

    struct AutoHeap heap;

    struct Array code;

    struct Scope scope;

    Decl* cur_decl;
    struct DeclFn* cur_fn;
    struct Array switch_cases;
    size_t continue_label;
    size_t break_label;
    size_t default_label;

    size_t frame_size;
    size_t max_frame_size;
    size_t next_label;

    unsigned debug_taces : 1;
};

void be_init(struct BackEnd* be, struct Parser* p, struct Elaborator* e, struct CodeGen* cg);
int be_compile(struct BackEnd* be);
void be_destroy(struct BackEnd* be);
