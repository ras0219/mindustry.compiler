#pragma once

#include "array.h"
#include "autoheap.h"
#include "fwd.h"
#include "scope.h"

struct BackEnd
{
    struct Parser* parser;
    struct Elaborator* elab;
    struct CodeGen* cg;

    // Set of program string literals for coalescing. Does not own their memory.
    struct Array aszConstants;
    struct AutoHeap const_ref;

    struct AutoHeap heap;

    struct Array code;

    struct Scope scope;
};

void be_init(struct BackEnd* be, struct Parser* p, struct Elaborator* e, struct CodeGen* cg);
int be_compile(struct BackEnd* be);
void be_destroy(struct BackEnd* be);
