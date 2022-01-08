#pragma once

#include "array.h"
#include "fwd.h"
#include "stdio.h"
#include "stringmap.h"
#include "tok.h"

struct FrontEnd
{
    struct Preprocessor* pp;
    struct Parser* parser;
    struct Elaborator* elab;
    struct BackEnd* be;
    struct CodeGen* cg;

    FILE* fout;
};
