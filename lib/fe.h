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

void fe_init(struct FrontEnd* fe, const char* include_paths);
void fe_destroy(struct FrontEnd* fe);
int fe_lex_file(struct FrontEnd* fe, const char* filename);
int fe_lex_file_opened(struct FrontEnd* fe, const char* filename, FILE* f);
