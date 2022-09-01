#pragma once
#pragma once

#include <stdio.h>

#include "fwd.h"

typedef struct Preprocessor Preprocessor;

Preprocessor* preproc_alloc(void);
void preproc_free(Preprocessor* pp);

void preproc_include_paths(Preprocessor* pp, const StrList* incs);
void preproc_framework_paths(Preprocessor* pp, const StrList* incs);
void preproc_frameworks(Preprocessor* pp, const StringSet* frameworks);

int preproc_file(Preprocessor* pp, FILE* f, const char* filename);
int preproc_text(Preprocessor* pp, const char* text);
int preproc_define(Preprocessor* pp, const char* macro);
const struct Token* preproc_tokens(const Preprocessor* pp);
const char* preproc_stringpool(const Preprocessor* pp);
const char* pp_token_str(const Preprocessor* pp, const struct Token* tk);
void preproc_dump(const Preprocessor* pp);
