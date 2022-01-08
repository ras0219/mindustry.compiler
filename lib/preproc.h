#pragma once
#pragma once

#include <stdio.h>

#include "fwd.h"

struct Preprocessor* preproc_alloc(const char* include_paths);
void preproc_free(struct Preprocessor* pp);
int preproc_file(struct Preprocessor* pp, FILE* f, const char* filename);
int preproc_define(struct Preprocessor* pp, const char* macro);
const struct Token* preproc_tokens(const struct Preprocessor* pp);
const char* preproc_stringpool(const struct Preprocessor* pp);
const char* pp_token_str(const struct Preprocessor* pp, const struct Token* tk);
void preproc_dump(const struct Preprocessor* pp);
