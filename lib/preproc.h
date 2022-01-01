#pragma once
#pragma once

#include <stdio.h>

#include "fwd.h"

struct Preprocessor* preproc_alloc(const char* include_paths);
int preproc_file(struct Preprocessor* pp, FILE* f, const char* filename);
void preproc_free(struct Preprocessor* pp);
const struct Token* preproc_tokens(const struct Preprocessor* pp);
const char* preproc_stringpool(const struct Preprocessor* pp);
const char* pp_token_str(const struct Preprocessor* pp, const struct Token* tk);
