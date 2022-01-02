#include "fe.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "be.h"
#include "cg.h"
#include "elaborator.h"
#include "errors.h"
#include "lexstate.h"
#include "parse.h"
#include "preproc.h"
#include "stdlibe.h"
#include "tok.h"
#include "unwrap.h"

void fe_init(struct FrontEnd* fe, const char* include_paths, const char* macro_names, size_t macro_names_sz)
{
    memset(fe, 0, sizeof(struct FrontEnd));
    fe->pp = preproc_alloc(include_paths);

    for (size_t i = 0; i < macro_names_sz;)
    {
        preproc_define(fe->pp, macro_names + i);
        size_t name_len = strlen(macro_names + i);
        i += name_len + 1;
    }

    fe->parser = (struct Parser*)my_malloc(sizeof(struct Parser));
    parser_init(fe->parser);

    fe->elab = (struct Elaborator*)my_malloc(sizeof(struct Elaborator));
    elaborator_init(fe->elab, fe->parser);

    fe->cg = (struct CodeGen*)my_malloc(sizeof(struct CodeGen));
    cg_init(fe->cg);

    fe->be = (struct BackEnd*)my_malloc(sizeof(struct BackEnd));
    be_init(fe->be, fe->parser, fe->elab, fe->cg);
}
void fe_destroy(struct FrontEnd* fe)
{
    cg_destroy(fe->cg);
    free(fe->cg);
    be_destroy(fe->be);
    free(fe->be);
    elaborator_destroy(fe->elab);
    free(fe->elab);
    parser_destroy(fe->parser);
    free(fe->parser);
    preproc_free(fe->pp);
}

int fe_preproc(struct FrontEnd* fe, const char* filename)
{
    FILE* f = fopen(filename, "rb");
    if (!f)
    {
        char buf[128];
        snprintf(buf, 128, "%s: failed to open", filename);
        perror(buf);
        return 1;
    }
    int rc = preproc_file(fe->pp, f, filename);
    fclose(f);
    return rc;
}

int fe_lex_file(struct FrontEnd* fe, const char* filename)
{
    int rc = 0;
    UNWRAP(fe_preproc(fe, filename));
    UNWRAP(parser_parse(fe->parser, preproc_tokens(fe->pp), preproc_stringpool(fe->pp)));
    UNWRAP(elaborate(fe->elab));
    UNWRAP(be_compile(fe->be));
    UNWRAP(cg_emit(fe->cg, fe->fout));
fail:
    return rc;
}

int fe_lex_file_opened(struct FrontEnd* fe, const char* filename, FILE* f) { return preproc_file(fe->pp, f, filename); }
