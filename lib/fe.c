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

void fe_init(struct FrontEnd* fe, const char* include_paths)
{
    memset(fe, 0, sizeof(struct FrontEnd));
    fe->pp = (struct Preprocessor*)my_malloc(sizeof(struct Preprocessor));
    preproc_init(fe->pp, include_paths);

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
    preproc_destroy(fe->pp);
    free(fe->pp);
}

int fe_lex_file(struct FrontEnd* fe, const char* filename)
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
    if (rc) return rc;
    if ((rc = parser_parse(fe->parser, (struct Token*)fe->pp->toks.data, (const char*)fe->pp->stringpool.data)))
        return rc;
    if ((rc = elaborate(fe->elab))) return rc;
    if ((rc = be_compile(fe->be))) return rc;
    return cg_emit(fe->cg, fe->fout);
}

int fe_lex_file_opened(struct FrontEnd* fe, const char* filename, FILE* f) { return preproc_file(fe->pp, f, filename); }
