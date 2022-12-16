#include "symbol.h"

#include "parse.h"
#include "token.h"

Decl* decl_for_param(const struct Parser* p, const Decl* decl, const struct Token* id)
{
    StmtDecls* const* const sdecls = p->expr_seqs.data;
    Decl* const* const decls = p->expr_seqs.data;
    FOREACH_SEQ(i, decl->decl_list)
    {
        StmtDecls* argdecl = sdecls[i];
        FOREACH_SEQ(j, argdecl->seq)
        {
            if (id->sp_offset == decls[j]->tok->sp_offset) return decls[j];
        }
    }
    return NULL;
}
