#include "elaborator.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "errors.h"
#include "lexstate.h"
#include "parse.h"
#include "stdlibe.h"
#include "symbol.h"
#include "token.h"
#include "typestr.h"

static struct RowCol s_elab_unknown_rc = {
    .file = "<elaborator>",
    .row = 1,
    .col = 1,
};

void elaborator_destroy(struct Elaborator* elab)
{
    array_destroy(&elab->fns);
    array_destroy(&elab->callees_spans);
    array_destroy(&elab->callees_seqs);
}

static void typestr_format_english(const struct TypeStr* ts, char* buf, size_t sz)
{
    if (!ts->used)
    {
        snprintf(buf, sz, "invalid type");
        return;
    }

    for (int i = ts->used - 1; i >= 0; --i)
    {
        char c = ts->buf[i];
        int to_write;
        if (c == 'c')
        {
            to_write = snprintf(buf, sz, "const ");
        }
        else if (c == 'I')
        {
            to_write = snprintf(buf, sz, "int ");
        }
        else if (c == 'V')
        {
            to_write = snprintf(buf, sz, "void ");
        }
        else if (c == 'M')
        {
            to_write = snprintf(buf, sz, "__string ");
        }
        else if (c == 'U')
        {
            to_write = snprintf(buf, sz, "__unit ");
        }
        else if (c == '$')
        {
            int end = i;
            for (--i; i >= 0; --i)
            {
                if (ts->buf[i] == '$') break;
            }
            to_write = snprintf(buf, sz, "struct %.*s ", end - i - 1, ts->buf + i + 1);
        }
        else if (c == 'C')
        {
            to_write = snprintf(buf, sz, "char ");
        }
        else if (c == 'p')
        {
            to_write = snprintf(buf, sz, "pointer to ");
        }
        else if (c == ']')
        {
            int j = i - 1;
            for (; j >= 0; --j)
            {
                if (ts->buf[j] == '[') break;
            }
            if (j == i - 1)
                to_write = snprintf(buf, sz, "unbound array of ");
            else
            {
                int arity = 0;
                sscanf(ts->buf + j + 1, "%x]", &arity);
                to_write = snprintf(buf, sz, "array of %d ", arity);
            }
            i = j;
        }
        else if (c == ')')
        {
            to_write = snprintf(buf, sz, "function of rev(");
        }
        else if (c == '(')
        {
            to_write = snprintf(buf, sz, ") returning ");
        }
        else
        {
            snprintf(buf, sz, "?");
            return;
        }
        if (to_write >= sz) return;
        buf += to_write;
        sz -= to_write;
    }
    // trim trailing space
    buf[-1] = '\0';
}

static void* array_find_ptr(void* arr_start, size_t arr_size, void* key)
{
    for (size_t i = 0; i < arr_size; i += sizeof(void*))
    {
        void* elem = arr_start + i;
        if (*(void**)elem == key) return elem;
    }
    return NULL;
}

static int is_builtin_fn_expr(struct Expr* fn)
{
    if (fn->kind != EXPR_SYM) return 0;
    struct ExprSym* sym = (struct ExprSym*)fn;
    if (sym->sym->decl->attr.asmstr) return 1;
    return 0;
}

static void typestr_pop_arg(struct TypeStr* fty, struct TypeStr* aty)
{
    int c = 0;
    int x = fty->used - 1;
    for (; x >= 0; --x)
    {
        if (fty->buf[x] == '(')
        {
            if (c == 0)
                break;
            else
                --c;
        }
        else if (fty->buf[x] == ',')
        {
            if (c == 0) break;
        }
        else if (fty->buf[x] == ')')
            ++c;
    }
    if (x == -1)
    {
        *aty = s_type_unknown;
    }
    else
    {
        aty->used = fty->used - (x + 1);
        memcpy(aty->buf, fty->buf + x + 1, aty->used);
        fty->used = x + 1;
    }
}

static int typestr_dereference(struct TypeStr* src, const struct RowCol* rc)
{
    if (!src->used) return 0;

    int is = src->used - 1;
    char cs = src->buf[is];
    if (cs == 'c')
    {
        --is;
        if (is < 0) abort();
        cs = src->buf[is];
    }
    if (cs == 'p')
    {
        src->used = is;
        return 0;
    }
    if (cs == ']')
    {
        // convert array to pointer
        do
        {
            --is;
            if (is < 0) abort();
            cs = src->buf[is];
        } while (cs != '[');
        src->used = is;
        return 0;
    }
    char buf[64];
    typestr_format_english(src, buf, sizeof(buf));
    return parser_ferror(rc, "error: expected pointer but got '%s'\n", buf);
}

static void typestr_decay(struct TypeStr* ts)
{
    if (!ts->used) return;

    int it = ts->used - 1;
    char ct = ts->buf[it];
    if (ct == 'c')
    {
        --it;
        if (it < 0) abort();
        ct = ts->buf[it];
    }
    if (ct == ']')
    {
        // convert array to pointer
        --it;
        if (it < 0) abort();
        ct = ts->buf[it];

        do
        {
            --it;
            if (it < 0) abort();
            ct = ts->buf[it];
        } while (ct != '[');
        ts->buf[it] = 'p';
        ts->used = it + 1;
    }
}

static int typestr_unify_decay_scalar(struct TypeStr* ts, const struct RowCol* rc)
{
    if (!ts->used) return 0;

    int it = ts->used - 1;
    char ct = ts->buf[it];
    if (ct == 'c')
    {
        --it;
        if (it < 0) abort();
        ct = ts->buf[it];
    }
    char ch = ts->buf[ts->used - 1];
    if (ch == 'p' || ch == 'I' || ch == 'C' || ch == ']')
    {
        return 0;
    }
    char buf[64];
    typestr_format_english(ts, buf, sizeof(buf));

    return parser_ferror(rc, "error: unexpected type, expected scalar type (e.g. int or pointer) but got '%s'\n", buf);
}

static int typestr_unify_decay(struct TypeStr* tgt, const struct TypeStr* src, const struct RowCol* rc)
{
    if (!tgt->used || !src->used) return 0;

    int is = src->used - 1;
    char cs = src->buf[is];
    if (cs == 'c')
    {
        --is;
        if (is < 0) abort();
        cs = src->buf[is];
    }

    int it = tgt->used - 1;
    char ct = tgt->buf[it];
    if (ct == 'c')
    {
        --it;
        if (it < 0) abort();
        ct = tgt->buf[it];
    }

    if (ct == 'p' && cs == 'p')
    {
        --it;
        if (it < 0) abort();
        ct = tgt->buf[it];

        --is;
        if (is < 0) abort();
        cs = src->buf[is];
    }
    else if (ct == 'p' && cs == ']')
    {
        // convert array to pointer
        --it;
        if (it < 0) abort();
        ct = tgt->buf[it];

        do
        {
            --is;
            if (is < 0) abort();
            cs = src->buf[is];
        } while (cs != '[');
        --is;
        if (is < 0) abort();
        cs = src->buf[is];
    }

    // trim one more layer of const off; 'int*' is convertible to 'const int*'
    if (ct == 'c' && cs != 'c')
    {
        --it;
        if (it < 0) abort();
        ct = tgt->buf[it];
    }

    if (it != is || memcmp(src->buf, tgt->buf, it + 1) != 0)
    {
        char buf[64];
        char buf2[64];
        typestr_format_english(tgt, buf, sizeof(buf));
        typestr_format_english(src, buf2, sizeof(buf2));

        return parser_ferror(rc, "internal error: unexpected type, expected '%s' but got '%s'\n", buf, buf2);
    }
    return 0;
}

static int operator_is_relation(const char* op)
{
    return (op[0] == '<' || op[0] == '>') || ((op[0] == '=' || op[0] == '!') && op[1] == '=');
}

static void elaborate_expr(struct Elaborator* elab,
                           struct ElaborateDeclCtx* ctx,
                           struct Expr* top_expr,
                           struct TypeStr* rty)
{
    void* top = top_expr;
    switch (top_expr->kind)
    {
        case STMT_NONE:
        case STMT_BREAK:
        case STMT_CONTINUE:
        case STMT_GOTO: *rty = s_type_unknown; return;
        case STMT_LABEL:
        {
            struct StmtLabel* expr = top;
            return elaborate_expr(elab, ctx, expr->stmt, rty);
        }
        case EXPR_LIT:
        {
            struct ExprLit* expr = top;
            if (expr->tok->type == LEX_NUMBER)
                *rty = s_type_literal_int;
            else if (expr->tok->type == LEX_STRING)
                *rty = s_type_literal_cstr;
            else
                abort();
            return;
        }

        case EXPR_SYM:
        {
            struct ExprSym* esym = top;
            struct Decl* callee = decl_get_def(esym->sym->decl);
            if (callee->is_function && !callee->attr.asmstr)
            {
                // reference non-builtin function
                struct Decl** decls_arr = elab->callees_seqs.data;
                decls_arr += ctx->callees_span.offset;
                if (!array_find_ptr(decls_arr, ctx->callees_span.extent * sizeof(struct Decl*), callee))
                {
                    ctx->callees_span.extent++;
                    array_push_ptr(&elab->callees_seqs, callee);
                }
            }
            else if (typestr_is_array(&callee->sym.type))
            {
                callee->sym.address_taken = 1;
                if (callee->sym.decl->parent_decl)
                {
                    if (callee->sym.decl->parent_decl != ctx->decl)
                    {
                        parser_ice(&s_elab_unknown_rc);
                        return abort();
                    }
                    ctx->decl->takes_local_addresses = 1;
                }
            }
            *rty = callee->sym.type;
            return;
        }
        case EXPR_CAST:
        {
            struct ExprCast* expr = top;
            elaborate_expr(elab, ctx, expr->expr, rty);
            *rty = expr->type->sym.type;
            return;
        }
        case STMT_RETURN:
        {
            struct StmtReturn* stmt = top;
            if (stmt->expr) elaborate_expr(elab, ctx, stmt->expr, rty);
            return;
        }
        case STMT_IF:
        {
            struct StmtIf* stmt = top;
            if (stmt->else_body)
            {
                elaborate_expr(elab, ctx, stmt->else_body, rty);
            }
            elaborate_expr(elab, ctx, stmt->if_body, rty);
            elaborate_expr(elab, ctx, stmt->cond, rty);
            return;
        }
        case EXPR_CALL:
        {
            struct ExprCall* expr = top;
            if (!is_builtin_fn_expr(expr->fn))
            {
                ctx->calls_non_builtins = 1;
            }
            elaborate_expr(elab, ctx, expr->fn, rty);
            if (!rty->used || rty->buf[rty->used - 1] != ')')
            {
                parser_ferror(&expr->tok->rc, "error: expected function type but got '%.*s'\n", rty->used, rty->buf);
                return;
            }
            rty->used--;
            struct TypeStr aty;
            struct TypeStr expected_aty;
            for (size_t i = expr->extent; i > 0; --i)
            {
                typestr_pop_arg(rty, &expected_aty);
                struct Expr** exprs = elab->p->expr_seqs.data;
                elaborate_expr(elab, ctx, exprs[expr->offset + i - 1], &aty);
                typestr_unify_decay(&expected_aty, &aty, &expr->tok->rc);
                if (rty->used > 0)
                {
                    if (rty->buf[rty->used - 1] == ',')
                    {
                        rty->used--;
                        continue;
                    }
                    else if (rty->buf[rty->used - 1] == '(')
                        break;
                }
                abort();
            }
            if (rty->used == 0) abort();
            if (rty->buf[rty->used - 1] != '(')
            {
                size_t count = expr->extent;
                while (rty->buf[rty->used - 1] != '(')
                {
                    typestr_pop_arg(rty, &expected_aty);
                    ++count;
                }
                parser_tok_error(expr->tok,
                                 "error: too few arguments in function call: got %zu, expected %zu\n",
                                 expr->extent,
                                 count);
                if (expr->fn->kind == EXPR_SYM)
                {
                    struct ExprSym* sym = (struct ExprSym*)expr->fn;
                    parser_tok_error(sym->tok, "       in call to %s\n", sym->sym->decl->name);
                }
                rty->used--;
                return;
            }
            rty->used--;
            return;
        }
        case EXPR_OP:
        {
            struct ExprOp* e = top;
            const char* op = token_str(elab->p, e->tok);
            elaborate_expr(elab, ctx, e->lhs, rty);
            struct TypeStr rhs_ty;
            if (e->rhs)
                elaborate_expr(elab, ctx, e->rhs, &rhs_ty);
            else
                rhs_ty = s_type_unknown;
            if (op[0] == '*' && op[1] == '\0' && !e->rhs)
            {
                typestr_dereference(rty, &e->tok->rc);
            }
            else if ((op[0] == '/' || op[0] == '%' || op[0] == '*') && op[1] == '\0')
            {
                typestr_unify_decay(&s_type_int, rty, &e->tok->rc);
                typestr_unify_decay(&s_type_int, &rhs_ty, &e->tok->rc);
            }
            else if (op[0] == '!' && op[1] == '\0')
            {
                typestr_decay(rty);
                typestr_unify_decay_scalar(rty, &e->tok->rc);
                *rty = s_type_int;
            }
            else if (op[0] == '+' && op[1] == '\0')
            {
                typestr_decay(rty);
                typestr_decay(&rhs_ty);
                if (typestr_is_pointer(rty))
                {
                    typestr_unify_decay(&s_type_int, &rhs_ty, &e->tok->rc);
                }
                else if (typestr_is_pointer(&rhs_ty))
                {
                    typestr_unify_decay(&s_type_int, rty, &e->tok->rc);
                    *rty = rhs_ty;
                }
                else
                {
                    typestr_unify_decay(&s_type_int, rty, &e->tok->rc);
                    typestr_unify_decay(&s_type_int, &rhs_ty, &e->tok->rc);
                }
            }
            else if (op[0] == '-' && op[1] == '\0')
            {
                typestr_unify_decay(rty, &rhs_ty, &e->tok->rc);
            }
            else if (operator_is_relation(op))
            {
                typestr_unify_decay(rty, &rhs_ty, &e->tok->rc);
                *rty = s_type_int;
            }
            else if ((op[0] == '&' || op[0] == '|') && op[1] == op[0])
            {
                typestr_unify_decay(&s_type_int, rty, &e->tok->rc);
                typestr_unify_decay(&s_type_int, &rhs_ty, &e->tok->rc);
            }
            else if (op[0] == '=' && op[1] == '\0')
            {
                typestr_unify_decay(rty, &rhs_ty, &e->tok->rc);
            }
            else if ((op[0] == '+' || op[0] == '-') && op[1] == op[0])
            {
                if (!rty->used) return;
                char ch = rty->buf[rty->used - 1];
                if (ch == 'I' || ch == 'p')
                    ;
                else
                {
                    char buf[64];
                    typestr_format_english(rty, buf, sizeof(buf));

                    parser_ferror(&e->tok->rc, "error: expected mutable incrementable type but got '%s'\n", buf);
                    return;
                }
            }
            else if ((op[0] == '+' || op[0] == '-') && op[1] == '=')
            {
                typestr_unify_decay(&s_type_int, rty, &e->tok->rc);
                typestr_unify_decay(&s_type_int, &rhs_ty, &e->tok->rc);
            }
            else if (op[0] == '&' && op[1] == '\0')
            {
                typestr_add_pointer(rty);
                if (e->lhs->kind != EXPR_SYM)
                {
                    parser_ferror(&e->tok->rc, "error: attempting to take address of non-symbol\n");
                    return;
                }
                struct ExprSym* lhs_sym = (struct ExprSym*)e->lhs;
                lhs_sym->sym->address_taken = 1;
                if (lhs_sym->sym->decl->parent_decl)
                {
                    if (lhs_sym->sym->decl->parent_decl != ctx->decl)
                    {
                        parser_ice(&s_elab_unknown_rc);
                        return abort();
                    }
                    ctx->decl->takes_local_addresses = 1;
                }
            }
            else if (op[0] == '[' && op[1] == '\0')
            {
                typestr_dereference(rty, &e->tok->rc);
                typestr_unify_decay(&s_type_int, &rhs_ty, &e->tok->rc);
            }
            else
                fprintf(stderr, "Warning: untyped operator '%s'\n", op);
            return;
        }
        case STMT_LOOP:
        {
            struct StmtLoop* e = top;
            elaborate_expr(elab, ctx, e->body, rty);
            if (e->advance) elaborate_expr(elab, ctx, e->advance, rty);
            elaborate_expr(elab, ctx, e->cond, rty);
            if (e->init) elaborate_expr(elab, ctx, e->init, rty);
            return;
        }
        case AST_DECL:
        {
            struct Decl* d = top;
            *rty = s_type_unknown;
            if (d->is_function) return;
            if (d->init)
            {
                elaborate_expr(elab, ctx, d->init, rty);
                typestr_unify_decay(&d->sym.type, rty, &d->id->rc);
            }
            return;
        }
        case STMT_DECLS:
        {
            struct StmtDecls* stmt = top;
            for (size_t i = 0; i < stmt->extent; ++i)
            {
                struct Expr** exprs = elab->p->expr_seqs.data;
                elaborate_expr(elab, ctx, exprs[stmt->offset + i], rty);
            }
            *rty = s_type_unknown;
            return;
        }
        case STMT_BLOCK:
        {
            struct StmtBlock* stmt = top;
            for (size_t i = 0; i < stmt->extent; ++i)
            {
                struct Expr** exprs = elab->p->expr_seqs.data;
                elaborate_expr(elab, ctx, exprs[stmt->offset + i], rty);
            }
            *rty = s_type_unknown;
            return;
        }
        default: fprintf(stderr, "unknown ast kind: %d\n", top_expr->kind); abort();
    }
}

static int elaborate_local_decl(struct Elaborator* elab, struct Decl* decl)
{
    if (elab->fdebug)
    {
        fprintf(
            elab->fdebug, "Decl: %s: %.*s\n", token_str(elab->p, decl->id), decl->sym.type.used, decl->sym.type.buf);
    }

    if (!decl->is_function)
    {
        if (decl->init)
        {
            if (decl->init->kind != EXPR_LIT)
            {
                return parser_ferror(&decl->id->rc, "error: global initializers must be constant literals\n");
            }
        }
        return 0;
    }

    if (!decl->init) return 0;
    decl->elab_index = elab->fns.sz / sizeof(void*);
    array_push_ptr(&elab->fns, decl);

    struct ElaborateDeclCtx ctx = {
        .decl = decl,
        .callees_span = {.offset = elab->callees_seqs.sz / sizeof(struct Decl*)},
    };

    struct TypeStr ty;
    elaborate_expr(elab, &ctx, decl->init, &ty);
    if (parser_has_errors()) return 1;

    array_push(&elab->callees_spans, &ctx.callees_span, sizeof(ctx.callees_span));

    if (!ctx.calls_non_builtins || decl->attr.is_nonreentrant)
    {
        decl->is_nonreentrant = 1;
    }
    return 0;
}

static int dfs_emit(const struct ArrSpan* edgespans, const int* edges, int* out_indexes, char* visited, int root)
{
    if (visited[root]) return 0;
    int emitted = 0;
    visited[root] = 1;
    const struct ArrSpan span = edgespans[root];
    for (size_t i = 0; i < span.extent; ++i)
    {
        emitted += dfs_emit(edgespans, edges, out_indexes + emitted, visited, edges[span.offset + i]);
    }
    out_indexes[emitted++] = root;
    return emitted;
}

static int elaborate_decls(struct Elaborator* elab, struct Expr** declstmts, size_t count)
{
    struct Expr** const seqs = elab->p->expr_seqs.data;
    for (size_t i = 0; i < count; ++i)
    {
        if (declstmts[i]->kind != STMT_DECLS) abort();
        struct StmtDecls* decls = (struct StmtDecls*)declstmts[i];
        for (size_t j = 0; j < decls->extent; ++j)
        {
            if (seqs[decls->offset + j]->kind != AST_DECL) abort();
            struct Decl* decl = (struct Decl*)seqs[decls->offset + j];
            if (elaborate_local_decl(elab, decl)) return 1;
            if (!elab->main && 0 == strcmp(token_str(elab->p, decl->id), "main"))
            {
                elab->main = decl_get_def(decl);
            }
        }
    }

    // walk callee graph to determine reentrancy
    // https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
    struct Decl** const fns = elab->fns.data;
    const size_t n_fns = elab->fns.sz / sizeof(void*);
    const size_t n_edges = elab->callees_seqs.sz / sizeof(void*);
    if (n_fns > 0)
    {
        struct ArrSpan* edgespans = elab->callees_spans.data;
        struct ArrSpan* rev_edgespans = my_malloc(sizeof(struct ArrSpan) * n_fns);
        memset(rev_edgespans, 0, sizeof(struct ArrSpan) * n_fns);
        int* edges = my_malloc(n_edges * sizeof(int));
        {
            struct Decl** edges_decls = elab->callees_seqs.data;
            for (size_t i = 0; i < n_edges; ++i)
            {
                // convert decls to indexes
                edges[i] = edges_decls[i]->elab_index;
                // count 'in' edges
                rev_edgespans[edges[i]].extent++;
            }
        }
        for (size_t i = 1; i < n_fns; ++i)
        {
            rev_edgespans[i].offset = rev_edgespans[i - 1].offset + rev_edgespans[i - 1].extent;
            rev_edgespans[i - 1].extent = 0;
        }
        rev_edgespans[n_fns - 1].extent = 0;
        int* rev_edges = my_malloc(n_edges * sizeof(int));
        char* is_reentrant = my_malloc(n_fns);
        memset(is_reentrant, 0, n_fns);
        for (size_t i = 0; i < n_fns; ++i)
        {
            const struct ArrSpan span = edgespans[i];
            for (size_t j = 0; j < span.extent; ++j)
            {
                int k = edges[span.offset + j];
                if (k == i) is_reentrant[i] = 1; // recursive
                struct ArrSpan* rev_span = rev_edgespans + k;
                rev_edges[rev_span->offset + rev_span->extent++] = i;
            }
        }

        char* arr_visited = my_malloc(n_fns);
        memset(arr_visited, 0, n_fns);
        int* arr_toposort = my_malloc(sizeof(int) * n_fns);
        int arr_toposort_offset = 0;
        for (size_t i = 0; i < n_fns; ++i)
        {
            arr_toposort_offset += dfs_emit(edgespans, edges, arr_toposort + arr_toposort_offset, arr_visited, i);
        }
        if (arr_toposort_offset != n_fns) abort();

        // dfs with reverse edges in reverse toposort order to find connected components
        memset(arr_visited, 0, n_fns);
        int* arr_rev_toposort = my_malloc(sizeof(int) * n_fns);
        for (size_t i = n_fns; i > 0; --i)
        {
            int emitted = dfs_emit(rev_edgespans, rev_edges, arr_rev_toposort, arr_visited, arr_toposort[i - 1]);
            if (emitted > 1)
            {
                for (size_t j = 0; j < emitted; ++j)
                {
                    is_reentrant[arr_rev_toposort[j]] = 1;
                }
            }
        }
        elab->all_stackless = 1;
        for (size_t i = 0; i < n_fns; ++i)
        {
            if (!is_reentrant[i])
            {
                fns[i]->is_nonreentrant = 1;
            }
            if (fns[i]->is_nonreentrant && !fns[i]->takes_local_addresses)
            {
                fns[i]->is_stackless = 1;
            }
            else
            {
                elab->all_stackless = 0;
            }
        }

        free(arr_visited);
        free(arr_rev_toposort);
        free(arr_toposort);
        free(is_reentrant);
        free(rev_edges);
        free(edges);
        free(rev_edgespans);
    }

    return 0;
}

void elaborator_init(struct Elaborator* elab, struct Parser* p)
{
    memset(elab, 0, sizeof(struct Elaborator));
    elab->p = p;
}

int elaborate(struct Elaborator* elab)
{
    struct Parser* const p = elab->p;
    return elaborate_decls(elab, p->arr_exprs.data, p->arr_exprs.sz / sizeof(struct Expr*));
}