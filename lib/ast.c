#include "ast.h"

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "json_dom.h"
#include "parse.h"
#include "rowcol.h"
#include "symbol.h"
#include "token.h"

int ast_kind_is_expr(enum AstKind k)
{
    static const unsigned char s_is_expr[AST_KIND_COUNT] = {
#define Y(A) [A] = 1,
        X_FOREACH_EXPR(Y)
#undef Y
    };
    return s_is_expr[k];
}

#define Y_CASE_TO_STR(Z) [Z] = #Z,

const char* ast_kind_to_string(enum AstKind k)
{
    static const char s_strings[AST_KIND_COUNT][32] = {X_AST_KIND(Y_CASE_TO_STR)};
    return s_strings[k];
#undef Y_CASE_TO_STR
}

static void ast_to_json_ast(struct Parser* p, JsonDOM* f, void* ptr, int depth);
static void ast_to_json_type_ast(struct Parser* p, JsonDOM* f, AstType* ast, int depth)
{
    if (!ast)
        jsondom_write_null(f);
    else if (ast->kind != AST_DECLSPEC)
        ast_to_json_ast(p, f, &ast->ast, depth);
    else
        jsondom_write_false(f);
}
static void ast_to_json_sizing(JsonDOM* f, Sizing sz)
{
    int64_t w = sz.width;
    if (sz.is_signed) w = -w;
    jsondom_write_i64(f, w);
}
static void ast_to_json_expr_seq(Parser* p, JsonDOM* f, SeqView seq, int depth)
{
    jsondom_write_open_arr(f);
    for (size_t i = 0; i < seq.ext; ++i)
    {
        ast_to_json_ast(p, f, ((Ast**)p->expr_seqs.data)[seq.off + i], depth + 1);
    }
    jsondom_write_close_arr(f);
}
static void ast_to_json_key_if_type(Parser* p, JsonDOM* f, const char* key, size_t keysz, void* ptr, int depth)
{
    if (ptr)
    {
        jsondom_write_key(f, key, keysz);
        ast_to_json_type_ast(p, f, ptr, depth + 1);
    }
}
static void ast_to_json_key_if(Parser* p, JsonDOM* f, const char* key, size_t keysz, void* ptr, int depth)
{
    if (ptr)
    {
        jsondom_write_key(f, key, keysz);
        ast_to_json_ast(p, f, ptr, depth + 1);
    }
}
#define CSTR(x) (x), (sizeof(x) - 1)

static void ast_to_json_key_if_bool(Parser* p, JsonDOM* f, const char* key, size_t keysz, int x)
{
    if (x)
    {
        jsondom_write_key(f, key, keysz);
        jsondom_write_true(f);
    }
}
static void ast_to_json_key_if_tok(Parser* p, JsonDOM* f, const Token* tok)
{
    if (tok)
    {
        jsondom_write_key(f, CSTR("tok"));
        jsondom_write_string(f, token_str(p, tok), tok->tok_len);
    }
}
static void ast_to_json_key_if_sizing(Parser* p, JsonDOM* f, Sizing sz)
{
    if (sz.width)
    {
        jsondom_write_key(f, CSTR("sizing"));
        ast_to_json_sizing(f, sz);
    }
}

static void ast_to_json_ast(struct Parser* p, JsonDOM* f, void* ptr, int depth)
{
    if (!ptr)
    {
        jsondom_write_null(f);
        return;
    }
    Ast* const ast = ptr;
    if (depth > 2000) abort();
    jsondom_write_open_obj(f);
    JSONDOM_WRITE_KEY(f, "id");
    const char* k = ast_kind_to_string(ast->kind);
    jsondom_write_string(f, k, strlen(k));
    switch (ast->kind)
    {
        case STMT_BLOCK:
        {
            struct StmtBlock* blk = (void*)ast;
            JSONDOM_WRITE_KEY(f, "seq");
            ast_to_json_expr_seq(p, f, blk->seq, depth);
            break;
        }
        case STMT_DECLS:
        {
            struct StmtDecls* blk = (void*)ast;
            ast_to_json_key_if(p, f, CSTR("specs"), blk->specs, depth);
            JSONDOM_WRITE_KEY(f, "seq");
            ast_to_json_expr_seq(p, f, blk->seq, depth);
            break;
        }
        case STMT_LOOP:
        {
            struct StmtLoop* blk = (void*)ast;
            ast_to_json_key_if(p, f, CSTR("init"), blk->init, depth);
            ast_to_json_key_if(p, f, CSTR("cond"), blk->cond, depth);
            ast_to_json_key_if(p, f, CSTR("adv"), blk->advance, depth);
            ast_to_json_key_if(p, f, CSTR("body"), blk->body, depth);
            break;
        }
        case STMT_IF:
        {
            struct StmtIf* blk = (void*)ast;
            JSONDOM_WRITE_KEY(f, "cond");
            ast_to_json_ast(p, f, blk->cond, depth + 1);
            JSONDOM_WRITE_KEY(f, "body");
            ast_to_json_ast(p, f, blk->if_body, depth + 1);
            ast_to_json_key_if(p, f, CSTR("else"), blk->else_body, depth);
            break;
        }
        case STMT_RETURN:
        {
            struct StmtReturn* blk = (void*)ast;
            ast_to_json_key_if(p, f, CSTR("expr"), blk->expr, depth);
            break;
        }
        case AST_DECL:
        {
            struct Decl* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            JSONDOM_WRITE_KEY(f, "type");
            ast_to_json_type_ast(p, f, blk->type, depth + 1);
            JSONDOM_WRITE_KEY(f, "decls");
            ast_to_json_expr_seq(p, f, blk->decl_list, depth);
            ast_to_json_key_if(p, f, CSTR("init"), blk->init, depth);
            break;
        }
        case AST_DECLSPEC:
        {
            DeclSpecs* blk = (void*)ast;
            ast_to_json_key_if_bool(p, f, CSTR("typedef"), blk->is_typedef);
            ast_to_json_key_if_tok(p, f, blk->tok);
            if (blk->name)
            {
                JSONDOM_WRITE_KEY(f, "name");
                jsondom_write_string(f, blk->name, strlen(blk->name));
            }
            ast_to_json_key_if_bool(p, f, CSTR("signed"), blk->is_signed);
            ast_to_json_key_if_bool(p, f, CSTR("unsigned"), blk->is_unsigned);
            ast_to_json_key_if_bool(p, f, CSTR("llong"), blk->is_longlong);
            ast_to_json_key_if_bool(p, f, CSTR("long"), blk->is_long);
            ast_to_json_key_if_bool(p, f, CSTR("short"), blk->is_short);
            ast_to_json_key_if_bool(p, f, CSTR("const"), blk->is_const);
            ast_to_json_key_if(p, f, CSTR("su"), blk->suinit, depth);
            ast_to_json_key_if(p, f, CSTR("enum"), blk->enum_init, depth);
            break;
        }
        case AST_DECLFN:
        {
            DeclFn* blk = (void*)ast;
            if (blk->is_param_list)
            {
                JSONDOM_WRITE_KEY(f, "plist");
                jsondom_write_true(f);
            }
            else
            {
                JSONDOM_WRITE_KEY(f, "args");
                ast_to_json_expr_seq(p, f, blk->seq, depth);
            }
            ast_to_json_key_if_bool(p, f, CSTR("varargs"), blk->is_varargs);
            ast_to_json_key_if_type(p, f, CSTR("type"), blk->type, depth);
            break;
        }
        case AST_DECLARR:
        {
            struct DeclArr* blk = (void*)ast;
            ast_to_json_key_if(p, f, CSTR("arity"), blk->arity, depth);
            ast_to_json_key_if_type(p, f, CSTR("type"), blk->type, depth);
            break;
        }
        case AST_DECLPTR:
        {
            struct DeclPtr* blk = (void*)ast;
            ast_to_json_key_if_type(p, f, CSTR("type"), blk->type, depth);
            break;
        }
        case EXPR_BINOP:
        {
            struct ExprBinOp* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            JSONDOM_WRITE_KEY(f, "lhs");
            ast_to_json_ast(p, f, blk->lhs, depth + 1);
            JSONDOM_WRITE_KEY(f, "rhs");
            ast_to_json_ast(p, f, blk->rhs, depth + 1);
            break;
        }
        case EXPR_ADD:
        {
            struct ExprAdd* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            JSONDOM_WRITE_KEY(f, "lhs");
            ast_to_json_ast(p, f, blk->lhs, depth + 1);
            JSONDOM_WRITE_KEY(f, "rhs");
            ast_to_json_ast(p, f, blk->rhs, depth + 1);
            if (blk->mult != 1)
            {
                JSONDOM_WRITE_KEY(f, "mult");
                jsondom_write_i64(f, blk->mult);
            }
            JSONDOM_WRITE_KEY(f, "sizing");
            ast_to_json_sizing(f, blk->sizing);
            break;
        }
        case EXPR_ASSIGN:
        {
            struct ExprAssign* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            JSONDOM_WRITE_KEY(f, "lhs");
            ast_to_json_ast(p, f, blk->lhs, depth + 1);
            JSONDOM_WRITE_KEY(f, "rhs");
            ast_to_json_ast(p, f, blk->rhs, depth + 1);
            break;
        }
        case EXPR_LIT:
        {
            struct ExprLit* blk = (void*)ast;
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            JSONDOM_WRITE_KEY(f, "value");
            jsondom_write_i64(f, blk->numeric);
            if (blk->suffix != LIT_SUFFIX_NONE)
            {
                JSONDOM_WRITE_KEY(f, "suffix");
                const char* str = suffix_to_string(blk->suffix);
                jsondom_write_string(f, str, strlen(str));
            }
            break;
        }
        case EXPR_STRLIT:
        {
            struct ExprStrLit* blk = (void*)ast;
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            break;
        }
        case EXPR_UNOP:
        {
            struct ExprUnOp* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            JSONDOM_WRITE_KEY(f, "sizeof");
            jsondom_write_u64(f, blk->sizeof_);
            JSONDOM_WRITE_KEY(f, "lhs");
            ast_to_json_ast(p, f, blk->lhs, depth + 1);
            break;
        }
        case EXPR_INCR:
        {
            struct ExprIncr* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            ast_to_json_key_if_bool(p, f, CSTR("postfix"), blk->postfix);
            JSONDOM_WRITE_KEY(f, "sizeof");
            jsondom_write_u64(f, blk->sizeof_);
            JSONDOM_WRITE_KEY(f, "lhs");
            ast_to_json_ast(p, f, blk->lhs, depth + 1);
            break;
        }
        case EXPR_DEREF:
        {
            struct ExprDeref* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            JSONDOM_WRITE_KEY(f, "lhs");
            ast_to_json_ast(p, f, blk->lhs, depth + 1);
            break;
        }
        case EXPR_ADDRESS:
        {
            struct ExprAddress* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            JSONDOM_WRITE_KEY(f, "lhs");
            ast_to_json_ast(p, f, blk->lhs, depth + 1);
            break;
        }
        case EXPR_FIELD:
        {
            struct ExprField* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            if (blk->fieldname)
            {
                JSONDOM_WRITE_KEY(f, "name");
                jsondom_write_string(f, blk->fieldname, strlen(blk->fieldname));
            }
            JSONDOM_WRITE_KEY(f, "lhs");
            ast_to_json_ast(p, f, blk->lhs, depth + 1);
            break;
        }
        case EXPR_CAST:
        {
            struct ExprCast* blk = (void*)ast;
            ast_to_json_key_if(p, f, CSTR("specs"), blk->specs, depth);
            ast_to_json_key_if_type(p, f, CSTR("type"), blk->type, depth);
            ast_to_json_key_if(p, f, CSTR("expr"), blk->expr, depth);
            break;
        }
        case EXPR_REF:
        {
            struct ExprRef* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            ast_to_json_key_if_sizing(p, f, blk->sizing);
            break;
        }
        case EXPR_CALL:
        {
            struct ExprCall* blk = (void*)ast;
            ast_to_json_key_if(p, f, CSTR("fn"), blk->fn, depth);
            JSONDOM_WRITE_KEY(f, "args");
            jsondom_write_open_arr(f);
            for (size_t i = 0; i < blk->param_extent; ++i)
            {
                ast_to_json_ast(p, f, &((CallParam*)p->callparams.data)[blk->param_offset + i].expr->ast, depth + 1);
            }
            jsondom_write_close_arr(f);
            break;
        }
        case EXPR_BUILTIN:
        {
            struct ExprBuiltin* blk = (void*)ast;
            ast_to_json_key_if_tok(p, f, blk->tok);
            ast_to_json_key_if_bool(p, f, CSTR("take_addr"), blk->take_address);
            ast_to_json_key_if_sizing(p, f, blk->sizing);
            ast_to_json_key_if(p, f, CSTR("specs"), blk->specs, depth);
            ast_to_json_key_if(p, f, CSTR("type"), blk->type, depth);
            ast_to_json_key_if(p, f, CSTR("expr1"), blk->expr1, depth);
            ast_to_json_key_if(p, f, CSTR("expr2"), blk->expr2, depth);
            JSONDOM_WRITE_KEY(f, "sizeof");
            jsondom_write_u64(f, blk->sizeof_size);
            break;
        }
        case AST_INIT:
        {
            struct AstInit* a = (void*)ast;
            JSONDOM_WRITE_KEY(f, "elems");
            jsondom_write_open_arr(f);
            while (a->init != NULL)
            {
                jsondom_write_u64(f, a->designator_offset);
                jsondom_write_u64(f, a->designator_extent);
                ast_to_json_ast(p, f, a->init, depth + 1);
                a = a->next;
            }
            jsondom_write_close_arr(f);
            break;
        }
        default: break;
    }
    jsondom_write_close_obj(f);
}

void ast_to_json(struct Parser* p, JsonDOM* f) { ast_to_json_ast(p, f, &p->top->ast, 1); }
