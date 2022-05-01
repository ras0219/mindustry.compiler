#include "be.h"

#include <string.h>

#include "cg.h"
#include "elaborator.h"
#include "errors.h"
#include "lexstate.h"
#include "parse.h"
#include "stdlibe.h"
#include "symbol.h"
#include "tac.h"
#include "token.h"
#include "unwrap.h"

const char* taca_to_string(enum TACAKind k)
{
#define Y(Z)                                                                                                           \
    case Z: return #Z;

    switch (k)
    {
        X_TACA_KIND(Y)
        default: abort();
    }
}
const char* taco_to_string(enum TACOKind k)
{
    switch (k)
    {
        X_TACO_KIND(Y)
        default: abort();
    }
}

#undef Y
static const struct TACAddress s_taca_void = {};

static __forceinline size_t round_to_alignment(size_t size, size_t align)
{
    size_t n = size + align - 1;
    return n - (n % align);
}

__forceinline static struct TACAddress taca_imm(size_t imm)
{
    struct TACAddress ret = {
        .kind = TACA_IMM,
        .imm = imm,
        .sizing = 8,
    };
    return ret;
}

__forceinline static struct TACAddress taca_llabel(const char* lbl)
{
    struct TACAddress ret = {
        .kind = TACA_LLABEL,
        .literal = lbl,
    };
    return ret;
}

__forceinline static struct TACAddress taca_alabel(size_t l)
{
    struct TACAddress ret = {
        .kind = TACA_ALABEL,
        .alabel = l,
    };
    return ret;
}

__forceinline static struct TACAddress taca_const_addr(size_t const_idx)
{
    struct TACAddress ret = {
        .kind = TACA_CONST,
        .is_addr = 1,
        .const_idx = const_idx,
        .sizing = 8,
    };
    return ret;
}

void be_init(struct BackEnd* be, struct Parser* p, struct Elaborator* e, struct CodeGen* cg)
{
    memset(be, 0, sizeof(struct BackEnd));
    be->parser = p;
    be->elab = e;
    be->cg = cg;
    be->debug_taces = 1;
}

static struct TACAddress be_push_tace(struct BackEnd* be, const struct TACEntry* e, int32_t sizing)
{
    const size_t offset = array_size(&be->code, sizeof(struct TACEntry));
    array_push(&be->code, e, sizeof(struct TACEntry));
    if (e->op == TACO_ASSIGN && e->arg2.kind == TACA_VOID) abort();
    if (!e->arg1.is_addr && e->arg1.sizing > 8 && e->op != TACO_LOAD) abort();
    if (!e->arg2.is_addr && e->arg2.sizing > 8 && e->op != TACO_ASSIGN && e->op != TACO_LOAD) abort();
    if (e->arg2.sizing == 0 && (e->arg2.kind != TACA_VOID && e->arg2.kind != TACA_ALABEL))
    {
        parser_ferror(e->rc, "0 sized arg\n");
    }
    struct TACAddress ret = {
        .kind = TACA_REF,
        .ref = offset,
        .sizing = sizing,
    };
    return ret;
}

static void be_push_label(struct BackEnd* be, size_t n)
{
    struct TACEntry* entry = array_push_zeroes(&be->code, sizeof(struct TACEntry));
    entry->op = TACO_LABEL;
    entry->arg1.kind = TACA_ALABEL;
    entry->arg1.alabel = n;
}

static void be_push_jump(struct BackEnd* be, size_t n)
{
    struct TACEntry* entry = array_push_zeroes(&be->code, sizeof(struct TACEntry));
    entry->op = TACO_JUMP;
    entry->arg1.kind = TACA_ALABEL;
    entry->arg1.alabel = n;
}

static int be_compile_increment(struct BackEnd* be, struct TACAddress* addr, size_t offset)
{
    if (addr->kind == TACA_FRAME && addr->is_addr)
    {
        addr->frame_offset += offset;
        return 0;
    }
    struct TACEntry tace = {
        .op = TACO_ADD,
        .arg1 = *addr,
        .arg2 = taca_imm(8),
    };
    *addr = be_push_tace(be, &tace, addr->sizing);
    return 0;
}

static struct TACAddress be_ensure_ref(struct BackEnd* be, const struct TACAddress* in)
{
    if (in->kind == TACA_REF) return *in;
    struct TACEntry tace = {
        .op = TACO_ADD,
        .arg1 = *in,
        .arg2 = taca_imm(0),
    };
    return be_push_tace(be, &tace, in->sizing);
}

static struct TACAddress be_alloc_temp(struct BackEnd* be, int32_t sizing)
{
    struct TACAddress o = {
        .kind = TACA_FRAME,
        .sizing = sizing,
        .frame_offset = be->frame_size,
    };
    be->frame_size += sizing < 0 ? -sizing : sizing;
    if (be->max_frame_size < be->frame_size) be->max_frame_size = be->frame_size;
    return o;
}

static struct TACAddress be_deref(struct BackEnd* be, const struct TACAddress* in, int32_t sizing)
{
    if (in->is_addr)
    {
        struct TACAddress out = *in;
        out.is_addr = 0;
        out.sizing = sizing;
        return out;
    }
    else
    {
        if (in->sizing != 8) abort();
        struct TACAddress out = be_alloc_temp(be, sizing);
        struct TACEntry tace = {
            .op = TACO_LOAD,
            .arg1 = out,
            .arg2 = *in,
        };
        be_push_tace(be, &tace, sizing);
        return out;
    }
}

static int be_dereference(struct BackEnd* be, struct TACAddress* out, int32_t sizing)
{
    *out = be_deref(be, out, sizing);
    return 0;
}

__attribute__((unused)) static int be_addressof(struct BackEnd* be, struct TACAddress* out)
{
    int rc = 0;
    if (out->is_addr) UNWRAP(parser_tok_error(NULL, "error: cannot take address of TACA %d\n", out->kind));

    out->is_addr = 1;
fail:
    return rc;
}

static int be_compile_stmt(struct BackEnd* be, struct Ast* e);
static int be_compile_expr(struct BackEnd* be, struct Expr* e, struct TACAddress* out);
static int be_compile_lvalue(struct BackEnd* be, struct Expr* e, struct TACAddress* out);
static int be_compile_init(struct BackEnd* be, struct Ast* e, size_t frame_offset, int32_t sizing)
{
    int rc = 0;
    if (e->kind == AST_INIT)
    {
        struct AstInit* block = (void*)e;
        for (; block->init; block = block->next)
        {
            UNWRAP(be_compile_init(be, block->init, frame_offset + block->offset, block->sizing));
            // TODO: calculate proper offsets
        }
    }
    else if (!ast_kind_is_expr(e->kind))
    {
        UNWRAP(parser_tok_error(e->tok, "error: unexpected expression in initialization\n"));
    }
    else
    {
        struct TACEntry assign = {
            .op = TACO_ASSIGN,
            .arg1 =
                {
                    .kind = TACA_FRAME,
                    .is_addr = 1,
                    .frame_offset = frame_offset,
                    .sizing = sizing,
                },
            .rc = &e->tok->rc,
        };
        UNWRAP(be_compile_expr(be, (struct Expr*)e, &assign.arg2));
        be_push_tace(be, &assign, 0);
    }
fail:
    return rc;
}
static int be_compile_stmts(struct BackEnd* be, size_t offset, size_t extent)
{
    int rc = 0;
    void** seqs = be->parser->expr_seqs.data;
    seqs += offset;
    for (size_t i = 0; i < extent; ++i)
    {
        UNWRAP(be_compile_stmt(be, seqs[i]));
    }
fail:
    return rc;
}

static int be_compile_StmtDecls(struct BackEnd* be, struct StmtDecls* stmt)
{
    return be_compile_stmts(be, stmt->offset, stmt->extent);
}

static int be_compile_StmtBlock(struct BackEnd* be, struct StmtBlock* stmt)
{
    int rc = 0;
    int start_frame_size = be->frame_size;
    UNWRAP(be_compile_stmts(be, stmt->offset, stmt->extent));

fail:
    be->frame_size = start_frame_size;
    return rc;
}

/// <returns>index of matching string, one past the end on failure</returns>
static size_t asz_find(struct Array* stringpool, const char* str)
{
    size_t const sz = stringpool->sz / sizeof(const char*);
    const char* const* const data = (const char* const*)stringpool->data;
    for (size_t i = 0; i < sz; ++i)
    {
        if (strcmp(data[i], str) == 0) return i;
    }
    return sz;
}

static int be_compile_ExprLit(struct BackEnd* be, struct ExprLit* e, struct TACAddress* out)
{
    int rc = 0;

    if (e->tok->type == LEX_NUMBER || e->tok->type == LEX_CHARLIT)
    {
        *out = taca_imm(e->numeric);
    }
    else if (e->tok->type == LEX_STRING)
    {
        const char* const s = token_str(be->parser, e->tok);
        const size_t i = asz_find(&be->aszConstants, s);
        const size_t n = array_size(&be->aszConstants, sizeof(void*));

        if (i == n)
        {
            // not found, append and emit new constant
            array_push_ptr(&be->aszConstants, (void*)s);
            cg_string_constant(be->cg, i, s);
        }

        *out = taca_const_addr(i);
    }
    else
    {
        rc = 1;
        parser_tok_error(e->tok, "error: unimplemented literal type (%d)\n", e->tok->type);
    }

    return rc;
}

static int be_compile_lvalue_ExprLit(struct BackEnd* be, struct ExprLit* e, struct TACAddress* out)
{
    int rc = 0;

    if (e->tok->type == LEX_NUMBER || e->tok->type == LEX_CHARLIT)
    {
        rc = parser_tok_error(e->tok, "error: cannot take address of numeric literals\n");
    }
    else if (e->tok->type == LEX_STRING)
    {
        const char* const s = token_str(be->parser, e->tok);
        const size_t i = asz_find(&be->aszConstants, s);
        const size_t n = array_size(&be->aszConstants, sizeof(void*));

        if (i == n)
        {
            // not found, append and emit new constant
            array_push_ptr(&be->aszConstants, (void*)s);
            cg_string_constant(be->cg, i, s);
        }

        *out = taca_const_addr(i);
    }
    else
    {
        rc = 1;
        parser_tok_error(e->tok, "error: unimplemented literal type (%d)\n", e->tok->type);
    }

    return rc;
}

static int is_sym_array_or_function(Symbol* sym)
{
top:
    switch (sym->type->kind)
    {
        case AST_DECLARR: return 1;
        case AST_DECLFN: return 1;
        case AST_DECLSPEC:
            if (((struct DeclSpecs*)sym->type)->_typedef)
            {
                sym = ((struct DeclSpecs*)sym->type)->_typedef;
                goto top;
            }
            return 0;
        default: return 0;
    }
}

static int be_compile_lvalue_ExprRef(struct BackEnd* be, struct ExprRef* e, struct TACAddress* out)
{
    struct Decl* decl = e->sym->def;
    out->is_addr = 1;
    if (decl->specs->parent)
    {
        if (decl->sym->arg_index > 0)
        {
            out->kind = TACA_ARG;
            out->arg_idx = decl->sym->arg_index - 1;
        }
        else
        {
            out->kind = TACA_FRAME;
            out->frame_offset = decl->sym->frame_offset;
        }
    }
    else
    {
        out->kind = decl->specs->is_static ? TACA_LNAME : TACA_NAME;
        out->name = decl->sym->name;
    }
    out->sizing = 8;
    return 0;
}

static int be_compile_ExprRef(struct BackEnd* be, struct ExprRef* esym, struct TACAddress* out)
{
    int rc = 0;
    if (esym->sym->is_enum_constant)
    {
        out->kind = TACA_IMM;
        out->imm = esym->sym->enum_value;
    }
    else
    {
        UNWRAP(be_compile_lvalue_ExprRef(be, esym, out));
        if (!is_sym_array_or_function(esym->sym))
        {
            UNWRAP(be_dereference(be, out, esym->sizing));
        }
    }
fail:
    return rc;
}
static int be_compile_ExprCall(struct BackEnd* be, struct ExprCall* e, struct TACAddress* out)
{
    if (!e->fn) return parser_ice_tok(e->tok);

    int rc = 0;
    struct Array param_addr = {};

    struct Expr* const* const expr_seqs = (struct Expr**)be->parser->expr_seqs.data;

    for (size_t i = 0; i < e->extent; ++i)
    {
        int32_t sz = ((struct ParamConversion*)be->elab->param_conversions.data)[e->offset + i].sizing;
        struct TACAddress* arg_addr = array_alloc(&param_addr, sizeof(struct TACAddress));
        UNWRAP(be_compile_expr(be, expr_seqs[e->offset + i], arg_addr));
        if (sz > 8)
        {
            UNWRAP(be_addressof(be, arg_addr));
        }
    }
    struct TACEntry call = {
        .op = TACO_CALL,
        .arg2 = taca_imm(e->extent),
        .rc = &e->tok->rc,
    };
    UNWRAP(be_compile_expr(be, e->fn, &call.arg1));

    int ret_is_memory = e->sizing > 8 || e->sizing < -8;

    struct TACEntry param = {
        .op = TACO_PARAM,
        .arg2 = {.kind = TACA_PARAM, .sizing = 8},
        .rc = &e->tok->rc,
    };
    for (size_t i = e->extent; i > 0; --i)
    {
        param.arg1 = ((struct TACAddress*)param_addr.data)[i - 1];
        param.arg2.param_idx = i - 1 + ret_is_memory;
        be_push_tace(be, &param, 0);
    }

    *out = be_push_tace(be, &call, e->sizing);

fail:
    return rc;
}
static int be_compile_lvalue_ExprCall(struct BackEnd* be, struct ExprCall* e, struct TACAddress* out)
{
    return be_compile_ExprCall(be, e, out);
}
static int be_compile_lvalue_ExprCast(struct BackEnd* be, struct ExprCast* e, struct TACAddress* out)
{
    return be_compile_lvalue(be, e->expr, out);
}

static int be_compile_add(struct BackEnd* be, struct ExprBinOp* e, int32_t sizing, struct TACAddress* out)
{
    int rc = 0;
    struct TACEntry tace = {};
    tace.rc = &e->tok->rc;
    UNWRAP(be_compile_expr(be, e->rhs, &tace.arg2));
    if (e->info == 0)
    {
        return parser_tok_error(e->tok, "warning: unsized addition\n");
    }
    if (e->info != 1)
    {
        /* adding to pointer, e->info is size of element */
        tace.op = TACO_MULT;
        tace.arg1 = taca_imm(e->info);
        tace.arg2 = be_push_tace(be, &tace, 8);
    }
    tace.op = TACO_ADD;
    UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
    *out = be_push_tace(be, &tace, sizing);
fail:
    return rc;
}

static int be_compile_sub(struct BackEnd* be, struct ExprBinOp* e, struct TACAddress* out)
{
    int rc = 0;
    struct TACEntry tace = {};
    tace.rc = &e->tok->rc;
    UNWRAP(be_compile_expr(be, e->rhs, &tace.arg2));
    if (e->info == 0)
    {
        return parser_tok_error(e->tok, "warning: unsized subtraction\n");
    }
    if (e->info > 1)
    {
        tace.op = TACO_MULT;
        tace.arg1 = taca_imm(e->info);
        tace.arg2 = be_push_tace(be, &tace, e->sizing);
    }
    tace.op = TACO_SUB;
    UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
    *out = be_push_tace(be, &tace, e->sizing);
    if (e->info < 1)
    {
        tace.op = TACO_DIV;
        tace.arg1 = *out;
        tace.arg2 = taca_imm(-e->info);
        *out = be_push_tace(be, &tace, e->sizing);
    }
fail:
    return rc;
}

static int be_compile_lvalue_ExprBinOp(struct BackEnd* be, struct ExprBinOp* e, struct TACAddress* out)
{
    int rc = 0;
    struct TACEntry tace = {};
    tace.rc = &e->tok->rc;
    switch (e->tok->type)
    {
        case TOKEN_SYM2('+', '='): tace.op = TACO_ADD; goto binary_op_assign;
        case TOKEN_SYM2('-', '='): tace.op = TACO_SUB; goto binary_op_assign;
        case TOKEN_SYM2('/', '='): tace.op = TACO_DIV; goto binary_op_assign;
        case TOKEN_SYM2('&', '='): tace.op = TACO_BAND; goto binary_op_assign;
        case TOKEN_SYM2('|', '='): tace.op = TACO_BOR; goto binary_op_assign;
        case TOKEN_SYM2('^', '='): tace.op = TACO_BXOR; goto binary_op_assign;
        case TOKEN_SYM2('*', '='): tace.op = TACO_MULT; goto binary_op_assign;
        case TOKEN_SYM3('<', '<', '='): tace.op = TACO_SHL; goto binary_op_assign;
        case TOKEN_SYM3('>', '>', '='): tace.op = TACO_SHR; goto binary_op_assign;
        case TOKEN_SYM1('='):
            tace.op = TACO_ASSIGN;
            UNWRAP(be_compile_expr(be, e->rhs, &tace.arg2));
            UNWRAP(be_compile_lvalue(be, e->lhs, out));
            tace.arg1 = *out;
            be_push_tace(be, &tace, e->sizing);
            goto fail;

        case TOKEN_SYM1('['): return be_compile_add(be, e, 8, out);
        default:
            UNWRAP(parser_tok_error(
                e->tok, "error: be_compile_lvalue_ExprBinOp unimplemented op (%s)\n", token_str(be->parser, e->tok)));
    }

binary_op_assign:
    UNWRAP(be_compile_expr(be, e->rhs, &tace.arg2));
    UNWRAP(be_compile_lvalue(be, e->lhs, out));
    tace.arg1 = be_deref(be, out, e->lhs->sizing);
    tace.arg2 = be_push_tace(be, &tace, e->sizing);
    tace.arg1 = *out;
    tace.op = TACO_ASSIGN;
    be_push_tace(be, &tace, e->sizing);
    goto fail;

fail:
    return rc;
}

static int be_compile_lvalue_ExprUnOp(struct BackEnd* be, struct ExprUnOp* e, struct TACAddress* out)
{
    int rc = 0;
    struct TACEntry tace = {};
    tace.rc = &e->tok->rc;
    switch (e->tok->type)
    {
        case TOKEN_SYM1('*'): UNWRAP(be_compile_expr(be, e->lhs, out)); goto fail;
        default:
            UNWRAP(parser_tok_error(
                e->tok, "error: be_compile_lvalue_ExprUnOp unimplemented op (%s)\n", token_str(be->parser, e->tok)));
    }

    (void)tace;

fail:
    return rc;
}

static int be_compile_ExprBuiltin(struct BackEnd* be, struct ExprBuiltin* e, struct TACAddress* out)
{
    int rc = 0;
    struct TACEntry tace = {.rc = &e->tok->rc};
    switch (e->tok->type)
    {
        case LEX_UUVA_START:
            // see https://uclibc.org/docs/psABI-x86_64.pdf
            tace.op = TACO_ASSIGN;
            tace.arg2.kind = TACA_FRAME;
            tace.arg2.is_addr = 1;
            tace.arg2.frame_offset = 0;
            tace.arg2.sizing = 8;
            UNWRAP(be_compile_lvalue(be, e->expr1, &tace.arg1));
            if (tace.arg1.kind != TACA_FRAME || !tace.arg1.is_addr)
                UNWRAP(
                    parser_tok_error(e->tok, "error: first argument of __builtin_va_start must be a local variable\n"));
            be_push_tace(be, &tace, 0);
            // Now initialize the va_list structure
            tace.arg1.frame_offset = 0;
            tace.arg1.sizing = 4;
            tace.arg2 = taca_imm(be->cur_fn->extent * 8);
            be_push_tace(be, &tace, 0);
            tace.arg1.frame_offset += 4;
            tace.arg1.sizing = 4;
            // TODO: fix floating point
            tace.arg2 = taca_imm(48);
            be_push_tace(be, &tace, 0);
            tace.arg1.frame_offset += 4;
            tace.arg1.sizing = 8;
            tace.arg2.kind = TACA_ARG;
            tace.arg2.is_addr = 1;
            tace.arg2.arg_idx = 6;
            be_push_tace(be, &tace, 0);
            tace.arg1.frame_offset += 8;
            tace.arg1.sizing = 8;
            tace.arg2.arg_idx = 0;
            be_push_tace(be, &tace, 0);
            *out = s_taca_void;
            break;
        case LEX_UUVA_END: *out = s_taca_void; break;
        case LEX_SIZEOF: *out = taca_imm(e->sizeof_size); break;
        default:
            UNWRAP(parser_tok_error(
                e->tok, "error: builtin operation not yet implemented: %s\n", token_str(be->parser, e->tok)));
            break;
    }
fail:
    return rc;
}

static int be_compile_lvalue_ExprBuiltin(struct BackEnd* be, struct ExprBuiltin* e, struct TACAddress* out)
{
    int rc = 0;
    switch (e->tok->type)
    {
        default:
            UNWRAP(parser_tok_error(
                e->tok, "error: lvalue builtin operation not yet implemented: %s\n", token_str(be->parser, e->tok)));
            break;
    }
fail:
    return rc;
}

static int be_compile_ExprUnOp(struct BackEnd* be, struct ExprUnOp* e, struct TACAddress* out)
{
    int rc = 0;
    if (!e->lhs) return parser_ice_tok(e->tok);

    struct TACAddress lhs_lvalue = {};
    struct TACEntry tace = {};
    tace.rc = &e->tok->rc;
    switch (e->tok->type)
    {
        case TOKEN_SYM2('-', '-'): tace.op = TACO_SUB; goto prepost_inc;
        case TOKEN_SYM2('+', '+'):
            tace.op = TACO_ADD;
        prepost_inc:
            UNWRAP(be_compile_lvalue(be, e->lhs, &lhs_lvalue));
            tace.arg1 = be_deref(be, &lhs_lvalue, e->lhs->sizing);
            if (e->info)
            {
                // postincrement
                *out = be_ensure_ref(be, &tace.arg1);
            }
            tace.arg2 = taca_imm(1);
            tace.arg2 = be_push_tace(be, &tace, e->sizing);
            tace.op = TACO_ASSIGN;
            tace.arg1 = lhs_lvalue;
            if (e->info)
            {
                be_push_tace(be, &tace, e->sizing);
            }
            else
            {
                // preincrement
                *out = be_push_tace(be, &tace, e->sizing);
            }
            break;
        case TOKEN_SYM1('!'):
            tace.op = TACO_EQ;
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
            tace.arg2 = taca_imm(0);
            *out = be_push_tace(be, &tace, e->sizing);
            break;
        case TOKEN_SYM1('-'):
            tace.op = TACO_SUB;
            tace.arg1 = taca_imm(0);
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg2));
            *out = be_push_tace(be, &tace, e->sizing);
            break;
        case TOKEN_SYM1('~'):
            tace.op = TACO_BNOT;
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
            *out = be_push_tace(be, &tace, e->sizing);
            break;
        case TOKEN_SYM1('*'):
            UNWRAP(be_compile_expr(be, e->lhs, out));
            UNWRAP(be_dereference(be, out, e->sizing));
            break;
        case TOKEN_SYM1('&'): UNWRAP(be_compile_lvalue(be, e->lhs, out)); break;
        default:
            UNWRAP(parser_tok_error(
                e->tok, "error: unary operation not yet implemented: %s\n", token_str(be->parser, e->tok)));
            break;
    }

fail:
    return rc;
}

static int be_compile_ExprBinOp(struct BackEnd* be, struct ExprBinOp* e, struct TACAddress* out)
{
    int rc = 0;
    if (!e->lhs || !e->rhs) return parser_ice_tok(e->tok);

    struct TACEntry tace = {};
    tace.rc = &e->tok->rc;
    switch (e->tok->type)
    {
        case TOKEN_SYM2('>', '='): tace.op = TACO_LTEQ; goto swapped_binary;
        case TOKEN_SYM1('>'): tace.op = TACO_LT; goto swapped_binary;
        case TOKEN_SYM2('<', '='): tace.op = TACO_LTEQ; goto basic_binary;
        case TOKEN_SYM1('<'): tace.op = TACO_LT; goto basic_binary;
        case TOKEN_SYM2('=', '='): tace.op = TACO_EQ; goto basic_binary;
        case TOKEN_SYM2('!', '='): tace.op = TACO_NEQ; goto basic_binary;
        case TOKEN_SYM1('*'): tace.op = TACO_MULT; goto basic_binary;
        case TOKEN_SYM1('+'): return be_compile_add(be, e, e->sizing, out);
        case TOKEN_SYM1('-'): return be_compile_sub(be, e, out);
        case TOKEN_SYM1('/'): tace.op = TACO_DIV; goto basic_binary;
        case TOKEN_SYM1('%'): tace.op = TACO_MOD; goto basic_binary;
        case TOKEN_SYM1('|'): tace.op = TACO_BOR; goto basic_binary;
        case TOKEN_SYM1('&'): tace.op = TACO_BAND; goto basic_binary;
        case TOKEN_SYM1('^'): tace.op = TACO_BXOR; goto basic_binary;
        case TOKEN_SYM2('<', '<'): tace.op = TACO_SHL; goto basic_binary;
        case TOKEN_SYM2('>', '>'): tace.op = TACO_SHR; goto basic_binary;
        case TOKEN_SYM2('+', '='):
        case TOKEN_SYM2('-', '='):
        case TOKEN_SYM2('/', '='):
        case TOKEN_SYM2('&', '='):
        case TOKEN_SYM2('|', '='):
        case TOKEN_SYM2('^', '='):
        case TOKEN_SYM2('*', '='):
        case TOKEN_SYM3('<', '<', '='):
        case TOKEN_SYM3('>', '>', '='):
        case TOKEN_SYM1('='):
        case TOKEN_SYM1('['):
            UNWRAP(be_compile_lvalue_ExprBinOp(be, e, out));
            UNWRAP(be_dereference(be, out, e->sizing));
            break;
        case TOKEN_SYM1(','):
            UNWRAP(be_compile_expr(be, e->rhs, &tace.arg2));
            UNWRAP(be_compile_expr(be, e->lhs, out));
            break;
        case TOKEN_SYM1('?'):
        {
            if (e->rhs->kind != EXPR_BINOP) abort();
            struct ExprBinOp* rhs = (struct ExprBinOp*)e->rhs;
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
            size_t on_false = ++be->next_label;
            size_t end = ++be->next_label;
            tace.op = TACO_BRZ;
            tace.arg2.kind = TACA_ALABEL;
            tace.arg2.alabel = on_false;
            be_push_tace(be, &tace, 0);
            UNWRAP(be_compile_expr(be, rhs->lhs, &tace.arg1));
            be_push_jump(be, end);
            be_push_label(be, on_false);
            UNWRAP(be_compile_expr(be, rhs->rhs, &tace.arg2));
            be_push_label(be, end);
            tace.op = TACO_PHI;
            *out = be_push_tace(be, &tace, e->sizing);
            break;
        }
        case TOKEN_SYM2('|', '|'):
        case TOKEN_SYM2('&', '&'):
        {
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
            size_t on_false = ++be->next_label;
            tace.op = e->tok->type == TOKEN_SYM2('&', '&') ? TACO_BRZ : TACO_BRNZ;
            tace.arg2.kind = TACA_ALABEL;
            tace.arg2.alabel = on_false;
            be_push_tace(be, &tace, 0);
            struct TACEntry logical = {
                .op = TACO_NEQ,
                .arg2 = taca_imm(0),
            };
            UNWRAP(be_compile_expr(be, e->rhs, &logical.arg1));
            tace.arg2 = be_push_tace(be, &logical, e->sizing);
            be_push_label(be, on_false);
            tace.op = TACO_PHI;
            *out = be_push_tace(be, &tace, e->sizing);
            break;
        }
        default:
            UNWRAP(parser_tok_error(
                e->tok, "error: binary operation not yet implemented: %s\n", token_str(be->parser, e->tok)));
            break;
    }
    goto fail;

swapped_binary:
    UNWRAP(be_compile_expr(be, e->rhs, &tace.arg1));
    UNWRAP(be_compile_expr(be, e->lhs, &tace.arg2));
    *out = be_push_tace(be, &tace, e->sizing);
    goto fail;

basic_binary:
    UNWRAP(be_compile_expr(be, e->rhs, &tace.arg2));
    UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
    *out = be_push_tace(be, &tace, e->sizing);
    goto fail;

fail:
    return rc;
}

static int be_compile_StmtContinue(struct BackEnd* be, struct StmtContinue* stmt)
{
    struct TACEntry entry = {
        .op = TACO_JUMP,
        .arg1 =
            {
                .kind = TACA_ALABEL,
                .alabel = be->continue_label,
            },
        .rc = &stmt->tok->rc,
    };
    be_push_tace(be, &entry, 0);
    return 0;
}

static int be_compile_StmtBreak(struct BackEnd* be, struct StmtBreak* stmt)
{
    struct TACEntry entry = {
        .op = TACO_JUMP,
        .arg1 =
            {
                .kind = TACA_ALABEL,
                .alabel = be->break_label,
            },
        .rc = &stmt->tok->rc,
    };
    be_push_tace(be, &entry, 0);
    return 0;
}

static int be_compile_StmtLabel(struct BackEnd* be, struct StmtLabel* stmt)
{
    struct TACEntry entry = {
        .op = TACO_LABEL,
        .arg1 = taca_llabel(token_str(be->parser, stmt->tok)),
        .rc = &stmt->tok->rc,
    };
    be_push_tace(be, &entry, 0);
    return be_compile_stmt(be, stmt->stmt);
}

static int be_compile_StmtGoto(struct BackEnd* be, struct StmtGoto* stmt)
{
    struct TACEntry entry = {
        .op = TACO_JUMP,
        .arg1 = taca_llabel(token_str(be->parser, stmt->dst)),
        .rc = &stmt->dst->rc,
    };
    be_push_tace(be, &entry, 0);
    return 0;
}

struct SwitchCase
{
    size_t imm;
    size_t label;
};

static int be_compile_StmtCase(struct BackEnd* be, struct StmtCase* stmt)
{
    if (stmt->expr)
    {
        struct SwitchCase* c = array_alloc(&be->switch_cases, sizeof(struct SwitchCase));
        c->imm = stmt->value;
        c->label = ++be->next_label;
        be_push_label(be, c->label);
    }
    else
    {
        // default
        be->default_label = ++be->next_label;
        be_push_label(be, be->default_label);
    }
    return 0;
}

static int be_compile_StmtSwitch(struct BackEnd* be, struct StmtSwitch* stmt)
{
    int rc = 0;
    int start_frame_size = be->frame_size;
    size_t prev_break = be->break_label;
    size_t switch_case_offset = array_size(&be->switch_cases, sizeof(struct SwitchCase));
    be->break_label = ++be->next_label;
    be->default_label = be->break_label;
    size_t jump_table = ++be->next_label;
    be_push_jump(be, jump_table);

    UNWRAP(be_compile_stmts(be, stmt->offset, stmt->extent));
    be_push_jump(be, be->break_label);
    be_push_label(be, jump_table);
    struct TACEntry load = {
        .op = TACO_ASSIGN,
        .arg1 = {.kind = TACA_TEMP, .sizing = 8},
        .rc = &stmt->tok->rc,
    };
    UNWRAP(be_compile_expr(be, stmt->expr, &load.arg2));
    be_push_tace(be, &load, 0);
    struct SwitchCase* cases = be->switch_cases.data;
    for (size_t i = switch_case_offset; i < array_size(&be->switch_cases, sizeof(struct SwitchCase)); ++i)
    {
        struct TACEntry _case = {
            .op = TACO_CTBZ,
            .arg1 = taca_imm(cases[i].imm),
            .arg2 = taca_alabel(cases[i].label),
        };
        be_push_tace(be, &_case, 0);
    }
    be_push_jump(be, be->default_label);
    be_push_label(be, be->break_label);

fail:
    be->frame_size = start_frame_size;
    be->break_label = prev_break;
    array_shrink(&be->switch_cases, switch_case_offset, sizeof(struct SwitchCase));
    return rc;
}

static int be_compile_StmtReturn(struct BackEnd* be, struct StmtReturn* stmt)
{
    int rc = 0;
    int start_frame_size = be->frame_size;
    struct TACEntry entry = {
        .op = TACO_RETURN,
        .rc = &stmt->tok->rc,
    };
    if (stmt->expr)
    {
        UNWRAP(be_compile_expr(be, stmt->expr, &entry.arg1));
    }
    be_push_tace(be, &entry, 0);
fail:
    be->frame_size = start_frame_size;
    return rc;
}

static int be_compile_StmtIf(struct BackEnd* be, struct StmtIf* stmt)
{
    int rc = 0;
    int start_frame_size = be->frame_size;
    size_t else_lbl = ++be->next_label;
    struct TACEntry e = {
        .op = TACO_BRZ,
        .arg2 = {.kind = TACA_ALABEL, .alabel = else_lbl},
    };
    UNWRAP(be_compile_expr(be, stmt->cond, &e.arg1));
    be_push_tace(be, &e, 0);
    be->frame_size = start_frame_size;
    UNWRAP(be_compile_stmt(be, stmt->if_body));

    if (stmt->else_body)
    {
        size_t end_lbl = ++be->next_label;
        be_push_jump(be, end_lbl);
        be_push_label(be, else_lbl);
        UNWRAP(be_compile_stmt(be, stmt->else_body));
        be_push_label(be, end_lbl);
    }
    else
    {
        be_push_label(be, else_lbl);
    }

fail:
    return rc;
}

static int be_compile_StmtLoop(struct BackEnd* be, struct StmtLoop* stmt)
{
    int rc = 0;
    size_t prev_continue_label = be->continue_label;
    size_t prev_break_label = be->break_label;
    be->continue_label = ++be->next_label;
    size_t first_lbl = ++be->next_label;
    be->break_label = ++be->next_label;

    if (stmt->init)
    {
        UNWRAP(be_compile_stmt(be, stmt->init));
    }
    if (stmt->advance || stmt->is_do_while)
    {
        be_push_jump(be, first_lbl);
    }
    be_push_label(be, be->continue_label);

    if (stmt->advance)
    {
        UNWRAP(be_compile_stmt(be, &stmt->advance->ast));
    }

    if (!stmt->is_do_while)
    {
        be_push_label(be, first_lbl);
    }
    if (stmt->cond)
    {
        int start_frame_size = be->frame_size;
        struct TACEntry brz = {
            .op = TACO_BRZ,
            .arg2 = {.kind = TACA_ALABEL, .alabel = be->break_label},
        };
        UNWRAP(be_compile_expr(be, stmt->cond, &brz.arg1));
        be_push_tace(be, &brz, 0);
        be->frame_size = start_frame_size;
    }
    if (stmt->is_do_while)
    {
        be_push_label(be, first_lbl);
    }

    UNWRAP(be_compile_stmt(be, stmt->body));
    be_push_jump(be, be->continue_label);
    be_push_label(be, be->break_label);
fail:
    be->continue_label = prev_continue_label;
    be->break_label = prev_break_label;
    return rc;
}

static int be_compile_Decl(struct BackEnd* be, struct Decl* decl)
{
    if (!decl->type) return 0;

    int rc = 0;
    decl->sym->frame_offset = round_to_alignment(be->frame_size, decl->sym->align);
    be->frame_size = decl->sym->frame_offset + decl->sym->size;
    if (decl->sym->size == 0) abort();
    be->max_frame_size = be->max_frame_size < be->frame_size ? be->frame_size : be->max_frame_size;

    if (decl->init)
    {
        struct Ast* init = decl->init;
        int start_frame_size = be->frame_size;
        UNWRAP(be_compile_init(be, init, decl->sym->frame_offset, decl->sym->size));
        be->frame_size = start_frame_size;
    }
fail:
    return rc;
}

static int be_compile_lvalue_ExprField(struct BackEnd* be, struct ExprField* e, struct TACAddress* out)
{
    int rc = 0;
    if (e->is_arrow)
    {
        UNWRAP(be_compile_expr(be, e->lhs, out));
    }
    else
    {
        UNWRAP(be_compile_lvalue(be, e->lhs, out));
    }

    if (e->sym->frame_offset != 0)
    {
        UNWRAP(be_compile_increment(be, out, e->sym->frame_offset));
    }

fail:
    return rc;
}

static int be_compile_ExprField(struct BackEnd* be, struct ExprField* e, struct TACAddress* out)
{
    int rc = 0;
    UNWRAP(be_compile_lvalue_ExprField(be, e, out));
    if (!is_sym_array_or_function(e->sym))
    {
        UNWRAP(be_dereference(be, out, e->sizing));
    }

fail:
    return rc;
}

static int be_compile_ExprCast(struct BackEnd* be, struct ExprCast* e, struct TACAddress* out)
{
    int rc = 0;
    UNWRAP(be_compile_expr(be, e->expr, out));

fail:
    return rc;
}

static int be_compile_lvalue(struct BackEnd* be, struct Expr* e, struct TACAddress* out)
{
#define DISPATCH(ENUM, TYPE)                                                                                           \
    case ENUM: return be_compile_lvalue_##TYPE(be, (struct TYPE*)e, out);

    switch (e->kind)
    {
        DISPATCH(EXPR_REF, ExprRef);
        DISPATCH(EXPR_FIELD, ExprField);
        DISPATCH(EXPR_CAST, ExprCast);
        DISPATCH(EXPR_CALL, ExprCall);
        DISPATCH(EXPR_LIT, ExprLit);
        DISPATCH(EXPR_BINOP, ExprBinOp);
        DISPATCH(EXPR_UNOP, ExprUnOp);
        DISPATCH(EXPR_BUILTIN, ExprBuiltin);
        case STMT_NONE: *out = s_taca_void; return 0;
        default:
            parser_tok_error(
                e->tok, "error: be_compile_lvalue unhandled expr: %s (%d)\n", ast_kind_to_string(e->kind), e->kind);
            return 1;
    }
    return 0;
#undef DISPATCH
}

static int be_compile_expr(struct BackEnd* be, struct Expr* e, struct TACAddress* out)
{
#define DISPATCH(ENUM, TYPE)                                                                                           \
    case ENUM: return be_compile_##TYPE(be, (struct TYPE*)e, out);

    switch (e->kind)
    {
        DISPATCH(EXPR_REF, ExprRef);
        DISPATCH(EXPR_CALL, ExprCall);
        DISPATCH(EXPR_LIT, ExprLit);
        DISPATCH(EXPR_BINOP, ExprBinOp);
        DISPATCH(EXPR_UNOP, ExprUnOp);
        DISPATCH(EXPR_BUILTIN, ExprBuiltin);
        DISPATCH(EXPR_CAST, ExprCast);
        DISPATCH(EXPR_FIELD, ExprField);
        default:
            parser_tok_error(
                e->tok, "error: be_compile_expr unhandled expr: %s (%d)\n", ast_kind_to_string(e->kind), e->kind);
            return 1;
    }
    return 0;
#undef DISPATCH
}

static int be_compile_stmt(struct BackEnd* be, struct Ast* e)
{
#define DISPATCH(ENUM, TYPE)                                                                                           \
    case ENUM: return be_compile_##TYPE(be, (struct TYPE*)e);

    switch (e->kind)
    {
        DISPATCH(STMT_DECLS, StmtDecls);
        DISPATCH(STMT_BLOCK, StmtBlock);
        DISPATCH(STMT_LOOP, StmtLoop);
        DISPATCH(STMT_IF, StmtIf);
        DISPATCH(STMT_RETURN, StmtReturn);
        DISPATCH(STMT_CONTINUE, StmtContinue);
        DISPATCH(STMT_BREAK, StmtBreak);
        DISPATCH(STMT_SWITCH, StmtSwitch);
        DISPATCH(STMT_CASE, StmtCase);
        DISPATCH(STMT_LABEL, StmtLabel);
        DISPATCH(STMT_GOTO, StmtGoto);
        DISPATCH(AST_DECL, Decl);
        case STMT_NONE: return 0;
        default:
        {
            if (ast_kind_is_expr(e->kind))
            {
                struct TACAddress _;
                return be_compile_expr(be, (struct Expr*)e, &_);
            }
            else
            {
                parser_tok_error(
                    e->tok, "error: be_compile_stmt unhandled stmt: %s (%d)\n", ast_kind_to_string(e->kind), e->kind);
                return 1;
            }
        }
    }
#undef DISPATCH
}

static void be_debug_print_taca(const struct TACAddress* addr)
{
    char buf[64];
    int i = snprintf(buf, sizeof(buf), "%-15s:%u:%d", taca_to_string(addr->kind), addr->is_addr, addr->sizing);
    if (i < sizeof(buf))
    {
        switch (addr->kind)
        {
            case TACA_CONST: snprintf(buf + i, sizeof(buf) - i, " %zu", addr->const_idx); break;
            case TACA_ALABEL: snprintf(buf + i, sizeof(buf) - i, " %zu", addr->alabel); break;
            case TACA_IMM: snprintf(buf + i, sizeof(buf) - i, " %zu", addr->imm); break;
            case TACA_FRAME: snprintf(buf + i, sizeof(buf) - i, " %zu", addr->frame_offset); break;
            case TACA_REF: snprintf(buf + i, sizeof(buf) - i, " %zu", addr->ref); break;
            case TACA_NAME: snprintf(buf + i, sizeof(buf) - i, " %s", addr->name); break;
            case TACA_PARAM: snprintf(buf + i, sizeof(buf) - i, " %zu", addr->param_idx); break;
            case TACA_ARG: snprintf(buf + i, sizeof(buf) - i, " %zu", addr->arg_idx); break;
            default: break;
        }
    }
    printf("%-25s", buf);
}

int be_compile(struct BackEnd* be)
{
    int rc = 0;

    // then compile all functions
    struct Ast* const* const ast_seqs = be->parser->expr_seqs.data;
    struct StmtBlock* const top = be->parser->top;

    for (size_t i = 0; i < top->extent; ++i)
    {
        if (ast_seqs[top->offset + i]->kind != STMT_DECLS) abort();

        struct StmtDecls* decls = (struct StmtDecls*)ast_seqs[top->offset + i];
        if (decls->specs->is_typedef) continue;
        for (size_t j = 0; j < decls->extent; ++j)
        {
            if (ast_seqs[decls->offset + j]->kind != AST_DECL) abort();
            struct Decl* decl = (struct Decl*)ast_seqs[decls->offset + j];
            struct Decl* def = decl->sym->def ? decl->sym->def : decl;
            if ((decl->type->kind == AST_DECLFN && !def->init) || def->specs->is_extern)
            {
                cg_declare_extern(be->cg, decl->sym->name);
            }
            else if (!decl->specs->is_static && !decl->specs->is_inline)
            {
                cg_declare_public(be->cg, decl->sym->name);
            }
            if (!decl->sym->name) abort();
            if (decl->type->kind == AST_DECLFN)
            {
                struct DeclFn* declfn = (struct DeclFn*)decl->type;
                if (decl->init)
                {
                    cg_mark_label(be->cg, decl->sym->name);
                    be->frame_size = 0;
                    be->max_frame_size = 0;
                    be->cur_decl = decl;
                    be->cur_fn = declfn;

                    size_t num_args = declfn->is_varargs ? 6 : declfn->extent;

                    struct TACEntry entry = {.op = TACO_ARG};
                    for (size_t i = 0; i < num_args; ++i)
                    {
                        entry.arg1.kind = TACA_ARG;
                        entry.arg1.arg_idx = i;
                        if (i < declfn->extent)
                            entry.arg1.sizing =
                                ((struct ParamConversion*)be->elab->param_conversions.data + declfn->offset + i)
                                    ->sizing;
                        else
                            entry.arg1.sizing = 8;
                        if (entry.arg1.sizing > 8 || entry.arg1.sizing < -8 || entry.arg1.sizing == 0) abort();
                        be_push_tace(be, &entry, 0);
                    }

                    if (declfn->is_varargs)
                    {
                        // reserve a va_list structure at beginning of frame
                        be->frame_size += 8 + 8 + 4 + 4;
                    }

                    UNWRAP(be_compile_stmt(be, decl->init));

                    if (be->debug_taces)
                    {
                        struct TACEntry* taces = (struct TACEntry*)be->code.data;
                        for (size_t i = 0; i < array_size(&be->code, sizeof(struct TACEntry)); ++i)
                        {
                            printf("%4zu: %-14s   ", i, taco_to_string(taces[i].op));
                            be_debug_print_taca(&taces[i].arg1);
                            printf("   ");
                            be_debug_print_taca(&taces[i].arg2);
                            printf("\n");
                        }
                    }
                    else
                    {
                        UNWRAP(cg_gen_taces(be->cg,
                                            (struct TACEntry*)be->code.data,
                                            array_size(&be->code, sizeof(struct TACEntry)),
                                            be->max_frame_size));
                    }
                    array_clear(&be->code);
                }
            }
            else if (!decl->specs->is_extern)
            {
                // global variable
                struct Array data = {};
                array_push_zeroes(&data, decl->sym->size);
                cg_reserve_data(be->cg, decl->sym->name, data.data, data.sz);
            }
        }
    }

fail:
    return rc;
}

void be_destroy(struct BackEnd* be)
{
    array_destroy(&be->aszConstants);
    autoheap_destroy(&be->const_ref);
    autoheap_destroy(&be->heap);
    scope_destroy(&be->scope);
}