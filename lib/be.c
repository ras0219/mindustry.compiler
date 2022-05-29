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
const char* register_to_string(enum Register k)
{
    switch (k)
    {
        X_REGISTER(Y)
        default: abort();
    }
}
#undef Y
static const struct TACAddress s_taca_void = {};

// static const int s_ms_arg_reg[] = {REG_RCX, REG_RDX, REG_R8, REG_R9};
static const int s_sysv_arg_reg[] = {REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9};

static __forceinline size_t round_to_alignment(size_t size, size_t align)
{
    size_t n = size + align - 1;
    return n - (n % align);
}

static __forceinline Sizing min_sizing(Sizing s1, Sizing s2)
{
    Sizing ret = {.is_signed = s2.is_signed, .width = s1.width < s2.width ? s1.width : s2.width};
    return ret;
}

static __forceinline size_t be_frame_alloc(struct BackEnd* be, size_t size, size_t align)
{
    size_t r = round_to_alignment(be->frame_size, align);
    be->frame_size = r + size;
    if (be->max_frame_size < be->frame_size) be->max_frame_size = be->frame_size;
    return r;
}

__forceinline static struct TACAddress taca_imm(size_t imm)
{
    struct TACAddress ret = {
        .kind = TACA_IMM,
        .imm = imm,
        .sizing = {1, 4},
    };
    return ret;
}

__forceinline static struct TACAddress taca_frame(size_t offset, Sizing sizing)
{
    struct TACAddress ret = {
        .kind = TACA_FRAME,
        .frame_offset = offset,
        .sizing = sizing,
    };
    if (ret.sizing.width == 0)
    {
        ret.is_addr = 1;
    }
    return ret;
}

__forceinline static struct TACAddress taca_reg(size_t reg, Sizing sizing)
{
    struct TACAddress ret = {
        .kind = TACA_REG,
        .reg = reg,
        .sizing = sizing,
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
    };
    return ret;
}

void be_init(struct BackEnd* be, struct Parser* p, struct Elaborator* e, struct CodeGen* cg)
{
    memset(be, 0, sizeof(struct BackEnd));
    be->parser = p;
    be->elab = e;
    be->cg = cg;
}

static struct TACAddress be_push_tace(struct BackEnd* be, const struct TACEntry* e, Sizing sizing)
{
    const size_t offset = array_size(&be->code, sizeof(struct TACEntry));
    array_push(&be->code, e, sizeof(struct TACEntry));
    if (e->op == TACO_ASSIGN)
    {
        if (e->arg2.kind == TACA_VOID) abort();
        if (e->arg1.sizing.width == 0) abort();
    }
    else if (e->arg1.is_addr)
    {
        if (e->arg1.kind == TACA_REG && e->op != TACO_ASSIGN) abort();
        if (e->arg1.sizing.width != 0) abort();
    }

    if (e->arg2.is_addr)
    {
        if (e->arg2.kind == TACA_REG) abort();
        if (e->arg2.sizing.width != 0) abort();
    }
    else if (e->arg2.sizing.width == 0 && (e->arg2.kind != TACA_VOID && e->arg2.kind != TACA_ALABEL))
    {
        parser_ferror(e->rc, "0 sized arg\n");
        abort();
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

static const Sizing s_sizing_pointer = {
    .is_signed = 0,
    .width = 8,
};
static const Sizing s_sizing_int = {
    .is_signed = 1,
    .width = 4,
};
static const Sizing s_sizing_uint = {
    .width = 4,
};
static const Sizing s_sizing_zero = {0};

static int sizing_is_pointer(Sizing s) { return s.width == 8 && s.is_signed == 0; }

static TACAddress be_increment(struct BackEnd* be, const TACAddress* addr, int offset)
{
    if (offset == 0) return *addr;
    if (addr->is_addr)
    {
        TACAddress out = *addr;
        if (addr->sizing.width != 0) abort();
        switch (addr->kind)
        {
            case TACA_FRAME: out.frame_offset += offset; return out;
            case TACA_ARG: out.arg_offset += offset; return out;
            case TACA_PARAM: out.param_offset += offset; return out;
            default: break;
        }
    }
    else if (!sizing_is_pointer(addr->sizing))
    {
        abort();
    }
    struct TACEntry tace = {
        .op = TACO_ADD,
        .arg1 = *addr,
        .arg2 = taca_imm(offset),
    };
    return be_push_tace(be, &tace, s_sizing_pointer);
}

static struct TACAddress be_alloc_temp(struct BackEnd* be, Sizing sizing)
{
    if (sizing.width == 0) abort();
    struct TACAddress o = {
        .kind = TACA_FRAME,
        .sizing = sizing,
        .frame_offset = be_frame_alloc(be, sizing.width, sizing.width < 8 ? sizing.width : 8),
    };
    return o;
}

static struct TACAddress be_ensure_ref(struct BackEnd* be, const struct TACAddress* in)
{
    if (in->kind == TACA_REF) return *in;
    if (in->is_addr || in->sizing.width <= 8)
    {
        struct TACEntry tace = {
            .op = TACO_ADD,
            .arg1 = *in,
            .arg2 = taca_imm(0),
        };
        return be_push_tace(be, &tace, in->sizing);
    }
    else
    {
        const TACAddress ret = be_alloc_temp(be, in->sizing);
        struct TACEntry tace = {
            .op = TACO_ASSIGN,
            .arg1 = ret,
            .arg2 = *in,
        };
        tace.arg1.is_addr = 1;
        be_push_tace(be, &tace, in->sizing);
        return ret;
    }
}

static TACAddress be_deref(struct BackEnd* be, const TACAddress* in, Sizing sizing)
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
        if (!sizing_is_pointer(in->sizing)) abort();
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

/// x->field
static TACAddress be_increment_deref(struct BackEnd* be, const TACAddress* in, int offset, Sizing sizing)
{
    const TACAddress addr1 = be_increment(be, in, offset);
    return be_deref(be, &addr1, sizing);
}

static int be_dereference(struct BackEnd* be, struct TACAddress* out, Sizing sizing)
{
    *out = be_deref(be, out, sizing);
    return 0;
}

static int be_compile_stmt(struct BackEnd* be, struct Ast* e);
static int be_compile_expr(struct BackEnd* be, struct Expr* e, struct TACAddress* out);
static int be_compile_lvalue(struct BackEnd* be, struct Expr* e, struct TACAddress* out);
static int be_compile_init(struct BackEnd* be, struct Ast* e, size_t frame_offset, Sizing sizing, uint8_t is_char_arr)
{
    int rc = 0;
    if (e->kind == AST_INIT)
    {
        const struct TACEntry tace = {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing = sizing, .frame_offset = frame_offset},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
        };
        be_push_tace(be, &tace, s_sizing_zero);
        struct AstInit* block = (void*)e;
        for (; block->init; block = block->next)
        {
            UNWRAP(be_compile_init(be, block->init, frame_offset + block->offset, block->sizing, block->is_char_arr));
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
                    .frame_offset = frame_offset,
                    .is_addr = 1,
                    .sizing = sizing,
                },
            .rc = &e->tok->rc,
        };
        UNWRAP(be_compile_expr(be, (struct Expr*)e, &assign.arg2));
        if (e->kind == EXPR_LIT)
        {
            ExprLit* lit = (void*)e;
            if (lit->tok->type == LEX_STRING && is_char_arr)
            {
                assign.arg2.is_addr = 0;
                assign.arg2.sizing.width = assign.arg1.sizing.width;
            }
        }
        be_push_tace(be, &assign, s_sizing_zero);
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

/// <returns>index of matching elem, size on failure</returns>
static size_t asz_find(struct Array* stringpool, size_t offset)
{
    size_t const sz = arrsz_size(stringpool);
    size_t const* const data = stringpool->data;
    for (size_t i = 0; i < sz; ++i)
    {
        if (data[i] == offset) return i;
    }
    return sz;
}

static size_t be_compile_ExprLit_String(BackEnd* be, ExprLit* e)
{
    const size_t i = asz_find(&be->aszConstants, e->tok->sp_offset);
    const size_t n = array_size(&be->aszConstants, sizeof(size_t));

    if (i == n)
    {
        // not found, append and emit new constant
        arrsz_push(&be->aszConstants, e->tok->sp_offset);
        const char* const s = token_str(be->parser, e->tok);
        cg_string_constant(be->cg, i, s, e->tok->tok_len);
    }
    return i;
}

static int be_compile_ExprLit(struct BackEnd* be, struct ExprLit* e, struct TACAddress* out)
{
    int rc = 0;

    if (e->tok->type == LEX_NUMBER || e->tok->type == LEX_CHARLIT)
    {
        *out = taca_imm(e->numeric);
        out->sizing = e->sizing;
    }
    else if (e->tok->type == LEX_STRING)
    {
        size_t i = be_compile_ExprLit_String(be, e);
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
        size_t i = be_compile_ExprLit_String(be, e);
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
    switch (typestr_byte(&sym->type))
    {
        case TYPE_BYTE_FUNCTION:
        case TYPE_BYTE_UNK_ARRAY:
        case TYPE_BYTE_ARRAY: return 1;
        default: return 0;
    }
}

static int be_compile_lvalue_ExprRef(struct BackEnd* be, struct ExprRef* e, struct TACAddress* out)
{
    Symbol* sym = e->sym;
    *out = sym->addr;
    out->is_addr = 1;
    out->sizing = s_sizing_zero;
    return 0;
}

static int be_compile_ExprRef(struct BackEnd* be, struct ExprRef* esym, struct TACAddress* out)
{
    int rc = 0;
    if (esym->sym->is_enum_constant)
    {
        out->kind = TACA_IMM;
        out->imm = esym->sym->enum_value;
        out->sizing = s_sizing_int;
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

    struct TACEntry call = {
        .op = TACO_CALL,
        .arg2 = taca_imm(e->param_extent),
        .rc = &e->tok->rc,
    };
    UNWRAP(be_compile_expr(be, e->fn, &call.arg1));

    int j = 0;
    if (e->sizing.width > 8)
    {
        out->kind = TACA_FRAME;
        out->frame_offset = be_frame_alloc(be, e->sizing.width, 8);
        out->sizing = e->sizing;
        TACEntry* tace_arg = array_push_zeroes(&param_addr, sizeof(struct TACEntry));
        tace_arg->op = TACO_ASSIGN;
        tace_arg->arg1 = taca_reg(s_sysv_arg_reg[0], s_sizing_pointer);
        tace_arg->arg1.is_addr = 1;

        tace_arg->arg2 = *out;
        tace_arg->arg2.is_addr = 1;
        tace_arg->arg2.sizing = s_sizing_zero;
        ++j;
    }

    size_t param_offset = 0;

    CallParam* params = be->parser->callparams.data;
    for (size_t i = 0; i < e->param_extent; ++i)
    {
        const CallParam* const param = params + e->param_offset + i;
        TACEntry* tace_arg = array_push_zeroes(&param_addr, sizeof(struct TACEntry));
        tace_arg->op = TACO_ASSIGN;
        tace_arg->rc = &param->expr->tok->rc;
        if (param->sizing.width <= 8 && j < 6)
        {
            tace_arg->arg1.kind = TACA_REG;
            tace_arg->arg1.reg = s_sysv_arg_reg[j];
            tace_arg->arg1.is_addr = 1;
            tace_arg->arg1.sizing = s_sizing_pointer;
            ++j;
        }
        else
        {
            param_offset = round_to_alignment(param_offset, param->align);
            tace_arg->arg1.kind = TACA_PARAM;
            tace_arg->arg1.param_offset = param_offset;
            tace_arg->arg1.is_addr = 1;
            tace_arg->arg1.sizing = param->sizing;
            if (param->sizing.width == 0) abort();
            param_offset += param->sizing.width;
            ++j;
        }
        UNWRAP(be_compile_expr(be, param->expr, &tace_arg->arg2));
    }

    for (size_t i = 0; i < array_size(&param_addr, sizeof(TACEntry)); ++i)
    {
        be_push_tace(be, (TACEntry*)param_addr.data + i, s_sizing_zero);
    }

    if (e->sizing.width <= 8)
    {
        *out = be_push_tace(be, &call, e->sizing);
    }
    else
    {
        be_push_tace(be, &call, e->sizing);
    }

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

static int be_compile_add(struct BackEnd* be, struct ExprBinOp* e, Sizing sizing, struct TACAddress* out)
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
        tace.arg2 = be_push_tace(be, &tace, s_sizing_pointer);
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
            tace.arg1.sizing = e->sizing;
            tace.arg2.sizing = min_sizing(e->sizing, tace.arg2.sizing);
            be_push_tace(be, &tace, e->sizing);
            goto fail;

        case TOKEN_SYM1('['): return be_compile_add(be, e, s_sizing_pointer, out);
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
    tace.arg1.sizing = e->sizing;
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
            UNWRAP(be_compile_lvalue(be, e->expr1, &tace.arg1));
            // gp_offset
            tace.arg1.is_addr = 1;
            tace.arg1.sizing = s_sizing_uint;
            tace.arg2 = taca_imm(be->cur_fn->extent * 8);
            be_push_tace(be, &tace, s_sizing_zero);
            // fp_offset
            // TODO: fix floating point
            tace.arg1.frame_offset += 4;
            tace.arg2 = taca_imm(48);
            be_push_tace(be, &tace, s_sizing_zero);
            // overflow_arg_area
            tace.arg1.frame_offset += 4;
            tace.arg1.sizing = s_sizing_pointer;
            tace.arg2.kind = TACA_ARG;
            tace.arg2.is_addr = 1;
            tace.arg2.sizing = s_sizing_zero;
            tace.arg2.arg_offset = 0;
            be_push_tace(be, &tace, s_sizing_zero);
            // reg_save_area
            tace.arg1.frame_offset += 8;
            tace.arg2.kind = TACA_FRAME;
            tace.arg2.is_addr = 1;
            tace.arg2.sizing = s_sizing_zero;
            tace.arg2.arg_offset = 0;
            be_push_tace(be, &tace, s_sizing_zero);
            *out = s_taca_void;
            break;
        case LEX_UUVA_ARG:
        {
            TACAddress va;
            UNWRAP(be_compile_lvalue(be, e->expr1, &va));
            TACAddress overflow_area_p = be_increment(be, &va, 8);
            const TACAddress overflow_area = be_deref(be, &overflow_area_p, s_sizing_pointer);
            overflow_area_p.sizing = s_sizing_pointer;
            const size_t complete_lbl = ++be->next_label;
            *out = be_alloc_temp(be, e->sizing);
            if (e->sizing.width <= 8)
            {
                // class INTEGER
                const TACAddress gp_offset = be_deref(be, &va, s_sizing_int);
                const TACEntry cmp_gp_offset = {
                    .rc = &e->tok->rc,
                    .op = TACO_LT,
                    .arg1 = gp_offset,
                    .arg2 = taca_imm(48),
                };
                const size_t use_memory_lbl = ++be->next_label;
                const TACEntry jump_stack = {
                    .rc = &e->tok->rc,
                    .op = TACO_BRZ,
                    .arg1 = be_push_tace(be, &cmp_gp_offset, s_sizing_pointer),
                    .arg2.kind = TACA_ALABEL,
                    .arg2.alabel = use_memory_lbl,
                };
                be_push_tace(be, &jump_stack, s_sizing_zero);
                const TACEntry calc_addr = {
                    .rc = &e->tok->rc,
                    .op = TACO_ADD,
                    .arg1 = be_increment_deref(be, &va, 16, s_sizing_pointer),
                    .arg2 = gp_offset,
                };
                const TACEntry load_addr = {
                    .op = TACO_LOAD,
                    .arg1 = *out,
                    .arg2 = be_push_tace(be, &calc_addr, s_sizing_pointer),
                };
                be_push_tace(be, &load_addr, s_sizing_zero);
                const TACEntry calc_new_offset = {
                    .rc = &e->tok->rc,
                    .op = TACO_ADD,
                    .arg1 = gp_offset,
                    .arg2 = taca_imm(8),
                };
                TACEntry write_gp_offset = {
                    .rc = &e->tok->rc,
                    .op = TACO_ASSIGN,
                    .arg1 = gp_offset,
                    .arg2 = be_push_tace(be, &calc_new_offset, s_sizing_int),
                };
                write_gp_offset.arg1.is_addr = 1;
                be_push_tace(be, &write_gp_offset, s_sizing_zero);
                be_push_jump(be, complete_lbl);
                be_push_label(be, use_memory_lbl);
            }
            const TACEntry load_overflow = {
                .op = TACO_LOAD,
                .arg1 = *out,
                .arg2 = overflow_area,
            };
            be_push_tace(be, &load_overflow, s_sizing_zero);
            const size_t inc = round_to_alignment(e->sizing.width, 8);
            const TACEntry calc_new_offset = {
                .rc = &e->tok->rc,
                .op = TACO_ADD,
                .arg1 = overflow_area,
                .arg2 = taca_imm(inc),
            };
            const TACEntry write_gp_offset = {
                .rc = &e->tok->rc,
                .op = TACO_ASSIGN,
                .arg1 = overflow_area_p,
                .arg2 = be_push_tace(be, &calc_new_offset, s_sizing_pointer),
            };
            be_push_tace(be, &write_gp_offset, s_sizing_zero);
            be_push_label(be, complete_lbl);
            break;
        }
        case LEX_UUVA_COPY:
            tace.op = TACO_ASSIGN;
            UNWRAP(be_compile_lvalue(be, e->expr1, &tace.arg1));
            tace.arg1.sizing = e->expr1->sizing;
            UNWRAP(be_compile_expr(be, e->expr2, &tace.arg2));
            tace.arg2 = be_deref(be, &tace.arg2, e->expr1->sizing);
            be_push_tace(be, &tace, s_sizing_zero);
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
            if (e->postfix)
            {
                // postincrement
                *out = be_ensure_ref(be, &tace.arg1);
            }
            tace.arg2 = taca_imm(e->sizeof_);
            tace.arg2 = be_push_tace(be, &tace, e->sizing);
            tace.op = TACO_ASSIGN;
            tace.arg1 = lhs_lvalue;
            tace.arg1.sizing = e->lhs->sizing;
            if (e->postfix)
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
            be_push_tace(be, &tace, s_sizing_zero);
            *out = be_alloc_temp(be, e->sizing);
            struct TACAddress ret_addr = *out;
            ret_addr.is_addr = 1;
            struct TACEntry assign = {
                .op = TACO_ASSIGN,
                .arg1 = ret_addr,
            };
            UNWRAP(be_compile_expr(be, rhs->lhs, &assign.arg2));
            be_push_tace(be, &assign, s_sizing_zero);
            be_push_jump(be, end);
            be_push_label(be, on_false);
            UNWRAP(be_compile_expr(be, rhs->rhs, &assign.arg2));
            be_push_tace(be, &assign, s_sizing_zero);
            be_push_label(be, end);
            break;
        }
        case TOKEN_SYM2('|', '|'):
        case TOKEN_SYM2('&', '&'):
        {
            *out = be_alloc_temp(be, e->sizing);
            TACEntry assign_out = {
                .op = TACO_ASSIGN,
                .arg1 = *out,
            };
            assign_out.arg1.is_addr = 1;
            UNWRAP(be_compile_expr(be, e->lhs, &assign_out.arg2));
            be_push_tace(be, &assign_out, e->sizing);
            size_t on_false = ++be->next_label;
            tace.op = e->tok->type == TOKEN_SYM2('&', '&') ? TACO_BRZ : TACO_BRNZ;
            tace.arg2.kind = TACA_ALABEL;
            tace.arg2.alabel = on_false;
            tace.arg1 = assign_out.arg2;
            be_push_tace(be, &tace, s_sizing_zero);
            UNWRAP(be_compile_expr(be, e->rhs, &assign_out.arg2));
            be_push_tace(be, &assign_out, e->sizing);
            be_push_label(be, on_false);
            const TACEntry logical = {
                .op = TACO_NEQ,
                .arg1 = *out,
                .arg2 = taca_imm(0),
            };
            *out = be_push_tace(be, &logical, e->sizing);
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
    be_push_tace(be, &entry, s_sizing_zero);
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
    be_push_tace(be, &entry, s_sizing_zero);
    return 0;
}

static int be_compile_StmtLabel(struct BackEnd* be, struct StmtLabel* stmt)
{
    struct TACEntry entry = {
        .op = TACO_LABEL,
        .arg1 = taca_llabel(token_str(be->parser, stmt->tok)),
        .rc = &stmt->tok->rc,
    };
    be_push_tace(be, &entry, s_sizing_zero);
    return be_compile_stmt(be, stmt->stmt);
}

static int be_compile_StmtGoto(struct BackEnd* be, struct StmtGoto* stmt)
{
    struct TACEntry entry = {
        .op = TACO_JUMP,
        .arg1 = taca_llabel(token_str(be->parser, stmt->dst)),
        .rc = &stmt->dst->rc,
    };
    be_push_tace(be, &entry, s_sizing_zero);
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
        .arg1 = {.kind = TACA_REG, .is_addr = 1, .sizing = s_sizing_pointer, .reg = REG_RCX},
        .rc = &stmt->tok->rc,
    };
    UNWRAP(be_compile_expr(be, stmt->expr, &load.arg2));
    be_push_tace(be, &load, s_sizing_zero);
    struct SwitchCase* cases = be->switch_cases.data;
    for (size_t i = switch_case_offset; i < array_size(&be->switch_cases, sizeof(struct SwitchCase)); ++i)
    {
        struct TACEntry _case = {
            .op = TACO_CTBZ,
            .arg1 = taca_imm(cases[i].imm),
            .arg2 = taca_alabel(cases[i].label),
        };
        be_push_tace(be, &_case, s_sizing_zero);
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
        if (be->cur_sym->fn_ret_sizing.width > 8)
        {
            struct TACEntry retassign = {
                .op = TACO_ASSIGN,
                .arg1 = {TACA_FRAME, .sizing = be->cur_sym->fn_ret_sizing, .frame_offset = 0},
            };
            UNWRAP(be_compile_expr(be, stmt->expr, &retassign.arg2));
            be_push_tace(be, &retassign, s_sizing_zero);
            struct TACEntry retval = {
                .op = TACO_ASSIGN,
                .arg1 = {TACA_REG, .is_addr = 1, .sizing = s_sizing_pointer, .reg = REG_RAX},
                .arg2 = {TACA_FRAME, .sizing = s_sizing_pointer, .frame_offset = 0},
            };
            be_push_tace(be, &retval, s_sizing_zero);
        }
        else
        {
            struct TACEntry retval = {
                .op = TACO_ASSIGN,
                .arg1 = {TACA_REG, .is_addr = 1, .sizing = be->cur_sym->fn_ret_sizing, .reg = REG_RAX},
            };
            UNWRAP(be_compile_expr(be, stmt->expr, &retval.arg2));
            be_push_tace(be, &retval, s_sizing_zero);
        }
    }
    be_push_tace(be, &entry, s_sizing_zero);
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
    be_push_tace(be, &e, s_sizing_zero);
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
        be_push_tace(be, &brz, s_sizing_zero);
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
    if (decl->sym->size.width == 0) abort();
    decl->sym->addr = taca_frame(be_frame_alloc(be, decl->sym->size.width, decl->sym->align), decl->sym->size);

    if (decl->init)
    {
        struct Ast* init = decl->init;
        int start_frame_size = be->frame_size;
        UNWRAP(be_compile_init(be, init, decl->sym->addr.frame_offset, decl->sym->size, decl->sym->is_char_array));
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

    *out = be_increment(be, out, e->sym->field_offset);

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

void debug_tace(Array* arr, const TACEntry* tace)
{
    array_push_byte(arr, '{');
    array_appends(arr, taco_to_string(tace->op));
    array_push_byte(arr, ',');
    array_push_byte(arr, ' ');
    debug_taca(arr, &tace->arg1);
    array_push_byte(arr, ',');
    array_push_byte(arr, ' ');
    debug_taca(arr, &tace->arg2);
    array_push_byte(arr, '}');
}

void debug_taca(Array* arr, const TACAddress* addr)
{
    array_push_byte(arr, '{');
    array_appends(arr, taca_to_string(addr->kind));
    if (addr->is_addr) array_appends(arr, ", .is_addr = 1");
    if (addr->sizing.width != 0)
    {
        array_appendf(arr, ", .sizing = %c, %d", addr->sizing.is_signed ? '1' : '0', addr->sizing.width);
    }
    switch (addr->kind)
    {
        case TACA_FRAME: array_appendf(arr, ", .frame_offset = %d}", addr->frame_offset); break;
        case TACA_IMM: array_appendf(arr, ", .imm = %zu}", addr->imm); break;
        case TACA_ARG: array_appendf(arr, ", .arg_offset = %zu}", addr->arg_offset); break;
        case TACA_REG: array_appendf(arr, ", .reg = %s}", register_to_string(addr->reg)); break;
        case TACA_REF: array_appendf(arr, ", .ref = %zu}", addr->ref); break;
        case TACA_ALABEL: array_appendf(arr, ", .alabel = %zu}", addr->alabel); break;
        case TACA_PARAM: array_appendf(arr, ", .param_offset = %zu}", addr->param_offset); break;
        case TACA_NAME: array_appendf(arr, ", .name = \"%s\"}", addr->name ? addr->name : "(null)"); break;
        case TACA_CONST: array_appendf(arr, ", .const_idx = %zu}", addr->const_idx); break;
        case TACA_VOID: array_push_byte(arr, '}'); break;
        default: array_appendf(arr, ", unimplemented = %p}", addr); break;
    }
}

int be_compile_toplevel_decl(struct BackEnd* be, Decl* decl)
{
    int rc = 0;
    array_clear(&be->code);
    struct Symbol* const sym = decl->sym;
    if (!sym->name) abort();

    struct Decl* def = sym->def ? sym->def : decl;
    if ((decl->type->kind == AST_DECLFN && !def->init) || def->specs->is_extern)
    {
        cg_declare_extern(be->cg, sym->name);
    }
    else if (!decl->specs->is_static && !decl->specs->is_inline)
    {
        cg_declare_public(be->cg, sym->name);
    }
    if (!decl->prev_decl)
    {
        sym->addr.kind = decl->specs->is_static ? TACA_LNAME : TACA_NAME;
        sym->addr.name = sym->name;
    }
    if (decl->type->kind == AST_DECLFN)
    {
        struct DeclFn* declfn = (struct DeclFn*)decl->type;
        if (decl->init)
        {
            cg_start_function(be->cg, sym->name);
            be->frame_size = 0;
            be->max_frame_size = 0;
            be->cur_sym = sym;
            be->cur_fn = declfn;

            int j = 0;

            TACEntry save_arg = {
                .rc = &decl->tok->rc,
                .op = TACO_ASSIGN,
                .arg1 = {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_pointer},
                .arg2 = {TACA_REG, .sizing = s_sizing_pointer, .reg = REG_RDI},
            };
            if (sym->fn_ret_sizing.width > 8)
            {
                be_frame_alloc(be, 8, 8);
                be_push_tace(be, &save_arg, s_sizing_zero);
                ++j;
            }

            size_t arg_offset = 0;

            Ast** const asts = be->parser->expr_seqs.data;
            for (size_t i = 0; i < declfn->extent; ++i)
            {
                Ast* ast = asts[declfn->offset + i];
                if (ast->kind != STMT_DECLS) abort();
                StmtDecls* decls = (void*)ast;
                if (decls->extent != 1) abort();
                Ast* arg_ast = asts[decls->offset];
                if (arg_ast->kind != AST_DECL) abort();
                Decl* arg_decl = (void*)arg_ast;

                if (arg_decl->sym->size.width > 8 || arg_decl->sym->align > 8 || j >= 6)
                {
                    // class MEMORY
                    arg_offset = round_to_alignment(arg_offset, arg_decl->sym->align);
                    arg_decl->sym->addr.kind = TACA_ARG;
                    arg_decl->sym->addr.sizing = arg_decl->sym->size;
                    arg_decl->sym->addr.arg_offset = arg_offset;
                    arg_offset += arg_decl->sym->size.width;
                }
                else
                {
                    // class INTEGER
                    size_t align = declfn->is_varargs ? 8 : arg_decl->sym->align;
                    save_arg.arg1 = arg_decl->sym->addr =
                        taca_frame(be_frame_alloc(be, arg_decl->sym->size.width, align), arg_decl->sym->size);
                    save_arg.arg1.is_addr = 1;
                    save_arg.arg2.sizing = arg_decl->sym->size;
                    save_arg.arg2.reg = s_sysv_arg_reg[j];
                    be_push_tace(be, &save_arg, s_sizing_zero);
                    ++j;
                }
            }

            if (declfn->is_varargs)
            {
                save_arg.arg2.kind = TACA_REG;
                save_arg.arg2.sizing = s_sizing_pointer;
                save_arg.arg1.kind = TACA_FRAME;
                save_arg.arg1.is_addr = 1;
                save_arg.arg1.sizing = s_sizing_pointer;
                for (; j < 6; ++j)
                {
                    save_arg.arg1.frame_offset = be_frame_alloc(be, 8, 8);
                    save_arg.arg2.reg = s_sysv_arg_reg[j];
                    be_push_tace(be, &save_arg, s_sizing_zero);
                }
            }

            UNWRAP(be_compile_stmt(be, decl->init));
        }
    }
    else if (!decl->specs->is_extern)
    {
        // global variable
        if (decl->init)
        {
            cg_reserve_data(
                be->cg, sym->name, (char*)be->elab->constinit.data + sym->constinit_offset, sym->size.width);
        }
        else
        {
            cg_reserve_zeroes(be->cg, sym->name, sym->size.width);
        }
    }

fail:
    return rc;
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
            UNWRAP(be_compile_toplevel_decl(be, decl));
            if (be->code.sz)
            {
                if (be->debug_taces)
                {
                    struct TACEntry* taces = (struct TACEntry*)be->code.data;
                    struct Array buf = {0};
                    for (size_t i = 0; i < array_size(&be->code, sizeof(struct TACEntry)); ++i)
                    {
                        array_clear(&buf);
                        debug_tace(&buf, taces + i);
                        printf("%.*s\n", (int)buf.sz, (char*)buf.data);
                    }
                }
                else
                {
                    UNWRAP(cg_gen_taces(be->cg,
                                        (struct TACEntry*)be->code.data,
                                        array_size(&be->code, sizeof(struct TACEntry)),
                                        be->max_frame_size));
                }
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