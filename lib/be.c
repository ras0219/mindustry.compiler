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

__forceinline static struct TACAddress taca_imm(size_t imm)
{
    struct TACAddress ret = {
        .kind = TACA_IMM,
        .imm = imm,
    };
    return ret;
}

__forceinline static struct TACAddress taca_literal(const char* lit)
{
    struct TACAddress ret = {
        .kind = TACA_LITERAL,
        .literal = lit,
    };
    return ret;
}

__forceinline static struct TACAddress taca_const(size_t const_idx)
{
    struct TACAddress ret = {
        .kind = TACA_CONST,
        .const_idx = const_idx,
    };
    return ret;
}

#if 0
struct ValDest
{
    union
    {
        const char* literal;
        struct Symbol* sym;
        struct RegMap* regmap;
        size_t const_idx;
    };
    enum ValDestKind kind : 8;
    int is_const : 1;
    int is_reference : 1;
};

static const struct ValDest s_void_destination = {
    .kind = DEST_VOID,
    .is_const = 0,
};

struct CompoundBlock
{
    struct Symbol* fn_sym;
    int frame_size;
    int has_returned;
    size_t scope_sz;

    const char* continue_label;
    const char* break_label;
};

enum CondInverted
{
    COND_NORMAL,
    COND_INVERTED,
};

struct Decl* decl_get_def(struct Decl* decl)
{
    while (decl->def)
        decl = decl->def;
    return decl;
}

static int compile_stmt_expr(struct BackEnd* p, struct Expr* expr, struct CompoundBlock* bb);
static int compile_conditional_expr(
    struct BackEnd* p, struct Expr* cond, struct CompoundBlock* bb, const char* jump_to, enum CondInverted inverted);
static int compile_decl_single(struct BackEnd* p, struct Decl* decl, struct CompoundBlock* bb);
static int compile_expr_op_add(struct BackEnd* p,
                               struct Expr* elhs,
                               struct Expr* erhs,
                               struct ValDest* dst,
                               struct CompoundBlock* bb,
                               const struct RowCol* rc);
static int compile_expr_lit(struct BackEnd* p, struct ExprLit* e, struct ValDest* dst, struct CompoundBlock* bb);

static int dest_is_any(struct ValDest* dst) { return dst->kind == DEST_REGMAP && !dst->regmap->rename.buf[0]; }

static struct ValDest dest_literal(const char* ch)
{
    struct ValDest ret = {
        .kind = DEST_LITERAL,
        .literal = ch,
        .is_const = 1,
    };
    return ret;
};
static struct ValDest dest_regmap(struct RegMap* rm)
{
    struct ValDest ret = {
        .kind = DEST_REGMAP,
        .regmap = rm,
        .is_const = 0,
    };
    return ret;
}
static struct ValDest dest_sym(struct Symbol* sym)
{
    struct ValDest ret = {
        .kind = DEST_SYM,
        .sym = sym,
        .is_const = sym->decl->specs.is_const,
    };
    return ret;
}
#define PARAM_COUNT 6

static const char* PARAM_NAMES_ARR[PARAM_COUNT] = {"reg1", "reg2", "reg3", "reg4", "reg5", "reg6"};
static struct FreeVar s_freevar_paramreg0 = {
    .buf = "reg1",
};

static struct FreeVar s_freevar_zero = {0};
static struct FreeVar free_var(struct BackEnd* p)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "_%d", p->free_var_counter++);
    return ret;
}
static struct FreeVar free_var_label(struct BackEnd* p)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "$%d$", p->free_var_counter++);
    return ret;
}

static struct FreeVar free_var_from(struct BackEnd* p, const char* base)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "_%d_%s", p->free_var_counter++, base);
    return ret;
}
static struct FreeVar extern_var_from(struct BackEnd* p, const char* base)
{
    struct FreeVar ret;
    if (sizeof(ret.buf) <= snprintf(ret.buf, sizeof(ret.buf), "$%s$", base))
    {
        fprintf(stderr, "resource exceeded -- extern identifier too long (max %zu chars)\n", sizeof(ret.buf) - 3);
        abort();
    }
    return ret;
}
static struct FreeVar nrfun_param(struct BackEnd* p, int index, const char* fun)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "_%d_%s", index, fun);
    return ret;
}
static struct FreeVar nrfun_param_ret(struct BackEnd* p, const char* fun)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "_r_%s", fun);
    return ret;
}


// requires !reg->prev
static void be_push_active_reg(struct BackEnd* p, struct RegMap* reg)
{
    if (!reg->rename.buf[0]) abort();
    if (reg->stack_addr > 0 && !reg->is_global)
    {
        if (!reg->mem_loc.buf[0]) abort();
        cg_write_inst_add(p->cg, reg->mem_loc.buf, "__ebp__", reg->stack_addr);
    }
    if (reg->next = p->first_active_reg)
    {
        reg->next->prev = &reg->next;
    }
    p->first_active_reg = reg;
    reg->prev = &p->first_active_reg;
}
static void parser_deactivate_reg(struct RegMap* reg)
{
    if (reg->prev) *reg->prev = reg->next;
    if (reg->next) reg->next->prev = reg->prev;
    reg->next = NULL;
    reg->prev = NULL;
}

static void cb_init(struct CompoundBlock* cb, struct CompoundBlock* parent, struct BackEnd* p)
{
    cb->fn_sym = parent ? parent->fn_sym : NULL;
    cb->frame_size = parent ? parent->frame_size : 1;
    cb->has_returned = 0;
    cb->continue_label = parent ? parent->continue_label : NULL;
    cb->break_label = parent ? parent->break_label : NULL;
    cb->scope_sz = scope_size(&p->scope);
}
static void cb_destroy(struct CompoundBlock* cb, struct BackEnd* p)
{
    size_t new_scope_sz = scope_size(&p->scope);
    struct Binding* const bind_begin = scope_data(&p->scope);
    for (size_t n = cb->scope_sz; n < new_scope_sz; ++n)
    {
        parser_deactivate_reg(&bind_begin[n].sym->reg);
    }
    scope_shrink(&p->scope, cb->scope_sz);
}

static int parser_flush_dirty(struct BackEnd* p)
{
    struct RegMap* s = p->first_active_reg;
    while (s)
    {
        struct RegMap* const next = s->next;
        if (s->is_dirty)
        {
            if (cg_write_mem(p->cg, s->mem_loc.buf, s->rename.buf, &s_unknown_rc)) return 1;
            s->is_dirty = 0;
        }
        s = next;
    }
    return 0;
}
static int parser_invalidate_reads(struct BackEnd* p)
{
    struct RegMap** ps = &p->first_active_reg;
    struct RegMap* s;
    while (s = *ps)
    {
        if (s->stack_addr >= 0 && !s->is_const)
        {
            if (s->is_dirty)
            {
                if (cg_write_mem(p->cg, s->mem_loc.buf, s->rename.buf, &s_unknown_rc)) return 1;
                s->is_dirty = 0;
            }
            parser_deactivate_reg(s);
        }
        else
        {
            ps = &s->next;
        }
    }
    return 0;
}
static int parser_ensure_loaded_reg(struct BackEnd* p, struct RegMap* reg, const struct RowCol* rc)
{
    if (!reg->prev && reg->stack_addr >= 0)
    {
        if (parser_flush_dirty(p)) return 1;
        be_push_active_reg(p, reg);
        return cg_read_mem(p->cg, reg->mem_loc.buf, reg->rename.buf, rc);
    }
    return 0;
}

static int compile_expr(struct BackEnd* p, struct Expr* expr, struct ValDest* dst, struct CompoundBlock* bb);

static int parser_ensure_loaded_sym(struct BackEnd* p, struct Symbol* sym)
{
    if (!sym->is_register)
    {
        if (typestr_is_array(&sym->type))
        {
            if (!sym->reg.prev && !sym->reg.is_global)
            {
                be_push_active_reg(p, &sym->reg);
                cg_write_inst_add(p->cg, sym->reg.rename.buf, "__ebp__", sym->reg.stack_addr);
                sym->reg.is_const = 1;
            }
        }
        else
        {
            return parser_ensure_loaded_reg(p, &sym->reg, sym_to_rc(sym));
        }
    }
    return 0;
}
static void parser_ensure_dirty_reg(struct BackEnd* p, struct RegMap* reg)
{
    if (!reg->prev && reg->stack_addr >= 0)
    {
        be_push_active_reg(p, reg);
    }
    reg->is_dirty = 1;
}
static void parser_ensure_dirty_sym(struct BackEnd* p, struct Symbol* sym)
{
    if (!sym->is_register)
    {
        parser_ensure_dirty_reg(p, &sym->reg);
    }
}

static int parser_spill_registers(struct BackEnd* p)
{
    struct RegMap* s = p->first_active_reg;
    p->first_active_reg = NULL;
    while (s)
    {
        struct RegMap* const next = s->next;
        if (s->is_dirty)
        {
            if (cg_write_mem(p->cg, s->mem_loc.buf, s->rename.buf, &s_unknown_rc)) return 1;
            s->is_dirty = 0;
        }
        s->prev = NULL;
        s->next = NULL;
        s = next;
    }
    return 0;
}
static const char* parser_prepare_dst_reg(struct BackEnd* p, struct ValDest* dst, struct CompoundBlock* bb)
{
    if (dst->is_const)
    {
        fprintf(stderr, "internal compiler error -- attempting to write to constant\n");
        abort();
    }
    if (dst->kind == DEST_SYM)
    {
        parser_ensure_dirty_sym(p, dst->sym);
        return dst->sym->reg.rename.buf;
    }
    else if (dst->kind == DEST_REGMAP)
    {
        if (!dst->regmap->rename.buf[0])
        {
            dst->regmap->rename = free_var(p);
            if (!bb->fn_sym || bb->fn_sym->decl->is_nonreentrant)
                dst->regmap->stack_addr = -1;
            else
            {
                dst->regmap->stack_addr = bb->frame_size++;
                dst->regmap->mem_loc = free_var(p);
            }
        }
        parser_ensure_dirty_reg(p, dst->regmap);
        return dst->regmap->rename.buf;
    }
    else if (dst->kind == DEST_VOID)
    {
        return "_";
    }
    else
    {
        abort();
    }
}
static const char* parser_prepare_src_reg(struct BackEnd* p, const struct ValDest* dst, const struct RowCol* rc)
{
    if (dst->kind == DEST_VOID)
    {
        fprintf(stderr, "internal compiler error -- attempting to read from void\n");
        abort();
    }
    else if (dst->kind == DEST_LITERAL)
    {
        return dst->literal;
    }
    else if (dst->kind == DEST_SYM)
    {
        if (parser_ensure_loaded_sym(p, dst->sym)) return NULL;
        return dst->sym->reg.rename.buf;
    }
    else if (dst->kind == DEST_REGMAP)
    {
        if (parser_ensure_loaded_reg(p, dst->regmap, rc)) return NULL;
        return dst->regmap->rename.buf;
    }
    else
    {
        abort();
    }
}

static int parser_assign_dsts(struct BackEnd* p,
                              struct ValDest* dst,
                              const struct ValDest* src,
                              struct CompoundBlock* bb,
                              const struct RowCol* rc)
{
    struct CodeGen* const cg = p->cg;
    if (dst->is_const)
    {
        fprintf(stderr, "internal compiler error -- attempting to write to constant\n");
        abort();
    }
    else if (dst->kind == DEST_VOID)
    {
        return 0;
    }
    else if (dst->kind == DEST_SYM)
    {
        if (src->kind == DEST_SYM && dst->sym == src->sym) return 0;
    }
    else if (dest_is_any(dst))
    {
        *dst = *src;
        return 0;
    }

    const char* src_reg = parser_prepare_src_reg(p, src, rc);
    if (!src_reg) return 1;

    if (src->is_reference)
    {
        if (parser_flush_dirty(p)) return 1;
    }
    if (dst->is_reference)
    {
        const char* dst_reg = parser_prepare_src_reg(p, dst, rc);
        if (!dst_reg) return 1;

        if (src->is_reference)
        {
            if (cg_read_mem(p->cg, src_reg, "_", rc)) return 1;
            src_reg = "_";
        }

        if (cg_write_mem(p->cg, dst_reg, src_reg, rc)) return 1;
        if (parser_invalidate_reads(p)) return 1;
        return 0;
    }

    const char* dst_reg = parser_prepare_dst_reg(p, dst, bb);
    if (!dst_reg) return 1;

    if (src->is_reference)
    {
        if (cg_read_mem(p->cg, src_reg, dst_reg, rc)) return 1;
        return 0;
    }

    if (src->kind == DEST_SYM && typestr_is_array(&src->sym->type))
    {
        if (!src->sym->reg.is_global)
        {
            cg_write_inst_add(p->cg, dst_reg, "__ebp__", src->sym->reg.stack_addr);
        }
        else
        {
            cg_write_inst_set(p->cg, dst_reg, src->sym->reg.rename.buf);
        }
    }
    else
    {
        cg_write_inst_set(cg, dst_reg, src_reg);
    }
    return 0;
}

static int expr_is_lit(struct Expr* e, const char* lit)
{
    if (e->kind != EXPR_LIT) return 0;
    struct ExprLit* e_lit = (struct ExprLit*)e;
    return strcmp(e_lit->text, lit) == 0;
}

static int compile_loop_stmt(struct BackEnd* p, struct StmtLoop* stmt, struct CompoundBlock* bb)
{
    struct FreeVar continue_lbl = free_var_label(p);
    struct FreeVar first_lbl = free_var_label(p);
    struct FreeVar break_lbl = free_var_label(p);
    struct CompoundBlock new_cb;
    cb_init(&new_cb, bb, p);
    new_cb.break_label = break_lbl.buf;
    new_cb.continue_label = continue_lbl.buf;
    if (stmt->init)
    {
        if (compile_stmt_expr(p, stmt->init, &new_cb)) return 1;
    }
    if (parser_spill_registers(p)) return 1;
    if (stmt->advance || stmt->is_do_while)
    {
        cg_write_inst_jump(p->cg, first_lbl.buf);
    }
    cg_mark_label(p->cg, continue_lbl.buf);
    if (stmt->advance)
        if (compile_stmt_expr(p, stmt->advance, &new_cb)) return 1;

    if (!stmt->is_do_while)
    {
        if (parser_spill_registers(p)) return 1;
        cg_mark_label(p->cg, first_lbl.buf);
    }
    if (stmt->cond)
    {
        if (!expr_is_lit(stmt->cond, "1"))
        {
            if (compile_conditional_expr(p, stmt->cond, bb, break_lbl.buf, COND_INVERTED)) return 1;
        }
    }
    if (stmt->is_do_while)
    {
        if (parser_spill_registers(p)) return 1;
        cg_mark_label(p->cg, first_lbl.buf);
    }

    if (compile_stmt_expr(p, stmt->body, &new_cb)) return 1;
    cb_destroy(&new_cb, p);
    if (parser_spill_registers(p)) return 1;
    cg_write_inst_jump(p->cg, continue_lbl.buf);
    cg_mark_label(p->cg, break_lbl.buf);
    return 0;
}

static int is_empty_stmt(struct BackEnd* be, struct Expr* expr)
{
    if (expr->kind == STMT_NONE) return 1;
    if (expr->kind == STMT_BLOCK)
    {
        struct StmtBlock* blk = (struct StmtBlock*)expr;
        struct Expr** exprs = be->parser->expr_seqs.data;
        for (size_t i = 0; i < blk->extent; ++i)
        {
            if (!is_empty_stmt(be, exprs[blk->offset + i]))
            {
                return 0;
            }
        }
        return 1;
    }
    return 0;
}

static int compile_if_stmt(struct BackEnd* p, struct StmtIf* stmt, struct CompoundBlock* bb)
{
    struct FreeVar endif_lbl = free_var_label(p);
    struct CompoundBlock new_cb;
    if (is_empty_stmt(p, stmt->if_body))
    {
        if (!stmt->else_body || is_empty_stmt(p, stmt->else_body))
        {
            struct ValDest dst = s_void_destination;
            return compile_expr(p, stmt->cond, &dst, bb);
        }

        if (compile_conditional_expr(p, stmt->cond, bb, endif_lbl.buf, COND_NORMAL)) return 1;
        cb_init(&new_cb, bb, p);
        if (compile_stmt_expr(p, stmt->else_body, &new_cb)) return 1;
        cb_destroy(&new_cb, p);
        if (parser_spill_registers(p)) return 1;
        cg_mark_label(p->cg, endif_lbl.buf);
    }
    else
    {
        struct FreeVar else_lbl = free_var_label(p);
        if (compile_conditional_expr(p, stmt->cond, bb, else_lbl.buf, COND_INVERTED)) return 1;
        cb_init(&new_cb, bb, p);
        if (compile_stmt_expr(p, stmt->if_body, &new_cb)) return 1;
        int has_returned = new_cb.has_returned;
        cb_destroy(&new_cb, p);
        if (parser_spill_registers(p)) return 1;
        if (stmt->else_body && !is_empty_stmt(p, stmt->else_body))
        {
            cg_write_inst_jump(p->cg, endif_lbl.buf);
            cg_mark_label(p->cg, else_lbl.buf);
            cb_init(&new_cb, bb, p);
            if (compile_stmt_expr(p, stmt->else_body, &new_cb)) return 1;
            if (has_returned && new_cb.has_returned) bb->has_returned = 1;
            cb_destroy(&new_cb, p);
            if (parser_spill_registers(p)) return 1;
            cg_mark_label(p->cg, endif_lbl.buf);
        }
        else
        {
            cg_mark_label(p->cg, else_lbl.buf);
        }
    }
    return 0;
}
static int compile_decl_init(struct BackEnd* p, struct Decl* decl, struct CompoundBlock* bb)
{
    if (decl->init)
    {
        if (decl->is_function)
        {
            if (!decl->name)
            {
                return parser_tok_error(decl->id, "error: declaration expected to have an identifier\n");
            }

            p->fn = decl;
            struct CompoundBlock new_cb;
            cb_init(&new_cb, NULL, p);

            new_cb.fn_sym = &decl->sym;
            // begin compound block
            if (sizeof(p->fn_label_prefix) <=
                snprintf(p->fn_label_prefix, sizeof(p->fn_label_prefix), "%d_", p->free_var_counter++))
            {
                fprintf(stderr, "resource exceeded -- too many symbols\n");
                abort();
            }
            cg_mark_label(p->cg, decl->sym.reg.rename.buf);
            if (decl->is_nonreentrant)
            {
                p->fn_ret_var = nrfun_param_ret(p, decl->name);
            }
            else
            {
                cg_write_push_ret(p->cg, &p->fn_ret_var);
            }
            if (!decl->is_stackless)
            {
                cg_write_prepare_stack(p->cg);
            }
            for (size_t i = 0; i < decl->extent; ++i)
            {
                struct Decl* arg_decl = ((struct Decl**)p->parser->expr_seqs.data)[decl->offset + i];
                if (compile_decl_single(p, arg_decl, &new_cb)) return 1;
                if (decl->is_nonreentrant)
                {
                    arg_decl->sym.reg.rename = nrfun_param(p, i, decl->name);
                }
            }
            if (!decl->is_nonreentrant)
            {
                if (decl->extent > PARAM_COUNT)
                {
                    return parser_tok_error(decl->id, "error: exceeded maximum argument count (%d)\n", PARAM_COUNT);
                }
                for (size_t i = 0; i < decl->extent; ++i)
                {
                    struct Decl* arg_decl = ((struct Decl**)p->parser->expr_seqs.data)[decl->offset + i];
                    struct ValDest dst = dest_sym(&arg_decl->sym);
                    const char* dst_reg = parser_prepare_dst_reg(p, &dst, &new_cb);
                    cg_write_inst_set(p->cg, dst_reg, PARAM_NAMES_ARR[i]);
                }
            }
            if (compile_stmt_expr(p, decl->init, &new_cb)) return 1;
            int has_returned = new_cb.has_returned;
            cb_destroy(&new_cb, p);
            if (!has_returned)
            {
                if (decl->is_nonreentrant && strcmp(decl->name, "main") == 0)
                {
                }
                else
                {
                    if (parser_spill_registers(p)) return 1;
                    if (!decl->is_stackless) cg_write_epilog(p->cg);
                }
                cg_write_return(p->cg, &p->fn_ret_var);
            }
            p->fn = NULL;
            p->fn_ret_var = s_freevar_zero;
        }
        else
        {
            struct ValDest vd = dest_sym(&decl->sym);
            vd.is_const = 0;
            if (compile_expr(p, decl->init, &vd, bb)) return 1;
        }
    }
    return 0;
}

static int compile_decl_expr(struct BackEnd* p, struct Decl* decl, struct CompoundBlock* bb)
{
    if (compile_decl_single(p, decl, bb)) return 1;
    return compile_decl_init(p, decl, bb);
}

static int typestr_is_void_fn(struct TypeStr* ts)
{
    if (!ts->used) return 0;
    if (ts->buf[ts->used - 1] != ')') return 0;
    char ch = '\0';
    int depth = 1;
    int i = ts->used - 2;
    for (; i >= 0; --i)
    {
        ch = ts->buf[i];
        if (ch == '(' && !(--depth))
            break;
        else if (ch == ')')
            ++depth;
    }
    return i >= 1 && ts->buf[i - 1] == 'V';
}

static int compile_stmt_expr(struct BackEnd* p, struct Expr* expr, struct CompoundBlock* bb)
{
    switch (expr->kind)
    {
        case STMT_DECLS:
        {
            struct StmtDecls* stmt = (struct StmtDecls*)expr;
            for (size_t i = 0; i < stmt->extent; ++i)
            {
                struct Decl* arg_decl = ((struct Decl**)p->parser->expr_seqs.data)[stmt->offset + i];
                if (compile_decl_expr(p, arg_decl, bb)) return 1;
            }
            return 0;
        }
        case STMT_RETURN:
        {
            bb->has_returned = 1;
            struct StmtReturn* stmt = (struct StmtReturn*)expr;
            struct TypeStr* ty_fn = &bb->fn_sym->type;
            if (typestr_is_void_fn(ty_fn))
            {
                if (stmt->expr)
                {
                    return parser_tok_error(stmt->tok,
                                            "error: unexpected return expression in function of type 'void'\n");
                }
            }
            else
            {
                if (!stmt->expr)
                {
                    return parser_tok_error(stmt->tok, "error: expected return expression in non-void function\n");
                }
                struct RegMap regmap = {
                    .rename = s_freevar_paramreg0,
                };
                struct ValDest dst = dest_regmap(&regmap);
                if (compile_expr(p, stmt->expr, &dst, bb)) return 1;
                parser_deactivate_reg(&regmap);
            }
            if (parser_spill_registers(p)) return 1;
            if (!p->fn->is_stackless) cg_write_epilog(p->cg);
            cg_write_return(p->cg, &p->fn_ret_var);
            return 0;
        }
        case STMT_IF:
        {
            return compile_if_stmt(p, (struct StmtIf*)expr, bb);
        }
        case STMT_CONTINUE:
        {
            if (!bb->continue_label)
            {
                return parser_tok_error(((struct StmtContinue*)expr)->tok, "error: not inside a loop\n");
            }

            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump(p->cg, bb->continue_label);
            return 0;
        }
        case STMT_BREAK:
        {
            if (!bb->break_label)
            {
                return parser_tok_error(((struct StmtBreak*)expr)->tok, "error: not inside a loop\n");
            }

            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump(p->cg, bb->break_label);
            return 0;
        }
        case STMT_LOOP:
        {
            return compile_loop_stmt(p, (struct StmtLoop*)expr, bb);
        }
        case STMT_LABEL:
        {
            struct StmtLabel* label = (struct StmtLabel*)expr;
            const char* const str = token_str(p->parser, label->tok);
            char buf[64];
            if (sizeof(buf) <= snprintf(buf, sizeof(buf), "$%s%s$", p->fn_label_prefix, str))
            {
                return parser_tok_error(label->tok, "error: resource exceeded: label too long: '%s'\n", str);
            }
            if (parser_spill_registers(p)) return 1;
            cg_mark_label(p->cg, buf);
            bb->has_returned = 0;
            return compile_stmt_expr(p, label->stmt, bb);
        }
        case STMT_GOTO:
        {
            struct StmtGoto* stmt = (struct StmtGoto*)expr;
            char buf[64];
            const char* const cur_tok_str = token_str(p->parser, stmt->dst);
            if (sizeof(buf) <= snprintf(buf, sizeof(buf), "$%s%s$", p->fn_label_prefix, cur_tok_str))
            {
                fprintf(stderr, "resource exceeded: label too long: '%s'\n", cur_tok_str);
                abort();
            }
            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump(p->cg, buf);
            bb->has_returned = 1;
            return 0;
        }
        case STMT_BLOCK:
        {
            struct StmtBlock* stmt = (struct StmtBlock*)expr;
            struct CompoundBlock new_cb;
            cb_init(&new_cb, bb, p);
            for (size_t i = stmt->offset; i < stmt->offset + stmt->extent; ++i)
            {
                struct Expr** expr_seqs = p->parser->expr_seqs.data;
                struct Expr* inner_stmt = expr_seqs[i];
                if (compile_stmt_expr(p, inner_stmt, bb)) return 1;
            }
            bb->has_returned |= new_cb.has_returned;
            cb_destroy(&new_cb, p);
            return 0;
        }
        case STMT_NONE: return 0;
        default:
            if (ast_kind_is_expr(expr->kind))
            {
                // tc_expr(p, expr);
                if (parser_has_errors()) return 1;
                struct ValDest dst = s_void_destination;
                if (compile_expr(p, expr, &dst, bb)) return 1;
                return 0;
            }
            abort();
            break;
    }
}

static int compile_expr_op(struct BackEnd* p, struct ExprOp* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    
}

// static struct Expr* parser_checked_expr_at(struct BackEnd* p, struct ExprCall* e, size_t index)
//{
//    if (index >= e->extent) return NULL;
//    return ((struct Expr**)p->expr_seqs.data)[e->offset + index];
//}

static int compile_expr_call_intrinsic(
    struct BackEnd* p, struct ExprCall* e, struct ValDest* dst, struct Symbol* fn, struct CompoundBlock* bb)
{
    if (e->extent > PARAM_COUNT)
    {
        return parser_tok_error(e->tok, "error: exceeded maximum call arguments (%d)\n", PARAM_COUNT);
    }

    if (!bb->fn_sym)
    {
        return parser_tok_error(e->tok, "error: not in function scope\n");
    }

    struct RegMap arg_regmap[PARAM_COUNT] = {0};
    struct ValDest arg_dsts[PARAM_COUNT] = {0};
    for (size_t i = 0; i < e->extent; ++i)
    {
        arg_dsts[i] = dest_regmap(arg_regmap + i);
        if (compile_expr(p, ((struct Expr**)p->parser->expr_seqs.data)[e->offset + i], arg_dsts + i, bb)) return 1;
    }

    // pseudo-format
    const char* s = fn->decl->attr.asmstr;
    char buf[128];
    size_t i = 0;
    while (1)
    {
        if (i == 127)
        {
            fprintf(stderr, "resource exceeded\n");
            abort();
        }
        if (!*s) break;
        if (*s == '%')
        {
            ++s;
            if (*s == 'r')
            {
                const char* const reg = parser_prepare_dst_reg(p, dst, bb);
                size_t len = strlen(reg);
                if (i + len > 127)
                {
                    fprintf(stderr, "resource exceeded\n");
                    abort();
                }
                memcpy(buf + i, reg, len);
                i += len;
            }
            else if (*s >= '0' && *s <= '9')
            {
                size_t n = *s - '0';
                if (n >= e->extent)
                {
                    return parser_tok_error(fn->decl->id, "error: invalid intrinsic string: wrong arguments\n");
                }
                const char* const reg = parser_prepare_src_reg(p, arg_dsts + n, &s_unknown_rc);
                if (!reg) return 1;
                size_t len = strlen(reg);
                if (i + len > 127)
                {
                    fprintf(stderr, "resource exceeded\n");
                    abort();
                }
                memcpy(buf + i, reg, len);
                i += len;
            }
            else
            {
                return parser_tok_error(fn->decl->id, "error: invalid intrinsic string: invalid format specifier\n");
            }
            ++s;
        }
        else
        {
            buf[i++] = *s++;
        }
    }
    buf[i] = 0;
    cg_write_inst(p->cg, buf);
    if (dest_is_any(dst))
    {
        *dst = s_void_destination;
    }

    for (size_t i = 0; i < e->extent; ++i)
    {
        parser_deactivate_reg(arg_regmap + i);
    }

    return 0;
}

static int parser_assign_load(struct BackEnd* p, struct ValDest src, const char* tgt)
{
    struct CodeGen* const cg = p->cg;
    if (src.kind == DEST_SYM)
    {
        if (src.sym->is_register)
        {
            cg_write_inst_set(cg, tgt, src.sym->reg.rename.buf);
            return 0;
        }
        src.regmap = &src.sym->reg;
        src.kind = DEST_REGMAP;
    }

    if (src.kind == DEST_LITERAL)
    {
        cg_write_inst_set(cg, tgt, src.literal);
        return 0;
    }
    else if (src.kind == DEST_VOID)
    {
        fprintf(stderr, "internal compiler error -- attempting to read void\n");
        abort();
    }
    else if (src.kind == DEST_REGMAP)
    {
        if (src.regmap->prev || src.regmap->stack_addr == -1)
        {
            cg_write_inst_set(cg, tgt, src.regmap->rename.buf);
            return 0;
        }
        else
        {
            cg_write_inst_add(cg, src.regmap->mem_loc.buf, "__ebp__", src.regmap->stack_addr);
            return cg_read_mem(cg, src.regmap->mem_loc.buf, tgt, &s_unknown_rc);
        }
    }
    else
        abort();
}

static int compile_expr_call(struct BackEnd* p, struct ExprCall* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    if (!e->fn) return parser_ice_tok(e->tok), 1;

    struct RegMap regmap = {0};
    struct ValDest fn = dest_regmap(&regmap);
    if (compile_expr(p, e->fn, &fn, bb)) return 1;

    if (fn.kind != DEST_SYM || !fn.sym->decl->is_function)
    {
        return parser_tok_error(e->tok, "error: attempting to call a non-function\n");
    }

    if (fn.sym->decl->extent != e->extent)
    {
        return parser_tok_error(
            e->tok, "error: function requires %d arguments, %lu provided\n", fn.sym->decl->extent, e->extent);
    }
    if (fn.sym->decl->attr.asmstr)
    {
        return compile_expr_call_intrinsic(p, e, dst, fn.sym, bb);
    }
    else
    {
        if (e->extent > PARAM_COUNT)
        {
            return parser_tok_error(e->tok, "error: exceeded maximum call arguments (%d)\n", PARAM_COUNT);
        }

        if (!bb->fn_sym)
        {
            return parser_tok_error(e->tok, "error: not in function scope\n");
        }

        struct RegMap arg_regmaps[PARAM_COUNT - 1] = {0};
        struct ValDest arg_dsts[PARAM_COUNT - 1] = {0};
        for (size_t i = 1; i < e->extent; ++i)
        {
            struct ValDest* arg_dst = arg_dsts + i - 1;
            *arg_dst = dest_regmap(arg_regmaps + i - 1);
            if (compile_expr(p, ((struct Expr**)p->parser->expr_seqs.data)[e->offset + i], arg_dst, bb)) return 1;
        }

        if (fn.sym->decl->is_nonreentrant)
        {
            const char* const name = token_str(p->parser, fn.sym->decl->id);
            struct RegMap arg0_regmap = {
                .rename = nrfun_param(p, 0, name),
            };
            if (e->extent > 0)
            {
                struct ValDest fv_dst = dest_regmap(&arg0_regmap);
                if (compile_expr(p, ((struct Expr**)p->parser->expr_seqs.data)[e->offset], &fv_dst, bb)) return 1;
            }
            parser_deactivate_reg(&arg0_regmap);
            for (size_t i = 1; i < e->extent; ++i)
            {
                struct FreeVar fv = nrfun_param(p, i, name);
                if (parser_assign_load(p, arg_dsts[i - 1], fv.buf)) return 1;
                parser_deactivate_reg(arg_regmaps + i - 1);
            }
            if (parser_spill_registers(p)) return 1;
            if (!bb->fn_sym->decl->is_stackless)
            {
                char buf[16];
                snprintf(buf, sizeof(buf), "%d", bb->frame_size);
                cg_write_inst_op(p->cg, "+", "__stk__", "__ebp__", buf);
            }
            const struct FreeVar fv_ret = nrfun_param_ret(p, name);
            cg_write_inst_op(p->cg, "+", fv_ret.buf, "1", "@counter");
        }
        else
        {
            if (e->extent > PARAM_COUNT)
            {
                return parser_tok_error(e->tok, "error: exceeded maximum call arguments (%d)\n", PARAM_COUNT);
            }
            struct RegMap arg0_regmap = {
                .rename = s_freevar_paramreg0,
            };
            if (e->extent > 0)
            {
                struct ValDest fv_dst = dest_regmap(&arg0_regmap);
                if (compile_expr(p, ((struct Expr**)p->parser->expr_seqs.data)[e->offset], &fv_dst, bb)) return 1;
            }
            parser_deactivate_reg(&arg0_regmap);
            for (size_t i = 1; i < e->extent; ++i)
            {
                if (parser_assign_load(p, arg_dsts[i - 1], PARAM_NAMES_ARR[i])) return 1;
                parser_deactivate_reg(arg_regmaps + i - 1);
            }
            if (parser_spill_registers(p)) return 1;
            cg_write_inst(p->cg, "op add ret 1 @counter");
        }
        cg_write_inst_jump(p->cg, fn.sym->reg.rename.buf);
        if (dst->kind != DEST_VOID)
        {
            struct RegMap ret_regmap = {
                .rename = s_freevar_paramreg0,
            };
            be_push_active_reg(p, &ret_regmap);
            struct ValDest ret_dst = dest_regmap(&ret_regmap);
            if (!parser_prepare_dst_reg(p, dst, bb)) return 1;
            if (parser_assign_dsts(p, dst, &ret_dst, bb, &s_unknown_rc)) return 1;
            parser_deactivate_reg(&ret_regmap);
        }
    }
    return 0;
}

static int compile_expr(struct BackEnd* p, struct Expr* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    switch (e->kind)
    {
        case EXPR_LIT: return compile_expr_lit(p, (struct ExprLit*)e, dst, bb);
        case EXPR_OP: return compile_expr_op(p, (struct ExprOp*)e, dst, bb);
        case EXPR_CALL: return compile_expr_call(p, (struct ExprCall*)e, dst, bb);
        case EXPR_SYM:
        {
            struct ExprSym* esym = (struct ExprSym*)e;
            struct Symbol* sym = &decl_get_def(esym->decl)->sym;
            struct ValDest sym_dst = dest_sym(sym);
            return parser_assign_dsts(p, dst, &sym_dst, bb, &s_unknown_rc);
        }
        case EXPR_CAST: return compile_expr(p, ((struct ExprCast*)e)->expr, dst, bb);
        default: return parser_ice(&s_unknown_rc), 1;
    }
}

static int compile_expr_op_add(struct BackEnd* p,
                               struct Expr* elhs,
                               struct Expr* erhs,
                               struct ValDest* dst,
                               struct CompoundBlock* bb,
                               const struct RowCol* rc)
{
    if (elhs->kind == EXPR_SYM && erhs->kind == EXPR_LIT)
    {
        struct ExprLit* l = (struct ExprLit*)erhs;
        struct ExprSym* s = (struct ExprSym*)elhs;
        if (typestr_is_array(&s->sym->type) && s->sym->reg.stack_addr >= 0 && l->tok->type == LEX_NUMBER)
        {
            if (s->sym->reg.is_global)
            {
                int offset = atoi(token_str(p->parser, l->tok));
                char buf[16];
                snprintf(buf, 16, "%d", s->sym->reg.stack_addr + offset);
                struct ValDest vd = dest_literal(buf);
                return parser_assign_dsts(p, dst, &vd, bb, rc);
            }
            else
            {
                const char* dst_reg = parser_prepare_dst_reg(p, dst, bb);
                if (!dst_reg) return 1;
                int offset = atoi(token_str(p->parser, l->tok));
                cg_write_inst_add(p->cg, dst_reg, "__ebp__", s->sym->reg.stack_addr + offset);
            }
            return 0;
        }
    }
    struct RegMap regmapl = {0};
    struct ValDest lhs = dest_regmap(&regmapl);
    struct RegMap regmapr = {0};
    struct ValDest rhs = dest_regmap(&regmapr);
    if (compile_expr(p, elhs, &lhs, bb)) return 1;
    if (compile_expr(p, erhs, &rhs, bb)) return 1;
    const char* r2 = parser_prepare_src_reg(p, &lhs, rc);
    if (!r2) return 1;
    const char* r3 = parser_prepare_src_reg(p, &rhs, rc);
    if (!r3) return 1;
    const char* r1 = parser_prepare_dst_reg(p, dst, bb);
    if (!r1) return 1;
    cg_write_inst_op(p->cg, "+", r1, r2, r3);
    parser_deactivate_reg(&regmapl);
    parser_deactivate_reg(&regmapr);
    return 0;
}

static int sizeof_typestr(struct TypeStr* ts, const struct RowCol* rc)
{
    if (!ts->used) return 0;
    int i = ts->used - 1;
    char c = ts->buf[i];
    if (c == 'c')
    {
        if (--i < 0) abort();
        c = ts->buf[i];
    }
    if (c == 'p')
        return 1;
    else if (c == 'I')
        return 1;
    else if (c == 'M')
        return 1;
    else if (c == 'S')
        return 1;
    else if (c == ']')
    {
        int j = i - 1;
        for (; j >= 0; --j)
        {
            if (ts->buf[j] == '[') break;
        }
        if (j != i - 1)
        {
            int arity = 0;
            sscanf(ts->buf + j + 1, "%x]", &arity);
            if (arity <= 0) abort();
            return arity;
        }
    }
    char buf[64];
    memcpy(buf, ts->buf, ts->used);
    buf[ts->used] = 0;
    // typestr_format_english(ts, buf, sizeof(buf));
    return parser_ferror(rc, "error: attempt to size incomplete type '%s'\n", buf);
}

static int compile_decl_single(struct BackEnd* p, struct Decl* decl, struct CompoundBlock* bb)
{
    struct Symbol* const sym = &decl->sym;

    if (!decl->id) abort();
    const char* const name = token_str(p->parser, decl->id);
    if (decl->attr.symname)
    {
        if (strlen(decl->attr.symname) >= sizeof(sym->reg.rename.buf))
        {
            return parser_ferror(&decl->id->rc,
                                 "error: symbol names have a maximum length of %lu characters\n",
                                 sizeof(sym->reg.rename.buf) - 1);
        }
        strcpy(sym->reg.rename.buf, decl->attr.symname);
    }

    if (decl->is_function)
    {
        if (sym->reg.rename.buf[0])
        {
            if (sym->reg.rename.buf[0] != '$' || !sym->reg.rename.buf[1] ||
                sym->reg.rename.buf[strlen(sym->reg.rename.buf) - 1] != '$')
            {
                return parser_ferror(&decl->id->rc,
                                     "error: external symbol names must be of the form '$foo$' (was '%s')\n",
                                     sym->reg.rename.buf);
            }
        }
        else
        {
            sym->reg.rename = extern_var_from(p, name);
        }
    }
    else
    {
        // not a function
        if (!sym->reg.rename.buf[0]) sym->reg.rename = free_var_from(p, name);

        if (!sym->address_taken && (!bb->fn_sym || bb->fn_sym->decl->is_nonreentrant))
        {
            sym->reg.stack_addr = -1;
        }
        else if (!bb->fn_sym || decl->specs.is_static)
        {
            sym->reg.stack_addr = p->globals_size;
            sym->reg.is_global = 1;
            snprintf(sym->reg.mem_loc.buf, sizeof(sym->reg.mem_loc.buf), "%d", sym->reg.stack_addr);
            if (typestr_is_array(&sym->type))
            {
                sym->reg.rename = sym->reg.mem_loc;
            }
            p->globals_size += sizeof_typestr(&sym->type, &decl->id->rc);
        }
        else
        {
            sym->reg.stack_addr = bb->frame_size;
            sym->reg.mem_loc = free_var(p);
            bb->frame_size += sizeof_typestr(&sym->type, &decl->id->rc);
        }

        scope_insert(&p->scope, name, sym);
    }
    return 0;
}

static const char* relation_invert(const char* op)
{
    switch (op[0])
    {
        case '!': return "==";
        case '=': return "!=";
        case '<': return op[1] == '=' ? ">" : ">=";
        case '>': return op[1] == '=' ? "<" : "<=";
        default: abort();
    }
}

static int compile_conditional_expr(
    struct BackEnd* p, struct Expr* cond, struct CompoundBlock* bb, const char* jump_to, enum CondInverted inverted)
{
    if (cond->kind == EXPR_OP)
    {
        struct ExprOp* cond_op = (struct ExprOp*)cond;
        const char* const op = token_str(p->parser, cond_op->tok);
        if (op[0] == '!' && op[1] == '\0')
        {
            return compile_conditional_expr(p, cond_op->lhs, bb, jump_to, inverted ? COND_NORMAL : COND_INVERTED);
        }
        enum Precedence pr = op_precedence(op);
        if (pr == PRECEDENCE_EQUALITY || pr == PRECEDENCE_RELATION)
        {
            struct RegMap lhs_regmap = {0};
            struct ValDest lhs = dest_regmap(&lhs_regmap);
            if (compile_expr(p, cond_op->lhs, &lhs, bb)) return 1;
            struct RegMap rhs_regmap = {0};
            struct ValDest rhs = dest_regmap(&rhs_regmap);
            if (compile_expr(p, cond_op->rhs, &rhs, bb)) return 1;

            const char* const lhs_reg = parser_prepare_src_reg(p, &lhs, &cond_op->tok->rc);
            if (!lhs_reg) return 1;
            const char* const rhs_reg = parser_prepare_src_reg(p, &rhs, &cond_op->tok->rc);
            if (!rhs_reg) return 1;

            parser_deactivate_reg(&lhs_regmap);
            parser_deactivate_reg(&rhs_regmap);

            // Do if-stmt fusion
            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump_op(p->cg, jump_to, inverted ? relation_invert(op) : op, lhs_reg, rhs_reg);
            return 0;
        }
        else if (pr == PRECEDENCE_OR)
        {
            if (inverted == COND_NORMAL)
            {
                if (compile_conditional_expr(p, cond_op->lhs, bb, jump_to, COND_NORMAL)) return 1;
                return compile_conditional_expr(p, cond_op->rhs, bb, jump_to, COND_NORMAL);
            }
            else
            {
                struct FreeVar end_lbl = free_var_label(p);
                if (compile_conditional_expr(p, cond_op->lhs, bb, end_lbl.buf, COND_NORMAL)) return 1;
                if (compile_conditional_expr(p, cond_op->rhs, bb, jump_to, COND_INVERTED)) return 1;
                cg_mark_label(p->cg, end_lbl.buf);
                return 0;
            }
        }
        else if (pr == PRECEDENCE_AND)
        {
            if (inverted == COND_INVERTED)
            {
                if (compile_conditional_expr(p, cond_op->lhs, bb, jump_to, COND_INVERTED)) return 1;
                return compile_conditional_expr(p, cond_op->rhs, bb, jump_to, COND_INVERTED);
            }
            else
            {
                struct FreeVar end_lbl = free_var_label(p);
                if (compile_conditional_expr(p, cond_op->lhs, bb, end_lbl.buf, COND_INVERTED)) return 1;
                if (compile_conditional_expr(p, cond_op->rhs, bb, jump_to, COND_NORMAL)) return 1;
                cg_mark_label(p->cg, end_lbl.buf);
                return 0;
            }
        }
    }

    struct RegMap lhs_regmap = {0};
    struct ValDest lhs = dest_regmap(&lhs_regmap);
    if (compile_expr(p, cond, &lhs, bb)) return 1;
    const char* const lhs_reg = parser_prepare_src_reg(p, &lhs, &s_unknown_rc);
    if (!lhs_reg) return 1;
    parser_deactivate_reg(&lhs_regmap);
    if (parser_spill_registers(p)) return 1;
    cg_write_inst_jump_op(p->cg, jump_to, inverted ? "!=" : "==", lhs_reg, "true");
    return 0;
}

int be_compile(struct BackEnd* be)
{
    struct CompoundBlock new_cb;
    cb_init(&new_cb, NULL, be);
    struct Expr** const seqs = be->parser->expr_seqs.data;
    // first compile all global variables
    for (size_t i = 0; i < be->parser->arr_exprs.sz / sizeof(struct Expr*); ++i)
    {
        struct Expr* expr = ((struct Expr**)be->parser->arr_exprs.data)[i];
        if (expr->kind != STMT_DECLS) abort();
        struct StmtDecls* decls = (struct StmtDecls*)expr;
        for (size_t j = 0; j < decls->extent; ++j)
        {
            if (seqs[decls->offset + j]->kind != AST_DECL) abort();
            struct Decl* decl = (struct Decl*)seqs[decls->offset + j];
            if (compile_decl_single(be, decl, &new_cb)) return 1;
            if (decl->is_function) continue;
            if (compile_decl_init(be, decl, &new_cb)) return 1;
        }
    }
    // then compile all functions
    for (size_t i = 0; i < be->parser->arr_exprs.sz / sizeof(struct Expr*); ++i)
    {
        struct Expr* expr = ((struct Expr**)be->parser->arr_exprs.data)[i];
        if (expr->kind != STMT_DECLS) abort();
        struct StmtDecls* decls = (struct StmtDecls*)expr;
        for (size_t j = 0; j < decls->extent; ++j)
        {
            if (seqs[decls->offset + j]->kind != AST_DECL) abort();
            struct Decl* decl = (struct Decl*)seqs[decls->offset + j];
            if (!decl->is_function) continue;
            if (decl->specs.is_extern)
            {
                cg_declare_extern(be->cg, decl->name);
            }
            else
            {
                if (!decl->name) abort();
                cg_declare_public(be->cg, decl->name);
                cg_mark_label(be->cg, decl->name);
                if (compile_decl_init(be, decl, &new_cb)) return 1;
            }
        }
    }

    cg_emit(be->cg, be->globals_size);
    return 0;
}

#endif

void be_init(struct BackEnd* be, struct Parser* p, struct Elaborator* e, struct CodeGen* cg)
{
    be->parser = p;
    be->elab = e;
    be->cg = cg;

    array_init(&be->aszConstants);
    autoheap_init(&be->const_ref);
    autoheap_init(&be->heap);
    array_init(&be->code);
    scope_init(&be->scope);
}

static struct TACAddress be_push_tace(struct BackEnd* be, const struct TACEntry* e)
{
    const size_t offset = array_size(&be->code, sizeof(struct TACEntry));
    array_push(&be->code, e, sizeof(struct TACEntry));
    struct TACAddress ret = {
        .kind = TACA_REF,
        .ref = offset,
    };
    return ret;
}

static int be_compile_expr(struct BackEnd* be, struct Expr* e, struct TACAddress* out);
static int be_compile_lvalue(struct BackEnd* be, struct Expr* e, struct TACAddress* out);

static int be_compile_StmtDecls(struct BackEnd* be, struct StmtDecls* stmt, struct TACAddress* out)
{
    int rc = 0;
    struct Expr* const* const expr_seqs = (struct Expr**)be->parser->expr_seqs.data;
    for (size_t i = 0; i < stmt->extent; ++i)
    {
        UNWRAP(be_compile_expr(be, expr_seqs[stmt->offset + i], out));
    }

fail:
    return rc;
}

static int be_compile_StmtBlock(struct BackEnd* be, struct StmtBlock* stmt, struct TACAddress* out)
{
    int rc = 0;
    int start_frame_size = be->frame_size;
    struct Expr* const* const expr_seqs = (struct Expr**)be->parser->expr_seqs.data;
    for (size_t i = 0; i < stmt->extent; ++i)
    {
        UNWRAP(be_compile_expr(be, expr_seqs[stmt->offset + i], out));
    }
    be->frame_size = start_frame_size;

fail:
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

    if (e->tok->type == LEX_NUMBER)
    {
        *out = taca_literal(token_str(be->parser, e->tok));
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

        *out = taca_const(i);
    }
    else
    {
        rc = 1;
        parser_tok_error(e->tok, "error: unimplemented literal type (%d)\n", e->tok->type);
    }

    return rc;
}

static int be_compile_ExprSym(struct BackEnd* be, struct ExprSym* esym, struct TACAddress* out)
{
    if (!esym->decl) abort();
    struct Decl* decl = esym->decl;
    if (decl->parent_decl)
    {
        if (decl->arg_index > 0)
        {
            out->kind = TACA_ARG;
            out->arg_idx = decl->arg_index - 1;
        }
        else
        {
            out->kind = TACA_FRAME;
            out->frame_offset = decl_get_def(esym->decl)->frame_offset;
        }
    }
    else
    {
        out->kind = TACA_NAME;
        out->name = decl_get_def(esym->decl)->name;
    }
    return 0;
}

static int be_compile_ExprCall(struct BackEnd* be, struct ExprCall* e, struct TACAddress* out)
{
    if (!e->fn) return parser_ice_tok(e->tok);

    int rc = 0;

    struct Expr* const* const expr_seqs = (struct Expr**)be->parser->expr_seqs.data;
    if (e->extent > 4) return parser_tok_error(e->tok, "error: only 4 parameters are supported (%uz).\n", e->extent);

    struct TACAddress param_addr[4] = {};

    for (size_t i = e->extent; i > 0; --i)
    {
        UNWRAP(be_compile_expr(be, expr_seqs[e->offset + i - 1], param_addr + i - 1));
    }
    struct TACEntry call = {
        .op = TACO_CALL,
        .arg1 = s_taca_void,
        .arg2 = taca_imm(e->extent),
    };
    UNWRAP(be_compile_expr(be, e->fn, &call.arg1));

    struct TACEntry param = {
        .op = TACO_PARAM,
        .arg1 = s_taca_void,
        .arg2 =
            {
                .kind = TACA_PARAM,
                .param_idx = 0,
            },
    };
    for (size_t i = e->extent; i > 0; --i)
    {
        param.arg1 = param_addr[i - 1];
        param.arg2.param_idx = i - 1;
        be_push_tace(be, &param);
    }

    *out = be_push_tace(be, &call);

fail:
    return rc;
}

static int be_compile_ExprOp(struct BackEnd* be, struct ExprOp* e, struct TACAddress* out)
{
    int rc = 0;
    if (!e->lhs) return parser_ice_tok(e->tok);
    const char* const op = token_str(be->parser, e->tok);

    if (e->rhs)
    {
        if (op[0] == '*' && op[1] == '\0')
        {
            struct TACEntry tace = {
                .op = TACO_MULT,
            };
            UNWRAP(be_compile_expr(be, e->rhs, &tace.arg2));
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
            *out = be_push_tace(be, &tace);
        }
        else if (op[0] == '+' && op[1] == '\0')
        {
            struct TACEntry tace = {
                .op = TACO_ADD,
            };
            UNWRAP(be_compile_expr(be, e->rhs, &tace.arg2));
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
            *out = be_push_tace(be, &tace);
        }
        else if (op[0] == '-' && op[1] == '>')
        {
            struct TACEntry tace = {
                .op = TACO_LOAD,
            };
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
            *out = be_push_tace(be, &tace);
        }
        else if (op[0] == '.' && op[1] == '\0')
        {
            UNWRAP(be_compile_expr(be, e->lhs, out));
        }
        else if (op[0] == '/' && op[1] == '\0')
        {
            struct TACEntry tace = {
                .op = TACO_DIV,
            };
            UNWRAP(be_compile_expr(be, e->rhs, &tace.arg2));
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
            *out = be_push_tace(be, &tace);
        }
        else
        {
            UNWRAP(parser_tok_error(e->tok, "error: operation not yet implemented: %s\n", op));
        }
    }
    else
    {
        if (op[0] == '*' && op[1] == '\0')
        {
            struct TACEntry tace = {
                .op = TACO_LOAD,
            };
            UNWRAP(be_compile_expr(be, e->lhs, &tace.arg1));
            *out = be_push_tace(be, &tace);
        }
        else if (op[0] == '+' && op[1] == '\0')
        {
            UNWRAP(parser_tok_error(e->tok, "error: unary plus not yet implemented\n"));
        }
        else
        {
            UNWRAP(parser_tok_error(e->tok, "error: operation not yet implemented: %s\n", op));
        }
    }

fail:
    return rc;
}

#if 0
    if (op[0] == '!' && op[1] == '\0')
    {
        struct RegMap regmap = {0};
        struct ValDest lhs = dest_regmap(&regmap);
        if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
        const char* const srcreg = parser_prepare_src_reg(p, &lhs, &s_unknown_rc);
        if (!srcreg) return 1;
        const char* const dstreg = parser_prepare_dst_reg(p, dst, bb);
        if (!dstreg) return 1;
        cg_write_inst_op(p->cg, "!=", dstreg, srcreg, "true");
        parser_deactivate_reg(&regmap);
        return 0;
    }
    else if (op[0] == '&' && op[1] == '\0' && !e->rhs)
    {
        if (e->lhs->kind != EXPR_SYM)
        {
            return parser_tok_error(e->tok, "error: attempting to take address of non-symbol\n");
        }
        struct ExprSym* lhs_sym = (struct ExprSym*)e->lhs;
        if (lhs_sym->sym->is_register || lhs_sym->sym->reg.stack_addr == -1)
        {
            return parser_tok_error(e->tok, "error: attempting to take address of register\n");
        }
        if (lhs_sym->sym->reg.is_global)
        {
            char buf[16];
            snprintf(buf, 16, "%d", lhs_sym->sym->reg.stack_addr);
            struct ValDest vd = dest_literal(buf);
            return parser_assign_dsts(p, dst, &vd, bb, &s_unknown_rc);
        }
        else
        {
            const char* const dstreg = parser_prepare_dst_reg(p, dst, bb);
            if (!dstreg) return 1;

            cg_write_inst_add(p->cg, dstreg, "__ebp__", lhs_sym->sym->reg.stack_addr);
        }
        return 0;
    }
    else if (op[0] == '*' && op[1] == '\0' && !e->rhs)
    {
        if (dest_is_any(dst))
        {
            struct RegMap* dst_regmap = dst->regmap;
            if (compile_expr(p, e->lhs, dst, bb)) return 1;
            if (dst->is_reference)
            {
                if (dst->kind == DEST_REGMAP && dst->regmap == dst_regmap)
                {
                    if (cg_read_mem(p->cg, dst->regmap->rename.buf, dst->regmap->rename.buf, &s_unknown_rc)) return 1;
                }
                else
                {
                    struct ValDest tmp = dest_regmap(dst->regmap);
                    parser_prepare_dst_reg(p, &tmp, bb);
                    if (parser_assign_dsts(p, &tmp, dst, bb, &s_unknown_rc)) return 1;
                    *dst = tmp;
                }
            }
            dst->is_const = 0;
            dst->is_reference = 1;
            return 0;
        }
        else
        {
            struct RegMap regmap = {0};
            struct ValDest lhs = dest_regmap(&regmap);
            if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
            if (lhs.is_reference)
            {
                if (lhs.kind == DEST_REGMAP && lhs.regmap == &regmap)
                {
                    if (cg_read_mem(p->cg, lhs.regmap->rename.buf, lhs.regmap->rename.buf, &s_unknown_rc)) return 1;
                }
                else
                {
                    struct ValDest tmp = dest_regmap(&regmap);
                    parser_prepare_dst_reg(p, &tmp, bb);
                    if (parser_assign_dsts(p, &tmp, &lhs, bb, &s_unknown_rc)) return 1;
                    lhs = tmp;
                }
            }
            lhs.is_reference = 1;
            lhs.is_const = 0;
            if (parser_assign_dsts(p, dst, &lhs, bb, &s_unknown_rc)) return 1;
            parser_deactivate_reg(&regmap);
        }
        return 0;
    }
    else if (op[0] == '+' && op[1] == '+')
    {
        struct RegMap regmap = {0};
        struct ValDest lhs = dest_regmap(&regmap);
        if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
        const char* const srcreg = parser_prepare_src_reg(p, &lhs, &s_unknown_rc);
        if (!srcreg) return 1;
        const char* const dstreg = parser_prepare_dst_reg(p, &lhs, bb);
        if (!dstreg) return 1;
        cg_write_inst_op(p->cg, "+", dstreg, srcreg, "1");
        parser_deactivate_reg(&regmap);
        return 0;
    }
    else if (op[0] == '-' && op[1] == '-')
    {
        struct RegMap regmap = {0};
        struct ValDest lhs = dest_regmap(&regmap);
        if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
        const char* const srcreg = parser_prepare_src_reg(p, &lhs, &s_unknown_rc);
        if (!srcreg) return 1;
        const char* const dstreg = parser_prepare_dst_reg(p, &lhs, bb);
        if (!dstreg) return 1;
        cg_write_inst_op(p->cg, "-", dstreg, srcreg, "1");
        parser_deactivate_reg(&regmap);
        return 0;
    }

    if (!e->rhs) return parser_ice_tok(e->tok);

    if (op[0] == '=' && op[1] == '\0')
    {
        if (dest_is_any(dst))
        {
            if (compile_expr(p, e->lhs, dst, bb)) return 1;
            if (dst->is_const)
            {
                return parser_tok_error(e->tok, "error: illegal assignment to constant\n");
            }
            return compile_expr(p, e->rhs, dst, bb);
        }
        else
        {
            struct RegMap regmap = {0};
            struct ValDest lhs = dest_regmap(&regmap);
            if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
            if (lhs.is_const)
            {
                return parser_tok_error(e->tok, "error: illegal assignment to constant\n");
            }
            if (compile_expr(p, e->rhs, &lhs, bb)) return 1;
            if (parser_assign_dsts(p, dst, &lhs, bb, &s_unknown_rc)) return 1;
            parser_deactivate_reg(&regmap);
            return 0;
        }
    }

    if (op[0] == '+' && op[1] == '=')
    {
        if (dest_is_any(dst))
        {
            if (compile_expr(p, e->lhs, dst, bb)) return 1;
            if (dst->is_const)
            {
                return parser_tok_error(e->tok, "error: illegal assignment to constant\n");
            }
            struct RegMap regmapr = {0};
            struct ValDest rhs = dest_regmap(&regmapr);
            if (compile_expr(p, e->rhs, &rhs, bb)) return 1;
            const char* r2 = parser_prepare_src_reg(p, dst, &s_unknown_rc);
            if (!r2) return 1;
            const char* r3 = parser_prepare_src_reg(p, &rhs, &s_unknown_rc);
            if (!r3) return 1;
            const char* r1 = parser_prepare_dst_reg(p, dst, bb);
            if (!r1) return 1;
            cg_write_inst_op(p->cg, "+", r1, r2, r3);
            parser_deactivate_reg(&regmapr);
        }
        else
        {
            struct RegMap regmap = {0};
            struct ValDest lhs = dest_regmap(&regmap);
            if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
            if (lhs.is_const)
            {
                return parser_tok_error(e->tok, "error: illegal assignment to constant\n");
            }
            struct RegMap regmapr = {0};
            struct ValDest rhs = dest_regmap(&regmapr);
            if (compile_expr(p, e->rhs, &rhs, bb)) return 1;
            const char* r2 = parser_prepare_src_reg(p, &lhs, &s_unknown_rc);
            if (!r2) return 1;
            const char* r3 = parser_prepare_src_reg(p, &rhs, &s_unknown_rc);
            if (!r3) return 1;
            const char* r1 = parser_prepare_dst_reg(p, &lhs, bb);
            if (!r1) return 1;
            cg_write_inst_op(p->cg, "+", r1, r2, r3);
            if (parser_assign_dsts(p, dst, &lhs, bb, &s_unknown_rc)) return 1;
            parser_deactivate_reg(&regmapr);
            parser_deactivate_reg(&regmap);
            return 0;
        }
    }
    if (op[0] == '|' && op[1] == '|')
    {
        struct FreeVar end_lbl = free_var_label(p);
        struct RegMap regmapl = {0};
        struct ValDest tmp = dest_regmap(&regmapl);
        struct ValDest* p_lhs = dest_is_any(dst) ? dst : &tmp;

        const char* lhs_reg = parser_prepare_dst_reg(p, p_lhs, bb);
        if (!lhs_reg) return 1;
        cg_write_inst_set(p->cg, lhs_reg, "1");
        if (compile_conditional_expr(p, (struct Expr*)e, bb, end_lbl.buf, COND_NORMAL)) return 1;
        lhs_reg = parser_prepare_dst_reg(p, p_lhs, bb);
        cg_write_inst_set(p->cg, lhs_reg, "0");
        if (parser_spill_registers(p)) return 1;
        cg_mark_label(p->cg, end_lbl.buf);
        if (p_lhs != dst)
        {
            if (parser_assign_dsts(p, dst, p_lhs, bb, &s_unknown_rc)) return 1;
            parser_deactivate_reg(&regmapl);
        }
        return 0;
    }
    if (op[0] == '&' && op[1] == '&')
    {
        struct FreeVar end_lbl = free_var_label(p);
        struct RegMap regmapl = {0};
        struct ValDest tmp = dest_regmap(&regmapl);
        struct ValDest* p_lhs = dest_is_any(dst) ? dst : &tmp;

        const char* lhs_reg = parser_prepare_dst_reg(p, p_lhs, bb);
        if (!lhs_reg) return 1;
        cg_write_inst_set(p->cg, lhs_reg, "0");
        if (compile_conditional_expr(p, (struct Expr*)e, bb, end_lbl.buf, COND_INVERTED)) return 1;
        lhs_reg = parser_prepare_dst_reg(p, p_lhs, bb);
        cg_write_inst_set(p->cg, lhs_reg, "1");
        if (parser_spill_registers(p)) return 1;
        cg_mark_label(p->cg, end_lbl.buf);
        if (p_lhs != dst)
        {
            if (parser_assign_dsts(p, dst, p_lhs, bb, &s_unknown_rc)) return 1;
            parser_deactivate_reg(&regmapl);
        }
        return 0;
    }

    if (op[0] == '+' && op[1] == '\0')
    {
        return compile_expr_op_add(p, e->lhs, e->rhs, dst, bb, &s_unknown_rc);
    }

    if (op[0] == '[' && op[1] == '\0')
    {
        if (dest_is_any(dst))
        {
            if (compile_expr_op_add(p, e->lhs, e->rhs, dst, bb, &s_unknown_rc)) return 1;
            dst->is_reference = 1;
            dst->is_const = 0;
            return 0;
        }
        struct RegMap regmapl = {0};
        struct ValDest lhs = dest_regmap(&regmapl);
        if (compile_expr_op_add(p, e->lhs, e->rhs, &lhs, bb, &s_unknown_rc)) return 1;
        lhs.is_reference = 1;
        dst->is_const = 0;
        if (parser_assign_dsts(p, dst, &lhs, bb, &s_unknown_rc)) return 1;
        parser_deactivate_reg(&regmapl);
        return 0;
    }

    struct RegMap regmapl = {0};
    struct ValDest lhs = dest_regmap(&regmapl);
    struct RegMap regmapr = {0};
    struct ValDest rhs = dest_regmap(&regmapr);
    if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
    if (compile_expr(p, e->rhs, &rhs, bb)) return 1;
    const char* r2 = parser_prepare_src_reg(p, &lhs, &s_unknown_rc);
    if (!r2) return 1;
    const char* r3 = parser_prepare_src_reg(p, &rhs, &s_unknown_rc);
    if (!r3) return 1;
    const char* r1 = parser_prepare_dst_reg(p, dst, bb);
    if (!r1) return 1;
    cg_write_inst_op(p->cg, op, r1, r2, r3);
    parser_deactivate_reg(&regmapl);
    parser_deactivate_reg(&regmapr);
    return 0;
#endif

static int be_compile_StmtReturn(struct BackEnd* be, struct StmtReturn* stmt, struct TACAddress* out)
{
    int rc;
    struct TACEntry entry = {
        .op = TACO_RETURN,
    };
    UNWRAP(be_compile_expr(be, stmt->expr, &entry.arg1));
    be_push_tace(be, &entry);
    *out = s_taca_void;
fail:
    return rc;
}

static int be_compile_Decl(struct BackEnd* be, struct Decl* decl, struct TACAddress* out)
{
    int rc = 0;
    decl->frame_offset = be->frame_size;
    be->frame_size += 8;
    be->max_frame_size = be->max_frame_size < be->frame_size ? be->frame_size : be->max_frame_size;

    if (decl->init)
    {
        struct TACEntry init = {
            .op = TACO_ASSIGN,
            .arg1 =
                {
                    .kind = TACA_FRAME,
                    .frame_offset = decl->frame_offset,
                },
        };
        UNWRAP(be_compile_expr(be, decl->init, &init.arg2));
        be_push_tace(be, &init);
    }

    *out = s_taca_void;
fail:
    return rc;
}

static int be_compile_lvalue_ExprSym(struct BackEnd* be, struct ExprSym* e, struct TACAddress* out)
{
    if (!e->decl) abort();
    struct Decl* decl = e->decl;
    if (decl->parent_decl)
    {
        if (decl->arg_index > 0)
        {
            out->kind = TACA_ARG_ADDR;
            out->arg_idx = decl->arg_index - 1;
        }
        else
        {
            out->kind = TACA_FRAME_ADDR;
            out->frame_offset = decl_get_def(decl)->frame_offset;
        }
    }
    else
    {
        out->kind = TACA_NAME_ADDR;
        out->name = decl_get_def(decl)->name;
    }
    return 0;
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

    if (e->decl->frame_offset != 0)
    {
        switch (out->kind)
        {
            case TACA_FRAME_ADDR: out->frame_offset += e->decl->frame_offset; break;
            case TACA_FRAME:
            case TACA_ARG:
            {
                struct TACEntry tace = {
                    .op = TACO_ADD,
                    .arg1 = *out,
                    .arg2 =
                        {
                            .kind = TACA_IMM,
                            .imm = e->decl->frame_offset,
                        },
                };
                *out = be_push_tace(be, &tace);
                break;
            }
            default:
                UNWRAP(parser_tok_error(
                    e->tok, "error: %s: unhandled taca: %s (%d)\n", __func__, taca_to_string(out->kind), out->kind));
        }
    }

fail:
    return rc;
}

static int be_dereference(struct BackEnd* be, struct TACAddress* out)
{
    switch (out->kind)
    {
        case TACA_FRAME_ADDR: out->kind = TACA_FRAME; break;
        case TACA_ARG_ADDR: out->kind = TACA_ARG; break;
        case TACA_NAME_ADDR: out->kind = TACA_NAME; break;
        default:
        {
            struct TACEntry tace = {
                .op = TACO_LOAD,
                .arg1 = *out,
            };
            *out = be_push_tace(be, &tace);
            break;
        }
    }
    return 0;
}

static int be_compile_ExprField(struct BackEnd* be, struct ExprField* e, struct TACAddress* out)
{
    int rc = 0;
    UNWRAP(be_compile_lvalue_ExprField(be, e, out));
    UNWRAP(be_dereference(be, out));

fail:
    return rc;
}

static int be_compile_lvalue(struct BackEnd* be, struct Expr* e, struct TACAddress* out)
{
#define DISPATCH(ENUM, TYPE)                                                                                           \
    case ENUM: return be_compile_lvalue_##TYPE(be, (struct TYPE*)e, out);

    switch (e->kind)
    {
        DISPATCH(EXPR_SYM, ExprSym);
        // DISPATCH(EXPR_CALL, ExprCall);
        // DISPATCH(EXPR_LIT, ExprLit);
        // DISPATCH(EXPR_OP, ExprOp);
        case STMT_NONE: *out = s_taca_void; return 0;
        default:
            parser_ferror(expr_to_rc(e),
                          "error: be_compile_lvalue unhandled expr: %s (%d)\n",
                          ast_kind_to_string(e->kind),
                          e->kind);
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
        DISPATCH(EXPR_SYM, ExprSym);
        DISPATCH(STMT_DECLS, StmtDecls);
        DISPATCH(STMT_BLOCK, StmtBlock);
        DISPATCH(EXPR_CALL, ExprCall);
        DISPATCH(EXPR_LIT, ExprLit);
        DISPATCH(EXPR_OP, ExprOp);
        DISPATCH(EXPR_FIELD, ExprField);
        DISPATCH(STMT_RETURN, StmtReturn);
        DISPATCH(AST_DECL, Decl);
        case STMT_NONE: *out = s_taca_void; return 0;
        default:
            parser_ferror(expr_to_rc(e),
                          "error: be_compile_expr unhandled expr: %s (%d)\n",
                          ast_kind_to_string(e->kind),
                          e->kind);
            return 1;
    }
    return 0;
#undef DISPATCH
}

#if 0
        case STMT_RETURN:
        {
            bb->has_returned = 1;
            struct StmtReturn* stmt = (struct StmtReturn*)e;
            struct TypeStr* ty_fn = &bb->fn_sym->type;
            if (typestr_is_void_fn(ty_fn))
            {
                if (stmt->expr)
                {
                    return parser_tok_error(stmt->tok,
                                            "error: unexpected return expression in function of type 'void'\n");
                }
            }
            else
            {
                if (!stmt->expr)
                {
                    return parser_tok_error(stmt->tok, "error: expected return expression in non-void function\n");
                }
                struct RegMap regmap = {
                    .rename = s_freevar_paramreg0,
                };
                struct ValDest dst = dest_regmap(&regmap);
                if (compile_expr(p, stmt->expr, &dst, bb)) return 1;
                parser_deactivate_reg(&regmap);
            }
            if (parser_spill_registers(p)) return 1;
            if (!p->fn->is_stackless) cg_write_epilog(p->cg);
            cg_write_return(p->cg, &p->fn_ret_var);
            return 0;
        }
        case STMT_IF:
        {
            return compile_if_stmt(p, (struct StmtIf*)expr, bb);
        }
        case STMT_CONTINUE:
        {
            if (!bb->continue_label)
            {
                return parser_tok_error(((struct StmtContinue*)expr)->tok, "error: not inside a loop\n");
            }

            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump(p->cg, bb->continue_label);
            return 0;
        }
        case STMT_BREAK:
        {
            if (!bb->break_label)
            {
                return parser_tok_error(((struct StmtBreak*)expr)->tok, "error: not inside a loop\n");
            }

            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump(p->cg, bb->break_label);
            return 0;
        }
        case STMT_LOOP:
        {
            return compile_loop_stmt(p, (struct StmtLoop*)expr, bb);
        }
        case STMT_LABEL:
        {
            struct StmtLabel* label = (struct StmtLabel*)expr;
            const char* const str = token_str(p->parser, label->tok);
            char buf[64];
            if (sizeof(buf) <= snprintf(buf, sizeof(buf), "$%s%s$", p->fn_label_prefix, str))
            {
                return parser_tok_error(label->tok, "error: resource exceeded: label too long: '%s'\n", str);
            }
            if (parser_spill_registers(p)) return 1;
            cg_mark_label(p->cg, buf);
            bb->has_returned = 0;
            return compile_stmt_expr(p, label->stmt, bb);
        }
        case STMT_GOTO:
        {
            struct StmtGoto* stmt = (struct StmtGoto*)expr;
            char buf[64];
            const char* const cur_tok_str = token_str(p->parser, stmt->dst);
            if (sizeof(buf) <= snprintf(buf, sizeof(buf), "$%s%s$", p->fn_label_prefix, cur_tok_str))
            {
                fprintf(stderr, "resource exceeded: label too long: '%s'\n", cur_tok_str);
                abort();
            }
            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump(p->cg, buf);
            bb->has_returned = 1;
            return 0;
        }
        case STMT_NONE: return 0;
        default:
            if (ast_kind_is_expr(expr->kind))
            {
                // tc_expr(p, expr);
                if (parser_has_errors()) return 1;
                struct ValDest dst = s_void_destination;
                if (compile_expr(p, expr, &dst, bb)) return 1;
                return 0;
            }
            abort();
            break;
#endif

int be_compile(struct BackEnd* be)
{
    int rc = 0;

    // then compile all functions
    struct Expr* const* const expr_seqs = be->parser->expr_seqs.data;
    struct Decl* const* const decl_seqs = be->parser->expr_seqs.data;
    struct Expr* const* const exprs_data = be->parser->arr_exprs.data;
    const size_t num_exprs = array_size(&be->parser->arr_exprs, sizeof(struct Expr*));
    for (size_t i = 0; i < num_exprs; ++i)
    {
        struct Expr* expr = exprs_data[i];
        if (expr->kind != STMT_DECLS) abort();
        struct StmtDecls decls = *(struct StmtDecls*)expr;
        for (size_t j = 0; j < decls.extent; ++j)
        {
            if (expr_seqs[decls.offset + j]->kind != AST_DECL) abort();
            struct Decl* decl = decl_seqs[decls.offset + j];
            // skip struct definitions
            if (!decl->type) continue;
            if (decl->type->kind != AST_DECLFN) continue;
            struct DeclFn* declfn = (struct DeclFn*)decl->type;
            if (!decl->name) abort();
            if (decl->specs->is_extern)
            {
                cg_declare_extern(be->cg, decl->name);
            }
            else
            {
                cg_declare_public(be->cg, decl->name);
            }
            if (decl->init)
            {
                cg_mark_label(be->cg, decl->name);
                be->frame_size = 0;
                be->max_frame_size = 0;

                struct TACEntry entry = {.op = TACO_ARG};
                for (size_t i = 0; i < declfn->extent; ++i)
                {
                    entry.arg1.kind = TACA_ARG;
                    entry.arg1.arg_idx = i;
                    be_push_tace(be, &entry);
                }

                UNWRAP(be_compile_expr(be, decl->init, &entry.arg1));

                UNWRAP(cg_gen_taces(be->cg,
                                    (struct TACEntry*)be->code.data,
                                    array_size(&be->code, sizeof(struct TACEntry)),
                                    be->max_frame_size));
                array_clear(&be->code);
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