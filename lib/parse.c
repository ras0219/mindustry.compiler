#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "cg.h"
#include "lexstate.h"
#include "parsestate.h"
#include "pool.h"
#include "symbol.h"
#include "tok.h"

struct Token
{
    struct RowCol rc;
    LexerState type;
    ptrdiff_t sp_offset;
};

enum ValDestKind
{
    DEST_VOID,
    DEST_REGMAP,
    DEST_SYM,
    DEST_LITERAL,
};

struct ValDest
{
    union
    {
        const char* literal;
        struct Symbol* sym;
        struct RegMap* regmap;
    };
    enum ValDestKind kind : 8;
    int is_const : 1;
    int is_reference : 1;
};

static const struct ValDest s_void_destination = {
    .kind = DEST_VOID,
    .is_const = 0,
};

struct Decl* decl_get_def(struct Decl* decl)
{
    while (decl->def)
        decl = decl->def;
    return decl;
}

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
static struct RowCol s_unknown_rc = {
    .file = "<unknown>",
    .row = 1,
    .col = 1,
};

static char* token_str(Parser* p, const struct Token* tk) { return (char*)p->stringpool.data + tk->sp_offset; }
static int token_is_sym(Parser* p, const struct Token* tk, char sym)
{
    return tk->type == LEX_SYMBOL && token_str(p, tk)[0] == sym;
}
static char token_expect_comma_or_cparen(Parser* p, const struct Token* tk)
{
    if (tk->type == LEX_SYMBOL)
    {
        const char ch = token_str(p, tk)[0];
        if (ch == ')' || ch == ',') return ch;
    }
    return parser_ferror(&tk->rc, "error: expected ',' or ')'\n"), '\0';
}
static struct Token* token_consume_sym(Parser* p, struct Token* tk, char sym)
{
    if (token_is_sym(p, tk, sym))
    {
        return tk + 1;
    }
    return parser_ferror(&tk->rc, "error: expected '%c'\n", sym), NULL;
}

#define REG_COUNT 6

static const char* PARAM_NAMES_ARR[REG_COUNT] = {"reg1", "reg2", "reg3", "reg4", "reg5", "reg6"};
static struct FreeVar s_freevar_paramreg0 = {
    .buf = "reg1",
};

static struct FreeVar s_freevar_zero = {0};
static struct FreeVar free_var(Parser* p)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "_%d", p->free_var_counter++);
    return ret;
}
static struct FreeVar free_var_label(Parser* p)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "$%d$", p->free_var_counter++);
    return ret;
}

static struct FreeVar free_var_from(Parser* p, const char* base)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "_%d_%s", p->free_var_counter++, base);
    return ret;
}
static struct FreeVar extern_var_from(Parser* p, const char* base)
{
    struct FreeVar ret;
    if (sizeof(ret.buf) <= snprintf(ret.buf, sizeof(ret.buf), "$%s$", base))
    {
        fprintf(stderr, "resource exceeded -- extern identifier too long (max %zu chars)\n", sizeof(ret.buf) - 3);
        abort();
    }
    return ret;
}
static struct FreeVar nrfun_param(Parser* p, int index, const char* fun)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "_%d_%s", index, fun);
    return ret;
}
static struct FreeVar nrfun_param_ret(Parser* p, const char* fun)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "_r_%s", fun);
    return ret;
}

static void symbol_init(struct Symbol* ret)
{
    memset(ret, 0, sizeof(struct Symbol));
    ret->kind.kind = AST_SYM;
}
static int symbol_is_equivalent_redecl(struct Symbol* orig, struct Symbol* redecl) { return 1; }

struct Binding
{
    size_t ident_offset;
    struct Symbol* sym;
};
static void scope_init(struct Scope* s)
{
    array_init(&s->binds);
    array_init(&s->strings);
}
static void scope_destroy(struct Scope* s)
{
    array_destroy(&s->binds);
    array_destroy(&s->strings);
}
static size_t scope_size(struct Scope* s) { return s->binds.sz / sizeof(struct Binding); }
static struct Binding* scope_data(struct Scope* s) { return s->binds.data; }
static void scope_shrink(struct Scope* s, size_t sz)
{
    if (sz < scope_size(s))
    {
        s->strings.sz = scope_data(s)[sz].ident_offset;
        s->binds.sz = sz * sizeof(struct Binding);
    }
}
static void scope_pop(struct Scope* s)
{
    const size_t sz = scope_size(s) - 1;
    s->strings.sz = scope_data(s)[sz].ident_offset;
    s->binds.sz = sz * sizeof(struct Binding);
}
static size_t scope_insert(struct Scope* s, const char* ident, struct Symbol* sym)
{
    const size_t sz = scope_size(s);
    struct Binding* const e = array_alloc(&s->binds, sizeof(struct Binding));
    e->ident_offset = s->strings.sz;
    e->sym = sym;
    array_push(&s->strings, ident, strlen(ident) + 1);
    return sz;
}
static struct Binding* scope_find(struct Scope* s, const char* id)
{
    struct Binding* const begin = scope_data(s);
    const size_t sz = scope_size(s);
    for (size_t i = 0; i < sz; ++i)
    {
        struct Binding* const e = begin + sz - i - 1;
        const char* const e_id = (char*)s->strings.data + e->ident_offset;
        if (strcmp(e_id, id) == 0)
        {
            return e;
        }
    }
    return NULL;
}

// requires !reg->prev
static void parser_push_active_reg(struct Parser* p, struct RegMap* reg)
{
    if (!reg->rename.buf[0]) abort();
    if (reg->stack_addr > 0 && !reg->is_global)
    {
        if (!reg->mem_loc.buf[0]) abort();
        cg_write_inst_add(&p->cg, reg->mem_loc.buf, "__ebp__", reg->stack_addr);
    }
    if (reg->next = p->first_active_reg)
    {
        reg->next->prev = &reg->next;
    }
    p->first_active_reg = reg;
    reg->prev = &p->first_active_reg;
}
static void parser_deactivate_reg(struct Parser* p, struct RegMap* reg)
{
    if (reg->prev) *reg->prev = reg->next;
    if (reg->next) reg->next->prev = reg->prev;
    reg->next = NULL;
    reg->prev = NULL;
}
static int parser_flush_dirty(struct Parser* p)
{
    struct RegMap* s = p->first_active_reg;
    while (s)
    {
        struct RegMap* const next = s->next;
        if (s->is_dirty)
        {
            if (cg_write_mem(&p->cg, s->mem_loc.buf, s->rename.buf, &s_unknown_rc)) return 1;
            s->is_dirty = 0;
        }
        s = next;
    }
    return 0;
}
static int parser_invalidate_reads(struct Parser* p)
{
    struct RegMap** ps = &p->first_active_reg;
    struct RegMap* s;
    while (s = *ps)
    {
        if (s->stack_addr >= 0 && !s->is_const)
        {
            if (s->is_dirty)
            {
                if (cg_write_mem(&p->cg, s->mem_loc.buf, s->rename.buf, &s_unknown_rc)) return 1;
                s->is_dirty = 0;
            }
            parser_deactivate_reg(p, s);
        }
        else
        {
            ps = &s->next;
        }
    }
    return 0;
}
static int parser_ensure_loaded_reg(struct Parser* p, struct RegMap* reg, const struct RowCol* rc)
{
    if (!reg->prev && reg->stack_addr >= 0)
    {
        if (parser_flush_dirty(p)) return 1;
        parser_push_active_reg(p, reg);
        return cg_read_mem(&p->cg, reg->mem_loc.buf, reg->rename.buf, rc);
    }
    return 0;
}

static int typestr_is_array(struct TypeStr* ts)
{
    if (!ts->used) return 0;
    return ts->buf[ts->used - 1] == ']';
}
static int typestr_is_pointer(struct TypeStr* ts)
{
    if (!ts->used) return 0;
    return ts->buf[ts->used - 1] == 'p';
}

static int parser_ensure_loaded_sym(struct Parser* p, struct Symbol* sym)
{
    if (!sym->is_register)
    {
        if (typestr_is_array(&sym->type))
        {
            if (!sym->reg.prev && !sym->reg.is_global)
            {
                parser_push_active_reg(p, &sym->reg);
                cg_write_inst_add(&p->cg, sym->reg.rename.buf, "__ebp__", sym->reg.stack_addr);
                sym->reg.is_const = 1;
            }
        }
        else
        {
            return parser_ensure_loaded_reg(p, &sym->reg, &sym->decl->id->rc);
        }
    }
    return 0;
}
static void parser_ensure_dirty_reg(struct Parser* p, struct RegMap* reg)
{
    if (!reg->prev && reg->stack_addr >= 0)
    {
        parser_push_active_reg(p, reg);
    }
    reg->is_dirty = 1;
}
static void parser_ensure_dirty_sym(struct Parser* p, struct Symbol* sym)
{
    if (!sym->is_register)
    {
        parser_ensure_dirty_reg(p, &sym->reg);
    }
}
struct CompoundBlock
{
    struct Symbol* fn_sym;
    int frame_size;
    int has_returned;
    size_t scope_sz;

    const char* continue_label;
    const char* break_label;
};
static void cb_init(struct CompoundBlock* cb, struct CompoundBlock* parent, struct Parser* p)
{
    cb->fn_sym = parent ? parent->fn_sym : NULL;
    cb->frame_size = parent ? parent->frame_size : 1;
    cb->has_returned = 0;
    cb->continue_label = parent ? parent->continue_label : NULL;
    cb->break_label = parent ? parent->break_label : NULL;
    cb->scope_sz = scope_size(&p->scope);
}
static void cb_destroy(struct CompoundBlock* cb, struct Parser* p)
{
    size_t new_scope_sz = scope_size(&p->scope);
    struct Binding* const bind_begin = scope_data(&p->scope);
    for (size_t n = cb->scope_sz; n < new_scope_sz; ++n)
    {
        parser_deactivate_reg(p, &bind_begin[n].sym->reg);
    }
    scope_shrink(&p->scope, cb->scope_sz);
}
static int parser_spill_registers(struct Parser* p)
{
    struct RegMap* s = p->first_active_reg;
    p->first_active_reg = NULL;
    while (s)
    {
        struct RegMap* const next = s->next;
        if (s->is_dirty)
        {
            if (cg_write_mem(&p->cg, s->mem_loc.buf, s->rename.buf, &s_unknown_rc)) return 1;
            s->is_dirty = 0;
        }
        s->prev = NULL;
        s->next = NULL;
        s = next;
    }
    return 0;
}
enum Precedence
{
    PRECEDENCE_ERROR,
    PRECEDENCE_COMMA,
    PRECEDENCE_ASSIGN,
    PRECEDENCE_OR,
    PRECEDENCE_AND,
    PRECEDENCE_EQUALITY,
    PRECEDENCE_RELATION,
    PRECEDENCE_ADD,
    PRECEDENCE_MULT,
};

static int compile_expr(Parser* p, struct Expr* expr, struct ValDest* dst, struct CompoundBlock* bb);

static char* my_strdup(const char* s) { return strcpy(malloc(strlen(s) + 1), s); }

static const char* parser_prepare_dst_reg(Parser* p, struct ValDest* dst, struct CompoundBlock* bb)
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
static const char* parser_prepare_src_reg(Parser* p, const struct ValDest* dst, const struct RowCol* rc)
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

static int parser_assign_dsts(
    struct Parser* p, struct ValDest* dst, const struct ValDest* src, struct CompoundBlock* bb, const struct RowCol* rc)
{
    struct CodeGen* const cg = &p->cg;
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
            if (cg_read_mem(&p->cg, src_reg, "_", rc)) return 1;
            src_reg = "_";
        }

        if (cg_write_mem(&p->cg, dst_reg, src_reg, rc)) return 1;
        if (parser_invalidate_reads(p)) return 1;
        return 0;
    }

    const char* dst_reg = parser_prepare_dst_reg(p, dst, bb);
    if (!dst_reg) return 1;

    if (src->is_reference)
    {
        if (cg_read_mem(&p->cg, src_reg, dst_reg, rc)) return 1;
        return 0;
    }

    if (src->kind == DEST_SYM && typestr_is_array(&src->sym->type))
    {
        if (!src->sym->reg.is_global)
        {
            cg_write_inst_add(&p->cg, dst_reg, "__ebp__", src->sym->reg.stack_addr);
        }
        else
        {
            cg_write_inst_set(&p->cg, dst_reg, src->sym->reg.rename.buf);
        }
    }
    else
    {
        cg_write_inst_set(cg, dst_reg, src_reg);
    }
    return 0;
}
static enum Precedence op_precedence(const char* op)
{
    switch (op[0])
    {
        case '=':
            if (op[1] == '=')
                return PRECEDENCE_EQUALITY;
            else
                return PRECEDENCE_ASSIGN;
        case '!':
            if (op[1] == '=')
                return PRECEDENCE_EQUALITY;
            else
                return PRECEDENCE_ERROR;
        case '|':
            if (op[1] == '|')
                return PRECEDENCE_OR;
            else
                return PRECEDENCE_ERROR;
        case '&':
            if (op[1] == '&')
                return PRECEDENCE_AND;
            else
                return PRECEDENCE_ERROR;
        case '>':
        case '<': return PRECEDENCE_RELATION;
        case '+':
            if (op[1] == '+') return PRECEDENCE_ERROR;
        case '-':
            return PRECEDENCE_ADD;
            if (op[1] == '-') return PRECEDENCE_ERROR;
        case '*':
        case '%':
        case '/': return PRECEDENCE_MULT;
        default: return PRECEDENCE_ERROR;
    }
}
static int op_precedence_assoc_right(enum Precedence p) { return p == PRECEDENCE_ASSIGN; }

static struct ExprOp* parse_alloc_expr_op(Parser* p, struct Token* tok, struct Expr* lhs, struct Expr* rhs)
{
    struct ExprOp* e = (struct ExprOp*)pool_alloc(&p->ast_pools[EXPR_OP], sizeof(struct ExprOp));
    e->kind.kind = EXPR_OP;
    e->tok = tok;
    e->lhs = lhs;
    e->rhs = rhs;
    return e;
}

static struct ExprSym* parse_alloc_expr_sym(Parser* p, struct Token* tok, struct Symbol* sym)
{
    struct ExprSym* e = (struct ExprSym*)pool_alloc(&p->ast_pools[EXPR_SYM], sizeof(struct ExprSym));
    e->kind.kind = EXPR_SYM;
    e->tok = tok;
    e->sym = sym;
    return e;
}
static struct ExprLit* parse_alloc_expr_lit(Parser* p, struct Token* tok)
{
    struct ExprLit* e = (struct ExprLit*)pool_alloc(&p->ast_pools[EXPR_LIT], sizeof(struct ExprLit));
    e->kind.kind = EXPR_LIT;
    e->tok = tok;
    return e;
}
static struct ExprCall* parse_alloc_expr_call(Parser* p, struct Token* tok, struct Expr* fn, size_t off, size_t ext)
{
    struct ExprCall* e = (struct ExprCall*)pool_alloc(&p->ast_pools[EXPR_CALL], sizeof(struct ExprCall));
    e->kind.kind = EXPR_CALL;
    e->tok = tok;
    e->fn = fn;
    e->offset = off;
    e->extent = ext;
    return e;
}

static struct Token* parse_expr(Parser* p, struct Token* cur_tok, struct Expr** ppe, int precedence);

static struct Token* parse_expr_post_unary(Parser* p, struct Token* cur_tok, struct Expr* lhs, struct Expr** ppe)
{
    if (cur_tok->type == LEX_SYMBOL)
    {
        struct Token* tok_op = cur_tok;
        char* op = token_str(p, tok_op);

        if (op[0] == '(')
        {
            struct Token* tk = ++cur_tok;
            struct ExprCall* op_expr;
            if (tk->type == LEX_SYMBOL && token_str(p, tk)[0] == ')')
            {
                ++cur_tok;
                op_expr = parse_alloc_expr_call(p, tok_op, lhs, 0, 0);
            }
            else
            {
                struct Expr* arg_exprs[REG_COUNT];
                size_t i = 0;
                do
                {
                    if (i == REG_COUNT)
                        return parser_ferror(
                                   &cur_tok->rc, "error: too many arguments for function (%d supported)", REG_COUNT),
                               NULL;
                    if (!(cur_tok = parse_expr(p, cur_tok, arg_exprs + i++, PRECEDENCE_ASSIGN))) return NULL;
                    char ch = token_expect_comma_or_cparen(p, cur_tok++);
                    if (ch == ',')
                        continue;
                    else if (ch == ')')
                        break;
                    else
                        return NULL;
                } while (1);
                size_t offset = p->expr_seqs.sz / sizeof(struct Expr*);
                size_t extent = i;
                array_push(&p->expr_seqs, arg_exprs, i * sizeof(struct Expr*));
                op_expr = parse_alloc_expr_call(p, tok_op, lhs, offset, extent);
            }
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)op_expr, ppe);
        }
        else if (op[0] == '[')
        {
            struct ExprOp* e = parse_alloc_expr_op(p, cur_tok++, lhs, NULL);
            if (!(cur_tok = parse_expr(p, cur_tok, &e->rhs, PRECEDENCE_COMMA))) return NULL;
            if (!(cur_tok = token_consume_sym(p, cur_tok, ']'))) return NULL;
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)e, ppe);
        }
        else if (op[0] == '+' && op[1] == '+')
        {
            struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, lhs, NULL);
            return parse_expr_post_unary(p, cur_tok + 1, (struct Expr*)e, ppe);
        }
        else if (op[0] == '-' && op[1] == '-')
        {
            struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, lhs, NULL);
            return parse_expr_post_unary(p, cur_tok + 1, (struct Expr*)e, ppe);
        }
    }
    *ppe = (struct Expr*)lhs;
    return cur_tok;
}
static struct Token* parse_expr_unary_atom(Parser* p, struct Token* cur_tok, struct Expr** ppe)
{
    switch (cur_tok->type)
    {
        case LEX_SYMBOL:
        {
            char* op = token_str(p, cur_tok);
            if ((op[0] == '!' || op[0] == '&' || op[0] == '*') && op[1] == '\0')
            {
                struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, NULL, NULL);
                *ppe = (struct Expr*)e;
                return parse_expr_unary_atom(p, cur_tok + 1, &e->lhs);
            }
            else if (op[0] == '(' && op[1] == '\0')
            {
                if (!(cur_tok = parse_expr(p, cur_tok + 1, ppe, PRECEDENCE_COMMA))) return NULL;
                return token_consume_sym(p, cur_tok, ')');
            }
            break;
        }
        case LEX_IDENT:
        {
            struct Token* lhs = cur_tok++;
            const char* lhs_str = token_str(p, lhs);
            struct Binding* const lhs_bind = scope_find(&p->scope, lhs_str);
            if (!lhs_bind)
            {
                return parser_ferror(&lhs->rc, "error: '%s' undeclared\n", lhs_str), NULL;
            }
            struct ExprSym* lhs_expr = parse_alloc_expr_sym(p, lhs, lhs_bind->sym);
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)lhs_expr, ppe);
        }
        case LEX_NUMBER:
        case LEX_STRING:
        {
            struct ExprLit* lhs_expr = parse_alloc_expr_lit(p, cur_tok++);
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)lhs_expr, ppe);
        }
        default: break;
    }
    return parser_ferror(&cur_tok->rc, "error: expected expression\n"), NULL;
}
static struct Token* parse_expr_continue(
    Parser* p, struct Token* cur_tok, struct Expr* lhs, struct Expr** ppe, enum Precedence precedence)
{
    if (cur_tok->type == LEX_SYMBOL)
    {
        struct Token* tok_op = cur_tok;
        char* op = token_str(p, tok_op);
        enum Precedence op_prec = op_precedence(op);
        if (op_prec)
        {
            if (precedence <= op_prec && op_precedence_assoc_right(op_prec))
            {
                if (op[0] == '=' && op[1] == '\0' && precedence <= PRECEDENCE_ASSIGN)
                {
                    struct ExprOp* op_expr = parse_alloc_expr_op(p, tok_op, lhs, NULL);
                    *ppe = (struct Expr*)op_expr;
                    if (!(cur_tok = parse_expr(p, cur_tok + 1, &op_expr->rhs, PRECEDENCE_ASSIGN))) return NULL;
                }
                else
                {
                    *ppe = lhs;
                }
                return cur_tok;
            }
            else if (precedence < op_prec)
            {
                struct Expr* rhs;
                if (!(cur_tok = parse_expr(p, cur_tok + 1, &rhs, op_prec))) return NULL;
                struct ExprOp* op_expr = parse_alloc_expr_op(p, tok_op, lhs, rhs);
                return parse_expr_continue(p, cur_tok, (struct Expr*)op_expr, ppe, precedence);
            }
        }
    }
    *ppe = lhs;
    return cur_tok;
}
static struct Token* parse_expr(Parser* p, struct Token* cur_tok, struct Expr** ppe, int precedence)
{
    struct Expr* lhs;
    if (!(cur_tok = parse_expr_unary_atom(p, cur_tok, &lhs))) return NULL;

    return parse_expr_continue(p, cur_tok, lhs, ppe, precedence);
}
enum TypeKind
{
    TYPE_UNKNOWN,
    TYPE_SYM,
    TYPE_INT,
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_MSTRING,
    TYPE_FUN,
    TYPE_PTR,
    TYPE_ARR,
};

enum ValueCategory
{
    CAT_UNKNOWN,
    CAT_LVALUE,
    CAT_XVALUE,
    CAT_PRVALUE,
};
struct TypeCategory
{
    enum ValueCategory cat;
    struct TypeStr* type;
};

static struct TypeStr s_type_unknown = {};
static struct TypeCategory s_typecat_unknown = {
    .type = &s_type_unknown,
    .cat = CAT_UNKNOWN,
};
static struct TypeStr s_type_literal_int = {
    .buf = {'I', 'c'},
    .used = 2,
};
static struct TypeStr s_type_int = {
    .buf = {'I'},
    .used = 1,
};
static struct TypeStr s_type_void = {
    .buf = {'V'},
    .used = 1,
};
static struct TypeStr s_type_char = {
    .buf = {'C'},
    .used = 1,
};
static struct TypeStr s_type_mstr = {
    .buf = {'M'},
    .used = 1,
};
static struct TypeStr s_type_unit = {
    .buf = {'U'},
    .used = 1,
};
static struct TypeStr s_type_literal_mstr = {
    .buf = {'M', 'c'},
    .used = 2,
};

static int typestr_add_const(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    char c = ts->buf[ts->used - 1];
    if (c == 'c') return 0;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = 'c';
    return 0;
}
static int typestr_add_pointer(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = 'p';
    return 0;
}
static int typestr_start_call(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = '(';
    return 0;
}
static int typestr_add_arg(struct TypeStr* ts, struct TypeStr* arg)
{
    if (!ts->used) return 1;
    if (ts->buf[ts->used - 1] != '(')
    {
        if (ts->used == sizeof(ts->buf)) return 1;
        ts->buf[ts->used++] = ',';
    }
    if (ts->used + arg->used > sizeof(ts->buf)) return 1;
    memcpy(ts->buf + ts->used, arg->buf, arg->used);
    ts->used += arg->used;
    return 0;
}
static int typestr_end_call(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = ')';
    return 0;
}
static int typestr_add_arr(struct TypeStr* ts, int arity)
{
    if (!ts->used) return 1;
    if (arity < 0)
    {
        if (ts->used + 2 > sizeof(ts->buf)) return 1;
        ts->buf[ts->used++] = '[';
        ts->buf[ts->used++] = ']';
    }
    else
    {
        char buf[32];
        int to_write = snprintf(buf, 32, "[%x]", arity);
        if (ts->used + to_write > sizeof(ts->buf)) return 1;
        memcpy(ts->buf + ts->used, buf, to_write);
        ts->used += to_write;
    }
    return 0;
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

static int compile_expr_lit(Parser* p, struct ExprLit* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    if (e->tok->type == LEX_NUMBER)
    {
        struct ValDest src = dest_literal(token_str(p, e->tok));
        return parser_assign_dsts(p, dst, &src, bb, &e->tok->rc);
    }
    else if (e->tok->type == LEX_STRING)
    {
        const char* s = token_str(p, e->tok);
        const size_t slen = strlen(s);
        if (memchr(s, '"', slen))
        {
            return parser_ferror(&e->tok->rc, "error: string constants cannot represent \"\n");
        }
        char* heap_s = (char*)malloc(slen + 3);
        array_push(&p->strings_to_free, &heap_s, sizeof(heap_s));
        heap_s[0] = '"';
        strcpy(heap_s + 1, s);
        heap_s[slen + 1] = '"';
        heap_s[slen + 2] = '\0';
        struct ValDest src = dest_literal(heap_s);
        return parser_assign_dsts(p, dst, &src, bb, &e->tok->rc);
    }
    else
        abort();
}

enum CondInverted
{
    COND_NORMAL,
    COND_INVERTED,
};
static int compile_conditional_expr(
    Parser* p, struct Expr* cond, struct CompoundBlock* bb, const char* jump_to, enum CondInverted inverted);

static int compile_expr_op_add(Parser* p,
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
                int offset = atoi(token_str(p, l->tok));
                char buf[16];
                snprintf(buf, 16, "%d", s->sym->reg.stack_addr + offset);
                struct ValDest vd = dest_literal(buf);
                return parser_assign_dsts(p, dst, &vd, bb, rc);
            }
            else
            {
                const char* dst_reg = parser_prepare_dst_reg(p, dst, bb);
                if (!dst_reg) return 1;
                int offset = atoi(token_str(p, l->tok));
                cg_write_inst_add(&p->cg, dst_reg, "__ebp__", s->sym->reg.stack_addr + offset);
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
    cg_write_inst_op(&p->cg, "+", r1, r2, r3);
    parser_deactivate_reg(p, &regmapl);
    parser_deactivate_reg(p, &regmapr);
    return 0;
}

static int compile_expr_op(Parser* p, struct ExprOp* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    if (!e->lhs) return parser_ice(&e->tok->rc);

    const char* const op = token_str(p, e->tok);
    if (op[0] == '!' && op[1] == '\0')
    {
        struct RegMap regmap = {0};
        struct ValDest lhs = dest_regmap(&regmap);
        if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
        const char* const srcreg = parser_prepare_src_reg(p, &lhs, &e->tok->rc);
        if (!srcreg) return 1;
        const char* const dstreg = parser_prepare_dst_reg(p, dst, bb);
        if (!dstreg) return 1;
        cg_write_inst_op(&p->cg, "!=", dstreg, srcreg, "true");
        parser_deactivate_reg(p, &regmap);
        return 0;
    }
    else if (op[0] == '&' && op[1] == '\0' && !e->rhs)
    {
        if (e->lhs->kind != EXPR_SYM)
        {
            return parser_ferror(&e->tok->rc, "error: attempting to take address of non-symbol\n");
        }
        struct ExprSym* lhs_sym = (struct ExprSym*)e->lhs;
        if (lhs_sym->sym->is_register || lhs_sym->sym->reg.stack_addr == -1)
        {
            return parser_ferror(&e->tok->rc, "error: attempting to take address of register\n");
        }
        const char* const dstreg = parser_prepare_dst_reg(p, dst, bb);
        if (!dstreg) return 1;
        char buf[16];
        snprintf(buf, 16, "%d", lhs_sym->sym->reg.stack_addr);

        cg_write_inst_op(&p->cg, "+", dstreg, "__ebp__", buf);
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
                    if (cg_read_mem(&p->cg, dst->regmap->rename.buf, dst->regmap->rename.buf, &e->tok->rc)) return 1;
                }
                else
                {
                    struct ValDest tmp = dest_regmap(dst->regmap);
                    parser_prepare_dst_reg(p, &tmp, bb);
                    if (parser_assign_dsts(p, &tmp, dst, bb, &e->tok->rc)) return 1;
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
                    if (cg_read_mem(&p->cg, lhs.regmap->rename.buf, lhs.regmap->rename.buf, &e->tok->rc)) return 1;
                }
                else
                {
                    struct ValDest tmp = dest_regmap(&regmap);
                    parser_prepare_dst_reg(p, &tmp, bb);
                    if (parser_assign_dsts(p, &tmp, &lhs, bb, &e->tok->rc)) return 1;
                    lhs = tmp;
                }
            }
            lhs.is_reference = 1;
            lhs.is_const = 0;
            if (parser_assign_dsts(p, dst, &lhs, bb, &e->tok->rc)) return 1;
            parser_deactivate_reg(p, &regmap);
        }
        return 0;
    }
    else if (op[0] == '+' && op[1] == '+')
    {
        struct RegMap regmap = {0};
        struct ValDest lhs = dest_regmap(&regmap);
        if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
        const char* const srcreg = parser_prepare_src_reg(p, &lhs, &e->tok->rc);
        if (!srcreg) return 1;
        const char* const dstreg = parser_prepare_dst_reg(p, &lhs, bb);
        if (!dstreg) return 1;
        cg_write_inst_op(&p->cg, "+", dstreg, srcreg, "1");
        parser_deactivate_reg(p, &regmap);
        return 0;
    }
    else if (op[0] == '-' && op[1] == '-')
    {
        struct RegMap regmap = {0};
        struct ValDest lhs = dest_regmap(&regmap);
        if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
        const char* const srcreg = parser_prepare_src_reg(p, &lhs, &e->tok->rc);
        if (!srcreg) return 1;
        const char* const dstreg = parser_prepare_dst_reg(p, &lhs, bb);
        if (!dstreg) return 1;
        cg_write_inst_op(&p->cg, "-", dstreg, srcreg, "1");
        parser_deactivate_reg(p, &regmap);
        return 0;
    }

    if (!e->rhs) return parser_ice(&e->tok->rc);

    if (op[0] == '=' && op[1] == '\0')
    {
        if (dest_is_any(dst))
        {
            if (compile_expr(p, e->lhs, dst, bb)) return 1;
            if (dst->is_const)
            {
                return parser_ferror(&e->tok->rc, "error: illegal assignment to constant\n");
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
                return parser_ferror(&e->tok->rc, "error: illegal assignment to constant\n");
            }
            if (compile_expr(p, e->rhs, &lhs, bb)) return 1;
            if (parser_assign_dsts(p, dst, &lhs, bb, &e->tok->rc)) return 1;
            parser_deactivate_reg(p, &regmap);
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
                return parser_ferror(&e->tok->rc, "error: illegal assignment to constant\n");
            }
            struct RegMap regmapr = {0};
            struct ValDest rhs = dest_regmap(&regmapr);
            if (compile_expr(p, e->rhs, &rhs, bb)) return 1;
            const char* r2 = parser_prepare_src_reg(p, dst, &e->tok->rc);
            if (!r2) return 1;
            const char* r3 = parser_prepare_src_reg(p, &rhs, &e->tok->rc);
            if (!r3) return 1;
            const char* r1 = parser_prepare_dst_reg(p, dst, bb);
            if (!r1) return 1;
            cg_write_inst_op(&p->cg, "+", r1, r2, r3);
            parser_deactivate_reg(p, &regmapr);
        }
        else
        {
            struct RegMap regmap = {0};
            struct ValDest lhs = dest_regmap(&regmap);
            if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
            if (lhs.is_const)
            {
                return parser_ferror(&e->tok->rc, "error: illegal assignment to constant\n");
            }
            struct RegMap regmapr = {0};
            struct ValDest rhs = dest_regmap(&regmapr);
            if (compile_expr(p, e->rhs, &rhs, bb)) return 1;
            const char* r2 = parser_prepare_src_reg(p, &lhs, &e->tok->rc);
            if (!r2) return 1;
            const char* r3 = parser_prepare_src_reg(p, &rhs, &e->tok->rc);
            if (!r3) return 1;
            const char* r1 = parser_prepare_dst_reg(p, &lhs, bb);
            if (!r1) return 1;
            cg_write_inst_op(&p->cg, "+", r1, r2, r3);
            if (parser_assign_dsts(p, dst, &lhs, bb, &e->tok->rc)) return 1;
            parser_deactivate_reg(p, &regmapr);
            parser_deactivate_reg(p, &regmap);
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
        cg_write_inst_set(&p->cg, lhs_reg, "1");
        if (compile_conditional_expr(p, (struct Expr*)e, bb, end_lbl.buf, COND_NORMAL)) return 1;
        lhs_reg = parser_prepare_dst_reg(p, p_lhs, bb);
        cg_write_inst_set(&p->cg, lhs_reg, "0");
        if (parser_spill_registers(p)) return 1;
        cg_mark_label(&p->cg, end_lbl.buf);
        if (p_lhs != dst)
        {
            if (parser_assign_dsts(p, dst, p_lhs, bb, &e->tok->rc)) return 1;
            parser_deactivate_reg(p, &regmapl);
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
        cg_write_inst_set(&p->cg, lhs_reg, "0");
        if (compile_conditional_expr(p, (struct Expr*)e, bb, end_lbl.buf, COND_INVERTED)) return 1;
        lhs_reg = parser_prepare_dst_reg(p, p_lhs, bb);
        cg_write_inst_set(&p->cg, lhs_reg, "1");
        if (parser_spill_registers(p)) return 1;
        cg_mark_label(&p->cg, end_lbl.buf);
        if (p_lhs != dst)
        {
            if (parser_assign_dsts(p, dst, p_lhs, bb, &e->tok->rc)) return 1;
            parser_deactivate_reg(p, &regmapl);
        }
        return 0;
    }

    if (op[0] == '+' && op[1] == '\0')
    {
        return compile_expr_op_add(p, e->lhs, e->rhs, dst, bb, &e->tok->rc);
    }

    if (op[0] == '[' && op[1] == '\0')
    {
        if (dest_is_any(dst))
        {
            if (compile_expr_op_add(p, e->lhs, e->rhs, dst, bb, &e->tok->rc)) return 1;
            dst->is_reference = 1;
            dst->is_const = 0;
            return 0;
        }
        struct RegMap regmapl = {0};
        struct ValDest lhs = dest_regmap(&regmapl);
        if (compile_expr_op_add(p, e->lhs, e->rhs, &lhs, bb, &e->tok->rc)) return 1;
        lhs.is_reference = 1;
        dst->is_const = 0;
        if (parser_assign_dsts(p, dst, &lhs, bb, &e->tok->rc)) return 1;
        parser_deactivate_reg(p, &regmapl);
        return 0;
    }

    struct RegMap regmapl = {0};
    struct ValDest lhs = dest_regmap(&regmapl);
    struct RegMap regmapr = {0};
    struct ValDest rhs = dest_regmap(&regmapr);
    if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
    if (compile_expr(p, e->rhs, &rhs, bb)) return 1;
    const char* r2 = parser_prepare_src_reg(p, &lhs, &e->tok->rc);
    if (!r2) return 1;
    const char* r3 = parser_prepare_src_reg(p, &rhs, &e->tok->rc);
    if (!r3) return 1;
    const char* r1 = parser_prepare_dst_reg(p, dst, bb);
    if (!r1) return 1;
    cg_write_inst_op(&p->cg, op, r1, r2, r3);
    parser_deactivate_reg(p, &regmapl);
    parser_deactivate_reg(p, &regmapr);
    return 0;
}

static struct Expr* parser_checked_expr_at(Parser* p, struct ExprCall* e, size_t index)
{
    if (index >= e->extent) return NULL;
    return ((struct Expr**)p->expr_seqs.data)[e->offset + index];
}

static int compile_expr_call_intrinsic(
    Parser* p, struct ExprCall* e, struct ValDest* dst, struct Symbol* fn, struct CompoundBlock* bb)
{
    if (e->extent > REG_COUNT)
    {
        return parser_ferror(&e->tok->rc, "error: exceeded maximum call arguments (%d)\n", REG_COUNT);
    }

    if (!bb->fn_sym)
    {
        return parser_ferror(&e->tok->rc, "error: not in function scope\n");
    }

    struct RegMap arg_regmap[REG_COUNT] = {0};
    struct ValDest arg_dsts[REG_COUNT] = {0};
    for (size_t i = 0; i < e->extent; ++i)
    {
        arg_dsts[i] = dest_regmap(arg_regmap + i);
        if (compile_expr(p, ((struct Expr**)p->expr_seqs.data)[e->offset + i], arg_dsts + i, bb)) return 1;
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
                    return parser_ferror(&fn->decl->id->rc, "error: invalid intrinsic string: wrong arguments\n");
                }
                const char* const reg = parser_prepare_src_reg(p, arg_dsts + n, &e->tok->rc);
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
                return parser_ferror(&fn->decl->id->rc, "error: invalid intrinsic string: invalid format specifier\n");
            }
            ++s;
        }
        else
        {
            buf[i++] = *s++;
        }
    }
    buf[i] = 0;
    cg_write_inst(&p->cg, buf);
    if (dest_is_any(dst))
    {
        *dst = s_void_destination;
    }

    for (size_t i = 0; i < e->extent; ++i)
    {
        parser_deactivate_reg(p, arg_regmap + i);
    }

    return 0;
}

static int parser_assign_load(Parser* p, struct ValDest src, const char* tgt)
{
    struct CodeGen* const cg = &p->cg;
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

static int compile_expr_call(Parser* p, struct ExprCall* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    if (!e->fn) return parser_ice(&e->tok->rc), 1;

    struct RegMap regmap = {0};
    struct ValDest fn = dest_regmap(&regmap);
    if (compile_expr(p, e->fn, &fn, bb)) return 1;

    if (fn.kind != DEST_SYM || !fn.sym->decl->is_function)
    {
        return parser_ferror(&e->tok->rc, "error: attempting to call a non-function\n");
    }

    if (fn.sym->decl->extent != e->extent)
    {
        return parser_ferror(
            &e->tok->rc, "error: function requires %d arguments, %lu provided\n", fn.sym->decl->extent, e->extent);
    }
    if (fn.sym->decl->attr.asmstr)
    {
        return compile_expr_call_intrinsic(p, e, dst, fn.sym, bb);
    }
    else
    {
        if (e->extent > REG_COUNT)
        {
            return parser_ferror(&e->tok->rc, "error: exceeded maximum call arguments (%d)\n", REG_COUNT);
        }

        if (!bb->fn_sym)
        {
            return parser_ferror(&e->tok->rc, "error: not in function scope\n");
        }

        struct RegMap arg_regmaps[REG_COUNT - 1] = {0};
        struct ValDest arg_dsts[REG_COUNT - 1] = {0};
        for (size_t i = 1; i < e->extent; ++i)
        {
            struct ValDest* arg_dst = arg_dsts + i - 1;
            *arg_dst = dest_regmap(arg_regmaps + i - 1);
            if (compile_expr(p, ((struct Expr**)p->expr_seqs.data)[e->offset + i], arg_dst, bb)) return 1;
        }

        if (fn.sym->decl->is_nonreentrant)
        {
            const char* const name = token_str(p, fn.sym->decl->id);
            struct RegMap arg0_regmap = {
                .rename = nrfun_param(p, 0, name),
            };
            if (e->extent > 0)
            {
                struct ValDest fv_dst = dest_regmap(&arg0_regmap);
                if (compile_expr(p, ((struct Expr**)p->expr_seqs.data)[e->offset], &fv_dst, bb)) return 1;
            }
            parser_deactivate_reg(p, &arg0_regmap);
            for (size_t i = 1; i < e->extent; ++i)
            {
                struct FreeVar fv = nrfun_param(p, i, name);
                if (parser_assign_load(p, arg_dsts[i - 1], fv.buf)) return 1;
                parser_deactivate_reg(p, arg_regmaps + i - 1);
            }
            if (parser_spill_registers(p)) return 1;
            if (!bb->fn_sym->decl->is_stackless)
            {
                char buf[16];
                snprintf(buf, sizeof(buf), "%d", bb->frame_size);
                cg_write_inst_op(&p->cg, "+", "__stk__", "__ebp__", buf);
            }
            const struct FreeVar fv_ret = nrfun_param_ret(p, name);
            cg_write_inst_op(&p->cg, "+", fv_ret.buf, "1", "@counter");
        }
        else
        {
            if (e->extent > REG_COUNT)
            {
                return parser_ferror(&e->tok->rc, "error: exceeded maximum call arguments (%d)\n", REG_COUNT);
            }
            struct RegMap arg0_regmap = {
                .rename = s_freevar_paramreg0,
            };
            if (e->extent > 0)
            {
                struct ValDest fv_dst = dest_regmap(&arg0_regmap);
                if (compile_expr(p, ((struct Expr**)p->expr_seqs.data)[e->offset], &fv_dst, bb)) return 1;
            }
            parser_deactivate_reg(p, &arg0_regmap);
            for (size_t i = 1; i < e->extent; ++i)
            {
                if (parser_assign_load(p, arg_dsts[i - 1], PARAM_NAMES_ARR[i])) return 1;
                parser_deactivate_reg(p, arg_regmaps + i - 1);
            }
            if (parser_spill_registers(p)) return 1;
            cg_write_inst(&p->cg, "op add ret 1 @counter");
        }
        cg_write_inst_jump(&p->cg, fn.sym->reg.rename.buf);
        if (dst->kind != DEST_VOID)
        {
            struct RegMap ret_regmap = {
                .rename = s_freevar_paramreg0,
            };
            parser_push_active_reg(p, &ret_regmap);
            struct ValDest ret_dst = dest_regmap(&ret_regmap);
            if (!parser_prepare_dst_reg(p, dst, bb)) return 1;
            if (parser_assign_dsts(p, dst, &ret_dst, bb, &e->tok->rc)) return 1;
            parser_deactivate_reg(p, &ret_regmap);
        }
    }
    return 0;
}

static int compile_expr(Parser* p, struct Expr* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    switch (e->kind)
    {
        case EXPR_LIT: return compile_expr_lit(p, (struct ExprLit*)e, dst, bb);
        case EXPR_OP: return compile_expr_op(p, (struct ExprOp*)e, dst, bb);
        case EXPR_CALL: return compile_expr_call(p, (struct ExprCall*)e, dst, bb);
        case EXPR_SYM:
        {
            struct ExprSym* esym = (struct ExprSym*)e;
            struct Symbol* sym = &decl_get_def(esym->sym->decl)->sym;
            struct ValDest sym_dst = dest_sym(sym);
            return parser_assign_dsts(p, dst, &sym_dst, bb, &esym->tok->rc);
        }
        default: return parser_ice(&s_unknown_rc), 1;
    }
}

enum AttrKind
{
    ATTR_SYM,
    ATTR_ASM,
    ATTR_NONREENTRANT,
};

static struct Token* parse_attribute(Parser* p, struct Token* cur_tok, struct Attribute* attr)
{
    enum AttrKind kind;
    if (cur_tok->type != LEX_IDENT) goto error;
    struct Token* attrkind = cur_tok++;
    const char* attrkind_str = token_str(p, attrkind);
    if (strcmp(attrkind_str, "sym") == 0)
    {
        kind = ATTR_SYM;
    }
    else if (strcmp(attrkind_str, "asmstr") == 0)
    {
        kind = ATTR_ASM;
    }
    else if (strcmp(attrkind_str, "nonreentrant") == 0)
    {
        attr->is_nonreentrant = 1;
        return cur_tok;
    }
    else
    {
        goto error;
    }

    if (!token_is_sym(p, cur_tok, '(')) goto error;
    ++cur_tok;
    if (cur_tok->type != LEX_STRING)
    {
        return parser_ferror(&cur_tok->rc, "error: expected attribute parameter\n"), NULL;
    }
    if (kind == ATTR_SYM)
    {
        attr->symname = token_str(p, cur_tok);
    }
    else if (kind == ATTR_ASM)
    {
        attr->asmstr = token_str(p, cur_tok);
    }
    else
    {
        abort();
    }
    ++cur_tok;
    if (!token_is_sym(p, cur_tok, ')')) goto error;
    ++cur_tok;
    return cur_tok;

error:
    return parser_ferror(&cur_tok->rc, "error: ill-formed attribute\n"), NULL;
}
static struct Token* parse_attribute_plist(Parser* p, struct Token* cur_tok, struct Attribute* attr)
{
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;
    if (!token_is_sym(p, cur_tok, ')'))
    {
        do
        {
            if (!(cur_tok = parse_attribute(p, cur_tok, attr))) return NULL;
            char ch = token_expect_comma_or_cparen(p, cur_tok);
            if (ch == ')') break;
            if (ch == ',')
            {
                cur_tok++;
                continue;
            }
            if (ch == '\0') return NULL;
        } while (1);
    }
    if (!(cur_tok = token_consume_sym(p, cur_tok, ')'))) return NULL;
    return token_consume_sym(p, cur_tok, ')');
}

static struct Token* parse_declspecs(Parser* p, struct Token* cur_tok, struct DeclSpecs* specs)
{
    do
    {
        if (cur_tok->type == LEX_IDENT)
        {
            struct Binding* const cur_bind = scope_find(&p->type_scope, token_str(p, cur_tok));
            if (!cur_bind)
            {
                if (!specs->type) return parser_ferror(&cur_tok->rc, "error: expected type before identifier\n"), NULL;
                return cur_tok;
            }
        }
        else if (cur_tok->type == LEX_STRUCT)
        {
            specs->type = cur_tok++;
            if (cur_tok->type != LEX_IDENT) return parser_ferror(&cur_tok->rc, "error: expected typename\n"), NULL;
        }
        else if (cur_tok->type == LEX_VOID || cur_tok->type == LEX_INT || cur_tok->type == LEX_MSTRING ||
                 cur_tok->type == LEX_UNIT)
        {
            if (specs->type)
                return parser_ferror(&cur_tok->rc, "error: repeated core declaration specifiers are not allowed\n"),
                       NULL;
            specs->type = cur_tok;
        }
        else if (cur_tok->type == LEX_CONST)
        {
            if (specs->is_const)
            {
                return parser_ferror(&cur_tok->rc, "error: repeated 'const' declaration specifiers are not allowed\n"),
                       NULL;
            }
            specs->is_const = 1;
        }
        else if (cur_tok->type == LEX_REGISTER)
        {
            if (specs->is_register)
            {
                return parser_ferror(&cur_tok->rc,
                                     "error: repeated 'register' declaration specifiers are not allowed\n"),
                       NULL;
            }
            specs->is_register = 1;
        }
        else if (cur_tok->type == LEX_VOLATILE)
        {
            if (specs->is_volatile)
            {
                return parser_ferror(&cur_tok->rc,
                                     "error: repeated 'volatile' declaration specifiers are not allowed\n"),
                       NULL;
            }
            specs->is_volatile = 1;
        }
        else
        {
            if (!specs->type) return parser_ferror(&cur_tok->rc, "error: expected type\n"), NULL;
            return cur_tok;
        }
        ++cur_tok;
    } while (1);
}

static struct TypeStr declspecs_to_type(Parser* p, struct DeclSpecs* specs)
{
    struct TypeStr ty;
    if (specs->type->type == LEX_INT)
    {
        ty = s_type_int;
    }
    else if (specs->type->type == LEX_VOID)
    {
        ty = s_type_void;
    }
    else if (specs->type->type == LEX_MSTRING)
    {
        ty = s_type_mstr;
    }
    else if (specs->type->type == LEX_UNIT)
    {
        ty = s_type_unit;
    }
    else if (specs->type->type == LEX_STRUCT)
    {
        const char* str = token_str(p, specs->type + 1);
        size_t len = strlen(str);
        if (len + 2 > sizeof(ty.buf))
        {
            parser_ferror(&specs->type[1].rc, "error: struct name too long\n");
            return s_type_unknown;
        }
        ty.used = len + 2;
        ty.buf[0] = '$';
        memcpy(ty.buf + 1, str, len);
        ty.buf[len + 1] = '$';
    }
    else
    {
        parser_ferror(&specs->type->rc, "error: unknown type\n");
        return s_type_unknown;
    }
    if (specs->is_const) typestr_add_const(&ty);
    return ty;
}

static struct Token* parse_declarator(Parser* p, struct Token* cur_tok, struct DeclSpecs* specs, struct Decl** pdecl)
{
    struct Decl* decl = *pdecl = pool_alloc(&p->ast_pools[AST_DECL], sizeof(struct Decl));
    memset(decl, 0, sizeof(struct Decl));
    decl->kind.kind = AST_DECL;
    decl->specs = *specs;
    symbol_init(&decl->sym);
    decl->sym.decl = decl;
    decl->sym.type = declspecs_to_type(p, specs);

    if (cur_tok->type == LEX_ATTRIBUTE)
    {
        ++cur_tok;
        cur_tok = parse_attribute_plist(p, cur_tok, &decl->attr);
        if (!cur_tok) return NULL;
    }
    while (1)
    {
        if (token_is_sym(p, cur_tok, '*'))
        {
            typestr_add_pointer(&decl->sym.type);
            decl->specs.is_const = 0;
            decl->specs.is_volatile = 0;
            ++cur_tok;
        }
        else if (cur_tok->type == LEX_CONST)
        {
            typestr_add_const(&decl->sym.type);
            decl->specs.is_const = 1;
            ++cur_tok;
        }
        else
            break;
    }
    if (cur_tok->type != LEX_IDENT)
    {
        return parser_ferror(&cur_tok->rc, "error: expected identifier\n"), NULL;
    }
    decl->id = cur_tok++;
    const char* const name = token_str(p, decl->id);

    struct Binding* prev_sym = scope_find(&p->scope, name);
    scope_insert(&p->scope, name, &decl->sym);

    if (token_is_sym(p, cur_tok, '('))
    {
        if (decl->is_array) return parser_ferror(&cur_tok->rc, "error: arrays of functions are not supported\n"), NULL;
        if (decl->pointer_levels)
            return parser_ferror(&cur_tok->rc, "error: pointers to functions are not supported\n"), NULL;
        decl->is_function = 1;
        ++cur_tok;
        typestr_start_call(&decl->sym.type);
        if (token_is_sym(p, cur_tok, ')'))
        {
            typestr_end_call(&decl->sym.type);
            ++cur_tok;
        }
        else
        {
#define MAX_ARG_DECLS 16
            struct Decl* arg_decls[MAX_ARG_DECLS];
            while (1)
            {
                if (decl->extent == MAX_ARG_DECLS)
                {
                    return parser_ferror(
                               &cur_tok->rc, "error: exceeded maximum function arguments (%lu)\n", MAX_ARG_DECLS),
                           NULL;
                }
                struct DeclSpecs arg_specs = {0};
                if (!(cur_tok = parse_declspecs(p, cur_tok, &arg_specs))) return NULL;
                struct Decl** arg_decl = arg_decls + decl->extent++;
                if (!(cur_tok = parse_declarator(p, cur_tok, &arg_specs, arg_decl))) return NULL;

                typestr_add_arg(&decl->sym.type, &(*arg_decl)->sym.type);

                if (cur_tok->type == LEX_SYMBOL)
                {
                    const char ch = token_str(p, cur_tok)[0];
                    if (ch == ',')
                    {
                        ++cur_tok;
                        continue;
                    }
                    else if (ch == ')')
                    {
                        typestr_end_call(&decl->sym.type);
                        ++cur_tok;
                        break;
                    }
                }
                return parser_ferror(&cur_tok->rc, "error: expected ',' and further parameter declarations or ')'\n"),
                       NULL;
            }
            decl->offset = p->expr_seqs.sz / sizeof(struct Expr*);
            array_push(&p->expr_seqs, arg_decls, decl->extent * sizeof(struct Expr*));
        }
        if (prev_sym)
        {
            // ensure symbols match
            if (!symbol_is_equivalent_redecl(prev_sym->sym, &decl->sym))
            {
                return parser_ferror(&decl->id->rc, "error: declaration doesn't match previous\n"), NULL;
            }
            if (prev_sym->sym->decl->init)
            {
                decl->def = prev_sym->sym->decl;
            }
            else
            {
                prev_sym->sym->decl->def = decl;
            }
        }
    }
    else if (token_is_sym(p, cur_tok, '['))
    {
        decl->specs.is_const = 0;
        decl->specs.is_volatile = 0;
        ++cur_tok;
        if (token_is_sym(p, cur_tok, ']'))
        {
            if (decl->is_array) return parser_ferror(&cur_tok->rc, "error: arrays of arrays are not supported\n"), NULL;
            decl->is_array = 1;
            decl->array_arity = ARRAY_ARITY_NONE;
            ++cur_tok;
        }
        else if (cur_tok->type == LEX_NUMBER)
        {
            decl->array_arity = atoi(token_str(p, cur_tok));
            if (decl->array_arity < 1)
                return parser_ferror(&cur_tok->rc, "error: array arity must be positive\n"), NULL;
            if (!(cur_tok = token_consume_sym(p, cur_tok + 1, ']'))) return NULL;
        }
        else
        {
            return parser_ferror(&cur_tok->rc, "error: expected number or ']'\n"), NULL;
        }
        typestr_add_arr(&decl->sym.type, decl->array_arity);
    }
    return cur_tok;
}

static struct Token* parse_decl(Parser* p, struct Token* cur_tok, struct Array* pdecls)
{
    struct DeclSpecs specs = {0};
    if (!(cur_tok = parse_declspecs(p, cur_tok, &specs))) return NULL;
    while (1)
    {
        const size_t scope_sz = scope_size(&p->scope);
        struct Decl* pdecl;
        if (!(cur_tok = parse_declarator(p, cur_tok, &specs, &pdecl))) return NULL;
        array_push(pdecls, &pdecl, sizeof(pdecl));
        if (cur_tok->type == LEX_SYMBOL)
        {
            const char ch = token_str(p, cur_tok)[0];
            if (ch == '{')
            {
                return cur_tok;
            }

            if (ch == '=')
            {
                if (pdecl->is_function)
                    return parser_ferror(&cur_tok->rc, "error: function cannot be initialized with '='\n"), NULL;
                if (!(cur_tok = parse_expr(p, cur_tok + 1, &pdecl->init, PRECEDENCE_ASSIGN))) return NULL;
            }
            // Remove all but first declarator from the scope
            scope_shrink(&p->scope, scope_sz + 1);

            if (cur_tok->type != LEX_SYMBOL) return parser_ferror(&cur_tok->rc, "error: expected ',' or ';'\n"), NULL;
            const char ch2 = *token_str(p, cur_tok);
            if (ch2 == ',')
            {
                ++cur_tok;
                continue;
            }
            else if (ch2 == ';')
            {
                return cur_tok;
            }
        }
        return parser_ferror(&cur_tok->rc, "error: expected ',' or ';'\n"), NULL;
    }
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
    typestr_format_english(ts, buf, sizeof(buf));
    return parser_ferror(rc, "error: attempt to size incomplete type '%s'\n", buf);
}

static int compile_decl_single(Parser* p, struct Decl* decl, struct CompoundBlock* bb)
{
    struct Symbol* const sym = &decl->sym;

    const char* const name = token_str(p, decl->id);
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
            sym->reg.stack_addr = -1;
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
            sym->decl->parent_decl = bb->fn_sym->decl;
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

static struct Token* compile_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb);

static const struct RowCol* expr_rc(struct Expr* e)
{
    switch (e->kind)
    {
        case EXPR_SYM: return &((struct ExprSym*)e)->tok->rc;
        case EXPR_CALL: return &((struct ExprCall*)e)->tok->rc;
        case EXPR_LIT: return &((struct ExprLit*)e)->tok->rc;
        case EXPR_OP: return &((struct ExprOp*)e)->tok->rc;
        default: return &s_unknown_rc;
    }
}

static int compile_conditional_expr(
    Parser* p, struct Expr* cond, struct CompoundBlock* bb, const char* jump_to, enum CondInverted inverted)
{
    if (cond->kind == EXPR_OP)
    {
        struct ExprOp* cond_op = (struct ExprOp*)cond;
        const char* const op = token_str(p, cond_op->tok);
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

            parser_deactivate_reg(p, &lhs_regmap);
            parser_deactivate_reg(p, &rhs_regmap);

            // Do if-stmt fusion
            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump_op(&p->cg, jump_to, inverted ? relation_invert(op) : op, lhs_reg, rhs_reg);
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
                cg_mark_label(&p->cg, end_lbl.buf);
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
                cg_mark_label(&p->cg, end_lbl.buf);
                return 0;
            }
        }
    }

    struct RegMap lhs_regmap = {0};
    struct ValDest lhs = dest_regmap(&lhs_regmap);
    if (compile_expr(p, cond, &lhs, bb)) return 1;
    const char* const lhs_reg = parser_prepare_src_reg(p, &lhs, expr_rc(cond));
    if (!lhs_reg) return 1;
    parser_deactivate_reg(p, &lhs_regmap);
    if (parser_spill_registers(p)) return 1;
    cg_write_inst_jump_op(&p->cg, jump_to, inverted ? "!=" : "==", lhs_reg, "true");
    return 0;
}

static struct Token* parse_conditional(Parser* p, struct Token* cur_tok, struct Expr** p_cond)
{
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;

    if (!(cur_tok = parse_expr(p, cur_tok, p_cond, PRECEDENCE_COMMA))) return NULL;

    return token_consume_sym(p, cur_tok, ')');
}

static struct Token* compile_conditional(
    Parser* p, struct Token* cur_tok, struct CompoundBlock* bb, const char* jump_to, enum CondInverted inverted)
{
    struct Expr* cond;
    if (!(cur_tok = parse_conditional(p, cur_tok, &cond))) return NULL;

    if (compile_conditional_expr(p, cond, bb, jump_to, inverted)) return NULL;
    return cur_tok;
}

struct StmtReturn
{
    struct Expr kind;
    struct Token* tok;
    // may be null
    struct Expr* expr;
};
struct StmtDecls
{
    struct Expr kind;
    size_t offset;
    size_t extent;
};
struct StmtIf
{
    struct Expr kind;
    struct Expr* cond;
    struct Expr* if_body;
    // may be null
    struct Expr* else_body;
};
struct StmtGoto
{
    struct Expr kind;
    struct Token* dst;
};
struct StmtLoop
{
    struct Expr kind;
    struct Expr* cond;
    struct Expr* body;
    // null for while/dowhile
    struct Expr* init;
    // null for while/dowhile
    struct Expr* advance;

    int is_do_while : 1;
};
struct StmtBlock
{
    struct Expr kind;
    size_t offset;
    size_t extent;
};
struct StmtLabel
{
    struct Expr kind;
    struct Token* tok;
    struct Expr* stmt;
};
struct StmtBreak
{
    struct Expr kind;
    struct Token* tok;
};
struct StmtContinue
{
    struct Expr kind;
    struct Token* tok;
};

static struct Expr s_stmt_none = {.kind = STMT_NONE};

static int is_empty_stmt(Parser* p, struct Expr* expr)
{
    if (expr->kind == STMT_NONE) return 1;
    if (expr->kind == STMT_BLOCK)
    {
        struct StmtBlock* blk = (struct StmtBlock*)expr;
        struct Expr** exprs = p->expr_seqs.data;
        for (size_t i = 0; i < blk->extent; ++i)
        {
            if (!is_empty_stmt(p, exprs[blk->offset + i]))
            {
                return 0;
            }
        }
        return 1;
    }
    return 0;
}

static int compile_stmt_expr(Parser* p, struct Expr* expr, struct CompoundBlock* bb);
static int compile_if_stmt(Parser* p, struct StmtIf* stmt, struct CompoundBlock* bb)
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
        cg_mark_label(&p->cg, endif_lbl.buf);
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
            cg_write_inst_jump(&p->cg, endif_lbl.buf);
            cg_mark_label(&p->cg, else_lbl.buf);
            cb_init(&new_cb, bb, p);
            if (compile_stmt_expr(p, stmt->else_body, &new_cb)) return 1;
            if (has_returned && new_cb.has_returned) bb->has_returned = 1;
            cb_destroy(&new_cb, p);
            if (parser_spill_registers(p)) return 1;
            cg_mark_label(&p->cg, endif_lbl.buf);
        }
        else
        {
            cg_mark_label(&p->cg, else_lbl.buf);
        }
    }
    return 0;
}
static int expr_is_lit(Parser* p, struct Expr* e, const char* lit)
{
    if (e->kind != EXPR_LIT) return 0;
    struct ExprLit* e_lit = (struct ExprLit*)e;
    return strcmp(token_str(p, e_lit->tok), lit) == 0;
}

static int compile_loop_stmt(Parser* p, struct StmtLoop* stmt, struct CompoundBlock* bb)
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
        cg_write_inst_jump(&p->cg, first_lbl.buf);
    }
    cg_mark_label(&p->cg, continue_lbl.buf);
    if (stmt->advance)
        if (compile_stmt_expr(p, stmt->advance, &new_cb)) return 1;

    if (!stmt->is_do_while)
    {
        if (parser_spill_registers(p)) return 1;
        cg_mark_label(&p->cg, first_lbl.buf);
    }
    if (stmt->cond)
    {
        if (!expr_is_lit(p, stmt->cond, "1"))
        {
            if (compile_conditional_expr(p, stmt->cond, bb, break_lbl.buf, COND_INVERTED)) return 1;
        }
    }
    if (stmt->is_do_while)
    {
        if (parser_spill_registers(p)) return 1;
        cg_mark_label(&p->cg, first_lbl.buf);
    }

    if (compile_stmt_expr(p, stmt->body, &new_cb)) return 1;
    cb_destroy(&new_cb, p);
    if (parser_spill_registers(p)) return 1;
    cg_write_inst_jump(&p->cg, continue_lbl.buf);
    cg_mark_label(&p->cg, break_lbl.buf);
    return 0;
}

static struct Token* compile_do_while_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb)
{
    struct FreeVar top_lbl = free_var_label(p);
    struct FreeVar continue_lbl = free_var_label(p);
    struct FreeVar break_lbl = free_var_label(p);
    if (parser_spill_registers(p)) return NULL;
    cg_mark_label(&p->cg, top_lbl.buf);
    struct CompoundBlock new_cb;
    cb_init(&new_cb, bb, p);
    new_cb.break_label = break_lbl.buf;
    new_cb.continue_label = continue_lbl.buf;
    if (!(cur_tok = compile_stmt(p, cur_tok, &new_cb))) return NULL;
    cb_destroy(&new_cb, p);
    if (cur_tok->type != LEX_WHILE)
    {
        return parser_ferror(&cur_tok->rc, "error: expected 'while'\n"), NULL;
    }
    ++cur_tok;
    if (parser_spill_registers(p)) return NULL;
    cg_mark_label(&p->cg, continue_lbl.buf);
    if (!(cur_tok = compile_conditional(p, cur_tok, bb, top_lbl.buf, COND_NORMAL))) return NULL;
    {
        if (parser_spill_registers(p)) return NULL;
        cg_mark_label(&p->cg, break_lbl.buf);
    }
    return token_consume_sym(p, cur_tok, ';');
}

static struct Token* parse_stmt(Parser* p, struct Token* cur_tok, struct Expr** p_expr);
static struct Token* parse_stmts(Parser* p, struct Token* cur_tok, struct Expr** p_expr);
static struct Token* parse_stmt_decl(Parser* p, struct Token* cur_tok, struct Expr** p_expr)
{
    const size_t scope_sz = scope_size(&p->scope);
    struct Array arr_decls;
    array_init(&arr_decls);
    if (!(cur_tok = parse_decl(p, cur_tok, &arr_decls))) return NULL;

    if (token_is_sym(p, cur_tok, '{'))
    {
        if (arr_decls.sz != sizeof(struct Decl*))
        {
            return parser_ferror(&cur_tok->rc, "error: functions must be declared alone\n"), NULL;
        }
        struct Decl* decl = *(struct Decl**)arr_decls.data;
        if (!decl->is_function)
        {
            return parser_ferror(&cur_tok->rc, "error: only functions may be initialized with a code block\n"), NULL;
        }
        if (!(cur_tok = parse_stmts(p, cur_tok + 1, &decl->init))) return NULL;
        if (!(cur_tok = token_consume_sym(p, cur_tok, '}'))) return NULL;
        // Remove function arguments from the scope
        scope_shrink(&p->scope, scope_sz + 1);
    }
    else
    {
        if (!(cur_tok = token_consume_sym(p, cur_tok, ';'))) return NULL;
    }

    struct StmtDecls ret = {
        .kind = STMT_DECLS,
        .offset = p->expr_seqs.sz / sizeof(struct Decl*),
        .extent = arr_decls.sz / sizeof(struct Decl*),
    };
    array_push(&p->expr_seqs, arr_decls.data, arr_decls.sz);
    array_destroy(&arr_decls);
    *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
    return cur_tok;
}

static struct Token* parse_stmts(Parser* p, struct Token* cur_tok, struct Expr** p_expr)
{
    struct Array arr_stmts;
    array_init(&arr_stmts);
    do
    {
        if (cur_tok->type == LEX_EOF || token_is_sym(p, cur_tok, '}')) break;
        if (!(cur_tok = parse_stmt(p, cur_tok, array_alloc(&arr_stmts, sizeof(struct Expr*))))) return NULL;
    } while (1);
    struct StmtBlock ret = {
        .kind = STMT_BLOCK,
        .offset = p->expr_seqs.sz / sizeof(struct Expr*),
        .extent = arr_stmts.sz / sizeof(struct Expr*),
    };
    array_push(&p->expr_seqs, arr_stmts.data, arr_stmts.sz);
    array_destroy(&arr_stmts);
    *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
    return cur_tok;
}

static struct Token* parse_stmt(Parser* p, struct Token* cur_tok, struct Expr** p_expr)
{
    switch (cur_tok->type)
    {
        case LEX_MSTRING:
        case LEX_INT:
        case LEX_UNIT:
        case LEX_VOLATILE:
        case LEX_CONST:
        case LEX_REGISTER:
        case LEX_STRUCT:
        case LEX_VOID:
        {
            return parse_stmt_decl(p, cur_tok, p_expr);
        }
        case LEX_RETURN:
        {
            struct StmtReturn ret = {
                .kind = STMT_RETURN,
                .tok = cur_tok,
            };
            if (token_is_sym(p, cur_tok + 1, ';'))
            {
                cur_tok += 2;
            }
            else if (!(cur_tok = parse_expr(p, cur_tok + 1, &ret.expr, PRECEDENCE_COMMA)))
                return NULL;
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_IF:
        {
            struct StmtIf ret = {
                .kind = STMT_IF,
            };
            size_t scope_sz = scope_size(&p->scope);
            if (!(cur_tok = parse_conditional(p, cur_tok + 1, &ret.cond))) return NULL;
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.if_body))) return NULL;
            scope_shrink(&p->scope, scope_sz);

            if (cur_tok->type == LEX_ELSE)
            {
                if (!(cur_tok = parse_stmt(p, cur_tok + 1, &ret.else_body))) return NULL;
                scope_shrink(&p->scope, scope_sz);
            }
            else
            {
                ret.else_body = NULL;
            }

            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_CONTINUE:
        {
            struct StmtContinue ret = {
                .kind = STMT_CONTINUE,
                .tok = cur_tok,
            };
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return token_consume_sym(p, cur_tok + 1, ';');
        }
        case LEX_BREAK:
        {
            struct StmtBreak ret = {
                .kind = STMT_BREAK,
                .tok = cur_tok,
            };
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return token_consume_sym(p, cur_tok + 1, ';');
        }
        case LEX_FOR:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
            };
            size_t scope_sz = scope_size(&p->scope);
            if (!(cur_tok = token_consume_sym(p, cur_tok + 1, '('))) return NULL;
            struct Token* init_tk = cur_tok;
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.init))) return NULL;
            if (ret.init->kind == STMT_DECLS)
            {
                struct StmtDecls* init = (struct StmtDecls*)ret.init;
                struct Expr** exprs = p->expr_seqs.data;
                for (size_t i = 0; i < init->extent; ++i)
                {
                    if (exprs[init->offset + i]->kind != AST_DECL) abort();
                    struct Decl* decl = (struct Decl*)exprs[init->offset + i];
                    if (decl->is_function)
                        return parser_ferror(
                                   &decl->id->rc,
                                   "error: declaration of non-variable '%s' in 'for' loop initial declaration\n",
                                   token_str(p, decl->id)),
                               NULL;
                }
            }
            else if (!ast_kind_is_expr(ret.init->kind))
            {
                return parser_ferror(&init_tk->rc, "error: expected expression or declaration in for loop\n"), NULL;
            }
            if (!token_is_sym(p, cur_tok, ';'))
            {
                if (!(cur_tok = parse_expr(p, cur_tok, &ret.cond, PRECEDENCE_COMMA))) return NULL;
            }
            if (!(cur_tok = token_consume_sym(p, cur_tok, ';'))) return NULL;
            if (!token_is_sym(p, cur_tok, ')'))
            {
                if (!(cur_tok = parse_expr(p, cur_tok, &ret.advance, PRECEDENCE_COMMA))) return NULL;
            }
            if (!(cur_tok = token_consume_sym(p, cur_tok, ')'))) return NULL;
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.body))) return NULL;
            scope_shrink(&p->scope, scope_sz);
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_WHILE:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
            };
            if (!(cur_tok = parse_conditional(p, cur_tok + 1, &ret.cond))) return NULL;
            size_t scope_sz = scope_size(&p->scope);
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.body))) return NULL;
            scope_shrink(&p->scope, scope_sz);
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_DO:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
                .is_do_while = 1,
            };
            size_t scope_sz = scope_size(&p->scope);
            if (!(cur_tok = parse_stmt(p, cur_tok + 1, &ret.body))) return NULL;
            scope_shrink(&p->scope, scope_sz);
            if (cur_tok->type != LEX_WHILE)
            {
                return parser_ferror(&cur_tok->rc, "error: expected 'while'\n"), NULL;
            }
            if (!(cur_tok = parse_conditional(p, cur_tok + 1, &ret.cond))) return NULL;
            if (!(cur_tok = token_consume_sym(p, cur_tok, ';'))) return NULL;
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_GOTO:
        {
            struct StmtGoto ret = {
                .kind = STMT_GOTO,
                .dst = ++cur_tok,
            };
            if (cur_tok->type != LEX_IDENT)
            {
                return parser_ferror(&cur_tok->rc, "error: expected label to go to\n"), NULL;
            }
            if (!(cur_tok = token_consume_sym(p, cur_tok + 1, ';'))) return NULL;
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_IDENT:
        {
            if (token_is_sym(p, cur_tok + 1, ':'))
            {
                struct StmtLabel ret = {
                    .kind = STMT_LABEL,
                    .tok = cur_tok,
                };
                if (!(cur_tok = parse_stmt(p, cur_tok + 2, &ret.stmt))) return NULL;
                *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
                return cur_tok;
            }

            struct Binding* const cur_bind = scope_find(&p->type_scope, token_str(p, cur_tok));
            if (cur_bind)
            {
                // this is a declaration
                return parse_stmt_decl(p, cur_tok, p_expr);
            }
        }
        case LEX_NUMBER:
        {
            if (!(cur_tok = parse_expr(p, cur_tok, p_expr, PRECEDENCE_COMMA))) return NULL;
            return token_consume_sym(p, cur_tok, ';');
        }
        case LEX_SYMBOL:
        {
            const char ch = token_str(p, cur_tok)[0];
            if (ch == ';')
            {
                *p_expr = &s_stmt_none;
                return cur_tok + 1;
            }
            else if (ch == '*' || ch == '(' || ch == '&')
            {
                if (!(cur_tok = parse_expr(p, cur_tok, p_expr, PRECEDENCE_COMMA))) return NULL;
                return token_consume_sym(p, cur_tok, ';');
            }
            else if (ch == '{')
            {
                size_t scope_sz = scope_size(&p->scope);
                if (!(cur_tok = parse_stmts(p, cur_tok + 1, p_expr))) return NULL;
                scope_shrink(&p->scope, scope_sz);
                return token_consume_sym(p, cur_tok, '}');
            }
            else
            {
                return parser_ferror(&cur_tok->rc, "error: expected statement\n"), NULL;
            }
        }
        default: return parser_ferror(&cur_tok->rc, "error: expected statement\n"), NULL;
    }
}

static struct Token* compile_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb)
{
    struct Expr* expr;
    if (!(cur_tok = parse_stmt(p, cur_tok, &expr))) return NULL;
    if (compile_stmt_expr(p, expr, bb)) return NULL;
    return cur_tok;
}
static int compile_decl_expr(Parser* p, struct Decl* decl, struct CompoundBlock* bb)
{
    if (compile_decl_single(p, decl, bb)) return 1;

    if (decl->init)
    {
        if (decl->is_function)
        {
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
            cg_mark_label(&p->cg, decl->sym.reg.rename.buf);
            const char* const name = token_str(p, decl->id);
            if (decl->is_nonreentrant)
            {
                p->fn_ret_var = nrfun_param_ret(p, token_str(p, decl->id));
            }
            else
            {
                cg_write_push_ret(&p->cg, &p->fn_ret_var);
            }
            if (!decl->is_stackless)
            {
                cg_write_prepare_stack(&p->cg);
            }
            for (size_t i = 0; i < decl->extent; ++i)
            {
                struct Decl* arg_decl = ((struct Decl**)p->expr_seqs.data)[decl->offset + i];
                if (compile_decl_single(p, arg_decl, &new_cb)) return 1;
                if (decl->is_nonreentrant)
                {
                    arg_decl->sym.reg.rename = nrfun_param(p, i, name);
                }
            }
            if (!decl->is_nonreentrant)
            {
                if (decl->extent > REG_COUNT)
                {
                    return parser_ferror(&decl->id->rc, "error: exceeded maximum argument count (%d)\n", REG_COUNT);
                }
                for (size_t i = 0; i < decl->extent; ++i)
                {
                    struct Decl* arg_decl = ((struct Decl**)p->expr_seqs.data)[decl->offset + i];
                    struct ValDest dst = dest_sym(&arg_decl->sym);
                    const char* dst_reg = parser_prepare_dst_reg(p, &dst, &new_cb);
                    cg_write_inst_set(&p->cg, dst_reg, PARAM_NAMES_ARR[i]);
                }
            }
            if (compile_stmt_expr(p, decl->init, &new_cb)) return 1;
            int has_returned = new_cb.has_returned;
            cb_destroy(&new_cb, p);
            if (!has_returned)
            {
                if (decl->is_nonreentrant && strcmp(token_str(p, decl->id), "main") == 0)
                {
                }
                else
                {
                    if (parser_spill_registers(p)) return 1;
                    if (!decl->is_stackless) cg_write_epilog(&p->cg);
                }
                cg_write_return(&p->cg, &p->fn_ret_var);
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

int ast_kind_is_expr(enum AstKind k)
{
    switch (k)
    {
        case EXPR_SYM:
        case EXPR_LIT:
        case EXPR_OP:
        case EXPR_CALL: return 1;
        default: return 0;
    }
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

static int compile_stmt_expr(Parser* p, struct Expr* expr, struct CompoundBlock* bb)
{
    switch (expr->kind)
    {
        case STMT_DECLS:
        {
            struct StmtDecls* stmt = (struct StmtDecls*)expr;
            for (size_t i = 0; i < stmt->extent; ++i)
            {
                struct Decl* arg_decl = ((struct Decl**)p->expr_seqs.data)[stmt->offset + i];
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
                    return parser_ferror(&stmt->tok->rc,
                                         "error: unexpected return expression in function of type 'void'\n");
                }
            }
            else
            {
                if (!stmt->expr)
                {
                    return parser_ferror(&stmt->tok->rc, "error: expected return expression in non-void function\n");
                }
                struct RegMap regmap = {
                    .rename = s_freevar_paramreg0,
                };
                struct ValDest dst = dest_regmap(&regmap);
                if (compile_expr(p, stmt->expr, &dst, bb)) return 1;
                parser_deactivate_reg(p, &regmap);
            }
            if (parser_spill_registers(p)) return 1;
            if (!p->fn->is_stackless) cg_write_epilog(&p->cg);
            cg_write_return(&p->cg, &p->fn_ret_var);
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
                return parser_ferror(&((struct StmtContinue*)expr)->tok->rc, "error: not inside a loop\n");
            }

            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump(&p->cg, bb->continue_label);
            return 0;
        }
        case STMT_BREAK:
        {
            if (!bb->break_label)
            {
                return parser_ferror(&((struct StmtBreak*)expr)->tok->rc, "error: not inside a loop\n");
            }

            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump(&p->cg, bb->break_label);
            return 0;
        }
        case STMT_LOOP:
        {
            return compile_loop_stmt(p, (struct StmtLoop*)expr, bb);
        }
        case STMT_LABEL:
        {
            struct StmtLabel* label = (struct StmtLabel*)expr;
            const char* const str = token_str(p, label->tok);
            char buf[64];
            if (sizeof(buf) <= snprintf(buf, sizeof(buf), "$%s%s$", p->fn_label_prefix, str))
            {
                return parser_ferror(&label->tok->rc, "error: resource exceeded: label too long: '%s'\n", str);
            }
            if (parser_spill_registers(p)) return 1;
            cg_mark_label(&p->cg, buf);
            bb->has_returned = 0;
            return compile_stmt_expr(p, label->stmt, bb);
        }
        case STMT_GOTO:
        {
            struct StmtGoto* stmt = (struct StmtGoto*)expr;
            char buf[64];
            const char* const cur_tok_str = token_str(p, stmt->dst);
            if (sizeof(buf) <= snprintf(buf, sizeof(buf), "$%s%s$", p->fn_label_prefix, cur_tok_str))
            {
                fprintf(stderr, "resource exceeded: label too long: '%s'\n", cur_tok_str);
                abort();
            }
            if (parser_spill_registers(p)) return 1;
            cg_write_inst_jump(&p->cg, buf);
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
                struct Expr** expr_seqs = p->expr_seqs.data;
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

void parser_init(Parser* p)
{
    array_init(&p->stringpool);
    array_init(&p->toks);
    array_init(&p->strings_to_free);
    scope_init(&p->scope);
    scope_init(&p->type_scope);
    cg_init(&p->cg);
    p->free_var_counter = 0;
    p->first_active_reg = NULL;
    memset(p->fn_label_prefix, 0, sizeof(p->fn_label_prefix));
    p->fn_ret_var = s_freevar_zero;
    p->fn = NULL;
    p->globals_size = 0;

    for (size_t i = 0; i < AST_KIND_END_POOLS; ++i)
    {
        pool_init(&p->ast_pools[i]);
    }
    array_init(&p->expr_seqs);

    pool_init(&p->type_pool);
    array_init(&p->type_seqs);
}
void parser_destroy(Parser* p)
{
    array_destroy(&p->toks);
    array_destroy(&p->stringpool);
    for (size_t i = 0; i < p->strings_to_free.sz / sizeof(char*); ++i)
    {
        free(((char**)p->strings_to_free.data)[i]);
    }
    array_destroy(&p->strings_to_free);
    scope_destroy(&p->scope);
    scope_destroy(&p->type_scope);
    cg_destroy(&p->cg);

    for (size_t i = 0; i < AST_KIND_END_POOLS; ++i)
    {
        pool_destroy(&p->ast_pools[i]);
    }
    array_destroy(&p->expr_seqs);

    pool_destroy(&p->type_pool);
    array_destroy(&p->type_seqs);
}

static int is_builtin_fn_expr(struct Expr* fn)
{
    if (fn->kind != EXPR_SYM) return 0;
    struct ExprSym* sym = (struct ExprSym*)fn;
    if (sym->sym->decl->attr.asmstr) return 1;
    return 0;
}

static void* find_with_stride(void* arr_start, size_t arr_size, void* key, size_t stride)
{
    for (size_t i = 0; i < arr_size; i += stride)
    {
        void* elem = arr_start + i;
        if (*(void**)elem == key) return elem;
    }
    return NULL;
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

struct Elaborator
{
    Parser* p;
    struct Array fns;
    struct Array callees_spans;
    struct Array callees_seqs;
};
static void elaborator_destroy(struct Elaborator* elab)
{
    array_destroy(&elab->fns);
    array_destroy(&elab->callees_spans);
    array_destroy(&elab->callees_seqs);
}

struct ElaborateDeclCtx
{
    struct Decl* decl;
    int calls_non_builtins;
    struct ArrSpan callees_span;
};

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

        return parser_ferror(rc, "error: unexpected type, expected '%s' but got '%s'\n", buf, buf2);
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
        case STMT_GOTO:
        case STMT_LABEL: *rty = s_type_unknown; return;
        case EXPR_LIT:
        {
            struct ExprLit* expr = top;
            if (expr->tok->type == LEX_NUMBER)
                *rty = s_type_literal_int;
            else if (expr->tok->type == LEX_STRING)
                *rty = s_type_literal_mstr;
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
                        parser_ice(&s_unknown_rc);
                        return abort();
                    }
                    ctx->decl->takes_addresses = 1;
                }
            }
            *rty = callee->sym.type;
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
                ctx->decl->takes_addresses = 1;
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
    if (elab->p->cg.fdebug)
        fprintf(elab->p->cg.fdebug,
                "Decl: %s: %.*s\n",
                token_str(elab->p, decl->id),
                decl->sym.type.used,
                decl->sym.type.buf);

    if (!decl->is_function)
    {
        if (decl->init)
        {
            return parser_ferror(&decl->id->rc, "error: global initializers are not supported\n");
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
    size_t const seqs_sz = elab->p->expr_seqs.sz / sizeof(struct Expr*);
    for (size_t i = 0; i < count; ++i)
    {
        if (declstmts[i]->kind != STMT_DECLS) abort();
        struct StmtDecls* decls = (struct StmtDecls*)declstmts[i];
        for (size_t j = 0; j < decls->extent; ++j)
        {
            if (seqs[decls->offset + j]->kind != AST_DECL) abort();
            struct Decl* decl = (struct Decl*)seqs[decls->offset + j];
            if (elaborate_local_decl(elab, decl)) return 1;
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
        struct ArrSpan* rev_edgespans = malloc(sizeof(struct ArrSpan) * n_fns);
        memset(rev_edgespans, 0, sizeof(struct ArrSpan) * n_fns);
        int* edges = malloc(n_edges * sizeof(int));
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
        int* rev_edges = malloc(n_edges * sizeof(int));
        char* is_reentrant = malloc(n_fns);
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

        char* arr_visited = malloc(n_fns);
        memset(arr_visited, 0, n_fns);
        int* arr_toposort = malloc(sizeof(int) * n_fns);
        int arr_toposort_offset = 0;
        for (size_t i = 0; i < n_fns; ++i)
        {
            arr_toposort_offset += dfs_emit(edgespans, edges, arr_toposort + arr_toposort_offset, arr_visited, i);
        }
        if (arr_toposort_offset != n_fns) abort();

        // dfs with reverse edges in reverse toposort order to find connected components
        memset(arr_visited, 0, n_fns);
        int* arr_rev_toposort = malloc(sizeof(int) * n_fns);
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
        for (size_t i = 0; i < n_fns; ++i)
        {
            if (!is_reentrant[i])
            {
                fns[i]->is_nonreentrant = 1;
            }
            if (fns[i]->is_nonreentrant && !fns[i]->takes_addresses)
            {
                fns[i]->is_stackless = 1;
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

int parse(Parser* p, Lexer* l)
{
    if (l->state == LEX_EOF)
    {
        {
            struct Token* const tk = (struct Token*)array_alloc(&p->toks, sizeof(struct Token));
            tk->type = l->state;
            tk->sp_offset = 0;
            tk->rc = l->rc;
        }

        // actually parse
        const size_t num_toks = p->toks.sz / sizeof(struct Token);
        struct Token* cur_tok = (struct Token*)p->toks.data;
        cg_write_bin_entry(&p->cg);

        struct Array arr_exprs;
        array_init(&arr_exprs);

        do
        {
            if (cur_tok->type == LEX_EOF) break;
            struct Expr* e;
            if (!(cur_tok = parse_stmt_decl(p, cur_tok, &e))) return 1;
            array_push(&arr_exprs, &e, sizeof(e));
        } while (1);
        scope_shrink(&p->scope, 0);
        struct Elaborator elab = {.p = p};
        if (elaborate_decls(&elab, arr_exprs.data, arr_exprs.sz / sizeof(struct Expr*))) return 1;

        struct CompoundBlock new_cb;
        cb_init(&new_cb, NULL, p);
        for (size_t i = 0; i < arr_exprs.sz / sizeof(struct Expr*); ++i)
        {
            if (compile_stmt_expr(p, ((struct Expr**)arr_exprs.data)[i], &new_cb)) return 1;
        }
        array_destroy(&arr_exprs);

        cg_emit(&p->cg, p->globals_size);
        return 0;
    }
    else
    {
        char* const s = array_push(&p->stringpool, l->tok, l->sz + 1);
        struct Token* const tk = array_alloc(&p->toks, sizeof(struct Token));
        tk->type = l->state;
        tk->sp_offset = s - (char*)p->stringpool.data;
        tk->rc = l->tok_rc;
    }
    return 0;
}
