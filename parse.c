#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "lexstate.h"
#include "parsestate.h"
#include "symbol.h"
#include "tok.h"

struct Token
{
    struct RowCol rc;
    LexerState type;
    ptrdiff_t sp_offset;
};

struct Attribute
{
    const char* symname;
    const char* asmstr;
    int is_nonreentrant : 1;
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
};

static const struct ValDest s_void_destination = {
    .kind = DEST_VOID,
    .is_const = 0,
};

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
        .is_const = sym->specs.is_const,
    };
    return ret;
}

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

static const char* PARAM_NAMES_ARR[REG_COUNT] = {"eax", "ebx", "ecx", "edx", "eex", "efx"};
static struct FreeVar s_freevar_paramreg0 = {
    .buf = {'e', 'a', 'x', '\0'},
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
        fprintf(stderr, "resource exceeded -- extern identifier too long (max %lu chars)\n", sizeof(ret.buf) - 3);
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
    ret->kind.kind = AST_SYM;
    memset(&ret->reg, 0, sizeof(ret->reg));
    ret->incomplete = 0;
    ret->is_function = 0;
    ret->is_nonreentrant = 0;
    memset(&ret->specs, 0, sizeof(ret->specs));
    ret->arg_count = 0;
    ret->token = NULL;
    ret->intrinsic_asm_str = NULL;
}
static int symbol_is_equivalent_redecl(struct Symbol* orig, struct Symbol* redecl) { return 1; }
static struct Symbol* symbol_alloc(struct Parser* p)
{
    struct Symbol* ret = pool_alloc(&p->sym_pool, sizeof(struct Symbol));
    symbol_init(ret);
    return ret;
}
struct CompoundBlock
{
    struct Symbol* fn_sym;
    int frame_size;
    int has_returned;
};
static void cb_init(struct CompoundBlock* cb, struct CompoundBlock* parent)
{
    cb->fn_sym = parent ? parent->fn_sym : NULL;
    cb->frame_size = parent ? cb->frame_size : 1;
    cb->has_returned = 0;
}
static struct RowCol s_unknown_rc = {
    .file = "<unknown>",
    .row = 1,
    .col = 1,
};
static int parser_spill_registers(struct Parser* p)
{
    struct RegMap* s = p->first_active_reg;
    p->first_active_reg = NULL;
    while (s)
    {
        struct RegMap* const next = s->next;
        if (s->is_dirty)
        {
            if (cg_store(&p->cg, s->stack_addr, s->rename.buf, &s_unknown_rc)) return 1;
            s->is_dirty = 0;
        }
        s->prev = NULL;
        s->next = NULL;
        s = next;
    }
    return 0;
}

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
static struct Binding* scope_data(struct Scope* s) { return (struct Binding*)s->binds.data; }
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
    struct Binding* const e = (struct Binding*)array_alloc(&s->binds, sizeof(struct Binding));
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

enum Precedence
{
    PRECEDENCE_ERROR,
    PRECEDENCE_COMMA,
    PRECEDENCE_ASSIGN,
    PRECEDENCE_EQUALITY,
    PRECEDENCE_RELATION,
    PRECEDENCE_ADD,
    PRECEDENCE_MULT,
};

static int compile_expr(Parser* p, struct Expr* expr, struct ValDest* dst, struct CompoundBlock* bb);

// requires !reg->prev
static void parser_push_active_reg(struct Parser* p, struct RegMap* reg)
{
    if (!reg->rename.buf[0]) abort();
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
static int parser_ensure_loaded_reg(struct Parser* p, struct RegMap* reg, const struct RowCol* rc)
{
    if (!reg->prev)
    {
        parser_push_active_reg(p, reg);

        if (cg_load(&p->cg, reg->stack_addr, reg->rename.buf, rc)) return 1;
    }
    return 0;
}
static int parser_ensure_loaded_sym(struct Parser* p, struct Symbol* sym)
{
    if (!sym->is_nonreentrant)
    {
        return parser_ensure_loaded_reg(p, &sym->reg, &sym->token->rc);
    }
    return 0;
}
static void parser_ensure_dirty_reg(struct Parser* p, struct RegMap* reg)
{
    if (!reg->prev)
    {
        parser_push_active_reg(p, reg);
    }
    reg->is_dirty = 1;
}
static void parser_ensure_dirty_sym(struct Parser* p, struct Symbol* sym)
{
    if (!sym->is_nonreentrant)
    {
        parser_ensure_dirty_reg(p, &sym->reg);
    }
}
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
            dst->regmap->stack_addr = bb->frame_size++;
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
static const char* parser_prepare_src_reg(Parser* p, struct ValDest* dst)
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
        if (parser_ensure_loaded_reg(p, dst->regmap, &s_unknown_rc)) return NULL;
        return dst->regmap->rename.buf;
    }
    else
    {
        abort();
    }
}

static int parser_assign_dsts(struct Parser* p, struct ValDest* dst, struct ValDest* src, struct CompoundBlock* bb)
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
    else if (dst->kind == DEST_REGMAP)
    {
        if (!dst->regmap->rename.buf[0])
        {
            *dst = *src;
            return 0;
        }
    }

    const char* src_reg = parser_prepare_src_reg(p, src);
    if (!src_reg) return 1;
    const char* dst_reg = parser_prepare_dst_reg(p, dst, bb);
    if (!dst_reg) return 1;
    cg_write_inst_set(cg, dst_reg, src_reg);
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
        case '>':
        case '<': return PRECEDENCE_RELATION;
        case '+':
        case '-': return PRECEDENCE_ADD;
        case '*':
        case '/': return PRECEDENCE_MULT;
        default: return PRECEDENCE_ERROR;
    }
}
static int op_precedence_assoc_right(enum Precedence p) { return p == PRECEDENCE_ASSIGN; }

static struct ExprOp* parse_alloc_expr_op(Parser* p, struct Token* tok, struct Expr* lhs, struct Expr* rhs)
{
    struct ExprOp* e = (struct ExprOp*)pool_alloc(&p->expr_op_pool, sizeof(struct ExprOp));
    e->kind.kind = EXPR_OP;
    e->tok = tok;
    e->lhs = lhs;
    e->rhs = rhs;
    return e;
}

static struct ExprSym* parse_alloc_expr_sym(Parser* p, struct Token* tok, struct Symbol* sym)
{
    struct ExprSym* e = (struct ExprSym*)pool_alloc(&p->expr_sym_pool, sizeof(struct ExprSym));
    e->kind.kind = EXPR_SYM;
    e->tok = tok;
    e->sym = sym;
    return e;
}
static struct ExprLit* parse_alloc_expr_lit(Parser* p, struct Token* tok)
{
    struct ExprLit* e = (struct ExprLit*)pool_alloc(&p->expr_lit_pool, sizeof(struct ExprLit));
    e->kind.kind = EXPR_LIT;
    e->tok = tok;
    return e;
}
static struct ExprCall* parse_alloc_expr_call(Parser* p, struct Token* tok, struct Expr* fn, size_t off, size_t ext)
{
    struct ExprCall* e = (struct ExprCall*)pool_alloc(&p->expr_call_pool, sizeof(struct ExprCall));
    e->kind.kind = EXPR_CALL;
    e->tok = tok;
    e->fn = fn;
    e->offset = off;
    e->extent = ext;
    return e;
}
static void parse_clear_exprs(Parser* p)
{
    pool_shrink(&p->expr_op_pool, 0);
    pool_shrink(&p->expr_lit_pool, 0);
    pool_shrink(&p->expr_call_pool, 0);
    pool_shrink(&p->expr_sym_pool, 0);
    p->expr_seqs.sz = 0;
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
            if (op[0] == '!')
            {
                struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, NULL, NULL);
                *ppe = (struct Expr*)e;
                return parse_expr_unary_atom(p, cur_tok + 1, &e->lhs);
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
            if (op_precedence_assoc_right(op_prec))
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

static int compile_expr_lit(Parser* p, struct ExprLit* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    if (e->tok->type == LEX_NUMBER)
    {
        struct ValDest src = dest_literal(token_str(p, e->tok));
        return parser_assign_dsts(p, dst, &src, bb);
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
        return parser_assign_dsts(p, dst, &src, bb);
    }
    else
        abort();
}

static int compile_expr_op(Parser* p, struct ExprOp* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    if (!e->lhs) return parser_ice(&e->tok->rc);

    const char* const op = token_str(p, e->tok);
    if (op[0] == '!' && op[1] == '\0')
    {
        if (dest_is_any(dst))
        {
            if (compile_expr(p, e->lhs, dst, bb)) return 1;
            const char* const srcreg = parser_prepare_src_reg(p, dst);
            if (!srcreg) return 1;
            const char* const dstreg = parser_prepare_dst_reg(p, dst, bb);
            if (!dstreg) return 1;
            cg_write_inst_op(&p->cg, "!=", dstreg, srcreg, "true");
        }
        else
        {
            struct RegMap regmap = {0};
            struct ValDest lhs = dest_regmap(&regmap);
            if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
            const char* const srcreg = parser_prepare_src_reg(p, &lhs);
            if (!srcreg) return 1;
            const char* const dstreg = parser_prepare_dst_reg(p, dst, bb);
            if (!dstreg) return 1;
            cg_write_inst_op(&p->cg, "!=", dstreg, srcreg, "true");
            parser_deactivate_reg(p, &regmap);
        }
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
            if (parser_assign_dsts(p, dst, &lhs, bb)) return 1;
            parser_deactivate_reg(p, &regmap);
            return 0;
        }
    }

    struct RegMap regmapl = {0};
    struct ValDest lhs = dest_regmap(&regmapl);
    struct RegMap regmapr = {0};
    struct ValDest rhs = dest_regmap(&regmapr);
    if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
    if (compile_expr(p, e->rhs, &rhs, bb)) return 1;
    const char* r2 = parser_prepare_src_reg(p, &lhs);
    if (!r2) return 1;
    const char* r3 = parser_prepare_src_reg(p, &rhs);
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

static int compile_expr_call_eval_args(
    Parser* p, struct ExprCall* e, struct ValDest* dst, struct Symbol* fn, struct CompoundBlock* bb)
{
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
    const char* s = fn->intrinsic_asm_str;
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
                size_t i = *s - '0';
                if (i >= e->extent)
                {
                    return parser_ferror(&fn->token->rc, "error: invalid intrinsic string: wrong arguments\n");
                }
                const char* const reg = parser_prepare_src_reg(p, arg_dsts + i);
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
                return parser_ferror(&fn->token->rc, "error: invalid intrinsic string: invalid format specifier\n");
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
        if (src.regmap->prev)
        {
            cg_write_inst_set(cg, tgt, src.regmap->rename.buf);
            return 0;
        }
        else
        {
            return cg_load(&p->cg, src.regmap->stack_addr, tgt, &s_unknown_rc);
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

    if (fn.kind != DEST_SYM || !fn.sym->is_function)
    {
        return parser_ferror(&e->tok->rc, "error: attempting to call a non-function\n");
    }

    if (fn.sym->arg_count != e->extent)
    {
        return parser_ferror(
            &e->tok->rc, "error: function requires %d arguments, %lu provided\n", fn.sym->arg_count, e->extent);
    }
    if (fn.sym->intrinsic_asm_str)
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

        if (fn.sym->is_nonreentrant)
        {
            const char* const name = token_str(p, fn.sym->token);
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
            if (!bb->fn_sym->is_nonreentrant)
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
            if (parser_assign_dsts(p, dst, &ret_dst, bb)) return 1;
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
            struct ValDest sym_dst = dest_sym(esym->sym);
            return parser_assign_dsts(p, dst, &sym_dst, bb);
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

static struct Token* compile_attribute(Parser* p, struct Token* cur_tok, struct Attribute* attr)
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
static struct Token* compile_attribute_plist(Parser* p, struct Token* cur_tok, struct Attribute* attr)
{
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;
    if (!token_is_sym(p, cur_tok, ')'))
    {
        do
        {
            if (!(cur_tok = compile_attribute(p, cur_tok, attr))) return NULL;
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

static struct Token* compile_decl(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb);
static struct Token* compile_declspecs(Parser* p, struct Token* cur_tok, struct DeclSpecs* specs);
static struct Token* compile_declarator(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb, struct Symbol* sym)
{
    struct Attribute attr = {0};
    if (cur_tok->type == LEX_ATTRIBUTE)
    {
        ++cur_tok;
        cur_tok = compile_attribute_plist(p, cur_tok, &attr);
        if (!cur_tok) return NULL;
    }
    if (cur_tok->type != LEX_IDENT)
    {
        return parser_ferror(&cur_tok->rc, "error: expected identifier\n"), NULL;
    }
    struct Token* id = cur_tok++;
    const char* const name = token_str(p, id);
    sym->token = id;
    if (attr.symname)
    {
        if (strlen(attr.symname) >= sizeof(sym->reg.rename.buf))
        {
            return parser_ferror(&cur_tok->rc,
                                 "error: symbol names have a maximum length of %lu characters\n",
                                 sizeof(sym->reg.rename.buf) - 1),
                   NULL;
        }
        strcpy(sym->reg.rename.buf, attr.symname);
    }
    sym->intrinsic_asm_str = attr.asmstr;
    sym->is_nonreentrant = attr.is_nonreentrant;

    const char ch = cur_tok->type == LEX_SYMBOL ? token_str(p, cur_tok)[0] : '\0';
    if (ch == '(')
    {
        sym->is_function = 1;
        if (sym->reg.rename.buf[0])
        {
            if (sym->reg.rename.buf[0] != '$' || !sym->reg.rename.buf[1] ||
                sym->reg.rename.buf[strlen(sym->reg.rename.buf) - 1] != '$')
            {
                return parser_ferror(&cur_tok->rc,
                                     "error: external symbol names must be of the form '$foo$' (was '%s')\n",
                                     sym->reg.rename.buf),
                       NULL;
            }
        }
        else
        {
            sym->reg.rename = extern_var_from(p, name);
        }
        ++cur_tok;
        if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == ')')
        {
            ++cur_tok;
        }
        else
        {
            struct Symbol* args[REG_COUNT];

            do
            {
                if (sym->arg_count == REG_COUNT)
                    return parser_ferror(&cur_tok->rc, "error: maximum argument count exceeded (%d)\n", REG_COUNT),
                           NULL;

                struct Symbol* const arg_sym = args[sym->arg_count++] = symbol_alloc(p);
                if (!(cur_tok = compile_declspecs(p, cur_tok, &arg_sym->specs))) return NULL;
                if (!(cur_tok = compile_declarator(p, cur_tok, bb, arg_sym))) return NULL;

                if (sym->is_nonreentrant)
                {
                    arg_sym->reg.rename = nrfun_param(p, sym->arg_count, name);
                }
                else
                {
                    arg_sym->reg.rename = free_var(p);
                }
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
                        ++cur_tok;
                        break;
                    }
                }
                return parser_ferror(&cur_tok->rc, "error: expected ',' and further parameter declarations or ')'\n"),
                       NULL;
            } while (1);

            sym->arg_offset = p->expr_seqs.sz / sizeof(struct Symbol*);
            array_push(&p->expr_seqs, args, sym->arg_count * sizeof(struct Symbol*));
        }
    }
    else
    {
        // not a function
        sym->is_nonreentrant |= !bb->fn_sym || bb->fn_sym->is_nonreentrant;
        if (!sym->reg.rename.buf[0]) sym->reg.rename = free_var_from(p, name);
    }
    return cur_tok;
}
static struct Token* compile_declspecs(Parser* p, struct Token* cur_tok, struct DeclSpecs* specs)
{
    do
    {
        if (cur_tok->type == LEX_IDENT)
        {
            struct Binding* const cur_bind = scope_find(&p->type_scope, token_str(p, cur_tok));
            if (!cur_bind)
            {
                return cur_tok;
            }
        }
        else if (cur_tok->type == LEX_VOID || cur_tok->type == LEX_INT || cur_tok->type == LEX_MSTRING)
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
            return cur_tok;
        }
        ++cur_tok;
    } while (1);
}
static struct Token* compile_compound_body(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb);
static struct Token* compile_compound_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb)
{
    size_t scope_sz = scope_size(&p->scope);
    if (cur_tok = compile_compound_body(p, cur_tok, bb))
    {
        cur_tok = token_consume_sym(p, cur_tok, '}');
    }
    scope_shrink(&p->scope, scope_sz);
    return cur_tok;
}
static struct Token* compile_return_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb) { }
static struct Token* compile_decl(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb)
{
    struct DeclSpecs specs = {0};
    if (cur_tok = compile_declspecs(p, cur_tok, &specs))
    {
        while (1)
        {
            struct Symbol* const sym = symbol_alloc(p);
            sym->specs = specs;
            if (!(cur_tok = compile_declarator(p, cur_tok, bb, sym))) return NULL;
            const char* name = token_str(p, sym->token);
            struct Binding* prev_sym = scope_find(&p->scope, name);
            if (prev_sym)
            {
                // ensure symbols match
                if (!symbol_is_equivalent_redecl(prev_sym->sym, sym))
                {
                    return parser_ferror(&cur_tok->rc, "error: declaration doesn't match previous\n"), NULL;
                }
            }
            scope_insert(&p->scope, name, sym);
            size_t scope_sz = scope_size(&p->scope);
            sym->reg.stack_addr = bb->frame_size++;
            if (cur_tok->type == LEX_SYMBOL)
            {
                const char ch = token_str(p, cur_tok)[0];
                if (ch == '{')
                {
                    struct CompoundBlock new_cb;
                    cb_init(&new_cb, NULL);
                    new_cb.fn_sym = sym;
                    // begin compound block
                    if (sizeof(p->fn_label_prefix) <=
                        snprintf(p->fn_label_prefix, sizeof(p->fn_label_prefix), "%d_", p->free_var_counter++))
                    {
                        fprintf(stderr, "resource exceeded -- too many symbols\n");
                        abort();
                    }
                    cg_mark_label(&p->cg, sym->reg.rename.buf);
                    struct Symbol* const* const sym_start = p->expr_seqs.data;
                    for (size_t i = 0; i < sym->arg_count; ++i)
                    {
                        struct Symbol* arg_sym = sym_start[sym->arg_offset + i];
                        if (arg_sym->token) scope_insert(&p->scope, token_str(p, arg_sym->token), arg_sym);
                    }
                    if (sym->is_nonreentrant)
                    {
                        p->fn_ret_var = nrfun_param_ret(p, token_str(p, sym->token));
                    }
                    else
                    {
                        if (sym->arg_count > REG_COUNT)
                        {
                            return parser_ferror(&cur_tok->rc, "error: exceeded maximum argument count\n"), NULL;
                        }
                        for (size_t i = 0; i < sym->arg_count; ++i)
                        {
                            struct Symbol* arg_sym = sym_start[sym->arg_offset + i];
                            arg_sym->reg.stack_addr = new_cb.frame_size++;
                            cg_write_inst_set(&p->cg, arg_sym->reg.rename.buf, PARAM_NAMES_ARR[i]);
                        }
                        cg_write_push_ret(&p->cg, &p->fn_ret_var);
                    }
                    if (cur_tok = compile_compound_stmt(p, cur_tok + 1, &new_cb))
                    {
                        if (!new_cb.has_returned)
                        {
                            if (parser_spill_registers(p)) return NULL;
                            cg_write_return(&p->cg, &p->fn_ret_var);
                        }
                        p->fn_ret_var = s_freevar_zero;
                        scope_shrink(&p->scope, scope_sz);
                    }
                    return cur_tok;
                }
                else
                {
                    // since we aren't defining a function body, remove any parameters from the scope
                    if (ch == '=')
                    {
                        struct ValDest dst = dest_sym(sym);

                        struct Expr* expr;
                        if (!(cur_tok = parse_expr(p, cur_tok + 1, &expr, PRECEDENCE_ASSIGN))) return NULL;
                        if (compile_expr(p, expr, &dst, bb)) return NULL;
                        parse_clear_exprs(p);
                    }
                    if (cur_tok->type != LEX_SYMBOL)
                        return parser_ferror(&cur_tok->rc, "error: expectedd ',' or ';'\n"), NULL;
                    const char ch2 = *token_str(p, cur_tok);
                    if (ch2 == ',')
                    {
                        ++cur_tok;
                        continue;
                    }
                    else if (ch2 == ';')
                    {
                        return cur_tok + 1;
                    }
                }
            }
            return parser_ferror(&cur_tok->rc, "error: expected ',' or ';'\n"), NULL;
        }
    }
    return cur_tok;
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

enum CondInverted
{
    COND_NORMAL,
    COND_INVERTED,
};
static struct Token* compile_conditional(
    Parser* p, struct Token* cur_tok, struct CompoundBlock* bb, const char* jump_to, enum CondInverted inverted)
{
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;

    struct Expr* cond;
    if (!(cur_tok = parse_expr(p, cur_tok, &cond, PRECEDENCE_COMMA))) return NULL;

    if (cond->kind == EXPR_OP)
    {
        struct ExprOp* cond_op = (struct ExprOp*)cond;
        const char* const op = token_str(p, cond_op->tok);
        enum Precedence pr = op_precedence(op);
        if (pr == PRECEDENCE_EQUALITY || pr == PRECEDENCE_RELATION)
        {
            struct RegMap lhs_regmap = {0};
            struct ValDest lhs = dest_regmap(&lhs_regmap);
            if (compile_expr(p, cond_op->lhs, &lhs, bb)) return NULL;
            struct RegMap rhs_regmap = {0};
            struct ValDest rhs = dest_regmap(&rhs_regmap);
            if (compile_expr(p, cond_op->rhs, &rhs, bb)) return NULL;

            parse_clear_exprs(p);

            const char* const lhs_reg = parser_prepare_src_reg(p, &lhs);
            if (!lhs_reg) return NULL;
            const char* const rhs_reg = parser_prepare_src_reg(p, &rhs);
            if (!rhs_reg) return NULL;

            parser_deactivate_reg(p, &lhs_regmap);
            parser_deactivate_reg(p, &rhs_regmap);

            // Do if-stmt fusion
            if (parser_spill_registers(p)) return NULL;
            cg_write_inst_jump_op(&p->cg, jump_to, inverted ? relation_invert(op) : op, lhs_reg, rhs_reg);
            return token_consume_sym(p, cur_tok, ')');
        }
    }

    struct RegMap lhs_regmap = {0};
    struct ValDest lhs = dest_regmap(&lhs_regmap);
    if (compile_expr(p, cond, &lhs, bb)) return NULL;
    parse_clear_exprs(p);
    const char* const lhs_reg = parser_prepare_src_reg(p, &lhs);
    if (!lhs_reg) return NULL;
    parser_deactivate_reg(p, &lhs_regmap);
    if (parser_spill_registers(p)) return NULL;
    cg_write_inst_jump_op(&p->cg, jump_to, inverted ? "!=" : "==", lhs_reg, "true");
    return token_consume_sym(p, cur_tok, ')');
}
static struct Token* compile_if_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb)
{
    struct FreeVar else_lbl = free_var_label(p);
    if (!(cur_tok = compile_conditional(p, cur_tok, bb, else_lbl.buf, COND_INVERTED))) return NULL;
    if (cur_tok = compile_stmt(p, cur_tok, bb))
    {
        if (parser_spill_registers(p)) return NULL;
        if (cur_tok->type == LEX_ELSE)
        {
            ++cur_tok;
            struct FreeVar endif_lbl = free_var_label(p);
            cg_write_inst_jump(&p->cg, endif_lbl.buf);
            cg_mark_label(&p->cg, else_lbl.buf);
            cur_tok = compile_stmt(p, cur_tok, bb);
            if (parser_spill_registers(p)) return NULL;
            cg_mark_label(&p->cg, endif_lbl.buf);
            return cur_tok;
        }
        else
        {
            cg_mark_label(&p->cg, else_lbl.buf);
        }
    }
    return cur_tok;
}
static struct Token* compile_while_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb)
{
    struct FreeVar continue_lbl = free_var_label(p);
    struct FreeVar break_lbl = free_var_label(p);
    if (parser_spill_registers(p)) return NULL;
    cg_mark_label(&p->cg, continue_lbl.buf);

    if (!(cur_tok = compile_conditional(p, cur_tok, bb, break_lbl.buf, COND_INVERTED))) return NULL;
    if (cur_tok = compile_stmt(p, cur_tok, bb))
    {
        if (parser_spill_registers(p)) return NULL;
        cg_write_inst_jump(&p->cg, continue_lbl.buf);
        cg_mark_label(&p->cg, break_lbl.buf);
    }
    return cur_tok;
}
static struct Token* compile_do_while_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb)
{
    struct FreeVar top_lbl = free_var_label(p);
    struct FreeVar continue_lbl = free_var_label(p);
    struct FreeVar break_lbl = free_var_label(p);
    if (parser_spill_registers(p)) return NULL;
    cg_mark_label(&p->cg, top_lbl.buf);
    if (!(cur_tok = compile_stmt(p, cur_tok, bb))) return NULL;
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

static struct Token* compile_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb)
{
    switch (cur_tok->type)
    {
        case LEX_INT:
        case LEX_VOLATILE:
        case LEX_CONST:
        case LEX_REGISTER:
        case LEX_VOID: return compile_decl(p, cur_tok, bb);
        case LEX_RETURN:
        {
            bb->has_returned = 1;
            struct Expr* e;
            if (cur_tok = parse_expr(p, cur_tok + 1, &e, PRECEDENCE_COMMA))
            {
                struct RegMap regmap = {
                    .rename = s_freevar_paramreg0,
                };
                struct ValDest dst = dest_regmap(&regmap);
                if (compile_expr(p, e, &dst, bb)) return NULL;
                parse_clear_exprs(p);
                parser_deactivate_reg(p, &regmap);
                if (parser_spill_registers(p)) return NULL;
                cg_write_return(&p->cg, &p->fn_ret_var);
            }
            return cur_tok;
        }
        case LEX_IF:
        {
            return compile_if_stmt(p, cur_tok + 1, bb);
        }
        case LEX_WHILE:
        {
            return compile_while_stmt(p, cur_tok + 1, bb);
        }
        case LEX_DO:
        {
            return compile_do_while_stmt(p, cur_tok + 1, bb);
        }
        case LEX_GOTO:
        {
            ++cur_tok;
            if (cur_tok->type != LEX_IDENT)
            {
                return parser_ferror(&cur_tok->rc, "error: expected label to go to\n"), NULL;
            }
            char buf[64];
            const char* const cur_tok_str = token_str(p, cur_tok);
            if (sizeof(buf) <= snprintf(buf, sizeof(buf), "$%s%s$", p->fn_label_prefix, cur_tok_str))
            {
                fprintf(stderr, "resource exceeded: label too long: '%s'\n", cur_tok_str);
                abort();
            }
            cg_write_inst_jump(&p->cg, buf);
            ++cur_tok;
            if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == ';')
            {
                ++cur_tok;
            }
            else
            {
                return parser_ferror(&cur_tok->rc, "error: expected semicolon\n"), NULL;
            }
            return cur_tok;
        }
        case LEX_IDENT:
        {
            struct Binding* const cur_bind = scope_find(&p->type_scope, token_str(p, cur_tok));
            if (cur_bind)
            {
                // this is a declaration
                return compile_decl(p, cur_tok, bb);
            }
        }
        case LEX_NUMBER:
        {
            struct Expr* e;
            if (!(cur_tok = parse_expr(p, cur_tok, &e, PRECEDENCE_COMMA))) return NULL;
            struct ValDest dst = s_void_destination;
            if (compile_expr(p, e, &dst, bb)) return NULL;
            parse_clear_exprs(p);
            return token_consume_sym(p, cur_tok, ';');
        }
        case LEX_SYMBOL:
        {
            const char ch = token_str(p, cur_tok)[0];
            if (ch == ';')
            {
                return cur_tok + 1;
            }
            else if (ch == '{')
            {
                const size_t scope_sz = scope_size(&p->scope);
                struct CompoundBlock new_cb;
                cb_init(&new_cb, bb);
                if (cur_tok = compile_compound_body(p, cur_tok + 1, &new_cb))
                {
                    if (cur_tok->type != LEX_SYMBOL || token_str(p, cur_tok)[0] != '}')
                    {
                        return parser_ferror(&cur_tok->rc, "error: expected '}'\n"), NULL;
                    }
                    ++cur_tok;
                }
                scope_shrink(&p->scope, scope_sz);
                return cur_tok;
            }
            else
            {
                return parser_ferror(&cur_tok->rc, "error: expected statement\n"), NULL;
            }
        }
        default: return parser_ferror(&cur_tok->rc, "error: expected statement\n"), NULL;
    }
}

static struct Token* compile_compound_body(Parser* p, struct Token* cur_tok, struct CompoundBlock* cb)
{
    do
    {
        if (cur_tok->type == LEX_EOF) return cur_tok;
        if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == '}') return cur_tok;

        // check for label
        if (cur_tok->type == LEX_IDENT && cur_tok[1].type == LEX_SYMBOL && token_str(p, cur_tok + 1)[0] == ':')
        {
            const char* const str = token_str(p, cur_tok);
            char buf[64];
            if (sizeof(buf) <= snprintf(buf, sizeof(buf), "$%s%s$", p->fn_label_prefix, str))
            {
                fprintf(stderr, "resource exceeded: label too long: '%s'\n", str);
                abort();
            }
            if (parser_spill_registers(p)) return NULL;
            cg_mark_label(&p->cg, buf);
            cur_tok += 2;
            cb->has_returned = 0;
        }
        cur_tok = compile_stmt(p, cur_tok, cb);
        if (!cur_tok) return NULL;
    } while (1);
}

void parser_init(Parser* p)
{
    array_init(&p->stringpool);
    array_init(&p->toks);
    array_init(&p->strings_to_free);
    p->fn_ret_var = s_freevar_zero;
    scope_init(&p->scope);
    scope_init(&p->type_scope);
    cg_init(&p->cg);
    p->free_var_counter = 0;
    p->first_active_reg = NULL;
    memset(p->fn_label_prefix, 0, sizeof(p->fn_label_prefix));

    pool_init(&p->sym_pool);
    pool_init(&p->expr_op_pool);
    pool_init(&p->expr_sym_pool);
    pool_init(&p->expr_call_pool);
    pool_init(&p->expr_lit_pool);
    array_init(&p->expr_seqs);
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

    pool_destroy(&p->sym_pool);
    pool_destroy(&p->expr_op_pool);
    pool_destroy(&p->expr_sym_pool);
    pool_destroy(&p->expr_call_pool);
    pool_destroy(&p->expr_lit_pool);
    array_destroy(&p->expr_seqs);
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

        struct CompoundBlock new_cb;
        cb_init(&new_cb, NULL);
        // actually parse
        const size_t num_toks = p->toks.sz / sizeof(struct Token);
        struct Token* const arr_toks = (struct Token*)p->toks.data;
        cg_write_bin_entry(&p->cg);
        struct Token* tk = compile_compound_body(p, arr_toks, &new_cb);
        if (!tk) return 1;
        if (tk->type != LEX_EOF)
        {
            return parser_ferror(&tk->rc, "error: expected eof\n"), 1;
        }

        cg_emit(&p->cg);
        return 0;
    }
    else
    {
        char* const s = (char*)array_push(&p->stringpool, l->tok, l->sz + 1);
        struct Token* const tk = (struct Token*)array_alloc(&p->toks, sizeof(struct Token));
        tk->type = l->state;
        tk->sp_offset = s - (char*)p->stringpool.data;
        tk->rc = l->tok_rc;
    }
    return 0;
}
