#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
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
    if (!reg->prev && reg->stack_addr >= 0)
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
        return parser_ensure_loaded_reg(p, &sym->reg, &sym->decl->id->rc);
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
    if (!sym->is_nonreentrant)
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
            if (!bb->fn_sym || bb->fn_sym->is_nonreentrant)
                dst->regmap->stack_addr = -1;
            else
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
    else if (dest_is_any(dst))
    {
        *dst = *src;
        return 0;
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
        else if (op[0] == '+' && op[1] == '+')
        {
            char* op = token_str(p, cur_tok);
            struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, lhs, NULL);
            return parse_expr_post_unary(p, cur_tok + 1, (struct Expr*)e, ppe);
        }
        else if (op[0] == '-' && op[1] == '-')
        {
            char* op = token_str(p, cur_tok);
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
struct Type
{
    enum TypeKind kind;
    int is_const : 1;
    int is_volatile : 1;
};
struct TypeFn
{
    struct Type base;
    struct Type* ret;
    size_t offset;
    size_t extent;
};
struct TypePtr
{
    struct Type base;
    struct Type* inner;
};
struct TypeCategory
{
    enum ValueCategory cat;
    struct Type* type;
};
static struct Type s_type_unknown = {
    .kind = TYPE_UNKNOWN,
};
static struct TypeCategory s_typecat_unknown = {
    .type = &s_type_unknown,
    .cat = CAT_UNKNOWN,
};
static struct Type s_type_literal_int = {
    .kind = TYPE_INT,
    .is_const = 1,
};
static struct Type s_type_int = {
    .kind = TYPE_INT,
};
static struct Type s_type_void = {
    .kind = TYPE_VOID,
};
static struct Type s_type_char = {
    .kind = TYPE_CHAR,
};
static struct Type s_type_mstr = {
    .kind = TYPE_MSTRING,
};
static struct Type s_type_literal_str = {
    .kind = TYPE_MSTRING,
    .is_const = 1,
};

static struct Type* tc_decl(Parser* p, struct Decl* decl);
static struct Type* tc_sym(Parser* p, struct Symbol* sym) { return sym->type; }
static struct Type* tc_decl(Parser* p, struct Decl* decl)
{
    if (decl->is_function)
    {
        struct TypeFn ty = {
            .base.kind = TYPE_FUN,
            .offset = p->type_seqs.sz / sizeof(struct Type*),
            .extent = decl->extent,
        };
        if (decl->specs.type->type == LEX_INT)
        {
            ty.ret = &s_type_int;
        }
        else if (decl->specs.type->type == LEX_VOID)
        {
            ty.ret = &s_type_void;
        }
        else if (decl->specs.type->type == LEX_MSTRING)
        {
            ty.ret = &s_type_mstr;
        }
        else
        {
            parser_ferror(&decl->specs.type->rc, "error: unknown return type\n");
            return &s_type_unknown;
        }
        array_alloc(&p->type_seqs, ty.extent * sizeof(struct Type*));
        struct Expr** exprs = p->expr_seqs.data;
        for (size_t i = 0; i < decl->extent; ++i)
        {
            struct Expr* arg_expr = exprs[decl->offset + i];
            struct Type* ty_arg;
            if (arg_expr->kind == AST_DECL)
                ty_arg = tc_decl(p, (struct Decl*)arg_expr);
            else if (arg_expr->kind == EXPR_SYM)
                ty_arg = tc_sym(p, (struct Symbol*)arg_expr);
            else
                abort();
            struct Type** types = p->type_seqs.data;
            types[ty.offset + i] = ty_arg;
        };
        return pool_push(&p->typefn_pool, &ty, sizeof(ty));
    }
    else
    {
        if (decl->specs.type->type == LEX_INT)
        {
            return &s_type_int;
        }
        else if (decl->specs.type->type == LEX_VOID)
        {
            return &s_type_void;
        }
        else if (decl->specs.type->type == LEX_MSTRING)
        {
            return &s_type_mstr;
        }
        else
            abort();
    }
}

static const struct RowCol* rc_for_expr(const struct Expr* e)
{
    switch (e->kind)
    {
        case EXPR_OP: return &((const struct ExprOp*)e)->tok->rc;
        case EXPR_LIT: return &((const struct ExprLit*)e)->tok->rc;
        case EXPR_SYM: return &((const struct ExprSym*)e)->tok->rc;
        case EXPR_CALL: return rc_for_expr(((const struct ExprCall*)e)->fn);
        default: return &s_unknown_rc;
    }
}

static const char* type_to_string(struct Type* t)
{
    switch (t->kind)
    {
        case TYPE_INT: return "int";
        case TYPE_CHAR: return "char";
        case TYPE_MSTRING: return "string literal";
        case TYPE_VOID: return "void";
        case TYPE_PTR: return "pointer";
        case TYPE_FUN: return "function";
        default: return "unknown";
    }
}

static struct TypeCategory tc_expr(Parser* p, struct Expr* e);
static struct TypeCategory tc_expr_unify(Parser* p, struct Expr* e, struct Type* t2)
{
    struct TypeCategory tc = tc_expr(p, e);
    if (tc.type->kind != t2->kind)
    {
        parser_ferror(
            rc_for_expr(e), "error: expected '%s' but got '%s'\n", type_to_string(t2), type_to_string(tc.type));
    }
    tc.type = t2;
    return tc;
}

static struct TypeCategory tc_expr_op(Parser* p, struct ExprOp* e)
{
    struct TypeCategory tc_lhs = tc_expr(p, e->lhs);
    const char* str = token_str(p, e->tok);
    if (!e->rhs) return tc_lhs;
    struct TypeCategory tc_rhs = tc_expr_unify(p, e->rhs, tc_lhs.type);
    tc_lhs.cat = CAT_PRVALUE;
    return tc_lhs;
}
static struct TypeCategory tc_expr(Parser* p, struct Expr* e)
{
    if (e->kind == EXPR_LIT)
    {
        struct ExprLit* f = (struct ExprLit*)e;
        if (f->tok->type == LEX_STRING)
        {
            struct TypeCategory ret = {
                .cat = CAT_LVALUE,
                .type = &s_type_literal_str,
            };
            return ret;
        }
        else if (f->tok->type == LEX_NUMBER)
        {
            struct TypeCategory ret = {
                .cat = CAT_PRVALUE,
                .type = &s_type_literal_int,
            };
            return ret;
        }
        else
        {
            abort();
        }
    }
    else if (e->kind == EXPR_OP)
    {
        return tc_expr_op(p, (struct ExprOp*)e);
    }
    else if (e->kind == EXPR_SYM)
    {
        struct TypeCategory ret = {
            .cat = CAT_LVALUE,
            .type = tc_sym(p, ((struct ExprSym*)e)->sym),
        };
        return ret;
    }
    else if (e->kind == EXPR_CALL)
    {
        struct ExprCall* f = (struct ExprCall*)e;
        struct TypeCategory tc_fn = tc_expr(p, f->fn);
        if (tc_fn.type->kind != TYPE_FUN)
        {
            parser_ferror(&f->tok->rc, "error: call must be applied to a function type\n");
            return s_typecat_unknown;
        }
        struct TypeFn* ty_fn = (struct TypeFn*)tc_fn.type;
        if (ty_fn->extent != f->extent)
        {
            parser_ferror(&f->tok->rc, "error: called with wrong number of arguments (expected %lu)\n", ty_fn->extent);
            return s_typecat_unknown;
        }
        struct Expr** expr_begin = p->expr_seqs.data;
        for (size_t i = 0; i < f->extent; ++i)
        {
            struct Type** types_begin = p->type_seqs.data;
            struct TypeCategory tc_arg = tc_expr_unify(p, expr_begin[f->offset + i], types_begin[ty_fn->offset + i]);
        }
        struct TypeCategory ret = {
            .cat = CAT_PRVALUE,
            .type = ty_fn->ret,
        };
        return ret;
    }
    else
    {
        abort();
    }
    return s_typecat_unknown;
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

enum CondInverted
{
    COND_NORMAL,
    COND_INVERTED,
};
static int compile_conditional_expr(
    Parser* p, struct Expr* cond, struct CompoundBlock* bb, const char* jump_to, enum CondInverted inverted);

static int compile_expr_op(Parser* p, struct ExprOp* e, struct ValDest* dst, struct CompoundBlock* bb)
{
    if (!e->lhs) return parser_ice(&e->tok->rc);

    const char* const op = token_str(p, e->tok);
    if (op[0] == '!' && op[1] == '\0')
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
        return 0;
    }
    else if (op[0] == '+' && op[1] == '+')
    {
        struct RegMap regmap = {0};
        struct ValDest lhs = dest_regmap(&regmap);
        if (compile_expr(p, e->lhs, &lhs, bb)) return 1;
        const char* const srcreg = parser_prepare_src_reg(p, &lhs);
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
        const char* const srcreg = parser_prepare_src_reg(p, &lhs);
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
            if (parser_assign_dsts(p, dst, &lhs, bb)) return 1;
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
    if (op[0] == '-' && op[1] == '=')
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
            if (parser_assign_dsts(p, dst, p_lhs, bb)) return 1;
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
            if (parser_assign_dsts(p, dst, p_lhs, bb)) return 1;
            parser_deactivate_reg(p, &regmapl);
        }
        return 0;
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
                const char* const reg = parser_prepare_src_reg(p, arg_dsts + n);
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
        if (src.sym->is_nonreentrant)
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
        if (src.regmap->prev)
        {
            cg_write_inst_set(cg, tgt, src.regmap->rename.buf);
            return 0;
        }
        else
        {
            return cg_load(cg, src.regmap->stack_addr, tgt, &s_unknown_rc);
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

        if (fn.sym->is_nonreentrant)
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
            struct Symbol* sym = &decl_get_def(esym->sym->decl)->sym;
            struct ValDest sym_dst = dest_sym(sym);
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
static struct Token* parse_declarator(Parser* p, struct Token* cur_tok, struct DeclSpecs* specs, struct Decl** pdecl)
{
    struct Decl* decl = *pdecl = pool_alloc(&p->ast_pools[AST_DECL], sizeof(struct Decl));
    memset(decl, 0, sizeof(struct Decl));
    decl->kind.kind = AST_DECL;
    decl->specs = *specs;
    symbol_init(&decl->sym);
    decl->sym.decl = decl;

    if (cur_tok->type == LEX_ATTRIBUTE)
    {
        ++cur_tok;
        cur_tok = parse_attribute_plist(p, cur_tok, &decl->attr);
        if (!cur_tok) return NULL;
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
        decl->is_function = 1;
        ++cur_tok;
        if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == ')')
        {
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
                if (!(cur_tok = parse_declarator(p, cur_tok, &arg_specs, arg_decls + decl->extent++))) return NULL;

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
    return cur_tok;
}

static struct Token* parse_decl(Parser* p, struct Token* cur_tok, struct Array* pdecls)
{
    struct DeclSpecs specs = {0};
    if (!(cur_tok = parse_declspecs(p, cur_tok, &specs))) return NULL;
    while (1)
    {
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
static int compile_decl_single(Parser* p, struct Decl* decl, struct CompoundBlock* bb)
{
    struct Symbol* const sym = &decl->sym;
    sym->type = tc_decl(p, decl);

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
        sym->is_nonreentrant |= !bb->fn_sym || bb->fn_sym->is_nonreentrant;
        if (!sym->reg.rename.buf[0]) sym->reg.rename = free_var_from(p, name);

        scope_insert(&p->scope, name, sym);
        sym->reg.stack_addr = bb->frame_size++;
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

static int compile_conditional_expr(
    Parser* p, struct Expr* cond, struct CompoundBlock* bb, const char* jump_to, enum CondInverted inverted)
{
    if (cond->kind == EXPR_OP)
    {
        struct ExprOp* cond_op = (struct ExprOp*)cond;
        const char* const op = token_str(p, cond_op->tok);
        enum Precedence pr = op_precedence(op);
        if (pr == PRECEDENCE_EQUALITY || pr == PRECEDENCE_RELATION)
        {
            struct RegMap lhs_regmap = {0};
            struct ValDest lhs = dest_regmap(&lhs_regmap);
            if (compile_expr(p, cond_op->lhs, &lhs, bb)) return 1;
            struct RegMap rhs_regmap = {0};
            struct ValDest rhs = dest_regmap(&rhs_regmap);
            if (compile_expr(p, cond_op->rhs, &rhs, bb)) return 1;

            const char* const lhs_reg = parser_prepare_src_reg(p, &lhs);
            if (!lhs_reg) return 1;
            const char* const rhs_reg = parser_prepare_src_reg(p, &rhs);
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
    const char* const lhs_reg = parser_prepare_src_reg(p, &lhs);
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

static int compile_stmt_expr(Parser* p, struct Expr* expr, struct CompoundBlock* bb);
static int compile_if_stmt(Parser* p, struct StmtIf* stmt, struct CompoundBlock* bb)
{
    struct FreeVar else_lbl = free_var_label(p);
    if (compile_conditional_expr(p, stmt->cond, bb, else_lbl.buf, COND_INVERTED)) return 1;
    struct CompoundBlock new_cb;
    cb_init(&new_cb, bb, p);
    if (compile_stmt_expr(p, stmt->if_body, &new_cb)) return 1;
    int has_returned = new_cb.has_returned;
    cb_destroy(&new_cb, p);
    if (parser_spill_registers(p)) return 1;
    if (stmt->else_body)
    {
        struct FreeVar endif_lbl = free_var_label(p);
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
    return 0;
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
        if (compile_conditional_expr(p, stmt->cond, bb, break_lbl.buf, COND_INVERTED)) return 1;
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
        case LEX_VOLATILE:
        case LEX_CONST:
        case LEX_REGISTER:
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
            for (size_t i = 0; i < decl->extent; ++i)
            {
                struct Decl* arg_decl = ((struct Decl**)p->expr_seqs.data)[decl->offset + i];
                if (compile_decl_single(p, arg_decl, &new_cb)) return 1;
                if (arg_decl->sym.is_nonreentrant)
                {
                    arg_decl->sym.reg.rename = nrfun_param(p, i, name);
                }
            }
            if (decl->sym.is_nonreentrant)
            {
                p->fn_ret_var = nrfun_param_ret(p, token_str(p, decl->id));
            }
            else
            {
                if (decl->extent > REG_COUNT)
                {
                    return parser_ferror(&decl->id->rc, "error: exceeded maximum argument count (%d)\n", REG_COUNT);
                }
                for (size_t i = 0; i < decl->extent; ++i)
                {
                    struct Decl* arg_decl = ((struct Decl**)p->expr_seqs.data)[decl->offset + i];
                    cg_write_inst_set(&p->cg, arg_decl->sym.reg.rename.buf, PARAM_NAMES_ARR[i]);
                }
                cg_write_push_ret(&p->cg, &p->fn_ret_var);
            }
            if (compile_stmt_expr(p, decl->init, &new_cb)) return 1;
            int has_returned = new_cb.has_returned;
            cb_destroy(&new_cb, p);
            if (!has_returned)
            {
                if (parser_spill_registers(p)) return 1;
                cg_write_return(&p->cg, &p->fn_ret_var);
            }
            p->fn_ret_var = s_freevar_zero;
        }
        else
        {
            tc_expr_unify(p, decl->init, decl->sym.type);
            if (parser_has_errors()) return 1;
            struct ValDest vd = dest_sym(&decl->sym);
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
            struct Type* ty_fn_ret = ((struct TypeFn*)bb->fn_sym->type)->ret;
            if (ty_fn_ret->kind == TYPE_VOID)
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
                tc_expr_unify(p, stmt->expr, ty_fn_ret);
                if (parser_has_errors()) return 1;
                struct RegMap regmap = {
                    .rename = s_freevar_paramreg0,
                };
                struct ValDest dst = dest_regmap(&regmap);
                if (compile_expr(p, stmt->expr, &dst, bb)) return 1;
                parser_deactivate_reg(p, &regmap);
            }
            if (parser_spill_registers(p)) return 1;
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
            return 0;
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
                tc_expr(p, expr);
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
    p->fn_ret_var = s_freevar_zero;
    scope_init(&p->scope);
    scope_init(&p->type_scope);
    cg_init(&p->cg);
    p->free_var_counter = 0;
    p->first_active_reg = NULL;
    memset(p->fn_label_prefix, 0, sizeof(p->fn_label_prefix));

    for (size_t i = 0; i < AST_KIND_END_POOLS; ++i)
    {
        pool_init(&p->ast_pools[i]);
    }
    array_init(&p->expr_seqs);

    pool_init(&p->typeprim_pool);
    pool_init(&p->typefn_pool);
    pool_init(&p->typeprim_pool);
    pool_init(&p->typeptr_pool);
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

    pool_destroy(&p->typeprim_pool);
    pool_destroy(&p->typefn_pool);
    pool_destroy(&p->typeprim_pool);
    pool_destroy(&p->typeptr_pool);
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

struct Elaborator
{
    Parser* p;
    struct Array callees_seqs;
};
static void elaborator_destroy(struct Elaborator* elab) { array_destroy(&elab->callees_seqs); }

static int elaborate_local_decl(struct Elaborator* elab, struct Decl* decl)
{
    if (!decl->is_function || !decl->init) return 0;

    int calls_non_builtins = 0;

    struct Array stk = {0};
    array_push_ptr(&stk, decl->init);
    while (stk.sz)
    {
        void* top = array_pop_ptr(&stk);
        struct Expr* top_expr = top;
        switch (top_expr->kind)
        {
            case STMT_NONE:
            case STMT_BREAK:
            case STMT_CONTINUE:
            case STMT_GOTO:
            case STMT_LABEL:
            case EXPR_LIT:
            case EXPR_SYM: break;
            case STMT_RETURN:
            {
                struct StmtReturn* stmt = top;
                array_push_ptr(&stk, stmt->expr);
                break;
            }
            case STMT_IF:
            {
                struct StmtIf* stmt = top;
                if (stmt->else_body) array_push_ptr(&stk, stmt->else_body);
                array_push_ptr(&stk, stmt->if_body);
                array_push_ptr(&stk, stmt->cond);
                break;
            }
            case EXPR_CALL:
            {
                struct ExprCall* expr = top;
                if (!is_builtin_fn_expr(expr->fn))
                {
                    calls_non_builtins = 1;
                }
                array_push_ptr(&stk, expr->fn);
                array_push(&stk,
                           elab->p->expr_seqs.data + expr->offset * sizeof(struct Expr*),
                           expr->extent * sizeof(struct Expr*));
                break;
            }
            case EXPR_OP:
            {
                struct ExprOp* e = top;
                if (e->lhs) array_push_ptr(&stk, e->lhs);
                if (e->rhs) array_push_ptr(&stk, e->rhs);
                break;
            }
            case STMT_LOOP:
            {
                struct StmtLoop* e = top;
                if (e->body) array_push_ptr(&stk, e->body);
                if (e->advance) array_push_ptr(&stk, e->advance);
                if (e->cond) array_push_ptr(&stk, e->cond);
                if (e->init) array_push_ptr(&stk, e->init);
                break;
            }
            case AST_DECL:
            {
                struct Decl* d = top;
                if (d->is_function) break;
                if (d->init) array_push_ptr(&stk, d->init);
                break;
            }
            case STMT_DECLS:
            {
                struct StmtDecls* stmt = top;
                array_push(&stk,
                           elab->p->expr_seqs.data + stmt->offset * sizeof(struct Expr*),
                           stmt->extent * sizeof(struct Expr*));
                break;
            }
            case STMT_BLOCK:
            {
                struct StmtBlock* stmt = top;
                array_push(&stk,
                           elab->p->expr_seqs.data + stmt->offset * sizeof(struct Expr*),
                           stmt->extent * sizeof(struct Expr*));
                break;
            }
            default: fprintf(stderr, "unknown ast kind: %d\n", top_expr->kind); abort();
        }
    }

    if (!calls_non_builtins || decl->attr.is_nonreentrant)
    {
        decl->sym.is_nonreentrant = 1;
    }
    array_destroy(&stk);
    return 0;
}

static int elaborate_declstmts(struct Elaborator* elab, struct Expr** declstmts, size_t count)
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
        if (elaborate_declstmts(&elab, arr_exprs.data, arr_exprs.sz / sizeof(struct Expr*))) return 1;

        struct CompoundBlock new_cb;
        cb_init(&new_cb, NULL, p);
        for (size_t i = 0; i < arr_exprs.sz / sizeof(struct Expr*); ++i)
        {
            if (compile_stmt_expr(p, ((struct Expr**)arr_exprs.data)[i], &new_cb)) return 1;
        }
        array_destroy(&arr_exprs);

        cg_emit(&p->cg);
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
