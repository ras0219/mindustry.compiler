#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexstate.h"
#include "parsestate.h"
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
struct Symbol
{
    char incomplete : 1;
    char is_function : 1;
    char is_const : 1;
    char is_nonreentrant : 1;
    char is_dirty : 1;
    char is_spilled : 1;
    int arg_count;
    int stack_addr;
    const struct Token* token;
    const char* intrinsic_asm_str;

    // register mapping
    struct FreeVar rename;
    struct Symbol* next_active_sym;
    struct Symbol** prev_active_sym;
};

struct ValDest
{
    union
    {
        const char* reg;
        struct Symbol* sym;
    };
    int is_const : 1;
    int is_void : 1;
    int is_sym : 1;
};

static const struct ValDest s_any_destination = {
    .reg = NULL,
    .is_const = 0,
    .is_void = 0,
    .is_sym = 0,
};

static const struct ValDest s_void_destination = {
    .reg = NULL,
    .is_const = 0,
    .is_void = 1,
    .is_sym = 0,
};
static struct ValDest dest_reg(const char* ch)
{
    struct ValDest ret = {
        .reg = ch,
        .is_const = 0,
        .is_void = 0,
        .is_sym = 0,
    };
    return ret;
}
static struct ValDest dest_sym(struct Symbol* sym)
{
    struct ValDest ret = {
        .sym = sym,
        .is_const = sym->is_const,
        .is_void = 0,
        .is_sym = 1,
    };
    return ret;
}
static struct ValDest dest_const(const char* ch)
{
    struct ValDest ret = {
        .reg = ch,
        .is_const = 1,
        .is_void = 0,
        .is_sym = 0,
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

static void symbol_destroy(struct Symbol* sym) { }
static void symbol_free(struct Symbol* sym)
{
    if (sym->prev_active_sym) *sym->prev_active_sym = sym->next_active_sym;
    free(sym);
}
static void symbol_init(struct Symbol* ret)
{
    ret->rename = s_freevar_zero;
    ret->incomplete = 0;
    ret->is_function = 0;
    ret->is_nonreentrant = 0;
    ret->is_const = 0;
    ret->is_dirty = 0;
    ret->arg_count = 0;
    ret->stack_addr = 0;
    ret->token = NULL;
    ret->intrinsic_asm_str = NULL;
    ret->next_active_sym = NULL;
    ret->prev_active_sym = NULL;
}
static int symbol_is_equivalent_redecl(struct Symbol* orig, struct Symbol* redecl) { return 1; }
static struct Symbol* symbol_alloc()
{
    struct Symbol* ret = (struct Symbol*)malloc(sizeof(struct Symbol));
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
    struct Symbol* s = p->first_active_sym;
    p->first_active_sym = NULL;
    while (s)
    {
        struct Symbol* const next = s->next_active_sym;
        if (s->is_dirty)
        {
            if (cg_store(&p->cg, s->stack_addr, s->rename.buf, &s_unknown_rc)) return 1;
            s->is_dirty = 0;
        }
        s->prev_active_sym = NULL;
        s->next_active_sym = NULL;
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
static void scope_shrink_free_syms(struct Scope* s, size_t sz)
{
    const size_t cur_sz = scope_size(s);
    if (sz < cur_sz)
    {
        const struct Binding* data = scope_data(s);
        for (size_t i = sz; i < cur_sz; ++i)
            symbol_free(data[i].sym);
        s->strings.sz = data[sz].ident_offset;
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

static struct Token* compile_expr(
    Parser* p, struct Token* cur_tok, struct ValDest* dst, struct CompoundBlock* bb, int precedence);

// requires !sym->prev_active_sym
static void parser_push_active_sym(struct Parser* p, struct Symbol* sym)
{
    if (sym->next_active_sym = p->first_active_sym)
    {
        p->first_active_sym->prev_active_sym = &sym->next_active_sym;
    }
    p->first_active_sym = sym;
    sym->prev_active_sym = &p->first_active_sym;
}
static int parser_ensure_loaded_sym(struct Parser* p, struct Symbol* sym)
{
    if (!sym->prev_active_sym && !sym->is_nonreentrant)
    {
        parser_push_active_sym(p, sym);

        if (cg_load(&p->cg, sym->stack_addr, sym->rename.buf, &sym->token->rc)) return 1;
    }
    return 0;
}
static void parser_ensure_dirty_sym(struct Parser* p, struct Symbol* sym)
{
    if (!sym->prev_active_sym && !sym->is_nonreentrant)
    {
        parser_push_active_sym(p, sym);
    }
    sym->is_dirty = 1;
}
static char* my_strdup(const char* s) { return strcpy(malloc(strlen(s) + 1), s); }

static const char* parser_prepare_dst_reg(Parser* p, struct ValDest* dst)
{
    if (dst->is_const)
    {
        fprintf(stderr, "internal compiler error -- attempting to write to constant\n");
        abort();
    }
    if (dst->is_sym)
    {
        parser_ensure_dirty_sym(p, dst->sym);
        return dst->sym->rename.buf;
    }
    else if (!dst->reg)
    {
        struct FreeVar fv_ret = free_var(p);
        const char* const reg = my_strdup(fv_ret.buf);
        array_push(&p->strings_to_free, &reg, sizeof(reg));
        *dst = dest_reg(reg);
    }
    return dst->reg;
}
static const char* parser_prepare_src_reg(Parser* p, struct ValDest* dst)
{
    if (dst->is_void)
    {
        fprintf(stderr, "internal compiler error -- attempting to read from void\n");
        abort();
    }
    if (dst->is_sym)
    {
        if (parser_ensure_loaded_sym(p, dst->sym)) return NULL;
        return dst->sym->rename.buf;
    }
    else if (!dst->reg)
    {
        fprintf(stderr, "internal compiler error -- attempting to read from void\n");
        abort();
    }
    else
    {
        return dst->reg;
    }
}

static int parser_assign_dsts(struct Parser* p, struct ValDest* dst, struct ValDest* src)
{
    struct CodeGen* const cg = &p->cg;
    if (dst->is_void)
    {
        return 0;
    }
    if (dst->is_const)
    {
        fprintf(stderr, "internal compiler error -- attempting to write to constant\n");
        abort();
    }
    if (dst->is_sym && src->is_sym && dst->sym == src->sym) return 0;
    if (!dst->is_sym && !dst->reg)
    {
        *dst = *src;
        return 0;
    }

    const char* src_reg = parser_prepare_src_reg(p, src);
    if (!src_reg) return 1;
    const char* dst_reg = parser_prepare_dst_reg(p, dst);
    if (!dst_reg) return 1;
    cg_write_inst_set(cg, dst_reg, src_reg);
    return 0;
}

struct ParamList
{
    struct ValDest param;
    int idx;
    struct ParamList* prev;
};

struct ParamList make_plist(struct ParamList* prev, struct ValDest param)
{
    struct ParamList ret = {
        .param = param,
        .idx = prev ? prev->idx + 1 : 0,
        .prev = prev,
    };
    return ret;
}

static struct Token* compile_call_expr(Parser* p,
                                       struct Token* cur_tok,
                                       struct ValDest* dst,
                                       struct Symbol* fn,
                                       struct ParamList* params,
                                       struct CompoundBlock* bb);

static struct ParamList* paramlist_nth(struct ParamList* params, int n)
{
    if (!params || n > params->idx) return NULL;
    do
    {
        if (!params) return NULL;
        if (n == params->idx) return params;
        params = params->prev;
    } while (1);
}

static struct Token* compile_call_expr_sym(Parser* p,
                                           struct Token* cur_tok,
                                           struct ValDest* dst,
                                           struct Symbol* fn,
                                           struct ParamList* params,
                                           struct CompoundBlock* bb)
{
    if (cur_tok->type == LEX_SYMBOL)
    {
        char ch = token_str(p, cur_tok)[0];
        if (ch == ')')
        {
            if ((params ? params->idx + 1 : 0) < fn->arg_count)
            {
                return parser_ferror(&cur_tok->rc, "error: function requires %d arguments\n", fn->arg_count), NULL;
            }

            if (fn->intrinsic_asm_str)
            {
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
                            const char* const reg = parser_prepare_dst_reg(p, dst);
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
                            struct ParamList* pl;
                            if (*s == '0')
                                pl = paramlist_nth(params, 0);
                            else if (*s == '1')
                                pl = paramlist_nth(params, 1);
                            else if (*s == '2')
                                pl = paramlist_nth(params, 2);
                            else if (*s == '3')
                                pl = paramlist_nth(params, 3);
                            else if (*s == '4')
                                pl = paramlist_nth(params, 4);
                            else if (*s == '5')
                                pl = paramlist_nth(params, 5);
                            else if (*s == '6')
                                pl = paramlist_nth(params, 6);
                            else
                            {
                                return parser_ferror(&cur_tok->rc,
                                                     "error: invalid intrinsic string: invalid format specifier\n"),
                                       NULL;
                            }
                            if (!pl)
                            {
                                return parser_ferror(&cur_tok->rc,
                                                     "error: invalid intrinsic string: wrong arguments\n"),
                                       NULL;
                            }
                            const char* const reg = parser_prepare_src_reg(p, &pl->param);
                            if (!reg) return NULL;
                            size_t len = strlen(reg);
                            if (i + len > 127)
                            {
                                fprintf(stderr, "resource exceeded\n");
                                abort();
                            }
                            memcpy(buf + i, reg, len);
                            i += len;
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
                if (!dst->is_sym && !dst->reg) *dst = s_void_destination;
            }
            else
            {
                if (!bb->fn_sym)
                {
                    return parser_ferror(&cur_tok->rc, "error: not in function\n"), NULL;
                }
                if (!bb->fn_sym->is_nonreentrant)
                {
                    char buf[16];
                    snprintf(buf, sizeof(buf), "%d", bb->frame_size);
                    cg_write_inst_op(&p->cg, "+", "__stk__", "__ebp__", buf);
                }
                if (fn->is_nonreentrant)
                {
                    const char* const name = token_str(p, fn->token);
                    struct FreeVar fv;
                    while (params)
                    {
                        fv = nrfun_param(p, params->idx, name);
                        struct ValDest fv_dst = dest_reg(fv.buf);
                        if (parser_assign_dsts(p, &fv_dst, &params->param)) return NULL;
                        params = params->prev;
                    }
                    if (parser_spill_registers(p)) return NULL;
                    const struct FreeVar fv_ret = nrfun_param_ret(p, name);
                    cg_write_inst_op(&p->cg, "+", fv_ret.buf, "1", "@counter");
                }
                else
                {
                    while (params)
                    {
                        if (params->idx >= REG_COUNT)
                        {
                            return parser_ferror(
                                       &cur_tok->rc, "error: exceeded maximum call arguments (%d)\n", REG_COUNT),
                                   NULL;
                        }
                        struct ValDest pdst = dest_reg(PARAM_NAMES_ARR[params->idx]);
                        if (parser_assign_dsts(p, &pdst, &params->param)) return NULL;
                        params = params->prev;
                    }
                    if (parser_spill_registers(p)) return NULL;
                    cg_write_inst(&p->cg, "op add ret 1 @counter");
                }
                cg_write_inst_jump(&p->cg, fn->rename.buf);
                if (!dst->is_void)
                {
                    if (!parser_prepare_dst_reg(p, dst)) return NULL;
                    struct ValDest ret_dst = dest_reg(PARAM_NAMES_ARR[0]);
                    if (parser_assign_dsts(p, dst, &ret_dst)) return NULL;
                }
            }
            return cur_tok + 1;
        }
        else if (ch == ',')
        {
            return compile_call_expr(p, cur_tok + 1, dst, fn, params, bb);
        }
    }
    return parser_ferror(&cur_tok->rc, "error: expected ',' or ')'\n"), NULL;
}

static struct Token* compile_call_expr(Parser* p,
                                       struct Token* cur_tok,
                                       struct ValDest* dst,
                                       struct Symbol* fn,
                                       struct ParamList* params,
                                       struct CompoundBlock* bb)
{
    struct ValDest new_dst = s_any_destination;
    if (cur_tok = compile_expr(p, cur_tok, &new_dst, bb, PRECEDENCE_ASSIGN))
    {
        struct ParamList plist = make_plist(params, new_dst);
        if (fn->arg_count <= plist.idx)
        {
            return parser_ferror(&cur_tok->rc, "error: function only accepts '%d' arguments\n", fn->arg_count), NULL;
        }
        cur_tok = compile_call_expr_sym(p, cur_tok, dst, fn, &plist, bb);
    }
    return cur_tok;
}

// https://en.cppreference.com/w/cpp/language/operator_precedence
// Precedence level 2
static struct Token* compile_expr_atom(Parser* p, struct Token* cur_tok, struct ValDest* dst, struct CompoundBlock* bb)
{
    switch (cur_tok->type)
    {
        case LEX_IDENT:
        {
            struct Token* lhs = cur_tok++;
            struct Binding* const lhs_bind = scope_find(&p->scope, token_str(p, lhs));
            if (!lhs_bind)
            {
                return parser_ferror(&lhs->rc, "error: '%s' undeclared\n", token_str(p, lhs)), NULL;
            }
            struct ValDest lhs_dst = dest_sym(lhs_bind->sym);
            if (cur_tok->type == LEX_SYMBOL)
            {
                struct Token* tok_op = cur_tok;
                char* op = token_str(p, tok_op);

                if (op[0] == '(')
                {
                    if (!lhs_bind->sym->is_function)
                    {
                        return parser_ferror(&cur_tok->rc, "error: '%s' is not a function\n", token_str(p, lhs)), NULL;
                    }
                    struct Token* tk = ++cur_tok;
                    if (tk->type == LEX_SYMBOL && token_str(p, tk)[0] == ')')
                    {
                        return compile_call_expr_sym(p, cur_tok, dst, lhs_bind->sym, NULL, bb);
                    }
                    else
                    {
                        return compile_call_expr(p, cur_tok, dst, lhs_bind->sym, NULL, bb);
                    }
                }
            }
            if (parser_assign_dsts(p, dst, &lhs_dst)) return NULL;
            return cur_tok;
        }
        case LEX_NUMBER:
        {
            struct ValDest src = dest_const(token_str(p, cur_tok));
            if (parser_assign_dsts(p, dst, &src)) return NULL;
            return cur_tok + 1;
        }
        case LEX_STRING:
        {
            const char* s = token_str(p, cur_tok);
            const size_t slen = strlen(s);
            if (memchr(s, '"', slen))
            {
                return parser_ferror(&cur_tok->rc, "error: string constants cannot represent \"\n"), NULL;
            }
            char* heap_s = (char*)malloc(slen + 3);
            array_push(&p->strings_to_free, &heap_s, sizeof(heap_s));
            heap_s[0] = '"';
            strcpy(heap_s + 1, s);
            heap_s[slen + 1] = '"';
            heap_s[slen + 2] = '\0';
            struct ValDest src = dest_const(heap_s);
            if (parser_assign_dsts(p, dst, &src)) return NULL;
            return cur_tok + 1;
        }
        default: return parser_ferror(&cur_tok->rc, "error: expected expression\n"), NULL;
    }
}

static struct Token* compile_expr_unary_atom(Parser* p,
                                             struct Token* cur_tok,
                                             struct ValDest* dst,
                                             struct CompoundBlock* bb)
{
    if (cur_tok->type == LEX_SYMBOL)
    {
        char* op = token_str(p, cur_tok);
        if (op[0] == '!')
        {
            struct ValDest lhs_dst = s_any_destination;
            if (!(cur_tok = compile_expr_unary_atom(p, cur_tok + 1, &lhs_dst, bb))) return NULL;
            const char* const srcreg = parser_prepare_src_reg(p, &lhs_dst);
            if (!srcreg) return NULL;
            const char* const dstreg = parser_prepare_dst_reg(p, dst);
            if (!dstreg) return NULL;
            cg_write_inst_op(&p->cg, "!=", dstreg, srcreg, "true");
            return cur_tok;
        }
    }

    return compile_expr_atom(p, cur_tok, dst, bb);
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

static struct Token* compile_expr_continue(Parser* p,
                                           struct Token* cur_tok,
                                           struct ValDest* dst,
                                           struct ValDest* lhs_dst,
                                           struct CompoundBlock* bb,
                                           enum Precedence precedence)
{
    if (cur_tok->type == LEX_SYMBOL)
    {
        struct Token* tok_op = cur_tok;
        char* op = token_str(p, tok_op);
        enum Precedence op_prec = op_precedence(op);
        if (op_prec)
            if (op_precedence_assoc_right(op_prec))
            {
                if (op[0] == '=' && op[1] == '\0' && precedence <= PRECEDENCE_ASSIGN)
                {
                    if (lhs_dst->is_const)
                    {
                        return parser_ferror(&cur_tok->rc, "error: illegal assignment to constant\n"), NULL;
                    }
                    cur_tok = compile_expr(p, cur_tok + 1, lhs_dst, bb, PRECEDENCE_ASSIGN);
                    if (!cur_tok) return NULL;
                    if (parser_assign_dsts(p, dst, lhs_dst)) return NULL;
                }
                return cur_tok;
            }
            else if (precedence < op_prec)
            {
                struct ValDest rhs = s_any_destination;
                if (!(cur_tok = compile_expr(p, cur_tok + 1, &rhs, bb, op_prec))) return NULL;
                const char* r2 = parser_prepare_src_reg(p, lhs_dst);
                if (!r2) return NULL;
                const char* r3 = parser_prepare_src_reg(p, &rhs);
                if (!r3) return NULL;
                if (token_is_sym(p, cur_tok, ';'))
                {
                    // hack to avoid emitting a temporary
                    const char* r1 = parser_prepare_dst_reg(p, dst);
                    if (!r1) return NULL;
                    cg_write_inst_op(&p->cg, op, r1, r2, r3);
                    return cur_tok;
                }
                struct ValDest res = s_any_destination;
                const char* r1 = parser_prepare_dst_reg(p, &res);
                if (!r1) return NULL;
                cg_write_inst_op(&p->cg, op, r1, r2, r3);
                return compile_expr_continue(p, cur_tok, dst, &res, bb, precedence);
            }
    }
    if (parser_assign_dsts(p, dst, lhs_dst)) return NULL;
    return cur_tok;
}
static struct Token* compile_expr(
    Parser* p, struct Token* cur_tok, struct ValDest* dst, struct CompoundBlock* bb, int precedence)
{
    struct ValDest lhs_dst = s_any_destination;
    if (!(cur_tok = compile_expr_unary_atom(p, cur_tok, &lhs_dst, bb))) return NULL;

    return compile_expr_continue(p, cur_tok, dst, &lhs_dst, bb, precedence);
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

struct DeclSpecs
{
    int is_const : 1;
    int is_volatile : 1;
    int is_register : 1;
};
static struct Token* compile_decl(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb);
static struct Token* compile_declspecs(Parser* p, struct Token* cur_tok, struct DeclSpecs* specs);
static struct Token* compile_declarator(Parser* p,
                                        struct Token* cur_tok,
                                        struct CompoundBlock* bb,
                                        struct Symbol** p_sym)
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
    const char* name = token_str(p, id);
    struct Symbol* const sym = symbol_alloc();
    sym->token = id;
    if (p_sym) *p_sym = sym;
    if (attr.symname)
    {
        if (strlen(attr.symname) >= sizeof(sym->rename.buf))
        {
            return parser_ferror(&cur_tok->rc,
                                 "error: symbol names have a maximum length of %lu characters\n",
                                 sizeof(sym->rename.buf) - 1),
                   NULL;
        }
        strcpy(sym->rename.buf, attr.symname);
    }
    sym->intrinsic_asm_str = attr.asmstr;
    sym->is_nonreentrant = attr.is_nonreentrant;

    const char ch = cur_tok->type == LEX_SYMBOL ? token_str(p, cur_tok)[0] : '\0';
    if (ch == '(')
    {
        struct Binding* prev_sym = scope_find(&p->scope, name);
        scope_insert(&p->scope, name, sym);

        if (sym->rename.buf[0])
        {
            if (sym->rename.buf[0] != '$' || !sym->rename.buf[1] || sym->rename.buf[strlen(sym->rename.buf) - 1] != '$')
            {
                return parser_ferror(&cur_tok->rc,
                                     "error: external symbol names must be of the form '$foo$' (was '%s')\n",
                                     sym->rename.buf),
                       NULL;
            }
        }
        else
        {
            sym->rename = extern_var_from(p, name);
        }
        sym->is_function = 1;
        ++cur_tok;
        if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == ')')
        {
            ++cur_tok;
        }
        else
        {
            struct DeclSpecs specs = {0};
            while (cur_tok = compile_declspecs(p, cur_tok, &specs))
            {
                struct Symbol* arg_sym;
                if (!(cur_tok = compile_declarator(p, cur_tok, bb, &arg_sym)))
                {
                    return NULL;
                }
                if (sym->is_nonreentrant)
                {
                    arg_sym->rename = nrfun_param(p, sym->arg_count, name);
                }
                else
                {
                    arg_sym->rename = free_var(p);
                }
                ++sym->arg_count;
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
        }
        if (prev_sym)
        {
            // ensure symbols match
            if (!symbol_is_equivalent_redecl(prev_sym->sym, sym))
            {
                return parser_ferror(&cur_tok->rc, "error: declaration doesn't match previous\n"), NULL;
            }
        }
    }
    else
    {
        // not a function
        sym->is_nonreentrant |= !bb->fn_sym || bb->fn_sym->is_nonreentrant;
        if (!sym->rename.buf[0]) sym->rename = free_var_from(p, name);
        scope_insert(&p->scope, name, sym);
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
    scope_shrink_free_syms(&p->scope, scope_sz);
    return cur_tok;
}
static struct Token* compile_return_stmt(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb) { }
static struct Token* compile_decl(Parser* p, struct Token* cur_tok, struct CompoundBlock* bb)
{
    struct DeclSpecs specs = {0};
    if (cur_tok = compile_declspecs(p, cur_tok, &specs))
    {
        size_t scope_sz;
        struct Symbol* sym = NULL;
        while (scope_sz = scope_size(&p->scope), cur_tok = compile_declarator(p, cur_tok, bb, &sym))
        {
            scope_data(&p->scope)[scope_sz].sym->stack_addr = bb->frame_size++;
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
                    cg_mark_label(&p->cg, sym->rename.buf);
                    if (sym->is_nonreentrant)
                    {
                        p->fn_ret_var = nrfun_param_ret(p, token_str(p, sym->token));
                    }
                    else
                    {
                        struct Binding* const sc_data = scope_data(&p->scope);
                        if (scope_sz + sym->arg_count + 1 != scope_size(&p->scope))
                        {
                            fprintf(stderr, "internal compiler error -- too many symbols introduced by decl\n");
                            abort();
                        }
                        if (sym->arg_count > REG_COUNT)
                        {
                            return parser_ferror(&cur_tok->rc, "error: expected semicolon\n"), NULL;
                        }
                        for (size_t i = 0; i < sym->arg_count; ++i)
                        {
                            struct Symbol* arg_sym = sc_data[scope_sz + 1 + i].sym;
                            arg_sym->stack_addr = new_cb.frame_size++;
                            cg_write_inst_set(&p->cg, arg_sym->rename.buf, PARAM_NAMES_ARR[i]);
                        }
                        cg_write_push_ret(&p->cg, &p->fn_ret_var);
                    }
                    cur_tok = compile_compound_stmt(p, cur_tok + 1, &new_cb);
                    if (!new_cb.has_returned)
                    {
                        if (parser_spill_registers(p)) return NULL;
                        cg_write_return(&p->cg, &p->fn_ret_var);
                    }
                    p->fn_ret_var = s_freevar_zero;
                    scope_shrink_free_syms(&p->scope, scope_sz + 1);
                    return cur_tok;
                }
                else
                {
                    // since we aren't defining a function body, remove any parameters from the scope
                    scope_shrink_free_syms(&p->scope, scope_sz + 1);
                    if (ch == '=')
                    {
                        struct ValDest dst = dest_sym(sym);
                        if (!(cur_tok = compile_expr(p, cur_tok + 1, &dst, bb, PRECEDENCE_ASSIGN))) return NULL;
                    }
                    sym->is_const = specs.is_const;
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

    struct ValDest lhs = s_any_destination;
    if (!(cur_tok = compile_expr(p, cur_tok, &lhs, bb, PRECEDENCE_RELATION))) return NULL;
    while (cur_tok->type == LEX_SYMBOL)
    {
        const char* const op = token_str(p, cur_tok);
        enum Precedence pr = op_precedence(op);
        if (pr == PRECEDENCE_EQUALITY || pr == PRECEDENCE_RELATION)
        {
            struct ValDest rhs = s_any_destination;
            if (!(cur_tok = compile_expr(p, cur_tok + 1, &rhs, bb, pr))) return NULL;
            const char* const lhs_reg = parser_prepare_src_reg(p, &lhs);
            if (!lhs_reg) return NULL;
            const char* const rhs_reg = parser_prepare_src_reg(p, &rhs);
            if (!rhs_reg) return NULL;
            if (token_is_sym(p, cur_tok, ')'))
            {
                // Do if-stmt fusion
                if (parser_spill_registers(p)) return NULL;
                cg_write_inst_jump_op(&p->cg, jump_to, inverted ? relation_invert(op) : op, lhs_reg, rhs_reg);
                break;
            }
            else
            {
                struct ValDest tmp = s_any_destination;
                const char* tmp_reg = parser_prepare_dst_reg(p, &tmp);
                if (!tmp_reg) return NULL;
                cg_write_inst_op(&p->cg, op, tmp_reg, lhs_reg, rhs_reg);
                lhs = tmp;
                continue;
            }
        }
        else if (op[0] == ')')
        {
            const char* const lhs_reg = parser_prepare_src_reg(p, &lhs);
            if (!lhs_reg) return NULL;
            if (parser_spill_registers(p)) return NULL;
            cg_write_inst_jump_op(&p->cg, jump_to, inverted ? "!=" : "==", lhs_reg, "true");
            break;
        }
        else
        {
            struct ValDest tmp = s_any_destination;
            if (!(cur_tok = compile_expr_continue(p, cur_tok, &tmp, &lhs, bb, PRECEDENCE_ASSIGN))) return NULL;
            const char* const tmp_reg = parser_prepare_src_reg(p, &tmp);
            if (!tmp_reg) return NULL;
            if (parser_spill_registers(p)) return NULL;
            cg_write_inst_jump_op(&p->cg, jump_to, inverted ? "!=" : "==", tmp_reg, "true");
            break;
        }
    }
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
            struct ValDest dst = dest_reg(PARAM_NAMES_ARR[0]);
            if (cur_tok = compile_expr(p, cur_tok + 1, &dst, bb, PRECEDENCE_COMMA))
            {
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
            else
            {
                struct ValDest dst = s_void_destination;
                if (cur_tok = compile_expr(p, cur_tok, &dst, bb, PRECEDENCE_COMMA))
                {
                    if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == ';')
                    {
                        ++cur_tok;
                    }
                    else
                    {
                        return parser_ferror(&cur_tok->rc, "error: expected semicolon\n"), NULL;
                    }
                }
            }
            return cur_tok;
        }
        case LEX_NUMBER:
        {
            struct ValDest dst = s_void_destination;
            if (cur_tok = compile_expr(p, cur_tok, &dst, bb, PRECEDENCE_COMMA))
            {
                if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == ';')
                {
                    ++cur_tok;
                }
                else
                {
                    return parser_ferror(&cur_tok->rc, "error: expected semicolon\n"), NULL;
                }
            }
            return cur_tok;
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
                scope_shrink_free_syms(&p->scope, scope_sz);
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
    p->first_active_sym = NULL;
    memset(p->fn_label_prefix, 0, sizeof(p->fn_label_prefix));
}
void parser_destroy(Parser* p)
{
    array_destroy(&p->stringpool);
    array_destroy(&p->toks);
    for (size_t i = 0; i < p->strings_to_free.sz / sizeof(char*); ++i)
    {
        free(((char**)p->strings_to_free.data)[i]);
    }
    array_destroy(&p->strings_to_free);
    scope_destroy(&p->scope);
    scope_destroy(&p->type_scope);
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
