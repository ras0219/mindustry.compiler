#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexstate.h"
#include "parsestate.h"
#include "tok.h"

struct ValDest
{
    const char* dst_name;
};

struct Token
{
    struct RowCol rc;
    LexerState type;
    ptrdiff_t sp_offset;
};

static struct ValDest dest_reg(const char* ch)
{
    struct ValDest ret = {
        .dst_name = ch,
    };
    return ret;
}

static char* token_str(Parser* p, const struct Token* tk) { return (char*)p->stringpool.data + tk->sp_offset; }

#define REG_COUNT 6

static const char* PARAM_NAMES_ARR[REG_COUNT] = {"eax", "ebx", "ecx", "edx", "eex", "efx"};

struct FreeVar
{
    char buf[24];
};
static struct FreeVar s_freevar_zero = {0};
static struct FreeVar free_var(Parser* p)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "_%d", p->free_var_counter++);
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

struct Symbol
{
    struct FreeVar rename;
    char incomplete : 1;
    char is_function : 1;
    int arg_count;
    const char* intrinsic_asm_str;
};

static void symbol_destroy(struct Symbol* sym) { }
static void symbol_free(struct Symbol* sym) { free(sym); }
static void symbol_init(struct Symbol* ret)
{
    ret->rename = s_freevar_zero;
    ret->incomplete = 0;
    ret->is_function = 0;
    ret->arg_count = 0;
    ret->intrinsic_asm_str = NULL;
}
static struct Symbol* symbol_alloc()
{
    struct Symbol* ret = (struct Symbol*)malloc(sizeof(struct Symbol));
    symbol_init(ret);
    return ret;
}
struct RegisterInfo
{
    struct RegisterInfo* prev;
    struct FreeVar renamed;
};
struct BasicBlock
{
    struct RegisterInfo* reg[REG_COUNT];
    size_t scope_sz;
};

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

static void init_basic_block(struct Parser* p, struct BasicBlock* b)
{
    memset(&b->reg, 0, sizeof(b->reg));
    b->scope_sz = scope_size(&p->scope);
}
static void bb_acquire_reg(Parser* p, struct BasicBlock* b, struct RegisterInfo* info, int reg)
{
    info->prev = b->reg[reg];
    if (info->prev && !info->prev->renamed.buf[0])
    {
        info->prev->renamed = free_var(p);
        cg_write_inst_set(&p->cg, info->prev->renamed.buf, PARAM_NAMES_ARR[reg]);
    }
}
static void bb_restore_reg(struct CodeGen* cg, struct BasicBlock* b, int reg)
{
    if (b->reg[reg]->renamed.buf[0])
    {
        cg_write_inst_set(cg, PARAM_NAMES_ARR[reg], b->reg[reg]->renamed.buf);
    }
}
static void bb_close(struct Parser* p, struct BasicBlock* b)
{
    struct Binding* const data = scope_data(&p->scope);
    const size_t sz = scope_size(&p->scope);
    for (size_t i = b->scope_sz; i < sz; ++i)
    {
        symbol_free(data[i].sym);
    }
    scope_shrink(&p->scope, b->scope_sz);
}

static struct Token* compile_expr(Parser* p, struct Token* cur_tok, struct ValDest* dst, struct BasicBlock* bb);

static void mov_or_assign_dst(struct CodeGen* cg, struct ValDest* dst, const char* val)
{
    if (dst->dst_name)
    {
        cg_write_inst_set(cg, dst->dst_name, val);
    }
    else
    {
        dst->dst_name = val;
    }
}

struct ParamList
{
    const struct FreeVar param;
    int idx;
    struct ParamList* prev;
};

struct ParamList make_plist(struct ParamList* prev, struct FreeVar param)
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
                                       struct BasicBlock* bb);

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

static char* my_strdup(const char* s) { return strcpy(malloc(strlen(s) + 1), s); }

static void dst_fill_name(Parser* p, struct ValDest* dst)
{
    if (!dst->dst_name)
    {
        struct FreeVar fv_ret = free_var(p);
        dst->dst_name = my_strdup(fv_ret.buf);
        array_push(&p->strings_to_free, dst->dst_name, sizeof(dst->dst_name));
    }
}

static struct Token* compile_call_expr_sym(Parser* p,
                                           struct Token* cur_tok,
                                           struct ValDest* dst,
                                           struct Symbol* fn,
                                           struct ParamList* params,
                                           struct BasicBlock* bb)
{
    if (cur_tok->type == LEX_SYMBOL)
    {
        char ch = token_str(p, cur_tok)[0];
        if (ch == ')')
        {
            if ((params ? params->idx + 1 : 0) < fn->arg_count)
            {
                snprintf(s_error_buffer,
                         sizeof(s_error_buffer),
                         "%d:%d: error: function requires %d arguments\n",
                         cur_tok->rc.row,
                         cur_tok->rc.col,
                         fn->arg_count);
                return NULL;
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
                            dst_fill_name(p, dst);
                            size_t len = strlen(dst->dst_name);
                            if (i + len > 127)
                            {
                                fprintf(stderr, "resource exceeded\n");
                                abort();
                            }
                            memcpy(buf + i, dst->dst_name, len);
                            i += len;
                        }
                        else
                        {
                            struct ParamList* p;
                            if (*s == '0')
                                p = paramlist_nth(params, 0);
                            else if (*s == '1')
                                p = paramlist_nth(params, 1);
                            else if (*s == '2')
                                p = paramlist_nth(params, 2);
                            else if (*s == '3')
                                p = paramlist_nth(params, 3);
                            else if (*s == '4')
                                p = paramlist_nth(params, 4);
                            else if (*s == '5')
                                p = paramlist_nth(params, 5);
                            else if (*s == '6')
                                p = paramlist_nth(params, 6);
                            else
                            {
                                snprintf(s_error_buffer,
                                         sizeof(s_error_buffer),
                                         "%d:%d: error: invalid intrinsic string: invalid format specifier\n",
                                         cur_tok->rc.row,
                                         cur_tok->rc.col);
                                return NULL;
                            }
                            if (!p)
                            {
                                snprintf(s_error_buffer,
                                         sizeof(s_error_buffer),
                                         "%d:%d: error: invalid intrinsic string: wrong arguments\n",
                                         cur_tok->rc.row,
                                         cur_tok->rc.col);
                                return NULL;
                            }
                            size_t len = strlen(p->param.buf);
                            if (i + len > 127)
                            {
                                fprintf(stderr, "resource exceeded\n");
                                abort();
                            }
                            memcpy(buf + i, p->param.buf, len);
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
                if (!dst->dst_name) dst->dst_name = "null";
            }
            else
            {
                while (params)
                {
                    if (params->idx >= REG_COUNT)
                    {
                        snprintf(s_error_buffer,
                                 sizeof(s_error_buffer),
                                 "%d:%d: error: exceeded maximum call arguments (%d)\n",
                                 cur_tok->rc.row,
                                 cur_tok->rc.col,
                                 REG_COUNT);
                        return NULL;
                    }
                    cg_write_inst_set(&p->cg, PARAM_NAMES_ARR[params->idx], params->param.buf);
                    params = params->prev;
                }

                const struct FreeVar fv_ret = free_var(p);
                cg_write_inst(&p->cg, "op add ret 1 @counter");
                cg_write_inst_set(&p->cg, "@counter", fn->rename.buf);
                mov_or_assign_dst(&p->cg, dst, PARAM_NAMES_ARR[0]);
            }
            return cur_tok + 1;
        }
        else if (ch == ',')
        {
            return compile_call_expr(p, cur_tok + 1, dst, fn, params, bb);
        }
    }
    snprintf(s_error_buffer,
             sizeof(s_error_buffer),
             "%d:%d: error: expected ',' or ')'\n",
             cur_tok->rc.row,
             cur_tok->rc.col);
    return NULL;
}

static struct Token* compile_call_expr(Parser* p,
                                       struct Token* cur_tok,
                                       struct ValDest* dst,
                                       struct Symbol* fn,
                                       struct ParamList* params,
                                       struct BasicBlock* bb)
{
    struct ParamList plist = make_plist(params, free_var(p));
    if (fn->arg_count <= plist.idx)
    {
        snprintf(s_error_buffer,
                 sizeof(s_error_buffer),
                 "%d:%d: error: function only accepts '%d' arguments\n",
                 cur_tok->rc.row,
                 cur_tok->rc.col,
                 fn->arg_count);
        return NULL;
    }
    struct ValDest new_dst = dest_reg(plist.param.buf);
    if (cur_tok = compile_expr(p, cur_tok, &new_dst, bb))
        cur_tok = compile_call_expr_sym(p, cur_tok, dst, fn, &plist, bb);
    return cur_tok;
}

static const struct ValDest s_any_destination = {.dst_name = NULL};

static struct Token* compile_expr(Parser* p, struct Token* cur_tok, struct ValDest* dst, struct BasicBlock* bb)
{
    switch (cur_tok->type)
    {
        case LEX_IDENT:
        {
            struct Token* lhs = cur_tok++;
            struct Binding* const lhs_bind = scope_find(&p->scope, token_str(p, lhs));
            if (!lhs_bind)
            {
                snprintf(s_error_buffer,
                         sizeof(s_error_buffer),
                         "%d:%d: error: '%s' undeclared\n",
                         cur_tok->rc.row,
                         cur_tok->rc.col,
                         token_str(p, lhs));
                return NULL;
            }
            if (cur_tok->type == LEX_SYMBOL)
            {
                struct Token* tok_op = cur_tok;
                char* op = token_str(p, tok_op);
                if (op[0] == '=')
                {
                    struct ValDest new_dst = dest_reg(lhs_bind->sym->rename.buf);
                    cur_tok = compile_expr(p, cur_tok + 1, &new_dst, bb);
                    if (!cur_tok) return NULL;
                    mov_or_assign_dst(&p->cg, dst, new_dst.dst_name);
                }
                else if (op[0] == '<')
                {
                    dst_fill_name(p, dst);
                    struct ValDest rhs = s_any_destination;
                    if (cur_tok = compile_expr(p, cur_tok + 1, &rhs, bb))
                    {
                        char buf[128];
                        snprintf(buf,
                                 sizeof(buf),
                                 "op lessThan %s %s %s",
                                 dst->dst_name,
                                 lhs_bind->sym->rename.buf,
                                 rhs.dst_name);
                        cg_write_inst(&p->cg, buf);
                    }
                }
                else if (op[0] == '(')
                {
                    if (!lhs_bind->sym->is_function)
                    {
                        snprintf(s_error_buffer,
                                 sizeof(s_error_buffer),
                                 "%d:%d: error: '%s' is not a function\n",
                                 cur_tok->rc.row,
                                 cur_tok->rc.col,
                                 token_str(p, lhs));
                        return NULL;
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
                else
                {
                    mov_or_assign_dst(&p->cg, dst, lhs_bind->sym->rename.buf);
                }
            }
            return cur_tok;
        }
        case LEX_NUMBER:
        {
            mov_or_assign_dst(&p->cg, dst, token_str(p, cur_tok));
            return cur_tok + 1;
        }
        default:
            snprintf(s_error_buffer,
                     sizeof(s_error_buffer),
                     "%d:%d: error: expected expression\n",
                     cur_tok->rc.row,
                     cur_tok->rc.col);
            return NULL;
    }
}
struct Attribute
{
    const char* symname;
    const char* asmstr;
};
static struct Token* compile_attribute_plist(Parser* p, struct Token* cur_tok, struct Attribute* attr)
{
    if (cur_tok->type != LEX_SYMBOL || token_str(p, cur_tok)[0] != '(') goto error;
    ++cur_tok;
    if (cur_tok->type != LEX_SYMBOL || token_str(p, cur_tok)[0] != '(') goto error;
    ++cur_tok;
    while (cur_tok->type != LEX_SYMBOL)
    {
        const struct Token* const attrkind = cur_tok++;
        if (attrkind->type != LEX_IDENT) goto error;
        if (cur_tok->type != LEX_SYMBOL || token_str(p, cur_tok)[0] != '(') goto error;
        ++cur_tok;
        if (cur_tok->type != LEX_STRING)
        {
            snprintf(s_error_buffer,
                     sizeof(s_error_buffer),
                     "%d:%d: error: expected attribute parameter\n",
                     cur_tok->rc.row,
                     cur_tok->rc.col);
            return NULL;
        }
        const char* attrkind_str = token_str(p, attrkind);
        if (strcmp(attrkind_str, "sym") == 0)
        {
            attr->symname = token_str(p, cur_tok);
        }
        else if (strcmp(attrkind_str, "asmstr") == 0)
        {
            attr->asmstr = token_str(p, cur_tok);
        }
        else
        {
            snprintf(s_error_buffer,
                     sizeof(s_error_buffer),
                     "%d:%d: error: unknown attribute: '%s'\n",
                     cur_tok->rc.row,
                     cur_tok->rc.col,
                     attrkind_str);
            return NULL;
        }
        ++cur_tok;
        if (cur_tok->type != LEX_SYMBOL || token_str(p, cur_tok)[0] != ')') goto error;
        ++cur_tok;
    }
    if (cur_tok->type != LEX_SYMBOL || token_str(p, cur_tok)[0] != ')') goto error;
    ++cur_tok;
    if (cur_tok->type != LEX_SYMBOL || token_str(p, cur_tok)[0] != ')') goto error;
    ++cur_tok;
    return cur_tok;

error:
    snprintf(s_error_buffer,
             sizeof(s_error_buffer),
             "%d:%d: error: ill-formed attribute\n",
             cur_tok->rc.row,
             cur_tok->rc.col);
    return NULL;
}

static struct Token* compile_declstmt(Parser* p, struct Token* cur_tok, struct BasicBlock* bb);
static struct Token* compile_declspecs(Parser* p, struct Token* cur_tok);
static struct Token* compile_declarator(Parser* p, struct Token* cur_tok, struct BasicBlock* bb, struct Symbol** p_sym)
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
        snprintf(s_error_buffer,
                 sizeof(s_error_buffer),
                 "%d:%d: error: expected identifier\n",
                 cur_tok->rc.row,
                 cur_tok->rc.col);
        return NULL;
    }
    struct Token* id = cur_tok++;
    const char* name = token_str(p, id);
    struct Symbol* const sym = symbol_alloc();
    if (p_sym) *p_sym = sym;
    if (attr.symname)
    {
        if (strlen(attr.symname) >= sizeof(sym->rename.buf))
        {
            snprintf(s_error_buffer,
                     sizeof(s_error_buffer),
                     "%d:%d: error: symbol names have a maximum length of %lu characters\n",
                     cur_tok->rc.row,
                     cur_tok->rc.col,
                     sizeof(sym->rename.buf) - 1);
            return NULL;
        }
        strcpy(sym->rename.buf, attr.symname);
    }
    sym->intrinsic_asm_str = attr.asmstr;

    const char ch = cur_tok->type == LEX_SYMBOL ? token_str(p, cur_tok)[0] : '\0';
    if (ch == '(')
    {
        if (sym->rename.buf[0])
        {
            if (sym->rename.buf[0] != '$' || !sym->rename.buf[1] || sym->rename.buf[strlen(sym->rename.buf) - 1] != '$')
            {
                snprintf(s_error_buffer,
                         sizeof(s_error_buffer),
                         "%d:%d: error: external symbol names must be of the form '$foo$' (was '%s')\n",
                         cur_tok->rc.row,
                         cur_tok->rc.col,
                         sym->rename.buf);
                return NULL;
            }
        }
        else
        {
            sym->rename = extern_var_from(p, name);
        }
        scope_insert(&p->scope, name, sym);
        sym->is_function = 1;
        ++cur_tok;
        if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == ')')
        {
            ++cur_tok;
        }
        else
        {
            while (cur_tok = compile_declspecs(p, cur_tok))
            {
                struct Symbol* arg_sym;
                if (!(cur_tok = compile_declarator(p, cur_tok, bb, &arg_sym)))
                {
                    return NULL;
                }
                if (sym->arg_count < REG_COUNT) strcpy(arg_sym->rename.buf, PARAM_NAMES_ARR[sym->arg_count]);
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
                snprintf(s_error_buffer,
                         sizeof(s_error_buffer),
                         "%d:%d: error: expected ',' and further parameter declarations or ')'\n",
                         cur_tok->rc.row,
                         cur_tok->rc.col);
                return NULL;
            }
        }
    }
    else
    {
        // not a function
        if (!sym->rename.buf[0]) sym->rename = free_var_from(p, name);
        scope_insert(&p->scope, name, sym);
        if (ch == '=')
        {
            struct ValDest dst = dest_reg(sym->rename.buf);
            cur_tok = compile_expr(p, cur_tok + 1, &dst, bb);
        }
    }
    return cur_tok;
}

static struct Token* compile_declspecs(Parser* p, struct Token* cur_tok)
{
    if (cur_tok->type == LEX_IDENT)
    {
        struct Binding* const cur_bind = scope_find(&p->type_scope, token_str(p, cur_tok));
        if (cur_bind)
        {
            cur_tok = compile_declspecs(p, cur_tok + 1);
        }
    }
    else if (cur_tok->type == LEX_VOID || cur_tok->type == LEX_INT)
    {
        cur_tok = compile_declspecs(p, cur_tok + 1);
    }
    return cur_tok;
}
static struct Token* compile_stmts(Parser* p, struct Token* cur_tok);
static struct Token* compile_compound_stmt(Parser* p, struct Token* cur_tok, struct BasicBlock* bb)
{
    size_t scope_sz = scope_size(&p->scope);
    if (cur_tok = compile_stmts(p, cur_tok))
    {
        if (cur_tok->type != LEX_SYMBOL || token_str(p, cur_tok)[0] != '}')
        {
            snprintf(s_error_buffer,
                     sizeof(s_error_buffer),
                     "%d:%d: error: expected '}'\n",
                     cur_tok->rc.row,
                     cur_tok->rc.col);
            return NULL;
        }
        ++cur_tok;
    }
    scope_shrink_free_syms(&p->scope, scope_sz);
    return cur_tok;
}
static struct Token* compile_declstmt(Parser* p, struct Token* cur_tok, struct BasicBlock* bb)
{
    if (cur_tok = compile_declspecs(p, cur_tok))
    {
        size_t scope_sz;
        struct Symbol* sym = NULL;
        while (scope_sz = scope_size(&p->scope), cur_tok = compile_declarator(p, cur_tok, bb, &sym))
        {
            if (cur_tok->type == LEX_SYMBOL)
            {
                const char ch = token_str(p, cur_tok)[0];
                if (ch == '{')
                {
                    // begin compound block
                    cg_mark_label(&p->cg, sym->rename.buf);
                    cg_write_push_ret(&p->cg);
                    cur_tok = compile_compound_stmt(p, cur_tok + 1, bb);
                    cg_write_return(&p->cg);
                    scope_shrink_free_syms(&p->scope, scope_sz + 1);
                    return cur_tok;
                }
                else
                {
                    // since we aren't defining the function body, remove the parameters from the scope
                    scope_shrink_free_syms(&p->scope, scope_sz + 1);
                    if (ch == ',')
                    {
                        ++cur_tok;
                        continue;
                    }
                    else if (ch == ';')
                    {
                        return cur_tok + 1;
                    }
                }
            }
            snprintf(s_error_buffer,
                     sizeof(s_error_buffer),
                     "%d:%d: error: expected ',' and further declarations or ';'\n",
                     cur_tok->rc.row,
                     cur_tok->rc.col);
            return NULL;
        }
    }
    return cur_tok;
}

static struct Token* compile_stmt(Parser* p, struct Token* cur_tok, struct BasicBlock* bb)
{
    switch (cur_tok->type)
    {
        case LEX_INT:
        case LEX_VOID: return compile_declstmt(p, cur_tok, bb);
        case LEX_RETURN:
        {
            struct ValDest dst = dest_reg(PARAM_NAMES_ARR[0]);
            if (cur_tok = compile_expr(p, cur_tok + 1, &dst, bb))
            {
                cg_write_return(&p->cg);
            }
            return cur_tok;
        }
        case LEX_IDENT:
        {
            struct Binding* const cur_bind = scope_find(&p->type_scope, token_str(p, cur_tok));
            if (cur_bind)
            {
                // this is a declaration
                return compile_declstmt(p, cur_tok, bb);
            }
            else
            {
                struct ValDest dst = s_any_destination;
                if (cur_tok = compile_expr(p, cur_tok, &dst, bb))
                {
                    if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == ';')
                    {
                        ++cur_tok;
                    }
                    else
                    {
                        snprintf(s_error_buffer,
                                 sizeof(s_error_buffer),
                                 "%d:%d: error: expected semicolon\n",
                                 cur_tok->rc.row,
                                 cur_tok->rc.col);
                        return NULL;
                    }
                }
            }
            return cur_tok;
        }
        case LEX_SYMBOL:
            if (token_str(p, cur_tok)[0] != ';')
            {
                snprintf(s_error_buffer,
                         sizeof(s_error_buffer),
                         "%d:%d: error: expected statement\n",
                         cur_tok->rc.row,
                         cur_tok->rc.col);
                return NULL;
            }
            return cur_tok + 1;
        default:
            snprintf(s_error_buffer,
                     sizeof(s_error_buffer),
                     "%d:%d: error: expected statement\n",
                     cur_tok->rc.row,
                     cur_tok->rc.col);
            return NULL;
    }
}

static struct Token* compile_stmts(Parser* p, struct Token* cur_tok)
{
    struct BasicBlock bb;
    init_basic_block(p, &bb);
    do
    {
        if (cur_tok->type == LEX_EOF) return cur_tok;
        if (cur_tok->type == LEX_SYMBOL && token_str(p, cur_tok)[0] == '}') return cur_tok;
        cur_tok = compile_stmt(p, cur_tok, &bb);
        if (!cur_tok) return NULL;
    } while (1);
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

        // actually parse
        const size_t num_toks = p->toks.sz / sizeof(struct Token);
        struct Token* const arr_toks = (struct Token*)p->toks.data;
        cg_write_bin_entry(&p->cg);
        struct Token* tk = compile_stmts(p, arr_toks);
        if (!tk) return 1;
        if (tk->type != LEX_EOF)
        {
            snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: expected eof\n", tk->rc.row, tk->rc.col);
            return 1;
        }

        printf("\nCode Gen\n--------\n");
        cg_emit(&p->cg);
        return 0;
    }
    else if (l->state == LEX_MULTILINE_COMMENT)
    {
        // drop comments
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
