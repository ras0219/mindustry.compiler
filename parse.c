#include "tok.h"
#include "parsestate.h"
#include "lexstate.h"
#include <malloc.h>
#include <stdio.h>
#include <string.h>

static void array_init(struct Array *arr)
{
    arr->sz = 0;
    arr->cap = 0;
    arr->data = 0;
}
static void *array_alloc(struct Array *arr, size_t sz)
{
    arr->sz += sz;
    if (arr->sz > arr->cap)
    {
        do
        {
            arr->cap = arr->cap ? arr->cap * 2 : sz * 4;
        } while (arr->sz > arr->cap);
        arr->data = realloc(arr->data, arr->cap);
    }
    return arr->data + arr->sz - sz;
}
static void *array_push(struct Array *arr, const void *src, size_t sz)
{
    void *dst = array_alloc(arr, sz);
    memcpy(dst, src, sz);
    return dst;
}
static void array_pop(struct Array *arr, size_t sz)
{
    arr->sz -= sz;
}
static void array_destroy(struct Array *arr)
{
    free(arr->data);
}

struct Token
{
    struct RowCol rc;
    LexerState type;
    ptrdiff_t sp_offset;
};

static struct ValDest dest_reg(const char *ch)
{
    struct ValDest ret = {
        .dst_name = ch,
    };
    return ret;
}

static char *token_str(Parser *p, const struct Token *tk)
{
    return (char *)p->stringpool.data + tk->sp_offset;
}

#define REG_COUNT 6

static const char *PARAM_NAMES_ARR[REG_COUNT] = {"eax", "ebx", "ecx", "edx", "eex", "efx"};

struct BasicBlock
{
    char reg_in_use[REG_COUNT];
};
static void init_basic_block(struct BasicBlock *b)
{
    memset(&b->reg_in_use, 0, sizeof(b->reg_in_use));
}

struct FreeVar
{
    char buf[16];
};
static struct FreeVar free_var(Parser *p)
{
    struct FreeVar ret;
    snprintf(ret.buf, sizeof(ret.buf), "g%d", p->free_var_counter++);
    return ret;
}

static void emit_push_stk(const char *reg)
{
    printf("write %s __mem esp\n", reg);
    printf("op add esp 1 esp\n");
}
static void emit_pop_stk(const char *reg)
{
    printf("op add esp -1 esp\n");
    printf("read %s __mem esp\n", reg);
}

static struct Token *compile_expr(Parser *p, struct Token *cur_tok, struct ValDest *dst);

static void mov_or_assign_dst(struct ValDest *dst, const char *val)
{
    if (dst->dst_name)
    {
        printf("mov %s, %s\n", dst->dst_name, val);
    }
    else
    {
        dst->dst_name = val;
    }
}

static struct Token *compile_call_expr(Parser *p, struct Token *cur_tok, struct ValDest *dst, struct Token *fn, int param_num)
{
    do
    {
        if (cur_tok->type == LEX_SYMBOL)
        {
            char ch = token_str(p, cur_tok)[0];
            if (ch == ')')
            {
                const struct FreeVar fv_ret = free_var(p);
                printf("set %s ret\n"
                       "op add ret 1 @counter\n"
                       "set @counter %s\n"
                       "set ret %s\n",
                       fv_ret.buf,
                       token_str(p, fn),
                       fv_ret.buf);
                mov_or_assign_dst(dst, PARAM_NAMES_ARR[0]);
                return cur_tok + 1;
            }
            else if (ch == ',')
            {
                if (param_num >= REG_COUNT)
                {
                    snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: a maximum of %d function arguments are supported.\n", cur_tok->rc.row, cur_tok->rc.col, REG_COUNT);
                    return NULL;
                }
                struct ValDest new_dst = dest_reg(PARAM_NAMES_ARR[param_num]);
                if (cur_tok = compile_expr(p, cur_tok + 1, &new_dst))
                    cur_tok = compile_call_expr(p, cur_tok, dst, fn, param_num + 1);
                return cur_tok;
            }
        }
        snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: expected ',' or ')'\n", cur_tok->rc.row, cur_tok->rc.col);
        return NULL;
    } while (1);
}

static char *my_strdup(const char *s)
{
    return strcpy(malloc(strlen(s) + 1), s);
}

static const struct ValDest s_any_destination = {.dst_name = NULL};

static struct Token *compile_expr(Parser *p, struct Token *cur_tok, struct ValDest *dst)
{
    switch (cur_tok->type)
    {
    case LEX_IDENT:
    {
        struct Token *lhs = cur_tok++;
        if (cur_tok->type == LEX_SYMBOL)
        {
            struct Token *tok_op = cur_tok;
            char *op = token_str(p, tok_op);
            if (op[0] == '=')
            {
                struct ValDest new_dst = dest_reg(token_str(p, lhs));
                cur_tok = compile_expr(p, cur_tok + 1, &new_dst);
                if (!cur_tok)
                    return NULL;
                mov_or_assign_dst(dst, new_dst.dst_name);
            }
            else if (op[0] == '<')
            {
                if (!dst->dst_name)
                {
                    struct FreeVar fv_ret = free_var(p);
                    dst->dst_name = my_strdup(fv_ret.buf);
                    array_push(&p->strings_to_free, dst->dst_name, sizeof(dst->dst_name));
                }
                struct ValDest rhs = s_any_destination;
                if (cur_tok = compile_expr(p, cur_tok + 1, &rhs))
                {
                    printf("op lessThan %s %s %s\n", dst->dst_name, token_str(p, lhs), rhs.dst_name);
                }
            }
            else if (op[0] == '(')
            {
                struct Token *tk = ++cur_tok;
                if (tk->type == LEX_SYMBOL && token_str(p, tk)[0] == ')')
                {
                    return compile_call_expr(p, cur_tok, dst, lhs, 0);
                }
                else
                {
                    struct ValDest new_dst = dest_reg(PARAM_NAMES_ARR[0]);
                    if (cur_tok = compile_expr(p, cur_tok, &new_dst))
                        cur_tok = compile_call_expr(p, cur_tok, dst, lhs, 1);
                    return cur_tok;
                }
            }
            else
            {
                mov_or_assign_dst(dst, token_str(p, lhs));
            }
        }
        return cur_tok;
    }
    case LEX_NUMBER:
    {
        mov_or_assign_dst(dst, token_str(p, cur_tok));
        return cur_tok + 1;
    }
    default:
        snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: expected expression\n", cur_tok->rc.row, cur_tok->rc.col);
        return NULL;
    }
}

static struct Token *compile_stmt(Parser *p, struct Token *cur_tok)
{
    switch (cur_tok->type)
    {
    case LEX_IDENT:
    {
        struct ValDest dst = s_any_destination;
        if (cur_tok = compile_expr(p, cur_tok, &dst))
        {
            if (cur_tok->type == LEX_SYMBOL)
            {
                char *sym = token_str(p, cur_tok);
                if (sym[0] != ';')
                {
                    snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: expected semicolon\n", cur_tok->rc.row, cur_tok->rc.col);
                    return NULL;
                }
                ++cur_tok;
            }
        }
        return cur_tok;
    }
    default:
        snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: expected statement\n", cur_tok->rc.row, cur_tok->rc.col);
        return NULL;
    }
}

static struct Token *compile_stmts(Parser *p, struct Token *cur_tok)
{
    do
    {
        if (cur_tok->type == LEX_EOF)
            return cur_tok;
        cur_tok = compile_stmt(p, cur_tok);
        if (!cur_tok)
            return NULL;
    } while (1);
}

void init_parser(Parser *p)
{
    array_init(&p->stringpool);
    array_init(&p->toks);
    array_init(&p->strings_to_free);
    p->free_var_counter = 0;
}
void destroy_parser(Parser *p)
{
    array_destroy(&p->stringpool);
    array_destroy(&p->toks);
    for (size_t i = 0; i < p->strings_to_free.sz / sizeof(char *); ++i)
    {
        free(((char **)p->strings_to_free.data)[i]);
    }
    array_destroy(&p->strings_to_free);
}

int parse(Parser *p, Lexer *l)
{
    if (l->state == LEX_EOF)
    {
        {
            struct Token *const tk = (struct Token *)array_alloc(&p->toks, sizeof(struct Token));
            tk->type = l->state;
            tk->sp_offset = 0;
            tk->rc = l->rc;
        }

        // actually parse
        const size_t num_toks = p->toks.sz / sizeof(struct Token);
        struct Token *const arr_toks = (struct Token *)p->toks.data;
        for (size_t i = 0; i < num_toks; ++i)
        {
            printf("parsing: %s\n", token_str(p, arr_toks + i));
        }
        printf("\nCode Gen\n--------\n");
        struct Token *tk = compile_stmts(p, arr_toks);
        if (tk)
        {
            if (tk->type == LEX_EOF)
                return 0;
            snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: expected eof\n", tk->rc.row, tk->rc.col);
            return 1;
        }
        else
            return 1;
    }
    else
    {
        char *const s = (char *)array_push(&p->stringpool, l->tok, l->sz + 1);
        struct Token *const tk = (struct Token *)array_alloc(&p->toks, sizeof(struct Token));
        tk->type = l->state;
        tk->sp_offset = s - (char *)p->stringpool.data;
        tk->rc = l->tok_rc;
    }
    return 0;
}
