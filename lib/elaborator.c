#include "elaborator.h"

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "autoheap.h"
#include "errors.h"
#include "lexstate.h"
#include "parse.h"
#include "stdlibe.h"
#include "symbol.h"
#include "token.h"
#include "typestr.h"
#include "unwrap.h"

static const struct TypeStr s_type_unknown = {};

typedef struct TypeTable
{
    struct Array typesyms;
    struct Array fn_args_ends;
    struct Array fn_args;
} TypeTable;

__forceinline static TypeSymbol* tt_get(const TypeTable* tt, uint32_t i)
{
    return ((TypeSymbol**)tt->typesyms.data)[i];
}

static int32_t eval_constant(struct Elaborator* elab, struct Expr* e)
{
    switch (e->kind)
    {
        case EXPR_LIT:
        {
            struct ExprLit* lit = (struct ExprLit*)e;
            if (lit->tok->type == LEX_NUMBER || lit->tok->type == LEX_CHARLIT)
            {
                if (lit->numeric > INT32_MAX)
                {
                    parser_tok_error(e->tok, "error: integer constant exceeded INT32_MAX: %llu\n", lit->numeric);
                    return 0;
                }
                return lit->numeric;
            }
            parser_tok_error(e->tok, "error: expected integer constant literal\n");
            return 0;
        }
        case EXPR_REF:
        {
            struct ExprRef* ref = (void*)e;
            if (ref->sym->is_enum_constant)
            {
                return ref->sym->enum_value;
            }
            parser_tok_error(ref->tok, "error: expected integer constant expression\n");
            return 0;
        }
        case EXPR_BINOP:
        {
            struct ExprBinOp* op = (struct ExprBinOp*)e;
            int32_t l = eval_constant(elab, op->lhs);
            int32_t r = eval_constant(elab, op->rhs);
            switch (op->tok->type)
            {
                case TOKEN_SYM1('*'):
                {
                    int64_t p = (int64_t)l * (int64_t)r;
                    if (p > INT32_MAX || p < INT32_MIN)
                        return parser_tok_error(op->tok, "error: integer constant exceeded INT32_MAX\n"), 0;
                    return (int32_t)p;
                }
                case TOKEN_SYM1('|'):
                {
                    int64_t p = (int64_t)l | (int64_t)r;
                    if (p > INT32_MAX || p < INT32_MIN)
                        return parser_tok_error(op->tok, "error: integer constant exceeded INT32_MAX\n"), 0;
                    return (int32_t)p;
                }
                case TOKEN_SYM1('-'):
                {
                    int64_t p = (int64_t)l - (int64_t)r;
                    if (p > INT32_MAX || p < INT32_MIN)
                        return parser_tok_error(op->tok, "error: integer constant exceeded INT32_MAX\n"), 0;
                    return (int32_t)p;
                }
                case TOKEN_SYM1('+'):
                {
                    int64_t p = (int64_t)l + (int64_t)r;
                    if (p > INT32_MAX || p < INT32_MIN)
                        return parser_tok_error(op->tok, "error: integer constant exceeded INT32_MAX\n"), 0;
                    return (int32_t)p;
                }
                case TOKEN_SYM2('<', '<'):
                {
                    if (r < 0 || r > 32)
                        return parser_tok_error(op->tok, "error: right hand side of '<<' was invalid: %d\n", r), 0;
                    int64_t p = (int64_t)l << r;
                    if (p > INT32_MAX || p < INT32_MIN)
                        return parser_tok_error(op->tok, "error: integer constant exceeded INT32_MAX\n"), 0;
                    return (int32_t)p;
                }
                default: break;
            }
            parser_tok_error(
                op->tok, "error: unimplemented op '%s' in integer constant expression\n", token_str(elab->p, op->tok));
            return 0;
        }
        case EXPR_UNOP:
        {
            struct ExprUnOp* expr = (void*)e;
            if (expr->tok->type == TOKEN_SYM1('-'))
            {
                return -eval_constant(elab, expr->lhs);
            }
            else if (expr->tok->type == TOKEN_SYM1('+'))
            {
                return eval_constant(elab, expr->lhs);
            }
            parser_tok_error(e->tok,
                             "error: unimplemented unary op for integer constant expression (%s)\n",
                             token_str(elab->p, e->tok));
            return 0;
        }
        case EXPR_CAST:
        {
            struct ExprCast* expr = (void*)e;
            return eval_constant(elab, expr->expr);
        }
        default:
            parser_tok_error(
                e->tok, "error: expected integer constant expression (not %s)\n", ast_kind_to_string(e->kind));
            return 0;
    }
}

#if 0
static int get_primitive_declspec_size(struct DeclSpecs* d)
{
    if (d->is_short) return 2;
    if (d->is_long) return d->tok->type == LEX_DOUBLE ? 16 : 4;
    if (d->is_longlong) return 8;

    switch (d->tok->type)
    {
        case LEX_VOID: return 0;
        case LEX_FLOAT:
        case LEX_SIGNED:
        case LEX_UNSIGNED:
        case LEX_INT: return 4;
        case LEX_UUVALIST: return 24;
        case LEX_UUINT64:
        case LEX_DOUBLE: return 8;
        case LEX_CHAR:
        case LEX_BOOL: return 1;
        default: parser_tok_error(d->tok, "error: unable to determine size of declspec\n"); return 1;
    }
}
#endif

enum
{
    TYPE_FLAGS_VOID = 1,
    TYPE_FLAGS_CHAR = 1 << 1,
    TYPE_FLAGS_INT = 1 << 2,
    TYPE_FLAGS_POINTER = 1 << 3,
    TYPE_FLAGS_STRUCT = 1 << 4,
    TYPE_FLAGS_UNION = 1 << 5,
    TYPE_FLAGS_FUNCTION = 1 << 6,
    TYPE_FLAGS_VAR = 1 << 7,
    TYPE_FLAGS_FLOAT = 1 << 8,
    TYPE_FLAGS_ARRAY = 1 << 9,
    TYPE_FLAGS_SIGNED = 1 << 10,

    TYPE_MASK_HAS_FIELDS = TYPE_FLAGS_UNION | TYPE_FLAGS_STRUCT,
    TYPE_MASK_ARITH = TYPE_FLAGS_FLOAT | TYPE_FLAGS_INT,
    TYPE_MASK_SCALAR = TYPE_FLAGS_POINTER | TYPE_MASK_ARITH,
    TYPE_MASK_OBJECT = TYPE_FLAGS_VOID | TYPE_FLAGS_INT | TYPE_FLAGS_POINTER | TYPE_FLAGS_STRUCT | TYPE_FLAGS_UNION,
};

unsigned int typestr_mask(const struct TypeStr* ts)
{
    switch (typestr_byte(ts))
    {
        case '\0': return 0;
        case TYPE_BYTE_VARIADIC: return TYPE_FLAGS_VAR;
        case TYPE_BYTE_VOID: return TYPE_FLAGS_VOID;
        case TYPE_BYTE_CHAR:
        case TYPE_BYTE_SCHAR: return TYPE_FLAGS_CHAR | TYPE_FLAGS_INT | TYPE_FLAGS_SIGNED;
        case TYPE_BYTE_UCHAR: return TYPE_FLAGS_CHAR | TYPE_FLAGS_INT;
        case TYPE_BYTE_ENUM:
        case TYPE_BYTE_INT:
        case TYPE_BYTE_SHORT:
        case TYPE_BYTE_LONG:
        case TYPE_BYTE_LLONG: return TYPE_FLAGS_INT | TYPE_FLAGS_SIGNED;
        case TYPE_BYTE_UINT:
        case TYPE_BYTE_USHORT:
        case TYPE_BYTE_ULONG:
        case TYPE_BYTE_ULLONG: return TYPE_FLAGS_INT;
        case TYPE_BYTE_FLOAT:
        case TYPE_BYTE_DOUBLE:
        case TYPE_BYTE_LDOUBLE: return TYPE_FLAGS_FLOAT;
        case TYPE_BYTE_POINTER: return TYPE_FLAGS_POINTER;
        case TYPE_BYTE_UNK_ARRAY:
        case TYPE_BYTE_ARRAY: return TYPE_FLAGS_ARRAY;
        case TYPE_BYTE_STRUCT: return TYPE_FLAGS_STRUCT;
        case TYPE_BYTE_UNION: return TYPE_FLAGS_UNION;
        case TYPE_BYTE_FUNCTION: return TYPE_FLAGS_FUNCTION;
        case TYPE_BYTE_UUVALIST: return TYPE_FLAGS_POINTER;
        default: abort();
    }
}

static const struct TypeStr s_type_void = {.buf = {1, TYPE_BYTE_VOID}};
static const struct TypeStr s_type_literal_int = {.buf = {1, TYPE_BYTE_INT}};
static const struct TypeStr s_type_mutable_char = {.buf = {1, TYPE_BYTE_CHAR}};
// static const struct TypeStr s_type_void = {.buf = {1, TYPE_BYTE_VOID}};

__forceinline static uint32_t typestr_get_offset_i(const struct TypeStr* ts, int i)
{
    uint32_t ret = UINT32_MAX;
    if (i >= 1 + sizeof(ret))
    {
        memcpy(&ret, ts->buf + i - sizeof(ret), sizeof(ret));
    }
    return ret;
}
__forceinline static uint32_t typestr_get_offset(const struct TypeStr* ts)
{
    return typestr_get_offset_i(ts, ts->buf[0]);
}

static void typestr_fmt(const struct TypeTable* tt, const struct TypeStr* ts, struct Array* buf);
static void typestr_fmt_i(const struct TypeTable* tt, const struct TypeStr* ts, size_t i, struct Array* buf)
{
    size_t depth = 0;
    const char* str;
    while (1)
    {
        char ch = ts->buf[i];
        switch (ch)
        {
            case '\0': array_appends(buf, "invalid type"); return;
            case TYPE_BYTE_VARIADIC: str = "..."; goto append_ret;
            case TYPE_BYTE_VOID: str = "void"; goto append_ret;
            case TYPE_BYTE_CHAR: str = "char"; goto append_ret;
            case TYPE_BYTE_SCHAR: str = "signed char"; goto append_ret;
            case TYPE_BYTE_UCHAR: str = "unsigned char"; goto append_ret;
            case TYPE_BYTE_INT: str = "int"; goto append_ret;
            case TYPE_BYTE_SHORT: str = "short"; goto append_ret;
            case TYPE_BYTE_LONG: str = "long"; goto append_ret;
            case TYPE_BYTE_LLONG: str = "long long"; goto append_ret;
            case TYPE_BYTE_UINT: str = "unsigned int"; goto append_ret;
            case TYPE_BYTE_USHORT: str = "unsigned short"; goto append_ret;
            case TYPE_BYTE_ULONG: str = "unsigned long"; goto append_ret;
            case TYPE_BYTE_ULLONG: str = "unsigned long long"; goto append_ret;
            case TYPE_BYTE_FLOAT: str = "float"; goto append_ret;
            case TYPE_BYTE_DOUBLE: str = "double"; goto append_ret;
            case TYPE_BYTE_UUVALIST: str = "va_list"; goto append_ret;
            case TYPE_BYTE_POINTER: str = "pointer to "; goto append_continue;
            case 'c': str = "const "; goto append_continue;
            case 'v': str = "volatile "; goto append_continue;
            case 'r': str = "restrict "; goto append_continue;
            case TYPE_BYTE_UNK_ARRAY: str = "array of "; goto append_continue;
            case TYPE_BYTE_ARRAY:
            {
                uint32_t u;
                i -= sizeof(u);
                memcpy(&u, ts->buf + i, sizeof(u));
                array_appendf(buf, "array of %u ", u);
                --i;
                continue;
            }
            case TYPE_BYTE_STRUCT: str = "struct"; goto sue;
            case TYPE_BYTE_UNION: str = "union"; goto sue;
            case TYPE_BYTE_ENUM: str = "enum"; goto sue;
            case TYPE_BYTE_FUNCTION:
            {
                array_appends(buf, "function (");
                uint32_t f_offset = typestr_get_offset_i(ts, i);
                size_t begin = f_offset ? arrsz_at(&tt->fn_args_ends, f_offset - 1) : 0;
                size_t end = arrsz_at(&tt->fn_args_ends, f_offset);
                const struct TypeStr* argtys = tt->fn_args.data;
                for (; begin != end; ++begin)
                {
                    typestr_fmt(tt, argtys + begin, buf);
                    if (begin + 1 != end) array_appends(buf, ", ");
                }
                str = ") returning ";
                i -= sizeof(uint32_t);
                goto append_continue;
            }
            default: abort();
        }
    append_continue:
        array_appends(buf, str);
        --i;
        continue;
    append_ret:
        array_appends(buf, str);
        --i;
        goto end;
    sue:;
        // struct, union, enum
        unsigned int u;
        i -= sizeof(u);
        memcpy(&u, ts->buf + i, sizeof(u));
        TypeSymbol* sym = tt_get(tt, u);
        array_appends(buf, str);
        array_push_byte(buf, ' ');
        if (sym->name)
            array_appends(buf, sym->name);
        else
            array_appendf(buf, "<anonymous#%u>", u);
        goto end;
    end:
        if (depth)
        {
            if (ts->buf[i] == ')')
            {
                --depth;
                str = ") returning ";
                goto append_continue;
            }
            else
            {
                array_appends(buf, ", ");
                continue;
            }
        }
        return;
    }
}

static void typestr_fmt(const struct TypeTable* tt, const struct TypeStr* ts, struct Array* buf)
{
    typestr_fmt_i(tt, ts, ts->buf[0], buf);
}

static __forceinline int typestr_is_pointer(const struct TypeStr* ts)
{
    return ts->buf[(int)ts->buf[0]] == TYPE_BYTE_POINTER;
}
const struct TypeStr s_void = {
    .buf = {1, TYPE_BYTE_VOID},
};
static __forceinline int typestr_is_fn(const struct TypeStr* ts) { return typestr_byte(ts) == TYPE_BYTE_FUNCTION; }
static __forceinline int typestr_is_variadic(const struct TypeStr* ts)
{
    return typestr_byte(ts) == TYPE_BYTE_VARIADIC;
}
static __forceinline int typestr_is_aggregate(const struct TypeStr* ts)
{
    char ch = typestr_byte(ts);
    return ch == TYPE_BYTE_STRUCT || ch == TYPE_BYTE_ARRAY || ch == TYPE_BYTE_UNION;
}

enum
{
    TYPESTR_CVR_C = 1,
    TYPESTR_CVR_V = 2,
    TYPESTR_CVR_R = 4,
};
static unsigned int typestr_strip_cvr(struct TypeStr* ts)
{
    unsigned int m = 0;
    if (ts->buf[(int)ts->buf[0]] == 'r') ts->buf[0]--, m |= TYPESTR_CVR_R;
    if (ts->buf[(int)ts->buf[0]] == 'v') ts->buf[0]--, m |= TYPESTR_CVR_V;
    if (ts->buf[(int)ts->buf[0]] == 'c') ts->buf[0]--, m |= TYPESTR_CVR_C;
    return m;
}
static void typestr_remove_array(struct TypeStr* ts)
{
    typestr_strip_cvr(ts);
    if (ts->buf[(int)ts->buf[0]] == TYPE_BYTE_ARRAY)
    {
        ts->buf[0] -= sizeof(uint32_t) + 1;
    }
    else
    {
        --ts->buf[0];
    }
}
static void typestr_dereference(struct TypeStr* ts)
{
    typestr_strip_cvr(ts);
    if (typestr_is_pointer(ts))
    {
        --ts->buf[0];
    }
    else
    {
        *ts = s_type_unknown;
    }
}

// fmt should contain exactly one %.*s
static void typestr_error1(const struct RowCol* rc,
                           const struct TypeTable* e,
                           const char* fmt,
                           const struct TypeStr* ts)
{
    struct Array arr = {};
    typestr_fmt(e, ts, &arr);
    parser_ferror(rc, fmt, arr.sz, arr.data);
    array_destroy(&arr);
}

// fmt should contain exactly two %.*s
static void typestr_error2(const struct RowCol* rc,
                           const struct TypeTable* e,
                           const char* fmt,
                           const struct TypeStr* t1,
                           const struct TypeStr* t2)
{
    struct Array arr = {}, arr2 = {};
    typestr_fmt(e, t1, &arr);
    typestr_fmt(e, t2, &arr2);
    parser_ferror(rc, fmt, arr.sz, arr.data, arr2.sz, arr2.data);
    array_destroy(&arr2);
    array_destroy(&arr);
}

__forceinline static int typestr_match(const struct TypeStr* tgt, const struct TypeStr* src)
{
    return memcmp(tgt->buf, src->buf, tgt->buf[0] + 1) == 0;
}

__forceinline static uint32_t typestr_pop_offset(struct TypeStr* ts)
{
    uint32_t r = typestr_get_offset(ts);
    if (r < UINT32_MAX)
    {
        ts->buf[0] -= 1 + sizeof(r);
    }
    return r;
}

TypeSymbol* typestr_get_decl(struct TypeTable* tt, const struct TypeStr* ts)
{
    char ch = ts->buf[(int)ts->buf[0]];
    if (ch == TYPE_BYTE_STRUCT || ch == TYPE_BYTE_UNION)
    {
        return tt_get(tt, typestr_get_offset(ts));
    }
    return NULL;
}

int typestr_is_arithmetic(const struct TypeStr* ts) { return !!(typestr_mask(ts) & TYPE_MASK_ARITH); }

static void typestr_decay(struct TypeStr* t)
{
    typestr_strip_cvr(t);
    char ch = t->buf[(int)t->buf[0]];
    switch (ch)
    {
        case TYPE_BYTE_ARRAY: t->buf[0] -= 4; // passthrough
        case TYPE_BYTE_UNK_ARRAY: t->buf[(int)t->buf[0]] = TYPE_BYTE_POINTER; return;
        case TYPE_BYTE_FUNCTION:
        {
            t->buf[0]++;
            if (t->buf[0] == TYPESTR_BUF_SIZE) abort();
            t->buf[(int)t->buf[0]] = TYPE_BYTE_POINTER;
            return;
        }
        default: return;
    }
}

static void typestr_implicit_conversion(struct TypeTable* types,
                                        const struct RowCol* rc,
                                        const struct TypeStr* orig_from,
                                        const struct TypeStr* orig_to)
{
    if (typestr_match(orig_from, orig_to)) return;

    // first, value transformations
    struct TypeStr from = *orig_from, to = *orig_to;
    typestr_decay(&from);
    typestr_strip_cvr(&to);
    if (typestr_match(&from, &to)) return;

    // then, check secondary conversions
    if (typestr_is_pointer(&from) && typestr_is_pointer(&to))
    {
        --from.buf[0];
        --to.buf[0];
        unsigned int cvr_from = typestr_strip_cvr(&from);
        unsigned int cvr_to = typestr_strip_cvr(&to);
        if ((cvr_from & cvr_to) == cvr_from)
        {
            // cvr_to contains cvr_from
            if (typestr_match(&to, &from) || typestr_match(&s_void, &to) || typestr_match(&s_void, &from)) return;
        }
    }
    else
    {
        unsigned int from_mask = typestr_mask(&from);
        unsigned int to_mask = typestr_mask(&to);
        if (from_mask & to_mask & TYPE_FLAGS_INT) return;
    }

    typestr_error2(rc, types, "error: could not implicitly convert '%.*s' to '%.*s'\n", orig_from, orig_to);
}

static void typestr_append_offset(struct TypeStr* s, uint32_t offset, char offset_type)
{
    if (TYPESTR_BUF_SIZE <= s->buf[0] + sizeof(uint32_t) + 1) abort();

    int i = s->buf[0] + 1;
    memcpy(s->buf + i, &offset, sizeof(offset));
    i += sizeof(offset);
    s->buf[i] = offset_type;
    s->buf[0] = i;
}

static void typestr_add_array(struct TypeStr* s, unsigned int n)
{
    if (s->buf[0] == 0) return;
    if (n == 0)
    {
        // unbounded array
        int i = ++s->buf[0];
        if (i >= TYPESTR_BUF_SIZE) abort();
        s->buf[i] = ']';
    }
    else
        typestr_append_offset(s, n, TYPE_BYTE_ARRAY);
}

__forceinline static void typestr_add_pointer(struct TypeStr* s)
{
    if (s->buf[0] == 0) return;
    size_t o = s->buf[0] += 1;
    if (o >= TYPESTR_BUF_SIZE) abort();
    s->buf[o] = TYPE_BYTE_POINTER;
}

static void typestr_add_cvr(struct TypeStr* s, unsigned int mask)
{
    if (s->buf[0] == 0) return;
    int i = s->buf[0];
    unsigned int m = 0;
    if (s->buf[i] == 'r') --i, m |= TYPESTR_CVR_R;
    if (s->buf[i] == 'v') --i, m |= TYPESTR_CVR_V;
    if (s->buf[i] == 'c') --i, m |= TYPESTR_CVR_C;
    if (i + 3 >= TYPESTR_BUF_SIZE) abort();
    if (!(mask & ~m)) return;
    m |= mask;
    if (m & TYPESTR_CVR_C) s->buf[++i] = 'c';
    if (m & TYPESTR_CVR_V) s->buf[++i] = 'v';
    if (m & TYPESTR_CVR_R) s->buf[++i] = 'r';
    s->buf[0] = i;
}

static void typestr_append_decltype(const struct Ast* const* expr_seqs,
                                    struct TypeTable* tt,
                                    struct TypeStr* s,
                                    const struct Ast* e);

static void typestr_append_decltype_DeclSpecs(const struct Ast* const* expr_seqs,
                                              struct TypeTable* tt,
                                              struct TypeStr* s,
                                              const struct DeclSpecs* d)
{
    if (d->_typedef)
    {
        return typestr_append_decltype(expr_seqs, tt, s, &d->_typedef->last_decl->ast);
    }
    else if (d->sym)
    {
        char ch;
        if (d->is_struct)
        {
            ch = TYPE_BYTE_STRUCT;
        }
        else if (d->is_union)
        {
            ch = TYPE_BYTE_UNION;
        }
        else if (d->is_enum)
        {
            ch = TYPE_BYTE_ENUM;
        }
        else
            abort();
        typestr_append_offset(s, d->sym->idx, ch);
    }
    else if (d->tok->type == LEX_UUVALIST)
    {
        s->buf[(int)++s->buf[0]] = TYPE_BYTE_VOID;
        s->buf[(int)++s->buf[0]] = TYPE_BYTE_POINTER;
    }
    else
    {
        char ch;
        switch (d->tok->type)
        {
            case LEX_VOID: ch = TYPE_BYTE_VOID; break;
            case LEX_LONG:
            case LEX_SHORT:
            case LEX_INT:
            case LEX_UUINT64:
            case LEX_SIGNED:
            case LEX_UNSIGNED:
                if (d->is_short)
                    ch = d->is_unsigned ? TYPE_BYTE_USHORT : TYPE_BYTE_SHORT;
                else if (d->is_long)
                    ch = d->is_unsigned ? TYPE_BYTE_ULONG : TYPE_BYTE_LONG;
                else if (d->is_longlong || d->tok->type == LEX_UUINT64)
                    ch = d->is_unsigned ? TYPE_BYTE_ULLONG : TYPE_BYTE_LLONG;
                else
                    ch = d->is_unsigned ? TYPE_BYTE_UINT : TYPE_BYTE_INT;
                break;
            case LEX_FLOAT: ch = TYPE_BYTE_FLOAT; break;
            case LEX_DOUBLE: ch = d->is_long ? TYPE_BYTE_LDOUBLE : TYPE_BYTE_DOUBLE; break;
            case LEX_CHAR:
                ch = d->is_signed ? TYPE_BYTE_SCHAR : d->is_unsigned ? TYPE_BYTE_UCHAR : TYPE_BYTE_CHAR;
                break;
            default: abort();
        }
        s->buf[(int)++s->buf[0]] = ch;
    }

    unsigned int m = 0;
    if (d->is_const) m |= TYPESTR_CVR_C;
    if (d->is_volatile) m |= TYPESTR_CVR_V;
    typestr_add_cvr(s, m);
    return;
}
static void typestr_append_decltype(const struct Ast* const* expr_seqs,
                                    struct TypeTable* tt,
                                    struct TypeStr* s,
                                    const struct Ast* e)
{
top:
    if (!e) abort();
    switch (e->kind)
    {
        case EXPR_REF: e = &((struct ExprRef*)e)->sym->last_decl->ast; goto top;
        case AST_DECL: e = ((struct Decl*)e)->type; goto top;
        case AST_DECLSPEC: return typestr_append_decltype_DeclSpecs(expr_seqs, tt, s, (struct DeclSpecs*)e);
        case AST_DECLPTR:
        {
            struct DeclPtr* d = (struct DeclPtr*)e;
            typestr_append_decltype(expr_seqs, tt, s, d->type);
            typestr_add_pointer(s);
            unsigned int m = 0;
            if (d->is_const) m |= TYPESTR_CVR_C;
            if (d->is_volatile) m |= TYPESTR_CVR_V;
            if (d->is_restrict) m |= TYPESTR_CVR_R;
            typestr_add_cvr(s, m);
            return;
        }
        case AST_DECLARR:
        {
            struct DeclArr* d = (struct DeclArr*)e;
            typestr_append_decltype(expr_seqs, tt, s, d->type);
            typestr_add_array(s, d->integer_arity);
            return;
        }
        case AST_DECLFN:
        {
            struct DeclFn* d = (struct DeclFn*)e;
            typestr_append_decltype(expr_seqs, tt, s, d->type);
            struct Array args = {};
            for (size_t i = 0; i < d->extent; ++i)
            {
                struct TypeStr* arg_ts = array_push_zeroes(&args, sizeof(struct TypeStr));
                struct StmtDecls* arg_decls = (void*)expr_seqs[d->offset + i];
                typestr_append_decltype(expr_seqs, tt, arg_ts, expr_seqs[arg_decls->offset]);
            }
            if (d->is_varargs)
            {
                struct TypeStr var = {1, TYPE_BYTE_VARIADIC};
                array_push(&args, &var, sizeof(var));
            }
            size_t i = 0;
            struct TypeStr* tt_fn_args = tt->fn_args.data;
            size_t fn_args_ends_sz = arrsz_size(&tt->fn_args_ends);
            size_t prev_end = 0;
            for (; i < fn_args_ends_sz; ++i)
            {
                size_t end = arrsz_at(&tt->fn_args_ends, i);
                if (end - prev_end == d->extent && memcmp(tt_fn_args + prev_end, args.data, args.sz) == 0)
                {
                    break;
                }
                prev_end = end;
            }
            if (i == fn_args_ends_sz)
            {
                array_push(&tt->fn_args, args.data, args.sz);
                arrsz_push(&tt->fn_args_ends, array_size(&tt->fn_args, sizeof(struct TypeStr)));
            }
            typestr_append_offset(s, i, TYPE_BYTE_FUNCTION);
            return;
        }
        default: break;
    }
    fprintf(stderr, "typestr_append_decltype(, %s) failed\n", ast_kind_to_string(e->kind));
    *s = s_type_unknown;
}

static void typestr_from_decltype(const struct Ast* const* expr_seqs,
                                  struct TypeTable* tt,
                                  struct TypeStr* s,
                                  struct Ast* e)
{
    // initialize type
    *s = s_type_unknown;
    typestr_append_decltype(expr_seqs, tt, s, e);
}
__forceinline static void typestr_from_decltype_Decl(const struct Ast* const* expr_seqs,
                                                     struct TypeTable* tt,
                                                     struct TypeStr* s,
                                                     struct Decl* d)
{
    return typestr_from_decltype(expr_seqs, tt, s, &d->ast);
}

static size_t typestr_get_size_i(struct Elaborator* elab, const struct TypeStr* ts, int i)
{
    if (ts->buf[i] == 'r') --i;
    if (ts->buf[i] == 'v') --i;
    if (ts->buf[i] == 'c') --i;
    switch (ts->buf[i])
    {
        case TYPE_BYTE_STRUCT:
        case TYPE_BYTE_UNION:
        {
            TypeSymbol* sym = tt_get(elab->types, typestr_get_offset_i(ts, i));
            if (!sym->size)
            {
                typestr_error1(NULL, elab->types, "error: unable to get size of incomplete type: %.*s\n", ts);
                return 1;
            }
            return sym->size;
        }
        case TYPE_BYTE_ENUM: return 4;
        case TYPE_BYTE_POINTER: return 8;
        case TYPE_BYTE_UUVALIST: return 8;
        case TYPE_BYTE_ULLONG:
        case TYPE_BYTE_ULONG:
        case TYPE_BYTE_LLONG:
        case TYPE_BYTE_LONG: return 8;
        case TYPE_BYTE_UINT:
        case TYPE_BYTE_INT: return 4;
        case TYPE_BYTE_USHORT:
        case TYPE_BYTE_SHORT: return 2;
        case TYPE_BYTE_VOID:
        case TYPE_BYTE_UCHAR:
        case TYPE_BYTE_SCHAR:
        case TYPE_BYTE_CHAR: return 1;
        case TYPE_BYTE_FUNCTION: return 1;
        case TYPE_BYTE_ARRAY:
        {
            uint32_t offset = typestr_get_offset_i(ts, i);
            size_t elem_size = typestr_get_size_i(elab, ts, i - sizeof(uint32_t) - 1);
            if (elem_size > UINT32_MAX) abort();
            return offset * elem_size;
        }
        default:
        {
            typestr_error1(NULL, elab->types, "error: unable to get size of type: %.*s\n", ts);
            return 1;
        }
    }
}

static size_t typestr_get_size(struct Elaborator* elab, const struct TypeStr* ts)
{
    return typestr_get_size_i(elab, ts, ts->buf[0]);
}

static size_t typestr_get_align_i(struct Elaborator* elab, const struct TypeStr* ts, int i)
{
    if (ts->buf[i] == 'r') --i;
    if (ts->buf[i] == 'v') --i;
    if (ts->buf[i] == 'c') --i;
    switch (ts->buf[i])
    {
        case TYPE_BYTE_STRUCT:
        case TYPE_BYTE_UNION:
        {
            TypeSymbol* sym = tt_get(elab->types, typestr_get_offset_i(ts, i));
            if (!sym->align)
            {
                typestr_error1(NULL, elab->types, "error: unable to get align of incomplete type: %.*s\n", ts);
                return 1;
            }
            return sym->align;
        }
        case TYPE_BYTE_ENUM: return 4;
        case TYPE_BYTE_POINTER: return 8;
        case TYPE_BYTE_UUVALIST: return 8;
        case TYPE_BYTE_ULLONG:
        case TYPE_BYTE_ULONG:
        case TYPE_BYTE_LLONG:
        case TYPE_BYTE_LONG: return 8;
        case TYPE_BYTE_UINT:
        case TYPE_BYTE_INT: return 4;
        case TYPE_BYTE_USHORT:
        case TYPE_BYTE_SHORT: return 2;
        case TYPE_BYTE_VOID:
        case TYPE_BYTE_UCHAR:
        case TYPE_BYTE_SCHAR:
        case TYPE_BYTE_CHAR: return 1;
        case TYPE_BYTE_FUNCTION: return 1;
        case TYPE_BYTE_UNK_ARRAY:
        {
            return typestr_get_align_i(elab, ts, i - 1);
        }
        case TYPE_BYTE_ARRAY:
        {
            return typestr_get_align_i(elab, ts, i - sizeof(uint32_t) - 1);
        }
        default:
        {
            typestr_error1(NULL, elab->types, "error: unable to get align of type: %.*s\n", ts);
            return 1;
        }
    }
}

static size_t typestr_get_align(struct Elaborator* elab, const struct TypeStr* ts)
{
    return typestr_get_align_i(elab, ts, ts->buf[0]);
}

static size_t typestr_get_elem_size(struct Elaborator* elab, const struct TypeStr* ts)
{
    int i = ts->buf[0];
    if (ts->buf[i] == 'r') --i;
    if (ts->buf[i] == 'v') --i;
    if (ts->buf[i] == 'c') --i;
    switch (ts->buf[i])
    {
        case TYPE_BYTE_POINTER: return typestr_get_size_i(elab, ts, i - 1);
        default:
        {
            struct Array buf = {};
            typestr_fmt(elab->types, ts, &buf);
            fprintf(stderr, "error: unable to get elem size of type: %.*s\n", (int)buf.sz, (char*)buf.data);
            array_destroy(&buf);
            return 1;
        }
    }
}

static int32_t typestr_calc_sizing(struct Elaborator* elab, const struct TypeStr* ts)
{
    size_t sz = typestr_get_size(elab, ts);
    if (sz > INT32_MAX) abort();
    if (typestr_mask(ts) & TYPE_FLAGS_SIGNED)
        return -(int32_t)sz;
    else
        return sz;
}

static struct Decl* find_field_by_name(struct DeclSpecs* def, const char* fieldname, struct Decl* const* const decls)
{
    if (!def || !def->suinit) abort();
    if (!def->first_member) return NULL;
    struct Decl* field = def->first_member;
    while (field)
    {
        if (strcmp(field->sym->name, fieldname) == 0) return field;
        field = field->sym->next_field;
    }
    return NULL;
}

static int elaborate_decl(struct Elaborator* elab, struct Decl* specs);
static int elaborate_declspecs(struct Elaborator* elab, struct DeclSpecs* specs);
static void elaborate_stmt(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, struct Ast* ast);
static void elaborate_expr(struct Elaborator* elab,
                           struct ElaborateDeclCtx* ctx,
                           struct Expr* top_expr,
                           struct TypeStr* rty);

static int is_zero_constant(struct Expr* e)
{
    if (e->kind != EXPR_LIT) return 0;
    struct ExprLit* lit = (void*)e;
    if (lit->tok->type != LEX_NUMBER) return 0;
    return lit->numeric == 0;
}

static void elaborate_binop(struct Elaborator* elab,
                            struct ElaborateDeclCtx* ctx,
                            struct ExprBinOp* e,
                            struct TypeStr* rty)
{
    elaborate_expr(elab, ctx, e->lhs, rty);
    const struct TypeStr orig_lhs = *rty;
    typestr_decay(rty);
    unsigned int lhs_mask = typestr_mask(rty);

    struct TypeStr rhs_ty;
    elaborate_expr(elab, ctx, e->rhs, &rhs_ty);
    const struct TypeStr orig_rhs = rhs_ty;
    typestr_decay(&rhs_ty);
    unsigned int rhs_mask = typestr_mask(&rhs_ty);
    // check lhs mask
    switch (e->tok->type)
    {
        case TOKEN_SYM2('<', '<'):
        case TOKEN_SYM2('>', '>'):
        case TOKEN_SYM3('<', '<', '='):
        case TOKEN_SYM3('>', '>', '='):
        case TOKEN_SYM1('|'):
        case TOKEN_SYM1('&'):
        case TOKEN_SYM1('^'):
        case TOKEN_SYM1('%'):
            if (!(lhs_mask & TYPE_FLAGS_INT))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected integer type in first argument but got '%.*s'\n",
                               &orig_lhs);
                *rty = s_type_literal_int;
            }
            break;
        case TOKEN_SYM1('/'):
        case TOKEN_SYM1('*'):
            if (!(lhs_mask & TYPE_MASK_ARITH))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected arithmetic type in first argument but got '%.*s'\n",
                               &orig_lhs);
            }
            break;
        case TOKEN_SYM2('|', '|'):
        case TOKEN_SYM2('&', '&'):
        case TOKEN_SYM2('=', '='):
        case TOKEN_SYM2('<', '='):
        case TOKEN_SYM2('>', '='):
        case TOKEN_SYM2('!', '='):
        case TOKEN_SYM1('<'):
        case TOKEN_SYM1('>'):
        case TOKEN_SYM1('['):
        case TOKEN_SYM1('+'):
        case TOKEN_SYM1('-'):
        case TOKEN_SYM2('+', '='):
        case TOKEN_SYM2('-', '='):
        case TOKEN_SYM1('?'):
            if (!(lhs_mask & TYPE_MASK_SCALAR))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected scalar type in first argument but got '%.*s'\n",
                               &orig_lhs);
                *rty = s_type_unknown;
            }
            break;
        default: break;
    }

    // check rhs mask
    switch (e->tok->type)
    {
        case TOKEN_SYM2('<', '<'):
        case TOKEN_SYM2('>', '>'):
        case TOKEN_SYM3('<', '<', '='):
        case TOKEN_SYM3('>', '>', '='):
        case TOKEN_SYM1('&'):
        case TOKEN_SYM1('|'):
        case TOKEN_SYM2('&', '='):
        case TOKEN_SYM2('|', '='):
        case TOKEN_SYM1('^'):
        case TOKEN_SYM1('%'):
            if (!(rhs_mask & TYPE_FLAGS_INT))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected integer type in second argument but got '%.*s'\n",
                               &orig_rhs);
            }
            break;
        case TOKEN_SYM1('/'):
        case TOKEN_SYM1('*'):
        case TOKEN_SYM2('*', '='):
        case TOKEN_SYM2('/', '='):
            if (!(rhs_mask & TYPE_MASK_ARITH))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected arithmetic type in second argument but got '%.*s'\n",
                               &orig_rhs);
            }
            break;
        case TOKEN_SYM2('|', '|'):
        case TOKEN_SYM2('&', '&'):
        case TOKEN_SYM2('=', '='):
        case TOKEN_SYM2('<', '='):
        case TOKEN_SYM2('>', '='):
        case TOKEN_SYM2('!', '='):
        case TOKEN_SYM1('<'):
        case TOKEN_SYM1('>'):
        case TOKEN_SYM1('['):
        case TOKEN_SYM1('+'):
        case TOKEN_SYM1('-'):
        case TOKEN_SYM2('+', '='):
        case TOKEN_SYM2('-', '='):
            if (!(rhs_mask & TYPE_MASK_SCALAR))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected scalar type in second argument but got '%.*s'\n",
                               &orig_rhs);
            }
            break;
        default: break;
    }

    switch (e->tok->type)
    {
        case TOKEN_SYM1('%'):
        case TOKEN_SYM1('/'):
        case TOKEN_SYM1('*'):
        case TOKEN_SYM1('&'):
        case TOKEN_SYM1('|'):
        case TOKEN_SYM1('^'):
        case TOKEN_SYM2('*', '='):
        case TOKEN_SYM2('&', '='):
        case TOKEN_SYM2('|', '='):
        case TOKEN_SYM2('+', '='):
        case TOKEN_SYM2('-', '='):
        case TOKEN_SYM2('<', '<'):
        case TOKEN_SYM2('>', '>'):
        case TOKEN_SYM3('<', '<', '='):
        case TOKEN_SYM3('>', '>', '='): break;
        case TOKEN_SYM1('+'):
            if (rhs_mask & lhs_mask & TYPE_FLAGS_POINTER)
            {
                typestr_error2(&e->tok->rc,
                               elab->types,
                               "error: expected only one pointer type, but got '%.*s' and '%.*s'\n",
                               &orig_lhs,
                               &orig_rhs);
            }
            if (rhs_mask & TYPE_FLAGS_POINTER)
            {
                *rty = rhs_ty;
            }
            if ((lhs_mask | rhs_mask) & TYPE_FLAGS_POINTER)
            {
                e->info = typestr_get_elem_size(elab, rty);
            }
            else
            {
                e->info = 1;
            }
            break;
        case TOKEN_SYM1('-'):
            if (lhs_mask & TYPE_FLAGS_POINTER)
            {
                e->info = typestr_get_elem_size(elab, rty);
            }
            else
            {
                e->info = 1;
            }
            if (rhs_mask & TYPE_FLAGS_POINTER)
            {
                if (!(lhs_mask & TYPE_FLAGS_POINTER))
                {
                    typestr_error2(&e->tok->rc,
                                   elab->types,
                                   "error: expected both pointer types, but got '%.*s' and '%.*s'\n",
                                   &orig_lhs,
                                   &orig_rhs);
                }
                *rty = s_type_literal_int;
                e->info = -e->info;
            }
            break;
        case TOKEN_SYM1('>'):
        case TOKEN_SYM1('<'):
        case TOKEN_SYM2('=', '='):
        case TOKEN_SYM2('!', '='):
        case TOKEN_SYM2('>', '='):
        case TOKEN_SYM2('<', '='):
        case TOKEN_SYM2('&', '&'):
        case TOKEN_SYM2('|', '|'): *rty = s_type_literal_int; break;
        case TOKEN_SYM1('='): typestr_implicit_conversion(elab->types, &e->tok->rc, &rhs_ty, rty); break;
        case TOKEN_SYM1('['):
            if (rhs_mask & lhs_mask & TYPE_FLAGS_POINTER || !((rhs_mask | lhs_mask) & TYPE_FLAGS_POINTER))
            {
                typestr_error2(&e->tok->rc,
                               elab->types,
                               "error: expected exactly one pointer type, but got '%.*s' and '%.*s'\n",
                               &orig_lhs,
                               &orig_rhs);
            }
            if (rhs_mask & TYPE_FLAGS_POINTER)
            {
                // normalize operation so pointer is always the LHS
                struct Expr* f = e->lhs;
                e->lhs = e->rhs;
                e->rhs = f;
                *rty = rhs_ty;
            }
            typestr_dereference(rty);
            e->info = typestr_get_size(elab, rty);
            break;
        case TOKEN_SYM1(','):
        case TOKEN_SYM1('?'): *rty = rhs_ty; break;
        case TOKEN_SYM1(':'):
            if (rhs_mask & lhs_mask & TYPE_MASK_ARITH)
            {
            }
            else if (typestr_match(&orig_lhs, &orig_rhs))
            {
            }
            else if (typestr_is_pointer(rty) && typestr_is_pointer(&rhs_ty))
            {
                --rty->buf[0];
                --rhs_ty.buf[0];
                unsigned int combined_cvr = typestr_strip_cvr(rty) | typestr_strip_cvr(&rhs_ty);
                if (typestr_match(rty, &s_void))
                {
                    *rty = rhs_ty;
                }
                if (typestr_match(&rhs_ty, &s_void) || typestr_match(rty, &rhs_ty))
                {
                    typestr_add_cvr(rty, combined_cvr);
                    typestr_add_pointer(rty);
                }
                else
                {
                    typestr_error2(
                        &e->tok->rc,
                        elab->types,
                        "error: unable to determine common pointer type in ternary between '%.*s' and '%.*s'\n",
                        &orig_lhs,
                        &orig_rhs);
                    *rty = s_type_unknown;
                }
            }
            else if (typestr_is_pointer(rty) && is_zero_constant(e->rhs))
            {
            }
            else if (typestr_is_pointer(&rhs_ty) && is_zero_constant(e->lhs))
            {
                *rty = rhs_ty;
            }
            else
            {
                typestr_error2(&e->tok->rc,
                               elab->types,
                               "error: unable to determine common type in ternary between '%.*s' and '%.*s'\n",
                               &orig_lhs,
                               &orig_rhs);
                *rty = s_type_unknown;
            }
            break;
        default:
            fprintf(stderr, "warning: untyped binary operator '%s'\n", token_str(elab->p, e->tok));
            *rty = s_type_unknown;
            break;
    }
}

static void elaborate_builtin(struct Elaborator* elab,
                              struct ElaborateDeclCtx* ctx,
                              struct ExprBuiltin* e,
                              struct TypeStr* rty)
{
    switch (e->tok->type)
    {
        case LEX_SIZEOF:
            if (e->expr1)
            {
                elaborate_expr(elab, ctx, e->expr1, rty);
            }
            else
            {
                elaborate_declspecs(elab, e->specs);
                elaborate_decl(elab, e->type);
                typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, e->type);
            }
            e->sizeof_size = typestr_get_size(elab, rty);
            *rty = s_type_literal_int;
            break;
        case LEX_UUVA_START:
            elaborate_expr(elab, ctx, e->expr2, rty);
            elaborate_expr(elab, ctx, e->expr1, rty);
            break;
        case LEX_UUVA_ARG:
            elaborate_expr(elab, ctx, e->expr1, rty);
            elaborate_declspecs(elab, e->specs);
            elaborate_decl(elab, e->type);
            typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, e->type);
            *rty = s_type_void;
            break;
        case LEX_UUVA_END: *rty = s_type_void; break;
        case LEX_UUVA_COPY:
            elaborate_expr(elab, ctx, e->expr2, rty);
            elaborate_expr(elab, ctx, e->expr1, rty);
            *rty = s_type_void;
            break;
        default:
            parser_tok_error(e->tok, "error: unimplemented builtin\n");
            *rty = s_type_unknown;
            break;
    }
}

static void elaborate_unop(struct Elaborator* elab,
                           struct ElaborateDeclCtx* ctx,
                           struct ExprUnOp* e,
                           struct TypeStr* rty)
{
    elaborate_expr(elab, ctx, e->lhs, rty);
    const struct TypeStr orig_lhs = *rty;
    typestr_decay(rty);
    unsigned int lhs_mask = typestr_mask(rty);
    switch (e->tok->type)
    {
        case TOKEN_SYM1('-'):
            if (!(lhs_mask & TYPE_MASK_ARITH))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected arithmetic type in first argument but got '%.*s'\n",
                               &orig_lhs);
                *rty = s_type_literal_int;
            }
            break;
        case TOKEN_SYM1('*'):
            if (typestr_is_pointer(rty))
            {
                typestr_dereference(rty);
            }
            else
            {
                typestr_error1(&e->tok->rc, elab->types, "error: cannot dereference value of type '%.*s'\n", rty);
                *rty = s_type_unknown;
            }
            break;
        case TOKEN_SYM1('&'):
        {
            *rty = orig_lhs;
            typestr_add_pointer(rty);
            break;
        }
        case TOKEN_SYM2('+', '+'):
        case TOKEN_SYM2('-', '-'): break;
        case TOKEN_SYM1('~'):
            if (!(lhs_mask & TYPE_FLAGS_INT))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected integer type in first argument but got '%.*s'\n",
                               &orig_lhs);
            }
            *rty = s_type_literal_int;
            break;
        case TOKEN_SYM1('!'):
            if (!(lhs_mask & TYPE_MASK_SCALAR))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected scalar type in first argument but got '%.*s'\n",
                               &orig_lhs);
            }
            *rty = s_type_literal_int;
            break;
        default:
            fprintf(stderr, "warning: untyped unary operator '%s'\n", token_str(elab->p, e->tok));
            *rty = s_type_unknown;
            break;
    }
}
static void elaborate_stmts(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, size_t offset, size_t extent)
{
    struct Ast** seqs = elab->p->expr_seqs.data;
    for (size_t i = 0; i < extent; ++i)
    {
        elaborate_stmt(elab, ctx, seqs[offset + i]);
    }
}

static void elaborate_init_ty(struct Elaborator* elab, size_t offset, const TypeStr* dty, struct Ast* ast);
static void elaborate_init(struct Elaborator* elab, size_t offset, struct Decl* decl, struct Ast* ast)
{
    struct TypeStr dty;
    typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, &dty, decl);
    typestr_strip_cvr(&dty);
    elaborate_init_ty(elab, offset, &dty, ast);
}

typedef struct DInitFrame
{
    size_t offset;
    Decl* field;
    uint8_t is_array : 1;
    uint8_t is_unk : 1;
    uint8_t is_union : 1;
    uint32_t extent;
    uint32_t index;
    uint32_t elem_size;
    /// Always cvr-stripped
    struct TypeStr ty;
} DInitFrame;

typedef struct DInitIter
{
    size_t offset;
    TypeStr cur_ty;
    /// Array<DInitFrame>
    Array stk;
} DInitIter;

static void di_init(DInitIter* i) { memset(i, 0, sizeof(DInitIter)); }
static void di_destroy(DInitIter* i) { array_destroy(&i->stk); }

static int di_fill_frame(DInitFrame* frame,
                         Elaborator* elab,
                         size_t offset,
                         const TypeStr* parent_ty,
                         size_t designator_idx,
                         const RowCol* rc)
{
    memset(frame, 0, sizeof(*frame));
    char b = typestr_byte(parent_ty);
    switch (b)
    {
        case TYPE_BYTE_UNK_ARRAY:
        case TYPE_BYTE_ARRAY:
        {
            frame->is_array = 1;
            frame->ty = *parent_ty;
            typestr_strip_cvr(&frame->ty);
            if (b == TYPE_BYTE_UNK_ARRAY)
            {
                --frame->ty.buf[0];
                frame->extent = UINT32_MAX;
            }
            else
            {
                frame->extent = typestr_pop_offset(&frame->ty);
            }
            typestr_strip_cvr(&frame->ty);
            frame->elem_size = typestr_get_size(elab, &frame->ty);
            if (frame->extent == 0)
            {
                return parser_ferror(rc, "error: array must have nonzero extent\n");
            }
            if (designator_idx != SIZE_MAX)
            {
                const Designator* const designator = (const Designator*)elab->p->designators.data + designator_idx;

                if (!designator->array_expr)
                {
                    return parser_ferror(rc, "error: invalid member designator for array object\n");
                }
                int32_t k = eval_constant(elab, designator->array_expr);
                if (k >= frame->extent)
                {
                    return parser_ferror(
                        rc, "error: array designator exceeds bounds: '%d' >= '%zu'\n", k, frame->extent);
                }
                frame->index = k;
            }
            frame->offset = offset + frame->elem_size * frame->index;
            return 0;
        }
        case TYPE_BYTE_UNION: frame->is_union = 1;
        case TYPE_BYTE_STRUCT:
        {
            TypeSymbol* sym = typestr_get_decl(elab->types, parent_ty);
            if (!sym || !sym->def)
            {
                return parser_ferror(rc, "error: incomplete type\n");
            }
            if (designator_idx == SIZE_MAX)
            {
                frame->field = sym->def->first_member;
            }
            else
            {
                const Designator* const designator = (const Designator*)elab->p->designators.data + designator_idx;

                if (!designator->field)
                {
                    return parser_ferror(rc, "error: invalid array designator for struct/union\n");
                }
                frame->field = find_field_by_name(sym->def, designator->field, elab->p->expr_seqs.data);
                if (!frame->field)
                {
                    return parser_ferror(rc, "error: field not found in structure: '%s'\n", designator->field);
                }
            }
            frame->offset = offset + frame->field->sym->frame_offset;
            typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, &frame->ty, frame->field);
            typestr_strip_cvr(&frame->ty);
            return 0;
        }
        default: return parser_ferror(rc, "error: unimplemented type for initializer list\n");
    }
}

static int di_enter(DInitIter* i, struct Elaborator* elab, const RowCol* rc)
{
    if (0 == i->stk.sz)
    {
        return di_fill_frame(array_alloc(&i->stk, sizeof(DInitFrame)), elab, i->offset, &i->cur_ty, SIZE_MAX, rc);
    }
    DInitFrame* new_frame = array_alloc(&i->stk, sizeof(DInitFrame));
    DInitFrame* prev_frame = new_frame - 1;
    return di_fill_frame(new_frame, elab, prev_frame->offset, &prev_frame->ty, SIZE_MAX, rc);
}

static int di_reset(DInitIter* i, struct Elaborator* elab, size_t offset, const struct TypeStr* dty, const RowCol* rc)
{
    i->offset = offset;
    i->cur_ty = *dty;
    array_clear(&i->stk);
    return di_enter(i, elab, rc);
}

static void di_next(DInitIter* i, Elaborator* elab)
{
    goto loop;
pop:
    array_pop(&i->stk, sizeof(DInitFrame));
loop:;
    if (0 == i->stk.sz) return;
    DInitFrame* back = array_back(&i->stk, sizeof(DInitFrame));
    if (back->is_array)
    {
        ++back->index;
        if (back->index == back->extent) goto pop;
        back->offset += back->elem_size;
    }
    else
    {
        const size_t prev_field_offset = back->field->sym->frame_offset;
        if (back->is_union)
            back->field = NULL;
        else
            back->field = back->field->sym->next_field;
        if (back->field == NULL) goto pop;
        back->offset += back->field->sym->frame_offset - prev_field_offset;
        typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, &back->ty, back->field);
        typestr_strip_cvr(&back->ty);
    }
}

static int di_end(DInitIter* i) { return 0 == i->stk.sz; }

static void elaborate_init_ty_AstInit(struct Elaborator* elab, size_t offset, const TypeStr* dty, struct AstInit* init)
{
    struct DInitIter iter;
    di_init(&iter);
    if (di_reset(&iter, elab, offset, dty, &init->tok->rc)) goto fail;

    for (; init->init; init = init->next, di_next(&iter, elab))
    {
        if (init->designator_extent != 0)
        {
            array_clear(&iter.stk);
            if (di_fill_frame(array_alloc(&iter.stk, sizeof(DInitFrame)),
                              elab,
                              iter.offset,
                              &iter.cur_ty,
                              init->designator_offset,
                              &init->tok->rc))
                goto fail;
            for (size_t k = 1; k < init->designator_extent; ++k)
            {
                DInitFrame* f = array_alloc(&iter.stk, sizeof(DInitFrame));
                if (di_fill_frame(f, elab, f[-1].offset, &f[-1].ty, init->designator_offset + k, &init->tok->rc))
                    goto fail;
            }
        }
        if (di_end(&iter)) break;
        DInitFrame* back = array_back(&iter.stk, sizeof(*back));
        if (init->init->kind == AST_INIT)
        {
            elaborate_init_ty_AstInit(elab, back->offset, &back->ty, (AstInit*)init->init);
        }
        else
        {
            if (!ast_kind_is_expr(init->init->kind))
            {
                parser_tok_error(init->init->tok, "error: expected expression in object initialization\n");
                goto fail;
            }
            Expr* expr = (Expr*)init->init;
            // standard expression initialization
            struct TypeStr ts;
            elaborate_expr(elab, NULL, expr, &ts);
            typestr_decay(&ts);

            while (typestr_is_aggregate(&back->ty))
            {
                if (typestr_match(&back->ty, &ts)) break;
                if (di_enter(&iter, elab, &init->tok->rc)) goto fail;
                back = array_back(&iter.stk, sizeof(*back));
            }
            typestr_implicit_conversion(elab->types, &init->init->tok->rc, &ts, &back->ty);
            init->offset = back->offset;
            init->sizing = typestr_calc_sizing(elab, &back->ty);
        }
    }

    if (di_end(&iter) && init->init)
    {
        parser_tok_error(init->tok, "error: too many initializers\n");
        goto fail;
    }

fail:
    di_destroy(&iter);
}

static void elaborate_init_ty(struct Elaborator* elab, size_t offset, const TypeStr* dty, struct Ast* ast)
{
    if (ast->kind == AST_INIT)
    {
        return elaborate_init_ty_AstInit(elab, offset, dty, (struct AstInit*)ast);
    }
    const char tyb = typestr_byte(dty);
    switch (tyb)
    {
        case TYPE_BYTE_FUNCTION: elaborate_stmt(elab, NULL, ast); break;
        case TYPE_BYTE_ARRAY:
        case TYPE_BYTE_UNK_ARRAY:
        {
            TypeStr ts = *dty;
            typestr_remove_array(&ts);
            typestr_strip_cvr(&ts);
            if (!typestr_match(&ts, &s_type_mutable_char))
            {
                parser_tok_error(ast->tok, "error: array initializer must be an initializer list\n");
                break;
            }
            if (ast->kind != EXPR_LIT || ast->tok->type != LEX_STRING)
            {
                parser_tok_error(ast->tok,
                                 "error: array initializer must be an initializer list or a string literal\n");
                break;
            }
            elaborate_expr(elab, NULL, (Expr*)ast, &ts);
            break;
        }
        case TYPE_BYTE_UNION:
        case TYPE_BYTE_STRUCT:
        default:
        {
            if (!ast_kind_is_expr(ast->kind))
            {
                parser_tok_error(ast->tok, "error: expected expression in object initialization\n");
                break;
            }
            Expr* expr = (Expr*)ast;
            // standard expression initialization
            struct TypeStr ts;
            elaborate_expr(elab, NULL, expr, &ts);
            typestr_implicit_conversion(elab->types, &ast->tok->rc, &ts, dty);
        }
    }
}

#define DISPATCH(X, Y)                                                                                                 \
    case X: return elaborate_stmt_##Y(elab, ctx, (struct Y*)ast)

static void elaborate_stmt_StmtCase(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, struct StmtCase* stmt)
{
    if (stmt->expr) stmt->value = eval_constant(elab, stmt->expr);
}

static void elaborate_stmt(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, struct Ast* ast)
{
    if (ast_kind_is_expr(ast->kind))
    {
        struct TypeStr ts;
        return elaborate_expr(elab, ctx, (struct Expr*)ast, &ts);
    }
    void* top = ast;
    switch (ast->kind)
    {
        DISPATCH(STMT_CASE, StmtCase);
        case STMT_NONE:
        case STMT_BREAK:
        case STMT_CONTINUE:
        case STMT_GOTO: return;
        case STMT_LABEL:
        {
            struct StmtLabel* expr = top;
            return elaborate_stmt(elab, ctx, expr->stmt);
        }
        case STMT_RETURN:
        {
            struct StmtReturn* stmt = top;
            if (stmt->expr)
            {
                struct TypeStr ts;
                elaborate_expr(elab, ctx, stmt->expr, &ts);
            }

            return;
        }
        case STMT_IF:
        {
            struct StmtIf* stmt = top;
            struct TypeStr ts;
            elaborate_expr(elab, ctx, stmt->cond, &ts);
            elaborate_stmt(elab, ctx, stmt->if_body);
            if (stmt->else_body)
            {
                elaborate_stmt(elab, ctx, stmt->else_body);
            }
            return;
        }
        case STMT_LOOP:
        {
            struct StmtLoop* e = top;
            struct TypeStr ts;
            elaborate_stmt(elab, ctx, e->body);
            if (e->advance) elaborate_expr(elab, ctx, e->advance, &ts);
            if (e->cond) elaborate_expr(elab, ctx, e->cond, &ts);
            if (e->init) elaborate_stmt(elab, ctx, e->init);
            return;
        }
        case AST_DECL:
        {
            struct Decl* d = top;
            if (!d->type) abort();
            elaborate_decl(elab, d);

            // typestr_from_decltype(elab->p->expr_seqs.data, elab->types, rty, d->type);
            // TODO: ensure valid initialization
            // typestr_implicit_conversion(elab->types, expr_to_rc(d->init), &init_ty, rty);
            return;
        }
        case STMT_DECLS:
        {
            struct StmtDecls* stmt = top;
            elaborate_declspecs(elab, stmt->specs);
            elaborate_stmts(elab, ctx, stmt->offset, stmt->extent);
            return;
        }
        case STMT_BLOCK:
        {
            struct StmtBlock* stmt = top;
            elaborate_stmts(elab, ctx, stmt->offset, stmt->extent);
            return;
        }
        case STMT_SWITCH:
        {
            struct StmtSwitch* stmt = top;
            struct TypeStr ts;
            elaborate_expr(elab, ctx, stmt->expr, &ts);
            typestr_decay(&ts);
            if (!(typestr_mask(&ts) & TYPE_MASK_SCALAR))
            {
                typestr_error1(&stmt->tok->rc,
                               elab->types,
                               "error: expected scalar type in switch condition but got '%.*s'\n",
                               &ts);
            }
            elaborate_stmts(elab, ctx, stmt->offset, stmt->extent);
            return;
        }
        default: parser_tok_error(NULL, "error: unknown stmt kind: %s\n", ast_kind_to_string(ast->kind)); return;
    }
}

static void elaborate_expr(struct Elaborator* elab,
                           struct ElaborateDeclCtx* ctx,
                           struct Expr* top_expr,
                           struct TypeStr* rty)
{
    void* top = top_expr;
    switch (top_expr->kind)
    {
        case EXPR_LIT:
        {
            struct ExprLit* expr = top;
            if (expr->tok->type == LEX_NUMBER)
                *rty = s_type_literal_int;
            else if (expr->tok->type == LEX_CHARLIT)
                *rty = s_type_literal_int;
            else if (expr->tok->type == LEX_STRING)
            {
                *rty = s_type_mutable_char;
                typestr_add_array(rty, strlen(expr->text) + 1);
            }
            else
            {
                parser_tok_error(expr->tok, "error: unknown literal type: %s\n", lexstate_to_string(expr->tok->type));
                *rty = s_type_unknown;
            }
            break;
        }

        case EXPR_REF:
        {
            struct ExprRef* esym = top;
            typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, esym->sym->last_decl);
            typestr_decay(rty);
            break;
        }
        case EXPR_CAST:
        {
            struct ExprCast* expr = top;
            elaborate_expr(elab, ctx, expr->expr, rty);
            elaborate_declspecs(elab, expr->specs);
            elaborate_decl(elab, expr->type);
            typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, expr->type);
            break;
        }
        case EXPR_CALL:
        {
            struct ExprCall* expr = top;
            elaborate_expr(elab, ctx, expr->fn, rty);
            struct TypeStr orig_fty = *rty;

            size_t args_fn_offset = 0;
            size_t args_fn_extent = 0;
            size_t is_variadic = 0;
            if (!typestr_is_fn(rty))
            {
                typestr_decay(rty);
                if (typestr_is_pointer(rty))
                {
                    --rty->buf[0]; // pop ptr
                }
            }

            if (typestr_is_fn(rty))
            {
                uint32_t x = typestr_pop_offset(rty);
                if (x == UINT32_MAX) abort();
                if (x > 0) args_fn_offset = arrsz_at(&elab->types->fn_args_ends, x - 1);
                args_fn_extent = arrsz_at(&elab->types->fn_args_ends, x) - args_fn_offset;
                if (args_fn_extent > 0)
                {
                    is_variadic = typestr_is_variadic((struct TypeStr*)elab->types->fn_args.data + args_fn_offset +
                                                      args_fn_extent - 1);
                    args_fn_extent -= is_variadic;
                }
                if (expr->extent < args_fn_extent || !is_variadic && expr->extent > args_fn_extent)
                {
                    parser_tok_error(expr->tok,
                                     "error: too many arguments in function call: got %zu but expected %zu\n",
                                     expr->extent,
                                     args_fn_extent);
                }
            }
            else
            {
                typestr_error1(
                    &expr->tok->rc, elab->types, "error: expected function type but got '%.*s'\n", &orig_fty);
            }

            struct Expr** exprs = elab->p->expr_seqs.data;
            struct TypeStr arg_expr_ty;
            for (size_t i = 0; i < expr->extent; ++i)
            {
                struct Expr* arg_expr = exprs[expr->offset + i];
                elaborate_expr(elab, ctx, arg_expr, &arg_expr_ty);
                struct TypeStr orig_arg_expr_ty = arg_expr_ty;
                typestr_decay(&arg_expr_ty);
                if (i < args_fn_extent)
                {
                    const struct TypeStr* orig_tt_arg = (struct TypeStr*)elab->types->fn_args.data + i + args_fn_offset;
                    typestr_implicit_conversion(elab->types, &arg_expr->tok->rc, &arg_expr_ty, orig_tt_arg);
                    ((struct ParamConversion*)elab->param_conversions.data + i + args_fn_offset)->sizing =
                        typestr_calc_sizing(elab, orig_tt_arg);
                }
                else
                {
                    // varargs
                    unsigned int lhs_mask = typestr_mask(&arg_expr_ty);
                    if (!(lhs_mask & TYPE_MASK_SCALAR))
                    {
                        typestr_error1(&arg_expr->tok->rc,
                                       elab->types,
                                       "error: expected scalar type in variadic arguments but got '%.*s'\n",
                                       &orig_arg_expr_ty);
                    }
                }
            }
            break;
        }
        case EXPR_FIELD:
        {
            struct ExprField* e = top;
            elaborate_expr(elab, ctx, e->lhs, rty);
            const struct TypeStr orig_lhs = *rty;
            if (e->is_arrow) typestr_dereference(rty);
            unsigned int cvr_mask = typestr_strip_cvr(rty);
            TypeSymbol* sym = typestr_get_decl(elab->types, rty);
            if (sym)
            {
                if (sym->def)
                {
                    // find field in decl
                    struct Decl* field =
                        find_field_by_name(sym->def, e->fieldname, (struct Decl* const*)elab->p->expr_seqs.data);
                    if (field)
                    {
                        e->sym = field->sym;
                        typestr_from_decltype(elab->p->expr_seqs.data, elab->types, rty, field->type);
                        typestr_add_cvr(rty, cvr_mask);
                    }
                    else
                    {
                        struct Array buf = {0};
                        typestr_fmt(elab->types, rty, &buf);
                        array_push_byte(&buf, 0);
                        parser_tok_error(
                            e->tok, "error: could not find member '%s' in type '%s'\n", e->fieldname, buf.data);
                        array_destroy(&buf);
                        *rty = s_type_unknown;
                    }
                }
                else
                {
                    typestr_error1(
                        &e->tok->rc, elab->types, "error: first argument was of incomplete type %.*s\n", rty);
                    *rty = s_type_unknown;
                }
            }
            else
            {
                const char* err_fmt;
                if (e->is_arrow)
                    err_fmt = "error: expected first argument to be pointer to struct or union type, but got "
                              "'%.*s'\n";
                else
                    err_fmt = "error: expected first argument to be of struct or union type, but got '%.*s'\n";
                typestr_error1(&e->tok->rc, elab->types, err_fmt, &orig_lhs);
                *rty = s_type_unknown;
            }
            break;
        }
        case EXPR_BINOP: elaborate_binop(elab, ctx, top, rty); break;
        case EXPR_UNOP: elaborate_unop(elab, ctx, top, rty); break;
        case EXPR_BUILTIN: elaborate_builtin(elab, ctx, top, rty); break;
        default: parser_tok_error(NULL, "error: unknown expr kind: %s\n", ast_kind_to_string(top_expr->kind)); return;
    }

    top_expr->sizing = typestr_calc_sizing(elab, rty);
}

static __forceinline size_t round_to_alignment(size_t size, size_t align)
{
    size_t n = size + align - 1;
    return n - (n % align);
}

static void elaborate_decltype(struct Elaborator* elab, struct Ast* ast)
{
    switch (ast->kind)
    {
        case AST_DECLARR:
        {
            struct DeclArr* arr = (void*)ast;
            elaborate_decltype(elab, arr->type);
            if (arr->arity != NULL)
            {
                arr->integer_arity = eval_constant(elab, arr->arity);
            }
            break;
        }
        case AST_DECLFN:
        {
            struct DeclFn* fn = (void*)ast;
            elaborate_decltype(elab, fn->type);
            elaborate_stmts(elab, NULL, fn->offset, fn->extent);
            break;
        }
        case AST_DECLPTR:
        {
            struct DeclPtr* ptr = (void*)ast;
            elaborate_decltype(elab, ptr->type);
            break;
        }
        case AST_DECLSPEC: /* already elaborated */ break;
        default: parser_tok_error(ast->tok, "error: unimplemented decltype.\n"); break;
    }
}

static int elaborate_decl(struct Elaborator* elab, struct Decl* decl)
{
    int rc = 0;
    decl->elaborated = 1;
    elaborate_decltype(elab, decl->type);
    Symbol* const sym = decl->sym;
    if (!sym)
    {
        rc = parser_has_errors();
        goto fail;
    }
    if (!decl->prev_decl)
    {
        typestr_from_decltype(elab->p->expr_seqs.data, elab->types, &sym->type, decl->type);
    }
    if (sym->def == decl)
    {
        // Refresh symbol's type with concrete definition information
        typestr_from_decltype(elab->p->expr_seqs.data, elab->types, &sym->type, decl->type);

        if (!decl->specs->is_typedef)
        {
            const char tyb = typestr_byte(&sym->type);
            if (tyb == TYPE_BYTE_FUNCTION)
            {
                if (!decl->init) abort();
                if (decl->type->kind != AST_DECLFN) abort();
                struct DeclFn* fn = (void*)decl->type;
                uint32_t f_offset = typestr_get_offset(&sym->type);
                size_t begin = f_offset ? arrsz_at(&elab->types->fn_args_ends, f_offset - 1) : 0;
                size_t end = arrsz_at(&elab->types->fn_args_ends, f_offset);
                struct ParamConversion* paramcvs = elab->param_conversions.data;
                if (end - begin < fn->extent) abort();
                for (size_t i = 0; i < fn->extent; ++i)
                {
                    paramcvs[fn->offset + i].sizing =
                        typestr_calc_sizing(elab, (struct TypeStr*)elab->types->fn_args.data + begin + i);
                }
                TypeStr ts = sym->type;
                typestr_pop_offset(&ts);
                sym->fn_ret_sizing = typestr_calc_sizing(elab, &ts);
            }
            if (decl->init)
            {
                elaborate_init(elab, 0, decl, decl->init);
            }
            if (tyb == TYPE_BYTE_UNK_ARRAY)
            {
                const size_t elem_size = typestr_get_size_i(elab, &sym->type, sym->type.buf[0] - 1);
                if (!decl->init)
                {
                    UNWRAP(parser_tok_error(
                        decl->tok, "error: definition of object with unknown array bounds must have an initializer\n"));
                }
                else if (decl->init->kind == AST_INIT)
                {
                    uint32_t max_assign = 0;
                    for (AstInit* i = (AstInit*)decl->init; i->init; i = i->next)
                    {
                        uint32_t new_max = i->offset / elem_size;
                        if (new_max > max_assign) max_assign = new_max;
                    }
                    if (max_assign == UINT32_MAX) abort();
                    --sym->type.buf[0];
                    typestr_append_offset(&sym->type, max_assign + 1, TYPE_BYTE_ARRAY);
                }
                else if (decl->init->kind == EXPR_LIT)
                {
                    struct ExprLit* l = (void*)decl->init;
                    --sym->type.buf[0];
                    typestr_append_offset(&sym->type, strlen(l->text) + 1, TYPE_BYTE_ARRAY);
                }
                else
                {
                    UNWRAP(parser_tok_error(
                        decl->init->tok,
                        "error: array initializer must be either a string literal or an initializer list\n"));
                }
            }

            sym->size = typestr_get_size(elab, &sym->type);
            sym->align = typestr_get_align(elab, &sym->type);

            if (sym->size == 0)
            {
                /* type may be incomplete */
                if (decl->specs->is_extern)
                {
                    /* it's extern -- OK */
                }
                else if (sym->arg_index > 0 && !((struct Decl*)decl->specs->parent)->init)
                {
                    /* arg of function prototype -- OK */
                }
                else
                {
                    parser_tok_error(decl->tok, "error: definition of object with incomplete size\n");
                    return 0;
                }
            }
        }
    }
    UNWRAP(parser_has_errors());

fail:
    return rc;
}

static int elaborate_declspecs(struct Elaborator* elab, struct DeclSpecs* specs)
{
    int rc = 0;
    specs->elaborated = 1;
    if (specs->_typedef)
    {
    }
    else if (specs->is_enum || specs->is_union || specs->is_struct)
    {
        if (!specs->prev_decl)
        {
            specs->sym->idx = array_size(&elab->types->typesyms, sizeof(void*));
            arrptr_push(&elab->types->typesyms, specs->sym);
            if (specs->is_enum)
            {
                specs->sym->size = 4;
                specs->sym->align = 4;
            }
        }

        if (specs->enum_init || specs->suinit)
        {
            if (specs->sym->def)
            {
                parser_tok_error(specs->tok, "error: multiple definitions of type.\n");
                UNWRAP(parser_tok_error(specs->sym->def->tok, "info: previous definition\n"));
            }
            specs->sym->def = specs;
        }

        if (specs->enum_init)
        {
            struct StmtDecls* block = specs->enum_init;
            struct Expr** const seqs = elab->p->expr_seqs.data;
            int enum_value = 0;
            for (size_t i = 0; i < block->extent; ++i)
            {
                if (seqs[i + block->offset]->kind != AST_DECL) abort();
                struct Decl* edecl = (struct Decl*)seqs[i + block->offset];
                if (edecl->init)
                {
                    if (!ast_kind_is_expr(edecl->init->kind))
                    {
                        UNWRAP(parser_tok_error(edecl->init->tok, "error: expected constant integer expression\n"));
                    }
                    edecl->sym->enum_value = enum_value = eval_constant(elab, (struct Expr*)edecl->init);
                }
                else
                {
                    edecl->sym->enum_value = enum_value++;
                }
                edecl->sym->is_enum_constant = 1;
            }
        }
        else if (specs->suinit)
        {
            // struct/union definition
            struct StmtBlock* block = specs->suinit;

            size_t struct_align = 1;
            size_t struct_size = 0;

            struct Decl** p_next_decl = &specs->first_member;

            struct Ast** const seqs = elab->p->expr_seqs.data;
            for (size_t i = 0; i < block->extent; ++i)
            {
                if (seqs[i + block->offset]->kind != STMT_DECLS) abort();
                struct StmtDecls* decls = (struct StmtDecls*)seqs[i + block->offset];
                UNWRAP(elaborate_declspecs(elab, decls->specs));
                if (decls->extent == 0 && !decls->specs->name)
                {
                    // nested anonymous struct/union
                    *p_next_decl = decls->specs->first_member;
                    while ((*p_next_decl)->sym->next_field)
                    {
                        p_next_decl = &(*p_next_decl)->sym->next_field;
                    }
                }
                for (size_t j = 0; j < decls->extent; ++j)
                {
                    if (seqs[decls->offset + j]->kind != AST_DECL) abort();
                    struct Decl* field = (struct Decl*)seqs[decls->offset + j];
                    UNWRAP(elaborate_decl(elab, field));
                    *p_next_decl = field;
                    p_next_decl = &field->sym->next_field;
                    if (field->type || !field->sym->name)
                    {
                        if (field->init && field->sym->name)
                        {
                            return parser_tok_error(field->tok,
                                                    "error: structure and union fields cannot have initializers\n");
                        }
                        // insert padding
                        struct_size = round_to_alignment(struct_size, field->sym->align);
                        if (specs->is_struct)
                        {
                            field->sym->frame_offset = struct_size;
                            struct_size += field->sym->size;
                        }
                        else
                        {
                            field->sym->frame_offset = 0;
                            if (field->sym->size > struct_size) struct_size = field->sym->size;
                        }
                        if (struct_align < field->sym->align) struct_align = field->sym->align;
                    }
                }
            }

            if (struct_size == 0) struct_size = 1;
            struct_size = round_to_alignment(struct_size, struct_align);
            specs->sym->align = struct_align;
            specs->sym->size = struct_size;
        }
    }

fail:
    return rc;
}

void elaborator_init(struct Elaborator* elab, struct Parser* p)
{
    memset(elab, 0, sizeof(struct Elaborator));
    elab->p = p;
    elab->types = my_malloc(sizeof(struct TypeTable));
    memset(elab->types, 0, sizeof(struct TypeTable));
}

int elaborate(struct Elaborator* elab)
{
    struct Parser* const p = elab->p;
    if (elab->param_conversions.sz != 0) abort();
    array_push_zeroes(&elab->param_conversions,
                      sizeof(struct ParamConversion) * array_size(&p->expr_seqs, sizeof(void*)));
    elaborate_stmt(elab, NULL, &p->top->ast);
    return parser_has_errors();
}

static void tt_destroy(struct TypeTable* tt) { array_destroy(&tt->typesyms); }
void elaborator_destroy(struct Elaborator* elab)
{
    tt_destroy(elab->types);
    my_free(elab->types);
}
