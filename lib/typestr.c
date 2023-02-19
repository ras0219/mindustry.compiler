#include "typestr.h"

#include "array.h"
#include "ast.h"
#include "errors.h"
#include "lexstate.h"
#include "memory.h"
#include "stdint.h"
#include "stdlib.h"
#include "stdlibe.h"
#include "symbol.h"
#include "token.h"

typedef struct TypeTable
{
    Array typesyms;
    Array fn_args_ends;
    Array fn_args;
} TypeTable;

static __forceinline uint32_t tsb_get_offset_i(const TypeStrBuf* ts, int i)
{
    uint32_t ret = UINT32_MAX;
    if (i >= 1 + sizeof(ret))
    {
        memcpy(&ret, ts->buf + i - sizeof(ret), sizeof(ret));
    }
    return ret;
}

static __forceinline unsigned int tsb_skip_cvr_i(const TypeStrBuf* ts, int* p_i)
{
    int i = *p_i;
    unsigned int m = 0;
    if (ts->buf[i] == TYPE_BYTE_RESTRICT) --i, m |= TYPESTR_CVR_R;
    if (ts->buf[i] == TYPE_BYTE_VOLATILE) --i, m |= TYPESTR_CVR_V;
    if (ts->buf[i] == TYPE_BYTE_CONST) --i, m |= TYPESTR_CVR_C;
    *p_i = i;
    return m;
}

__forceinline static TypeSymbol* tt_get(const TypeTable* tt, unsigned int i)
{
    return ((TypeSymbol**)tt->typesyms.data)[i];
}
unsigned int tt_register(TypeTable* tt, struct TypeSymbol* tsym)
{
    unsigned int ret = array_size(&tt->typesyms, sizeof(void*));
    arrptr_push(&tt->typesyms, tsym);
    return ret;
}

void constant_load_lvalue(Constant* c)
{
    if (c->is_const)
    {
        if (c->is_lvalue)
        {
            if (mp_is_nonzero(c->value))
            {
                *c = s_not_constant;
            }
            else
            {
                *c = c->sym->const_init;
            }
        }
    }
}
void constant_addressof(Constant* c)
{
    if (c->is_const)
    {
        if (c->is_lvalue)
            c->is_lvalue = 0;
        else
            *c = s_not_constant;
    }
}

void typestr_promote_integer(TypeStr* rty)
{
    if (typestr_mask(rty) & TYPE_FLAGS_PROMOTE_INT)
    {
        typestr_apply_integral_type(rty, &s_type_int);
    }
}

void typestr_implicit_conversion2(TypeTable* types,
                                  const struct RowCol* rc,
                                  const TypeStr* orig_from,
                                  const TypeStrBuf* orig_to)
{
    if (tsb_match(&orig_from->buf, orig_to)) return;

    // first, value transformations
    TypeStr from = *orig_from;
    TypeStrBuf to = *orig_to;
    typestr_decay(&from);
    tsb_strip_cvr(&to);
    if (tsb_match(&from.buf, &to)) return;

    // then, check secondary conversions
    const unsigned from_mask = typestr_mask(&from), to_mask = tsb_mask(&to);
    if (from_mask & to_mask & TYPE_FLAGS_POINTER)
    {
        --from.buf.buf[0];
        --to.buf[0];
        unsigned int cvr_from = typestr_strip_cvr(&from);
        unsigned int cvr_to = tsb_strip_cvr(&to);
        if ((cvr_from & cvr_to) == cvr_from)
        {
            // cvr_to contains cvr_from
            if (tsb_match(&to, &from.buf)) return;
            if (tsb_match(&s_type_void.buf, &to)) return;
            if (tsb_match(&s_type_void.buf, &from.buf)) return;
        }
    }
    else if (from_mask & TYPE_FLAGS_INT)
    {
        if (to_mask & TYPE_FLAGS_INT) return;
        if ((to_mask & TYPE_FLAGS_POINTER) && typestr_is_constant_zero(orig_from)) return;
    }
    TypeStr ty_to = {.buf = *orig_to};
    typestr_error2(rc, types, "error: could not implicitly convert '%.*s' to '%.*s'\n", orig_from, &ty_to);
}

void tsb_append_offset(TypeStrBuf* s, uint32_t offset, char offset_type)
{
    if (s->buf[0] < 0) abort();
    if (TYPESTR_BUF_SIZE <= s->buf[0] + sizeof(uint32_t) + 1) abort();

    int i = s->buf[0] + 1;
    memcpy(s->buf + i, &offset, sizeof(offset));
    i += sizeof(offset);
    s->buf[i] = offset_type;
    s->buf[0] = i;
}

void tsb_add_array(TypeStrBuf* s, unsigned int n)
{
    if (s->buf[0] == 0) return;
    if (n == 0)
    {
        // unbounded array
        int i = ++s->buf[0];
        if (i >= TYPESTR_BUF_SIZE) abort();
        s->buf[i] = TYPE_BYTE_UNK_ARRAY;
    }
    else
        tsb_append_offset(s, n, TYPE_BYTE_ARRAY);
}

void tsb_add_pointer(TypeStrBuf* s)
{
    unsigned i = s->buf[0];
    if (i == 0) return;
    ++i;
    if (i >= TYPESTR_BUF_SIZE) abort();
    s->buf[i] = TYPE_BYTE_POINTER;
    s->buf[0] = i;
}
void typestr_add_pointer(TypeStr* s)
{
    tsb_add_pointer(&s->buf);
    if (s->c.is_const && s->c.is_lvalue)
    {
        s->c.is_lvalue = 0;
    }
    else
    {
        s->c = s_not_constant;
    }
}

void typestr_addressof(TypeStr* s)
{
    int i = s->buf.buf[0];
    if (i == 0) return;
    tsb_skip_cvr_i(&s->buf, &i);
    const unsigned char byte = s->buf.buf[i];
    if (byte == TYPE_BYTE_ARRAY)
    {
        i -= 4;
    }
    else if (byte == TYPE_BYTE_UNK_ARRAY)
    {
    }
    else if (byte == TYPE_BYTE_FUNCTION)
    {
        ++i;
    }
    else
    {
        i = s->buf.buf[0] + 1;
    }
    if (i >= TYPESTR_BUF_SIZE) abort();
    s->buf.buf[i] = TYPE_BYTE_POINTER;
    s->buf.buf[0] = i;
    if (s->c.is_const && s->c.is_lvalue)
    {
        s->c.is_lvalue = 0;
    }
    else
    {
        s->c = s_not_constant;
    }
}

void tsb_add_cvr(TypeStrBuf* s, unsigned int mask)
{
    if (s->buf[0] == 0) return;
    unsigned int m = tsb_strip_cvr(s);
    int i = s->buf[0];
    if (i == 0) return;
    if (i + 3 >= TYPESTR_BUF_SIZE) abort();
    if (!(mask & ~m)) return;
    m |= mask;
    if (m & TYPESTR_CVR_C) s->buf[++i] = TYPE_BYTE_CONST;
    if (m & TYPESTR_CVR_V) s->buf[++i] = TYPE_BYTE_VOLATILE;
    if (m & TYPESTR_CVR_R) s->buf[++i] = TYPE_BYTE_RESTRICT;
    s->buf[0] = i;
}

static void tsb_append_decltype(const void* const* expr_seqs, TypeTable* tt, TypeStrBuf* s, const AstType* e);

static void tsb_append_tsb(TypeStrBuf* out, const TypeStrBuf* in)
{
#if defined(TRACING_ELAB)
    fprintf(stderr, "typestr_append_typestr\n");
#endif
    if (out->buf[0] < 0 || in->buf[0] < 0) abort();
    if (out->buf[0] + in->buf[0] + 1 >= sizeof(out->buf)) abort();
    memcpy(out->buf + out->buf[0] + 1, in->buf + 1, in->buf[0]);
    out->buf[0] += in->buf[0];
}

static void tsb_append_decltype_DeclSpecs(TypeTable* tt, TypeStrBuf* s, const DeclSpecs* d)
{
#if defined(TRACING_ELAB)
    fprintf(stderr, "typestr_append_decltype_DeclSpecs\n");
#endif
    if (d->_typedef)
    {
        tsb_append_tsb(s, &d->_typedef->type.buf);
    }
    else if (d->sym)
    {
#if defined(TRACING_ELAB)
        fprintf(stderr, "d->sym\n");
#endif
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
        tsb_append_offset(s, d->sym->idx, ch);
    }
    else if (d->tok->type == LEX_UUVALIST)
    {
        if (s->buf[0] < 0 || s->buf[0] >= TYPESTR_BUF_SIZE - 1) abort();
        s->buf[++s->buf[0]] = TYPE_BYTE_UUVALIST;
        tsb_append_offset(s, 1, TYPE_BYTE_ARRAY);
    }
    else
    {
#if defined(TRACING_ELAB)
        fprintf(stderr, "d->tok->type=%d\n", d->tok->type);
#endif
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
        s->buf[++s->buf[0]] = ch;
    }

    unsigned int m = 0;
    if (d->is_const) m |= TYPESTR_CVR_C;
    if (d->is_volatile) m |= TYPESTR_CVR_V;
    tsb_add_cvr(s, m);
    return;
}

static size_t tt_insert_fnargs(TypeTable* tt, const Array* args)
{
    size_t i = 0;
    const size_t args_sz = array_size(args, sizeof(TypeStrBuf));
    TypeStrBuf* tt_fn_args = tt->fn_args.data;
    size_t fn_args_ends_sz = arrsz_size(&tt->fn_args_ends);
    size_t prev_end = 0;
    for (; i < fn_args_ends_sz; ++i)
    {
        size_t end = arrsz_at(&tt->fn_args_ends, i);
        if (end - prev_end == args_sz && (args_sz == 0 || memcmp(tt_fn_args + prev_end, args->data, args->sz) == 0))
        {
            goto found;
        }
        prev_end = end;
    }
    // not found, insert seq at end
    array_push(&tt->fn_args, args->data, args->sz);
    arrsz_push(&tt->fn_args_ends, array_size(&tt->fn_args, sizeof(TypeStrBuf)));
found:
    return i;
}

static void tsb_append_decltype(const void* const* expr_seqs, TypeTable* tt, TypeStrBuf* s, const AstType* e)
{
    if (!e) abort();
    switch (e->kind)
    {
        case AST_DECLSPEC: tsb_append_decltype_DeclSpecs(tt, s, (struct DeclSpecs*)e); break;
        case AST_DECLPTR:
        {
            struct DeclPtr* d = (struct DeclPtr*)e;
            tsb_append_decltype(expr_seqs, tt, s, d->type);
            tsb_add_pointer(s);
            unsigned int m = 0;
            if (d->is_const) m |= TYPESTR_CVR_C;
            if (d->is_volatile) m |= TYPESTR_CVR_V;
            if (d->is_restrict) m |= TYPESTR_CVR_R;
            tsb_add_cvr(s, m);
            break;
        }
        case AST_DECLARR:
        {
            struct DeclArr* d = (struct DeclArr*)e;
            tsb_append_decltype(expr_seqs, tt, s, d->type);
            tsb_add_array(s, d->integer_arity);
            break;
        }
        case AST_DECLFN:
        {
            struct DeclFn* d = (struct DeclFn*)e;
            tsb_append_decltype(expr_seqs, tt, s, d->type);
            Array args = {0};
            if (d->is_param_list)
            {
                // ?
            }
            else
            {
                const StmtDecls* const* const d_seq = (void*)(expr_seqs + d->seq.off);
                for (size_t i = 0; i < d->seq.ext; ++i)
                {
                    TypeStrBuf* arg_ts = array_alloc(&args, sizeof(TypeStrBuf));
                    const struct StmtDecls* arg_decls = d_seq[i];
                    if (arg_decls->seq.ext != 1) abort();
                    tsb_from_decltype_Decl(expr_seqs, tt, arg_ts, expr_seqs[arg_decls->seq.off]);
                    tsb_decay(arg_ts);
                }
                if (d->is_varargs)
                {
                    TypeStrBuf var = {1, TYPE_BYTE_VARIADIC};
                    array_push(&args, &var, sizeof(var));
                }
            }
            tsb_append_offset(s, tt_insert_fnargs(tt, &args), TYPE_BYTE_FUNCTION);
            array_destroy(&args);
            break;
        }
        default:
            parser_tok_error(e->tok, "error: typestr_append_decltype(, %s) failed\n", ast_kind_to_string(e->kind));
            memset(s, 0, sizeof(*s));
            break;
    }
}

void tsb_from_decltype_Decl(const void* const* expr_seqs, TypeTable* tt, TypeStrBuf* s, const Decl* d)
{
    memset(s, 0, sizeof(*s));
    tsb_append_decltype(expr_seqs, tt, s, d->type);
}

void typestr_from_decltype_Decl(const void* const* expr_seqs, TypeTable* tt, TypeStr* s, const Decl* d)
{
    // initialize type
    *s = s_type_unknown;
    tsb_append_decltype(expr_seqs, tt, &s->buf, d->type);
    if (d->sym)
    {
        if (d->sym->is_enum_constant)
        {
            typestr_assign_constant_value(s, mp_from_i64(d->sym->enum_value));
        }
        else
        {
            s->c.sym = d->sym;
            s->c.is_lvalue = 1;
            s->c.is_const = 1;
        }
    }
    if (s->buf.buf[0] == 0) abort();
    if (d->specs->is_fn_arg > 0)
    {
        typestr_decay(s);
    }
}

static __forceinline size_t tsb_get_size_i_inner(const TypeTable* types, const TypeStrBuf* buf, int i)
{
    switch (buf->buf[i])
    {
        case TYPE_BYTE_STRUCT:
        case TYPE_BYTE_UNION: return tt_get(types, tsb_get_offset_i(buf, i))->size.width;
        case TYPE_BYTE_ENUM: return 4;
        case TYPE_BYTE_POINTER: return 8;
        case TYPE_BYTE_UUVALIST: return 24;
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
        default: return 0;
    }
}

unsigned long long tsb_get_size_i(const TypeTable* types, const TypeStrBuf* buf, int i, const RowCol* rc)
{
    size_t multiplier = 1;
    for (tsb_skip_cvr_i(buf, &i); buf->buf[i] == TYPE_BYTE_ARRAY; tsb_skip_cvr_i(buf, &i))
    {
        multiplier *= tsb_get_offset_i(buf, i);
        if (multiplier > UINT32_MAX)
        {
            tsb_error1(rc, types, "error: type too large: %.*s\n", buf);
            return 1;
        }
        i -= sizeof(uint32_t) + 1;
    }
    multiplier *= tsb_get_size_i_inner(types, buf, i);
    if (multiplier == 0)
    {
        tsb_error1(rc, types, "error: unable to get size of incomplete type: %.*s\n", buf);
        return 1;
    }
    else if (multiplier > UINT32_MAX)
    {
        tsb_error1(rc, types, "error: type too large: %.*s\n", buf);
        return 1;
    }
    return multiplier;
}

unsigned long long tsb_get_align_i(const TypeTable* types, const TypeStrBuf* buf, int i)
{
top:
    tsb_skip_cvr_i(buf, &i);
    switch (buf->buf[i])
    {
        case TYPE_BYTE_UNK_ARRAY:
        {
            --i;
            goto top;
        }
        case TYPE_BYTE_ARRAY:
        {
            i -= 1 + sizeof(uint32_t);
            goto top;
        }
        case TYPE_BYTE_STRUCT:
        case TYPE_BYTE_UNION:
        {
            TypeSymbol* sym = tt_get(types, tsb_get_offset_i(buf, i));
            if (!sym->align)
            {
                tsb_error1(NULL, types, "error: unable to get align of incomplete type: %.*s\n", buf);
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
        default:
        {
            tsb_error1(NULL, types, "error: unable to get align of type: %.*s\n", buf);
            return 1;
        }
    }
}

unsigned long long tsb_get_add_size(const TypeTable* types, const TypeStrBuf* ts, const RowCol* rc)
{
    int i = ts->buf[0];
    tsb_skip_cvr_i(ts, &i);
    return ts->buf[i] == TYPE_BYTE_POINTER ? tsb_get_size_i(types, ts, i - 1, rc) : 1;
}

static const Sizing s_sizing_zero = {0};
// static const Sizing s_sizing_one = {.width = 1};

static Sizing tsb_calc_sizing_i(const TypeTable* types, const TypeStrBuf* ts, int i, const RowCol* rc)
{
    tsb_skip_cvr_i(ts, &i);
    const size_t sz = tsb_get_size_i(types, ts, i, rc);
    if (sz > INT32_MAX) abort();
    const Sizing ret = {
        .width = sz,
        .is_signed = !!(s_typestr_mask_data[ts->buf[i]] & TYPE_FLAGS_SIGNED),
    };
    return ret;
}

Sizing tsb_calc_elem_sizing(const TypeTable* types, const TypeStrBuf* ts, const RowCol* rc)
{
    int i = ts->buf[0];
    if (i == 0) return s_sizing_zero;

    tsb_skip_cvr_i(ts, &i);
    if (ts->buf[i] == TYPE_BYTE_POINTER)
    {
        --i;
        return tsb_calc_sizing_i(types, ts, i, rc);
    }
    else
    {
        tsb_error1(rc, types, "error: expected pointer type: %.*s\n", ts);
        return s_sizing_zero;
    }
}

Sizing tsb_calc_sizing_zero_void(const TypeTable* types, const TypeStrBuf* ts, const RowCol* rc)
{
    int i = ts->buf[0];
    tsb_skip_cvr_i(ts, &i);
    if ((i == 1 && ts->buf[1] == TYPE_BYTE_VOID) || ts->buf[i] == TYPE_BYTE_UNK_ARRAY)
    {
        return s_sizing_zero;
    }
    return tsb_calc_sizing_i(types, ts, i, rc);
}
Sizing tsb_calc_sizing(const TypeTable* types, const TypeStrBuf* ts, const RowCol* rc)
{
    return tsb_calc_sizing_i(types, ts, ts->buf[0], rc);
}

enum
{
    TYPE_COMMON_FLAGS_CHAR = TYPE_FLAGS_CHAR | TYPE_FLAGS_INT | TYPE_FLAGS_PROMOTE_INT | TYPE_FLAGS_WIDTH1,
    TYPE_COMMON_FLAGS_SHORT = TYPE_FLAGS_INT | TYPE_FLAGS_PROMOTE_INT | TYPE_FLAGS_WIDTH2,
    TYPE_COMMON_FLAGS_INT = TYPE_FLAGS_INT | TYPE_FLAGS_WIDTH4,
    TYPE_COMMON_FLAGS_LONG = TYPE_FLAGS_INT | TYPE_FLAGS_WIDTH8,
};

const unsigned int s_typestr_mask_data[256] = {
    [TYPE_BYTE_VARIADIC] = TYPE_FLAGS_VAR,
    [TYPE_BYTE_VOID] = TYPE_FLAGS_VOID,
    [TYPE_BYTE_POINTER] = TYPE_FLAGS_POINTER | TYPE_FLAGS_WIDTH8,

    [TYPE_BYTE_CHAR] = TYPE_COMMON_FLAGS_CHAR | TYPE_FLAGS_SIGNED,
    [TYPE_BYTE_SCHAR] = TYPE_COMMON_FLAGS_CHAR | TYPE_FLAGS_SIGNED,
    [TYPE_BYTE_UCHAR] = TYPE_COMMON_FLAGS_CHAR,

    [TYPE_BYTE_SHORT] = TYPE_COMMON_FLAGS_SHORT | TYPE_FLAGS_SIGNED,
    [TYPE_BYTE_USHORT] = TYPE_COMMON_FLAGS_SHORT,

    [TYPE_BYTE_ENUM] = TYPE_COMMON_FLAGS_INT | TYPE_FLAGS_SIGNED,
    [TYPE_BYTE_INT] = TYPE_COMMON_FLAGS_INT | TYPE_FLAGS_SIGNED,
    [TYPE_BYTE_UINT] = TYPE_COMMON_FLAGS_INT,

    [TYPE_BYTE_LONG] = TYPE_COMMON_FLAGS_LONG | TYPE_FLAGS_SIGNED,
    [TYPE_BYTE_LLONG] = TYPE_COMMON_FLAGS_LONG | TYPE_FLAGS_SIGNED,
    [TYPE_BYTE_ULONG] = TYPE_COMMON_FLAGS_LONG,
    [TYPE_BYTE_ULLONG] = TYPE_COMMON_FLAGS_LONG,

    [TYPE_BYTE_FLOAT] = TYPE_FLAGS_FLOAT | TYPE_FLAGS_WIDTH4,
    [TYPE_BYTE_DOUBLE] = TYPE_FLAGS_FLOAT | TYPE_FLAGS_WIDTH8,
    [TYPE_BYTE_LDOUBLE] = TYPE_FLAGS_FLOAT | TYPE_FLAGS_WIDTH8,

    [TYPE_BYTE_UNK_ARRAY] = TYPE_FLAGS_ARRAY,
    [TYPE_BYTE_ARRAY] = TYPE_FLAGS_ARRAY,
    [TYPE_BYTE_STRUCT] = TYPE_FLAGS_STRUCT,
    [TYPE_BYTE_UNION] = TYPE_FLAGS_UNION,
    [TYPE_BYTE_FUNCTION] = TYPE_FLAGS_FUNCTION,
    [TYPE_BYTE_UUVALIST] = TYPE_FLAGS_STRUCT,
};

unsigned char tsb_byte(const TypeStrBuf* ts)
{
    int i = ts->buf[0];
    tsb_skip_cvr_i(ts, &i);
    return ts->buf[i];
}

unsigned int tsb_get_offset(const TypeStrBuf* ts) { return tsb_get_offset_i(ts, ts->buf[0]); }

static const char* const s_typestr_fmt_strs[128] = {
    [TYPE_BYTE_INVALID] = "invalid type",
    [TYPE_BYTE_VARIADIC] = "...",
    [TYPE_BYTE_VOID] = "void",
    [TYPE_BYTE_CHAR] = "char",
    [TYPE_BYTE_SCHAR] = "signed char",
    [TYPE_BYTE_UCHAR] = "unsigned char",
    [TYPE_BYTE_SHORT] = "short",
    [TYPE_BYTE_USHORT] = "unsigned short",
    [TYPE_BYTE_INT] = "int",
    [TYPE_BYTE_UINT] = "unsigned int",
    [TYPE_BYTE_LONG] = "long",
    [TYPE_BYTE_ULONG] = "unsigned long",
    [TYPE_BYTE_LLONG] = "long long",
    [TYPE_BYTE_ULLONG] = "unsigned long long",
    [TYPE_BYTE_FLOAT] = "float",
    [TYPE_BYTE_DOUBLE] = "double",
    [TYPE_BYTE_UUVALIST] = "__builtin_va_list",

    [TYPE_BYTE_POINTER] = "pointer to ",
    [TYPE_BYTE_CONST] = "const ",
    [TYPE_BYTE_VOLATILE] = "volatile ",
    [TYPE_BYTE_RESTRICT] = "restrict ",
    [TYPE_BYTE_UNK_ARRAY] = "array of ",
    [TYPE_BYTE_ARRAY] = "array of ",

    [TYPE_BYTE_STRUCT] = "struct ",
    [TYPE_BYTE_UNION] = "union ",
    [TYPE_BYTE_ENUM] = "enum ",

    [TYPE_BYTE_FUNCTION] = "function of (",
};

enum
{
    TYPESTR_FMT_COMPLETE,
    TYPESTR_FMT_CONTINUE,
    TYPESTR_FMT_SUE,
    TYPESTR_FMT_FN,
    TYPESTR_FMT_ARR,
};

static const unsigned char s_typestr_fmt_ctrl[128] = {
    [TYPE_BYTE_INVALID] = TYPESTR_FMT_COMPLETE,   [TYPE_BYTE_VARIADIC] = TYPESTR_FMT_COMPLETE,
    [TYPE_BYTE_VOID] = TYPESTR_FMT_COMPLETE,      [TYPE_BYTE_CHAR] = TYPESTR_FMT_COMPLETE,
    [TYPE_BYTE_SCHAR] = TYPESTR_FMT_COMPLETE,     [TYPE_BYTE_UCHAR] = TYPESTR_FMT_COMPLETE,
    [TYPE_BYTE_SHORT] = TYPESTR_FMT_COMPLETE,     [TYPE_BYTE_USHORT] = TYPESTR_FMT_COMPLETE,
    [TYPE_BYTE_INT] = TYPESTR_FMT_COMPLETE,       [TYPE_BYTE_UINT] = TYPESTR_FMT_COMPLETE,
    [TYPE_BYTE_LONG] = TYPESTR_FMT_COMPLETE,      [TYPE_BYTE_ULONG] = TYPESTR_FMT_COMPLETE,
    [TYPE_BYTE_LLONG] = TYPESTR_FMT_COMPLETE,     [TYPE_BYTE_ULLONG] = TYPESTR_FMT_COMPLETE,
    [TYPE_BYTE_FLOAT] = TYPESTR_FMT_COMPLETE,     [TYPE_BYTE_DOUBLE] = TYPESTR_FMT_COMPLETE,
    [TYPE_BYTE_UUVALIST] = TYPESTR_FMT_COMPLETE,

    [TYPE_BYTE_POINTER] = TYPESTR_FMT_CONTINUE,   [TYPE_BYTE_CONST] = TYPESTR_FMT_CONTINUE,
    [TYPE_BYTE_VOLATILE] = TYPESTR_FMT_CONTINUE,  [TYPE_BYTE_RESTRICT] = TYPESTR_FMT_CONTINUE,
    [TYPE_BYTE_UNK_ARRAY] = TYPESTR_FMT_CONTINUE,

    [TYPE_BYTE_ARRAY] = TYPESTR_FMT_ARR,

    [TYPE_BYTE_STRUCT] = TYPESTR_FMT_SUE,         [TYPE_BYTE_UNION] = TYPESTR_FMT_SUE,
    [TYPE_BYTE_ENUM] = TYPESTR_FMT_SUE,

    [TYPE_BYTE_FUNCTION] = TYPESTR_FMT_FN,
};

static void tsb_fmt_i(const TypeTable* tt, const TypeStrBuf* ts, size_t i, Array* buf)
{
    while (1)
    {
        if (i == 0) abort();
        const unsigned char ch = ts->buf[i];
        array_appends(buf, s_typestr_fmt_strs[ch]);

        const unsigned char ctrl = s_typestr_fmt_ctrl[ch];
        if (ctrl == TYPESTR_FMT_COMPLETE)
        {
            return;
        }
        --i;
        if (ctrl == TYPESTR_FMT_CONTINUE)
        {
            continue;
        }
        const uint32_t u = tsb_get_offset_i(ts, i + 1);
        i -= sizeof(u);
        if (ctrl == TYPESTR_FMT_ARR)
        {
            array_appendf(buf, "%u ", u);
        }
        else if (ctrl == TYPESTR_FMT_SUE)
        {
            // struct, union, enum
            TypeSymbol* sym = tt_get(tt, u);
            if (sym->name)
                array_appends(buf, sym->name);
            else
                array_appendf(buf, "<anonymous#%u>", u);
            return;
        }
        else if (ctrl == TYPESTR_FMT_FN)
        {
            size_t begin = u ? arrsz_at(&tt->fn_args_ends, u - 1) : 0;
            size_t end = arrsz_at(&tt->fn_args_ends, u);
            const TypeStrBuf* argtys = tt->fn_args.data;
            if (begin != end)
            {
                tsb_fmt(tt, argtys + begin, buf);
                for (++begin; begin != end; ++begin)
                {
                    array_appends(buf, ", ");
                    tsb_fmt(tt, argtys + begin, buf);
                }
            }
            array_appends(buf, ") returning ");
        }
        else
            abort();
    }
}

void tsb_fmt(const TypeTable* tt, const TypeStrBuf* ts, Array* buf)
{
    if (ts->buf[0] == 0)
        array_appends(buf, "invalid type");
    else
        tsb_fmt_i(tt, ts, ts->buf[0], buf);
}

void typestr_fmt(const TypeTable* tt, const TypeStr* ts, Array* buf)
{
    tsb_fmt(tt, &ts->buf, buf);
    if (ts->c.is_const && !ts->c.is_lvalue && !ts->c.sym)
    {
        array_appendf(buf, " (constant %zu)", ts->c.value.lower);
    }
}

int typestr_is_char_array(const TypeStr* ts)
{
    int i = ts->buf.buf[0];
    tsb_skip_cvr_i(&ts->buf, &i);
    if (ts->buf.buf[i] == TYPE_BYTE_ARRAY)
    {
        i -= 5;
        goto then;
    }
    else if (ts->buf.buf[i] == TYPE_BYTE_UNK_ARRAY)
    {
        --i;
    then:
        tsb_skip_cvr_i(&ts->buf, &i);
        return !!(s_typestr_mask_data[ts->buf.buf[i]] & TYPE_FLAGS_CHAR);
    }
    return 0;
}
unsigned int tsb_get_cvr(const TypeStrBuf* ts)
{
    int i = ts->buf[0];
    return tsb_skip_cvr_i(ts, &i);
}
unsigned int tsb_strip_cvr(TypeStrBuf* ts)
{
    int i = ts->buf[0];
    unsigned int m = tsb_skip_cvr_i(ts, &i);
    ts->buf[0] = i;
    return m;
}

void typestr_remove_array(TypeStr* ts)
{
    typestr_strip_cvr(ts);
    if (ts->buf.buf[ts->buf.buf[0]] == TYPE_BYTE_ARRAY)
    {
        ts->buf.buf[0] -= sizeof(uint32_t) + 1;
    }
    else
    {
        --ts->buf.buf[0];
    }
    memset(&ts->c, 0, sizeof(ts->c));
}

void typestr_dereference(TypeStr* ts)
{
    typestr_strip_cvr(ts);
    if (ts->buf.buf[ts->buf.buf[0]] == TYPE_BYTE_POINTER)
    {
        --ts->buf.buf[0];
        if (ts->c.is_const)
        {
            if (ts->c.sym && !ts->c.is_lvalue)
            {
                ts->c.is_lvalue = 1;
            }
            else
            {
                memset(&ts->c, 0, sizeof(ts->c));
            }
        }
    }
    else
    {
        *ts = s_type_unknown;
    }
}

void typestr_error1(const struct RowCol* rc, const TypeTable* e, const char* fmt, const TypeStr* ts)
{
    Array arr = {};
    typestr_fmt(e, ts, &arr);
    parser_ferror(rc, fmt, arr.sz, arr.data);
    array_destroy(&arr);
}

void typestr_error2(const struct RowCol* rc, const TypeTable* e, const char* fmt, const TypeStr* t1, const TypeStr* t2)
{
    Array arr = {};
    typestr_fmt(e, t1, &arr);
    const size_t sz = arr.sz;
    typestr_fmt(e, t2, &arr);
    parser_ferror(rc, fmt, sz, arr.data, arr.sz - sz, (char*)arr.data + sz);
    array_destroy(&arr);
}

void tsb_error1(const struct RowCol* rc, const TypeTable* e, const char* fmt, const TypeStrBuf* ts)
{
    Array arr = {};
    tsb_fmt(e, ts, &arr);
    parser_ferror(rc, fmt, arr.sz, arr.data);
    array_destroy(&arr);
}

void tsb_error2(
    const struct RowCol* rc, const TypeTable* e, const char* fmt, const TypeStrBuf* t1, const TypeStrBuf* t2)
{
    Array arr = {};
    tsb_fmt(e, t1, &arr);
    const size_t sz = arr.sz;
    tsb_fmt(e, t2, &arr);
    parser_ferror(rc, fmt, sz, arr.data, arr.sz - sz, (char*)arr.data + sz);
    array_destroy(&arr);
}

int tsb_match(const TypeStrBuf* a, const TypeStrBuf* b)
{
    return a->buf[0] == b->buf[0] && 0 == memcmp(a->buf, b->buf, a->buf[0] + 1);
}

uint32_t tsb_pop_offset(TypeStrBuf* ts)
{
    uint32_t r = tsb_get_offset(ts);
    if (r < UINT32_MAX)
    {
        ts->buf[0] -= 1 + sizeof(r);
    }
    return r;
}
#define CC_ASSERT(X)                                                                                                   \
    if (X)                                                                                                             \
    {                                                                                                                  \
    }                                                                                                                  \
    else                                                                                                               \
    {                                                                                                                  \
        abort();                                                                                                       \
    }
TypeSymbol* tsb_get_decl(TypeTable* tt, const TypeStrBuf* ts)
{
    char ch = ts->buf[ts->buf[0]];
    if (ch == TYPE_BYTE_STRUCT || ch == TYPE_BYTE_UNION)
    {
        TypeSymbol* r = tt_get(tt, tsb_get_offset(ts));
        CC_ASSERT(r);
        return r;
    }
    return NULL;
}

int tsb_decay(TypeStrBuf* t)
{
    tsb_strip_cvr(t);
    switch (t->buf[t->buf[0]])
    {
        case TYPE_BYTE_ARRAY: t->buf[0] -= 4; // passthrough
        case TYPE_BYTE_UNK_ARRAY: t->buf[t->buf[0]] = TYPE_BYTE_POINTER; return 1;
        case TYPE_BYTE_FUNCTION:
        {
            t->buf[0]++;
            if (t->buf[0] == TYPESTR_BUF_SIZE) abort();
            t->buf[t->buf[0]] = TYPE_BYTE_POINTER;
            return 1;
        }
        default: return 0;
    }
}

int typestr_decay(TypeStr* t)
{
    int r = tsb_decay(&t->buf);
    if (r && t->c.is_const)
    {
        if (!t->c.is_lvalue) abort();
        if (!t->c.sym) abort();
        t->c.is_lvalue = 0;
    }
    return r;
}

FnTypeInfo typestr_strip_fn(const TypeTable* tt, TypeStr* t)
{
    FnTypeInfo ret = {0};
    if (t->buf.buf[t->buf.buf[0]] == TYPE_BYTE_POINTER)
    {
        typestr_dereference(t);
    }
    if (t->buf.buf[t->buf.buf[0]] == TYPE_BYTE_FUNCTION)
    {
        t->c = s_not_constant;
        uint32_t x = typestr_pop_offset(t);
        if (x == UINT32_MAX) abort();
        if (x > 0) ret.offset = arrsz_at(&tt->fn_args_ends, x - 1);
        ret.extent = arrsz_at(&tt->fn_args_ends, x) - ret.offset;
        if (ret.extent > 0)
        {
            ret.is_variadic =
                tsb_byte((TypeStrBuf*)tt->fn_args.data + ret.offset + ret.extent - 1) == TYPE_BYTE_VARIADIC;
            ret.extent -= ret.is_variadic;
        }
    }
    else
    {
        *t = s_type_unknown;
    }
    return ret;
}

void typestr_apply_integral_type(TypeStr* dst, const TypeStr* src)
{
    dst->buf = src->buf;
    if (dst->c.is_const && !dst->c.sym)
    {
        unsigned mask = typestr_mask(dst);
        dst->c.value = mp_cast(dst->c.value, mask);
    }
    else
    {
        dst->c = s_not_constant;
    }
}

void typestr_assign_constant_value(TypeStr* t, Constant128 n)
{
    unsigned mask = typestr_mask(t);
    t->c.is_const = 1;
    t->c.is_lvalue = 0;
    t->c.sym = NULL;
    t->c.value = mp_cast(n, mask);
}

void typestr_assign_constant_bool(TypeStr* t, int n)
{
    t->c.is_const = 1;
    t->c.is_lvalue = 0;
    t->c.sym = NULL;
    t->c.value = n ? s_one_c128 : s_zero_c128;
}

const TypeStrBuf* typestr_get_arg(const TypeTable* tt, const FnTypeInfo* info, unsigned index)
{
    if (index >= info->extent) abort();
    return (TypeStrBuf*)tt->fn_args.data + index + info->offset;
}

TypeTable* tt_alloc()
{
    TypeTable* types = my_malloc(sizeof(TypeTable));
    memset(types, 0, sizeof(TypeTable));
    return types;
}
void tt_free(TypeTable* tt)
{
    if (tt == NULL) return;
    array_destroy(&tt->fn_args);
    array_destroy(&tt->fn_args_ends);
    array_destroy(&tt->typesyms);
    my_free(tt);
}
