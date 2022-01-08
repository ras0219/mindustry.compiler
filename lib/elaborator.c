#include "elaborator.h"

#include <limits.h>
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

//#define X_TYPE_KIND_POOL(Y) Y(TYPE_DECL, TypeDecl) Y(TYPE_CVR, TypeCVR) Y(TYPE_PTR, TypePtr) Y(TYPE_ARR, TypeArr)
//#define X_TYPE_KIND(Y) X_TYPE_KIND_POOL(Y) Y(TYPE_BASIC, TypeBasic)
//#define Y_COMMA(E, T) E,
// enum TypeKind
//{
//    X_TYPE_KIND(Y_COMMA)
//};
//#undef Y_COMMA
//#define Y_COUNT(E, T) +1
// enum
//{
//    TYPE_KIND_POOL_COUNT = X_TYPE_KIND_POOL(Y_COUNT),
//};
//#undef Y_COUNT
//
// struct TypeStorage
//{
//    struct PoolSet type_pool[TYPE_KIND_POOL_COUNT];
//};
//
// struct TypeBasic
//{
//    uintptr_t kind;
//    uintptr_t basic_type;
//};

// static const struct Expr s_expr_int_singleton = {.kind = TYPE_BUILTIN_INT};
// static const struct TypeStr s_type_literal_int = {.expr = &s_expr_int_singleton};
// static const struct Expr s_expr_char_singleton = {.kind = TYPE_BUILTIN_CHAR};
// static const struct TypeStr s_type_literal_char = {.expr = &s_expr_char_singleton};
// static const struct TypeStr s_type_literal_cstr = {.expr = &s_expr_char_singleton, .is_pointer = 1, .is_temporary =
// 1};

static const struct TypeStr s_type_unknown = {};

#if 0
static struct TypeStr declspecs_to_type(struct Parser* p, struct DeclSpecs* specs)
{
    struct TypeStr ty;
    if (specs->type->type == LEX_INT)
    {
        ty = s_type_int;
    }
    else if (specs->type->type == LEX_LONG)
    {
        ty = s_type_long;
    }
    else if (specs->type->type == LEX_SHORT)
    {
        ty = s_type_short;
    }
    else if (specs->type->type == LEX_CHAR)
    {
        ty = s_type_char;
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
#endif

struct TypeTable
{
    struct AutoHeap typenames;
    struct Array decls;
    struct Array fn_args_ends;
    struct Array fn_args;
};

static __forceinline struct Decl** tt_get_decl(const struct TypeTable* tt, size_t i)
{
    return ((struct Decl**)tt->decls.data) + i;
}

static size_t findstr(const char* str, const char* const* heap, size_t heap_size)
{
    size_t i = 0;
    for (; i < heap_size; ++i)
    {
        if (strcmp(str, heap[i]) == 0) break;
    }
    return i;
}

static __forceinline size_t tt_find_insert_null(struct TypeTable* tt, const char* str)
{
    const size_t n = array_size(&tt->typenames.arr, sizeof(const char*));
    size_t offset = findstr(str, (const char* const*)tt->typenames.arr.data, n);
    if (offset == n)
    {
        size_t len = strlen(str);
        memcpy(autoheap_alloc(&tt->typenames, len + 1), str, len + 1);
        array_push_ptr(&tt->decls, NULL);
    }
    return offset;
}

static __forceinline const char* tt_get_name(const struct TypeTable* tt, size_t i)
{
    return ((const char**)tt->typenames.arr.data)[i];
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
                    parser_ferror(expr_to_rc(e), "error: integer constant exceeded INT32_MAX: %llu\n", lit->numeric);
                    return 0;
                }
                return lit->numeric;
            }
            parser_ferror(expr_to_rc(e), "error: expected integer constant literal\n");
            return 0;
        }
        case EXPR_SYM:
        {
            struct ExprSym* sym = (void*)e;
            if (sym->decl->is_enum_constant)
            {
                return sym->decl->enum_value;
            }
            parser_tok_error(sym->tok, "error: expected integer constant expression\n");
            return 0;
        }
        case EXPR_OP:
        {
            struct ExprOp* op = (struct ExprOp*)e;
            int32_t l = eval_constant(elab, op->lhs);
            if (op->rhs)
            {
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
                    default: break;
                }
            }
            else
            {
                switch (op->tok->type)
                {
                    case TOKEN_SYM1('+'): return l;
                    default: break;
                }
            }
            parser_tok_error(
                op->tok, "error: unimplemented op '%s' in integer constant expression\n", token_str(elab->p, op->tok));
            return 0;
        }

        case EXPR_CAST:
        {
            struct ExprCast* expr = (void*)e;
            return eval_constant(elab, expr->expr);
        }
        default:
            parser_ferror(
                expr_to_rc(e), "error: expected integer constant expression (not %s)\n", ast_kind_to_string(e->kind));
            return 0;
    }
}

static int get_primitive_declspec_size(struct DeclSpecs* d)
{
    if (d->is_short) return 2;
    if (d->is_long) return d->tok->type == LEX_DOUBLE ? 16 : 4;
    if (d->is_longlong) return 8;

    switch (d->tok->type)
    {
        case LEX_VOID: parser_tok_error(d->tok, "error: cannot size field of type void\n"); return 1;
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

static int get_decl_align(struct Elaborator* elab, struct Expr* e)
{
    switch (e->kind)
    {
        case AST_DECL: return ((struct Decl*)e)->align;
        case AST_DECLPTR: return 8;
        case AST_DECLFN: return 1;
        case AST_DECLARR: return get_decl_align(elab, ((struct DeclArr*)e)->type);
        case AST_DECLSPEC:
        {
            struct DeclSpecs* d = (struct DeclSpecs*)e;
            if (d->type)
            {
                return d->type->align;
            }
            else if (d->name)
            {
                uint32_t offset = tt_find_insert_null(elab->types, d->name);
                struct Decl* const inner_decl = *tt_get_decl(elab->types, offset);
                if (!inner_decl)
                {
                    parser_tok_error(d->tok, "error: '%s' was incomplete while getting alignment\n", d->name);
                    return 1;
                }
                return inner_decl->align;
            }
            else if (d->tok->type == LEX_UUVALIST)
            {
                return 8;
            }

            return get_primitive_declspec_size(d);
        }
        default: parser_ferror(expr_to_rc(e), "error: cannot calculate align of typeexpr\n"); return 1;
    }
}

static int get_decl_size(struct Elaborator* elab, struct Expr* e)
{
    switch (e->kind)
    {
        case AST_DECL: return ((struct Decl*)e)->size;
        case AST_DECLPTR: return 8;
        case AST_DECLFN: return 0;
        case AST_DECLARR:
        {
            struct DeclArr* d = (struct DeclArr*)e;
            int base = get_decl_size(elab, d->type);
            if (!d->arity)
            {
                return 1;
            }
            long long numeric_arity = eval_constant(elab, d->arity);
            if (numeric_arity > 0) d->integer_arity = numeric_arity;
            if (numeric_arity >= 0 && numeric_arity < 0x7FFFFFFF) numeric_arity *= base;
            if (numeric_arity < 0 || numeric_arity > 0x7FFFFFFF)
            {
                parser_tok_error(d->tok, "error: array arity must be between 1 and INT_MAX\n");
                return 1;
            }
            return numeric_arity;
        }
        case AST_DECLSPEC:
        {
            struct DeclSpecs* d = (struct DeclSpecs*)e;
            if (d->type)
            {
                return get_decl_size(elab, &d->type->kind);
            }
            else if (d->name)
            {
                uint32_t offset = tt_find_insert_null(elab->types, d->name);
                struct Decl* const inner_decl = *tt_get_decl(elab->types, offset);
                if (!inner_decl)
                {
                    parser_tok_error(d->tok, "error: '%s' was incomplete while getting size\n", d->name);
                    return 1;
                }
                return get_decl_size(elab, &inner_decl->kind);
            }

            return get_primitive_declspec_size(d);
        }
        default: parser_ferror(expr_to_rc(e), "error: cannot calculate size of decl\n"); return 1;
    }
}

#define X_TYPE_BYTE_INT(Y)                                                                                             \
    Y(TYPE_BYTE_CHAR, 'C')                                                                                             \
    Y(TYPE_BYTE_SCHAR, 'h')                                                                                            \
    Y(TYPE_BYTE_UCHAR, 'H')                                                                                            \
    Y(TYPE_BYTE_INT, 'i')                                                                                              \
    Y(TYPE_BYTE_SHORT, 's')                                                                                            \
    Y(TYPE_BYTE_LONG, 'l')                                                                                             \
    Y(TYPE_BYTE_LLONG, 'y')                                                                                            \
    Y(TYPE_BYTE_UINT, 'I')                                                                                             \
    Y(TYPE_BYTE_USHORT, 'S')                                                                                           \
    Y(TYPE_BYTE_ULONG, 'L')                                                                                            \
    Y(TYPE_BYTE_ULLONG, 'Y')

#define X_TYPE_BYTE_ARITH(Y)                                                                                           \
    X_TYPE_BYTE_INT(Y)                                                                                                 \
    Y(TYPE_BYTE_FLOAT, 'F')                                                                                            \
    Y(TYPE_BYTE_DOUBLE, 'D')                                                                                           \
    Y(TYPE_BYTE_LDOUBLE, 'E')

#define X_TYPE_BYTE(Y)                                                                                                 \
    Y(TYPE_BYTE_VOID, 'V')                                                                                             \
    Y(TYPE_BYTE_UUVALIST, '_')                                                                                         \
    Y(TYPE_BYTE_VARIADIC, '.')                                                                                         \
    X_TYPE_BYTE_ARITH(Y)                                                                                               \
    Y(TYPE_BYTE_STRUCT, '$')                                                                                           \
    Y(TYPE_BYTE_UNION, 'u')                                                                                            \
    Y(TYPE_BYTE_ENUM, 'e')                                                                                             \
    Y(TYPE_BYTE_POINTER, 'p')                                                                                          \
    Y(TYPE_BYTE_FUNCTION, '(')

#define Y_COMMA(E, CH) E = CH,
enum
{
    X_TYPE_BYTE(Y_COMMA)
};
#undef Y_COMMA

static const struct TypeStr s_type_literal_int = {.buf = {1, TYPE_BYTE_INT}};
static const struct TypeStr s_type_literal_char = {.buf = {2, TYPE_BYTE_CHAR, 'c'}};
static const struct TypeStr s_type_void = {.buf = {1, TYPE_BYTE_VOID}};

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

static void typestr_fmt(const struct TypeTable* tt, const struct TypeStr* ts, struct Array* buf)
{
    size_t i = ts->buf[0];
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
            case ']': str = "array of "; goto append_continue;
            case '[':
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
        array_appendf(buf, "%s %s", str, tt_get_name(tt, u));
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

static __forceinline int typestr_is_pointer(const struct TypeStr* ts)
{
    return ts->buf[(int)ts->buf[0]] == TYPE_BYTE_POINTER;
}
const struct TypeStr s_void = {
    .buf = {1, TYPE_BYTE_VOID},
};
static __forceinline int typestr_is_fn(const struct TypeStr* ts)
{
    return ts->buf[(int)ts->buf[0]] == TYPE_BYTE_FUNCTION;
}
static __forceinline int typestr_is_variadic(const struct TypeStr* ts)
{
    return ts->buf[(int)ts->buf[0]] == TYPE_BYTE_VARIADIC;
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
    if (ts->buf[(int)ts->buf[0]] == 'r') ts->buf[(int)ts->buf[0]--] = '\0', m |= TYPESTR_CVR_R;
    if (ts->buf[(int)ts->buf[0]] == 'v') ts->buf[(int)ts->buf[0]--] = '\0', m |= TYPESTR_CVR_V;
    if (ts->buf[(int)ts->buf[0]] == 'c') ts->buf[(int)ts->buf[0]--] = '\0', m |= TYPESTR_CVR_C;
    return m;
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
enum
{
    TYPE_FLAGS_VOID = 1,
    TYPE_FLAGS_CHAR = 2,
    TYPE_FLAGS_INT = 4,
    TYPE_FLAGS_POINTER = 8,
    TYPE_FLAGS_STRUCT = 16,
    TYPE_FLAGS_UNION = 32,
    TYPE_FLAGS_FUNCTION = 64,
    TYPE_FLAGS_VAR = 128,
    TYPE_FLAGS_FLOAT = 256,
    TYPE_FLAGS_ARRAY = 512,

    TYPE_MASK_HAS_FIELDS = TYPE_FLAGS_UNION | TYPE_FLAGS_STRUCT,
    TYPE_MASK_ARITH = TYPE_FLAGS_FLOAT | TYPE_FLAGS_INT,
    TYPE_MASK_SCALAR = TYPE_FLAGS_POINTER | TYPE_MASK_ARITH,
    TYPE_MASK_OBJECT = TYPE_FLAGS_VOID | TYPE_FLAGS_INT | TYPE_FLAGS_POINTER | TYPE_FLAGS_STRUCT | TYPE_FLAGS_UNION,
};

unsigned int typestr_mask(const struct TypeStr* ts)
{
    switch (ts->buf[(int)ts->buf[0]])
    {
        case 'c':
        case 'v':
        case 'r':
        case '\0': return 0;
        case TYPE_BYTE_VARIADIC: return TYPE_FLAGS_VAR;
        case TYPE_BYTE_VOID: return TYPE_FLAGS_VOID;
        case TYPE_BYTE_CHAR:
        case TYPE_BYTE_SCHAR:
        case TYPE_BYTE_UCHAR: return TYPE_FLAGS_CHAR | TYPE_FLAGS_INT;
        case TYPE_BYTE_ENUM:
        case TYPE_BYTE_INT:
        case TYPE_BYTE_SHORT:
        case TYPE_BYTE_LONG:
        case TYPE_BYTE_LLONG:
        case TYPE_BYTE_UINT:
        case TYPE_BYTE_USHORT:
        case TYPE_BYTE_ULONG:
        case TYPE_BYTE_ULLONG: return TYPE_FLAGS_INT;
        case TYPE_BYTE_FLOAT:
        case TYPE_BYTE_DOUBLE:
        case TYPE_BYTE_LDOUBLE: return TYPE_FLAGS_FLOAT;
        case TYPE_BYTE_POINTER: return TYPE_FLAGS_POINTER;
        case ']':
        case '[': return TYPE_FLAGS_ARRAY;
        case TYPE_BYTE_STRUCT: return TYPE_FLAGS_STRUCT;
        case TYPE_BYTE_UNION: return TYPE_FLAGS_UNION;
        case TYPE_BYTE_FUNCTION: return TYPE_FLAGS_FUNCTION;
        case TYPE_BYTE_UUVALIST: return TYPE_FLAGS_POINTER;
        default: abort();
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

struct Decl* typestr_get_decl(struct TypeTable* tt, const struct TypeStr* ts)
{
    char ch = ts->buf[(int)ts->buf[0]];
    if (ch == TYPE_BYTE_STRUCT || ch == TYPE_BYTE_UNION)
    {
        return *tt_get_decl(tt, typestr_get_offset(ts));
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
        case ']': t->buf[(int)t->buf[0]] = TYPE_BYTE_POINTER; return;
        case '[':
        {
            t->buf[0] -= 4;
            t->buf[(int)t->buf[0]] = TYPE_BYTE_POINTER;
            return;
        }
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
        typestr_append_offset(s, n, '[');
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

static void typestr_append_decltype(const struct Expr* const* expr_seqs,
                                    struct TypeTable* tt,
                                    struct TypeStr* s,
                                    const struct Expr* e)
{
top:
    if (!e) abort();
    switch (e->kind)
    {
        case EXPR_SYM:
        {
            e = (struct Expr*)((struct ExprSym*)e)->decl;
            goto top;
        }
        case AST_DECLSPEC:
        {
            struct DeclSpecs* d = (struct DeclSpecs*)e;
            if (d->type)
            {
                typestr_append_decltype(expr_seqs, tt, s, (struct Expr*)d->type);
            }
            else if (d->name)
            {
                uint32_t offset = tt_find_insert_null(tt, d->name);
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
                {
                    abort();
                }
                typestr_append_offset(s, offset, ch);
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
        case AST_DECL:
        {
            struct Decl* d = (struct Decl*)e;
            if (d->type)
            {
                e = d->type;
                goto top;
            }
            uint32_t offset;
            if (d->name)
            {
                // struct/enum/union decl
                offset = tt_find_insert_null(tt, d->name);
            }
            else
            {
                // anonymous struct/union/enum
                char buf[] = "$00000000";
                uint32_t anon_idx = d->anon_idx;
                for (size_t i = 0; i < 8; ++i)
                {
                    buf[8 - i] = "0123456789ABCDEF"[0xF & anon_idx];
                    anon_idx >>= 4;
                }
                offset = tt_find_insert_null(tt, buf);
            }
            char ch;
            if (d->specs->is_struct)
            {
                ch = TYPE_BYTE_STRUCT;
            }
            else if (d->specs->is_union)
            {
                ch = TYPE_BYTE_UNION;
            }
            else if (d->specs->is_enum)
            {
                ch = TYPE_BYTE_ENUM;
            }
            else
            {
                abort();
            }
            typestr_append_offset(s, offset, ch);
            return;
        }
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
                typestr_append_decltype(expr_seqs, tt, arg_ts, expr_seqs[d->offset + i]);
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
                array_push_size_t(&tt->fn_args_ends, array_size(&tt->fn_args, sizeof(struct TypeStr)));
            }
            typestr_append_offset(s, i, TYPE_BYTE_FUNCTION);
            return;
        }
        default: break;
    }
    fprintf(stderr, "typestr_append_decltype(, %s) failed\n", ast_kind_to_string(e->kind));
    *s = s_type_unknown;
}
static void typestr_from_decltype(const struct Expr* const* expr_seqs,
                                  struct TypeTable* tt,
                                  struct TypeStr* s,
                                  struct Expr* e)
{
    // initialize type
    *s = s_type_unknown;
    typestr_append_decltype(expr_seqs, tt, s, e);
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
            return get_decl_size(elab, (struct Expr*)*tt_get_decl(elab->types, typestr_get_offset_i(ts, i)));
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
        case TYPE_BYTE_UCHAR:
        case TYPE_BYTE_SCHAR:
        case TYPE_BYTE_CHAR: return 1;
        default:
        {
            struct Array buf = {};
            typestr_fmt(elab->types, ts, &buf);
            fprintf(stderr, "error: unable to get size of type: %.*s\n", (int)buf.sz, (char*)buf.data);
            array_destroy(&buf);
            return 1;
        }
    }
}

static size_t typestr_get_size(struct Elaborator* elab, const struct TypeStr* ts)
{
    return typestr_get_size_i(elab, ts, ts->buf[0]);
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

static struct Decl* find_field_by_name(struct Decl* decl, const char* fieldname, struct Expr* const* const exprs)
{
    if (!decl->init || decl->init->kind != STMT_BLOCK) abort();
    struct StmtBlock* block = (struct StmtBlock*)decl->init;
    for (size_t i = 0; i < block->extent; ++i)
    {
        struct Expr* e = exprs[block->offset + i];
        if (e->kind != STMT_DECLS) abort();
        struct StmtDecls* f = (struct StmtDecls*)e;
        for (size_t i = 0; i < f->extent; ++i)
        {
            struct Expr* g = exprs[f->offset + i];
            if (g->kind != AST_DECL) abort();
            struct Decl* h = (struct Decl*)g;
            if (!h->type)
            {
                if (!h->name)
                {
                    struct Decl* x = find_field_by_name(h, fieldname, exprs);
                    if (x) return x;
                }
            }
            else if (!h->name)
                abort();
            else if (strcmp(h->name, fieldname) == 0)
                return h;
        }
    }
    return NULL;
}
static void elaborate_expr(struct Elaborator* elab,
                           struct ElaborateDeclCtx* ctx,
                           struct Expr* top_expr,
                           struct TypeStr* rty);

static void elaborate_op(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, struct ExprOp* e, struct TypeStr* rty)
{
    elaborate_expr(elab, ctx, e->lhs, rty);
    const struct TypeStr orig_lhs = *rty;
    typestr_decay(rty);
    unsigned int lhs_mask = typestr_mask(rty);

    if (e->rhs)
    {
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
            case LEX_UUVA_START: break;
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
                    e->size = typestr_get_elem_size(elab, rty);
                }
                else
                {
                    e->size = 1;
                }
                break;
            case TOKEN_SYM1('-'):
                if (lhs_mask & TYPE_FLAGS_POINTER)
                {
                    e->size = typestr_get_elem_size(elab, rty);
                }
                else
                {
                    e->size = 1;
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
                    e->size = -e->size;
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
                e->size = typestr_get_size(elab, rty);
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
    else
    {
        switch (e->tok->type)
        {
            case LEX_UUVA_END: break;
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
            case LEX_SIZEOF:
                e->size = typestr_get_size(elab, rty);
                *rty = s_type_literal_int;
                break;
            default:
                fprintf(stderr, "warning: untyped unary operator '%s'\n", token_str(elab->p, e->tok));
                *rty = s_type_unknown;
                break;
        }
    }
}

static void elaborate_exprs(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, size_t offset, size_t extent)
{
    struct TypeStr ty;
    struct Expr** seqs = elab->p->expr_seqs.data;
    for (size_t i = 0; i < extent; ++i)
    {
        elaborate_expr(elab, ctx, seqs[offset + i], &ty);
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
        case STMT_NONE:
        case STMT_BREAK:
        case STMT_CONTINUE:
        case STMT_GOTO: *rty = s_type_void; return;
        case STMT_CASE:
        {
            struct StmtCase* stmt = top;
            if (stmt->expr) stmt->value = eval_constant(elab, stmt->expr);
            return;
        }
        case STMT_LABEL:
        {
            struct StmtLabel* expr = top;
            return elaborate_expr(elab, ctx, expr->stmt, rty);
        }
        case EXPR_LIT:
        {
            struct ExprLit* expr = top;
            if (expr->tok->type == LEX_NUMBER)
                *rty = s_type_literal_int;
            else if (expr->tok->type == LEX_CHARLIT)
                *rty = s_type_literal_int;
            else if (expr->tok->type == LEX_STRING)
            {
                *rty = s_type_literal_char;
                // TODO: calculate string lengths
                typestr_add_array(rty, strlen(expr->text) + 1);
            }
            else
            {
                parser_tok_error(expr->tok, "error: unknown literal type: %s\n", lexstate_to_string(expr->tok->type));
                *rty = s_type_unknown;
            }
            return;
        }

        case EXPR_SYM:
        {
            struct ExprSym* esym = top;
            typestr_from_decltype(elab->p->expr_seqs.data, elab->types, rty, (struct Expr*)esym->decl);
            return;
        }
        case EXPR_CAST:
        {
            struct ExprCast* expr = top;
            elaborate_expr(elab, ctx, (struct Expr*)expr->type, rty);
            elaborate_expr(elab, ctx, expr->expr, rty);
            typestr_from_decltype(elab->p->expr_seqs.data, elab->types, rty, (struct Expr*)expr->type);
            return;
        }
        case STMT_RETURN:
        {
            struct StmtReturn* stmt = top;
            if (stmt->expr)
            {
                elaborate_expr(elab, ctx, stmt->expr, rty);
            }
            else
            {
                *rty = s_type_void;
            }

            return;
        }
        case STMT_IF:
        {
            struct StmtIf* stmt = top;
            elaborate_expr(elab, ctx, stmt->cond, rty);
            elaborate_expr(elab, ctx, stmt->if_body, rty);
            if (stmt->else_body)
            {
                elaborate_expr(elab, ctx, stmt->else_body, rty);
            }
            *rty = s_type_void;
            return;
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
                    typestr_implicit_conversion(elab->types, expr_to_rc(arg_expr), &arg_expr_ty, orig_tt_arg);
                }
                else
                {
                    // varargs
                    unsigned int lhs_mask = typestr_mask(&arg_expr_ty);
                    if (!(lhs_mask & TYPE_MASK_SCALAR))
                    {
                        typestr_error1(expr_to_rc(arg_expr),
                                       elab->types,
                                       "error: expected scalar type in variadic arguments but got '%.*s'\n",
                                       &orig_arg_expr_ty);
                    }
                }
            }
            return;
        }
        case EXPR_FIELD:
        {
            struct ExprField* e = top;
            elaborate_expr(elab, ctx, e->lhs, rty);
            const struct TypeStr orig_lhs = *rty;
            if (e->is_arrow) typestr_dereference(rty);
            unsigned int cvr_mask = typestr_strip_cvr(rty);
            struct Decl* decl = typestr_get_decl(elab->types, rty);
            if (decl)
            {
                // find field in decl
                struct Decl* field =
                    find_field_by_name(decl, e->fieldname, (struct Expr* const*)elab->p->expr_seqs.data);
                if (field)
                {
                    e->decl = field;
                    typestr_from_decltype(elab->p->expr_seqs.data, elab->types, rty, field->type);
                    typestr_add_cvr(rty, cvr_mask);
                }
                else
                {
                    parser_tok_error(
                        e->tok, "error: could not find member '%s' in type '%s'\n", e->fieldname, decl->name);
                    *rty = s_type_unknown;
                }
            }
            else
            {
                unsigned int lhs_mask = typestr_mask(rty);
                if (lhs_mask & TYPE_MASK_HAS_FIELDS)
                {
                    typestr_error1(
                        &e->tok->rc, elab->types, "error: first argument was of incomplete type %.*s\n", rty);
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
                }
                *rty = s_type_unknown;
            }
            return;
        }
        case EXPR_OP: elaborate_op(elab, ctx, top, rty); return;
        case STMT_LOOP:
        {
            struct StmtLoop* e = top;
            elaborate_expr(elab, ctx, e->body, rty);
            if (e->advance) elaborate_expr(elab, ctx, e->advance, rty);
            if (e->cond) elaborate_expr(elab, ctx, e->cond, rty);
            if (e->init) elaborate_expr(elab, ctx, e->init, rty);
            return;
        }
        case AST_DECL:
        {
            struct Decl* d = top;
            if (!d->type) return;

            if (d->type->kind == AST_DECLARR)
            {
                struct DeclArr* arr = (void*)d->type;
                if (arr->arity == NULL)
                {
                    if (d->init)
                    {
                        // arr->integer_arity = block->extent;
                        if (d->init->kind == AST_INIT)
                        {
                            struct ASTInit* i = (void*)d->init;
                            arr->integer_arity = i->extent;
                        }
                        else if (d->init->kind == EXPR_LIT)
                        {
                            struct ExprLit* l = (void*)d->init;
                            arr->integer_arity = strlen(l->text) + 1;
                        }
                        else
                        {
                            parser_tok_error(
                                d->id,
                                "error: array initializer must be either a string literal or an initializer list\n");
                        }
                    }
                    else
                    {
                        parser_tok_error(
                            d->id,
                            "error: definition of variable with array type needs an explicit size or an initializer\n");
                    }
                }
            }

            typestr_from_decltype(elab->p->expr_seqs.data, elab->types, rty, d->type);
            if (d->init)
            {
                struct TypeStr init_ty;
                struct Decl* parent = ctx->decl;
                ctx->decl = d;
                elaborate_expr(elab, ctx, d->init, &init_ty);
                ctx->decl = parent;
                // TODO: ensure valid initialization
                // typestr_implicit_conversion(elab->types, expr_to_rc(d->init), &init_ty, rty);
            }
            return;
        }
        case AST_INIT:
        {
            struct ASTInit* block = top;
            // TODO: validate initializer lists
            for (size_t i = 0; i < block->extent; ++i)
            {
                struct Expr* e = ((struct Expr**)elab->p->expr_seqs.data)[block->offset + i];
                struct TypeStr s;
                elaborate_expr(elab, ctx, e, &s);
            }
            typestr_from_decltype(elab->p->expr_seqs.data, elab->types, rty, ctx->decl->type);
            return;
        }
        case STMT_DECLS:
        {
            struct StmtDecls* stmt = top;
            elaborate_exprs(elab, ctx, stmt->offset, stmt->extent);
            *rty = s_type_void;
            return;
        }
        case STMT_BLOCK:
        {
            struct StmtBlock* stmt = top;
            elaborate_exprs(elab, ctx, stmt->offset, stmt->extent);
            *rty = s_type_void;
            return;
        }
        case STMT_SWITCH:
        {
            struct StmtSwitch* stmt = top;
            elaborate_expr(elab, ctx, stmt->expr, rty);
            typestr_decay(rty);
            if (!(typestr_mask(rty) & TYPE_MASK_SCALAR))
            {
                typestr_error1(&stmt->tok->rc,
                               elab->types,
                               "error: expected scalar type in switch condition but got '%.*s'\n",
                               rty);
            }
            elaborate_exprs(elab, ctx, stmt->offset, stmt->extent);
            *rty = s_type_void;
            return;
        }
        default:
            parser_tok_error(NULL, "error: unknown ast kind: %s\n", ast_kind_to_string(top_expr->kind));
            *rty = s_type_unknown;
            return;
    }
}

static __forceinline size_t round_to_alignment(size_t size, size_t align)
{
    size_t n = size + align - 1;
    return n - (n % align);
}

static int elaborate_decl(struct Elaborator* elab, struct Decl* decl)
{
    int rc = 0;
    if (decl->type)
    {
        decl->size = get_decl_size(elab, decl->type);
        decl->align = get_decl_align(elab, decl->type);
        if (!decl->init) return 0;

        struct ElaborateDeclCtx ctx = {
            .decl = decl,
        };

        struct TypeStr ty;
        elaborate_expr(elab, &ctx, decl->init, &ty);
        UNWRAP(parser_has_errors());
    }
    else if (decl->specs->is_enum && decl->init)
    {
        if (decl->init->kind != STMT_DECLS) abort();
        struct StmtDecls* block = (struct StmtDecls*)decl->init;
        struct Expr** const seqs = elab->p->expr_seqs.data;
        int enum_value = 0;
        for (size_t i = 0; i < block->extent; ++i)
        {
            if (seqs[i + block->offset]->kind != AST_DECL) abort();
            struct Decl* edecl = (struct Decl*)seqs[i + block->offset];
            if (edecl->init)
            {
                edecl->enum_value = enum_value = eval_constant(elab, edecl->init);
            }
            else
            {
                edecl->enum_value = enum_value++;
            }
            edecl->is_enum_constant = 1;
        }

        if (decl->name)
        {
            size_t i = tt_find_insert_null(elab->types, decl->name);
            struct Decl** d = tt_get_decl(elab->types, i);
            if (*d != NULL)
            {
                UNWRAP(parser_tok_error(decl->id, "error: redefinition of enum '%s'\n", decl->name));
            }
            *d = decl;
        }
    }
    else if (decl->init)
    {
        // struct/union definition
        if (decl->init->kind != STMT_BLOCK) abort();
        struct StmtBlock* block = (struct StmtBlock*)decl->init;

        size_t struct_align = 1;
        size_t struct_size = 0;

        struct Expr** const seqs = elab->p->expr_seqs.data;
        for (size_t i = 0; i < block->extent; ++i)
        {
            if (seqs[i + block->offset]->kind != STMT_DECLS) abort();
            struct StmtDecls* decls = (struct StmtDecls*)seqs[i + block->offset];
            for (size_t j = 0; j < decls->extent; ++j)
            {
                if (seqs[decls->offset + j]->kind != AST_DECL) abort();
                struct Decl* field = (struct Decl*)seqs[decls->offset + j];
                // nested struct definition
                if (field->specs->is_struct) continue;
                if (field->init && field->name)
                {
                    return parser_tok_error(field->id, "error: structure and union fields cannot have initializers\n");
                }
                UNWRAP(elaborate_decl(elab, field));
                // insert padding
                struct_size = round_to_alignment(struct_size, field->align);
                field->frame_offset = struct_size;
                struct_size += field->size;
                if (struct_align < field->align) struct_align = field->align;
            }
        }

        if (struct_size == 0) struct_size = 1;
        struct_size = round_to_alignment(struct_size, struct_align);
        decl->align = struct_align;
        decl->size = struct_size;

        if (decl->name)
        {
            size_t i = tt_find_insert_null(elab->types, decl->name);
            struct Decl** d = tt_get_decl(elab->types, i);
            if (*d != NULL)
            {
                UNWRAP(parser_tok_error(decl->id, "error: redefinition of struct/union '%s'\n", decl->name));
            }
            *d = decl;
        }
    }

fail:
    return rc;
}

static int elaborate_decls(struct Elaborator* elab, struct Expr** declstmts, size_t count)
{
    int rc = 0;
    struct Expr** const seqs = elab->p->expr_seqs.data;
    for (size_t i = 0; i < count; ++i)
    {
        if (declstmts[i]->kind != STMT_DECLS) abort();
        struct StmtDecls* decls = (struct StmtDecls*)declstmts[i];
        for (size_t j = 0; j < decls->extent; ++j)
        {
            if (seqs[decls->offset + j]->kind != AST_DECL) abort();
            struct Decl* decl = (struct Decl*)seqs[decls->offset + j];

            UNWRAP(elaborate_decl(elab, decl));
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
    return elaborate_decls(elab, p->arr_exprs.data, p->arr_exprs.sz / sizeof(struct Expr*));
}

static void tt_destroy(struct TypeTable* tt)
{
    autoheap_destroy(&tt->typenames);
    array_destroy(&tt->decls);
}
void elaborator_destroy(struct Elaborator* elab)
{
    tt_destroy(elab->types);
    my_free(elab->types);
}
