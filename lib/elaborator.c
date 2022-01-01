#include "elaborator.h"

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
    Y(TYPE_BYTE_DOUBLE, 'D')

#define X_TYPE_BYTE(Y)                                                                                                 \
    Y(TYPE_BYTE_VOID, 'V')                                                                                             \
    X_TYPE_BYTE_ARITH(Y)                                                                                               \
    Y(TYPE_BYTE_STRUCT, '$')                                                                                           \
    Y(TYPE_BYTE_UNION, 'u')                                                                                            \
    Y(TYPE_BYTE_ENUM, 'e')

#define Y_COMMA(E, CH) E = CH,
enum
{
    X_TYPE_BYTE(Y_COMMA)
};
#undef Y_COMMA

static const struct TypeStr s_type_literal_int = {.buf = {1, TYPE_BYTE_INT}};
static const struct TypeStr s_type_literal_char = {.buf = {1, TYPE_BYTE_CHAR}};
static const struct TypeStr s_type_void = {.buf = {1, TYPE_BYTE_VOID}};

static void typestr_fmt(const struct TypeTable* e, const struct TypeStr* ts, struct Array* buf)
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
            case '.': str = "..."; goto append_ret;
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
            case 'p': str = "pointer to "; goto append_continue;
            case 'c': str = "const "; goto append_continue;
            case 'v': str = "volatile "; goto append_continue;
            case 'r': str = "restrict "; goto append_continue;
            case ']': str = "array of "; goto append_continue;
            case '[':
            {
                unsigned int u;
                i -= sizeof(u);
                memcpy(&u, ts->buf + i, sizeof(u));
                array_appendf(buf, "array of %u ", u);
                --i;
                continue;
            }
            case TYPE_BYTE_STRUCT: str = "struct"; goto sue;
            case TYPE_BYTE_UNION: str = "union"; goto sue;
            case TYPE_BYTE_ENUM: str = "enum"; goto sue;
            case '(':
            {
                --i;
                array_appends(buf, "function (");
                if (ts->buf[i] == ')')
                {
                    str = ") returning ";
                    goto append_continue;
                }
                ++depth;
                continue;
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
        array_appendf(buf, "%s %s", str, tt_get_name(e, u));
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

#if 0
static void* array_find_ptr(void* arr_start, size_t arr_size, void* key)
{
    for (size_t i = 0; i < arr_size; i += sizeof(void*))
    {
        void* elem = arr_start + i;
        if (*(void**)elem == key) return elem;
    }
    return NULL;
}
static int is_builtin_fn_expr(struct Expr* fn)
{
    if (fn->kind != EXPR_SYM) return 0;
    struct ExprSym* sym = (struct ExprSym*)fn;
    if (sym->decl->attr.asmstr) return 1;
    return 0;
}
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

static int typestr_dereference(struct Elaborator* e, struct TypeStr* src, const struct RowCol* rc)
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
    typestr_format_english(e, src, buf, sizeof(buf));
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

static int typestr_is_struct(struct TypeStr* ts)
{
    if (!ts->used) return 0;
    return ts->buf[0] == '$' && memchr(ts->buf + 1, '$', ts->used - 1) == (ts->buf + ts->used - 1);
}

static int typestr_unify_decay_scalar(struct Elaborator* e, struct TypeStr* ts, const struct RowCol* rc)
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
    typestr_format_english(e, ts, buf, sizeof(buf));

    return parser_ferror(rc, "error: unexpected type, expected scalar type (e.g. int or pointer) but got '%s'\n", buf);
}

#endif

__forceinline int typestr_is_array(const struct TypeStr* ts) { return ts->buf[(int)ts->buf[0]] == ']'; }
__forceinline int typestr_is_pointer(const struct TypeStr* ts) { return ts->buf[(int)ts->buf[0]] == 'p'; }
__forceinline int typestr_is_const(const struct TypeStr* ts) { return ts->buf[(int)ts->buf[0]] == 'c'; }
__forceinline int typestr_is_restrict(const struct TypeStr* ts) { return ts->buf[(int)ts->buf[0]] == 'r'; }
__forceinline int typestr_is_volatile(const struct TypeStr* ts) { return ts->buf[(int)ts->buf[0]] == 'v'; }

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
        case '.': return TYPE_FLAGS_VAR;
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
        case TYPE_BYTE_DOUBLE: return TYPE_FLAGS_FLOAT;
        case 'p': return TYPE_FLAGS_POINTER;
        case ']':
        case '[': return TYPE_FLAGS_ARRAY;
        case TYPE_BYTE_STRUCT: return TYPE_FLAGS_STRUCT;
        case TYPE_BYTE_UNION: return TYPE_FLAGS_UNION;
        case '(': return TYPE_FLAGS_FUNCTION;
        default: abort();
    }
}

struct Decl* typestr_get_decl(struct TypeTable* tt, const struct TypeStr* ts)
{
    char ch = ts->buf[(int)ts->buf[0]];
    if (ch == TYPE_BYTE_STRUCT || ch == TYPE_BYTE_UNION)
    {
        uint32_t x;
        memcpy(&x, ts->buf + (int)ts->buf[0] - sizeof(x), sizeof(x));
        return *tt_get_decl(tt, x);
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
        case ']': t->buf[(int)t->buf[0]] = 'p'; return;
        case '[':
        {
            t->buf[0] -= 4;
            t->buf[(int)t->buf[0]] = 'p';
            return;
        }
        case '(':
        {
            t->buf[0]++;
            if (t->buf[0] == TYPESTR_BUF_SIZE) abort();
            t->buf[(int)t->buf[0]] = 'p';
            return;
        }
        default: return;
    }
}

static void typestr_pop(struct TypeStr* ts, struct TypeStr* into)
{
    size_t i = ts->buf[0];
    size_t depth = 0;
    while (1)
    {
        char ch = ts->buf[i];
        switch (ch)
        {
            case '\0': *into = s_type_unknown; return;
            case '.': goto append_ret;
            case TYPE_BYTE_VOID: goto append_ret;
            case TYPE_BYTE_CHAR: goto append_ret;
            case TYPE_BYTE_SCHAR: goto append_ret;
            case TYPE_BYTE_UCHAR: goto append_ret;
            case TYPE_BYTE_INT: goto append_ret;
            case TYPE_BYTE_SHORT: goto append_ret;
            case TYPE_BYTE_LONG: goto append_ret;
            case TYPE_BYTE_LLONG: goto append_ret;
            case TYPE_BYTE_UINT: goto append_ret;
            case TYPE_BYTE_USHORT: goto append_ret;
            case TYPE_BYTE_ULONG: goto append_ret;
            case TYPE_BYTE_ULLONG: goto append_ret;
            case TYPE_BYTE_FLOAT: goto append_ret;
            case TYPE_BYTE_DOUBLE: goto append_ret;
            case 'p':
            case 'c':
            case 'v':
            case 'r':
            case ']': --i; continue;

            case TYPE_BYTE_STRUCT:
            case TYPE_BYTE_UNION:
            case TYPE_BYTE_ENUM: i -= sizeof(unsigned int); goto append_ret;
            case '[': i -= sizeof(unsigned int) + 1; continue;

            case '(':
            {
                --i;
                if (ts->buf[i] == ')')
                {
                    --i;
                    continue;
                }
                ++depth;
                continue;
            }
            default: abort();
        }

    append_ret:
        --i;
        if (depth)
        {
            if (ts->buf[i] == ')')
            {
                --depth;
                --i;
            }
            continue;
        }
        // recognized at i
        into->buf[0] = ts->buf[0] - i;
        memcpy(into->buf + 1, ts->buf + i, into->buf[0]);
        ts->buf[0] = i;
        return;
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
#if 0
static void typestr_dereference(const struct RowCol* rc, const struct TypeTable* e, struct TypeStr* t)
{
    struct TypeStr s = *t;
    typestr_decay(&s);
    if (s.buf[(int)s.buf[0]] == 'p')
    {
        --s.buf[0];
        *t = s;
    }
    else
    {
        typestr_error1(rc, e, "error: unable to dereference value of type '%.*s'\n", t);
        *t = s_type_unknown;
    }
}
#endif

static int typestr_match(const struct TypeStr* tgt, const struct TypeStr* src)
{
    return memcmp(tgt->buf, src->buf, tgt->buf[0]) == 0;
}
//
///// <param name="tgt">expected type</param>
///// <param name="src">actual type</param>
// static void typestr_unify_decay(struct Elaborator* e, struct TypeStr tgt, struct TypeStr src, const struct RowCol*
// rc)
//{
//    if (!tgt.expr || !src.expr) return;
//
//    typestr_decay(&tgt);
//    typestr_decay(&src);
//
//    if (!typestr_match(e, tgt, src))
//    {
//        struct Array s_tgt = {}, s_src = {};
//        typestr_fmt(&tgt, &s_tgt);
//        typestr_fmt(&src, &s_src);
//        parser_ferror(rc,
//                      "error: could not match types: expected '%.*s' but found '%.*s'",
//                      s_tgt.sz,
//                      s_tgt.data,
//                      s_src.sz,
//                      s_src.data);
//        array_destroy(&s_tgt);
//        array_destroy(&s_src);
//    }
//}

static void typestr_add_array(struct TypeStr* s, unsigned int n)
{
    size_t o = s->buf[0] += 1 + sizeof(n);
    if (o >= TYPESTR_BUF_SIZE) abort();
    s->buf[o] = '[';
    memcpy(s->buf + (o - sizeof(n)), &n, sizeof(n));
}

static void typestr_add_cvr(struct TypeStr* s, unsigned int mask)
{
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

static void typestr_append_decltype(struct TypeTable* tt, struct TypeStr* s, struct Expr* e)
{
top:
    if (!e) abort();
    switch (e->kind)
    {
        case EXPR_SYM:
        {
            e = &((struct ExprSym*)e)->decl->kind;
            goto top;
        }
        case AST_DECLSPEC:
        {
            if (TYPESTR_BUF_SIZE <= s->buf[0] + sizeof(uint32_t) + 1) abort();
            struct DeclSpecs* d = (struct DeclSpecs*)e;
            if (d->type)
            {
                typestr_append_decltype(tt, s, d->type->type);
            }
            else if (d->name)
            {
                uint32_t offset = tt_find_insert_null(tt, d->name);
                int i = s->buf[0] + 1;
                memcpy(s->buf + i, &offset, sizeof(offset));
                i += sizeof(offset);
                if (d->is_struct)
                {
                    s->buf[i] = TYPE_BYTE_STRUCT;
                }
                else if (d->is_union)
                {
                    s->buf[i] = TYPE_BYTE_UNION;
                }
                else if (d->is_enum)
                {
                    s->buf[i] = TYPE_BYTE_ENUM;
                }
                else
                {
                    abort();
                }
                s->buf[0] = i;
            }
            else
            {
                char ch;
                switch (d->tok->type)
                {
                    case LEX_VOID: ch = TYPE_BYTE_VOID; break;
                    case LEX_LONG: ch = d->is_unsigned ? TYPE_BYTE_ULONG : TYPE_BYTE_LONG; break;
                    case LEX_INT: ch = d->is_unsigned ? TYPE_BYTE_UINT : TYPE_BYTE_INT; break;
                    case LEX_SHORT: ch = d->is_unsigned ? TYPE_BYTE_USHORT : TYPE_BYTE_SHORT; break;
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
            e = ((struct Decl*)e)->type;
            goto top;
        }
        case AST_DECLPTR:
        {
            struct DeclPtr* d = (struct DeclPtr*)e;
            typestr_append_decltype(tt, s, d->type);
            if (s->buf[0] + 1 >= TYPESTR_BUF_SIZE) abort();
            s->buf[(int)++s->buf[0]] = 'p';
            unsigned int m = 0;
            if (d->is_const) m |= TYPESTR_CVR_C;
            if (d->is_volatile) m |= TYPESTR_CVR_V;
            if (d->is_restrict) m |= TYPESTR_CVR_R;
            typestr_add_cvr(s, m);
            return;
        }
        default: break;
    }
    fprintf(stderr, "typestr_append_decltype(, %s)\n", ast_kind_to_string(e->kind));
    *s = s_type_unknown;
}
static void typestr_from_decltype(struct TypeTable* tt, struct TypeStr* s, struct Expr* e)
{
    // initialize type
    *s = s_type_unknown;
    typestr_append_decltype(tt, s, e);
}
static int operator_is_relation(const char* op)
{
    return (op[0] == '<' || op[0] == '>') || ((op[0] == '=' || op[0] == '!') && op[1] == '=');
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
            if (!h->type) continue;
            if (!h->name) abort();
            if (strcmp(h->name, fieldname) == 0) return h;
        }
    }
    return NULL;
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
            else if (expr->tok->type == LEX_STRING)
            {
                *rty = s_type_literal_char;
                // TODO: calculate string lengths
                typestr_add_array(rty, 3);
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
            typestr_from_decltype(elab->types, rty, esym->decl->type);
            return;
        }
        case EXPR_CAST:
        {
            struct ExprCast* expr = top;
            elaborate_expr(elab, ctx, expr->expr, rty);
            typestr_from_decltype(elab->types, rty, expr->type->type);
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
            if (rty->buf[(int)rty->buf[0]] != '(')
            {
                typestr_error1(&expr->tok->rc, elab->types, "error: expected function type but got '%.*s'\n", rty);
                *rty = s_type_unknown;
                return;
            }
            // pop into function type
            --rty->buf[0];
            struct TypeStr expected_aty;
            struct Expr** exprs = elab->p->expr_seqs.data;
            size_t i = 0;
            while (rty->buf[(int)rty->buf[0]] != ')' && i < expr->extent)
            {
                struct Expr* arg_expr = exprs[expr->offset + i];
                struct TypeStr aty;
                elaborate_expr(elab, ctx, arg_expr, &aty);
                const struct TypeStr orig_aty = aty;
                typestr_decay(&aty);
                typestr_pop(rty, &expected_aty);
                typestr_decay(&expected_aty);
                if (!typestr_match(&aty, &expected_aty))
                {
                    // expected to match
                    typestr_error2(expr_to_rc(arg_expr),
                                   elab->types,
                                   "error: expected type '%.*s' but got '%.*s'\n",
                                   &expected_aty,
                                   &orig_aty);
                    *rty = s_type_unknown;
                    return;
                }
                ++i;
            }
            if (i < expr->extent)
            {
                parser_tok_error(expr->tok,
                                 "error: too many arguments in function call: got %zu but expected %zu\n",
                                 expr->extent,
                                 i);
                *rty = s_type_unknown;
                return;
            }
            if (rty->buf[(int)rty->buf[0]] != ')')
            {
                parser_tok_error(expr->tok, "error: too few arguments in function call: got %zu\n", expr->extent);
                *rty = s_type_unknown;
                return;
            }
            // pop ')'
            --rty->buf[0];
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
                    typestr_from_decltype(elab->types, rty, field->type);
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
        case EXPR_OP:
        {
            struct ExprOp* e = top;
            const char* op = token_str(elab->p, e->tok);
            elaborate_expr(elab, ctx, e->lhs, rty);
            const struct TypeStr orig_lhs = *rty;
            struct TypeStr rhs_ty;
            if (e->rhs)
                elaborate_expr(elab, ctx, e->rhs, &rhs_ty);
            else
                rhs_ty = s_type_unknown;
            const struct TypeStr orig_rhs = rhs_ty;
            typestr_decay(rty);
            typestr_decay(&rhs_ty);
            unsigned int lhs_mask = typestr_mask(rty);
            unsigned int rhs_mask = typestr_mask(&rhs_ty);
            if (op[0] == '*' && op[1] == '\0' && !e->rhs)
            {
                typestr_error1(&e->tok->rc, elab->types, "error: unimplemented '*' on '%.*s'\n", rty);
                *rty = s_type_unknown;
            }
            else if (op[0] == '%' && op[1] == '\0')
            {
                if (!(lhs_mask & TYPE_FLAGS_INT))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected integer type in first argument but got '%.*s'\n",
                                   &orig_lhs);
                    *rty = s_type_literal_int;
                }
                if (!(rhs_mask & TYPE_FLAGS_INT))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected integer type in second argument but got '%.*s'\n",
                                   &orig_rhs);
                }
            }
            else if ((op[0] == '/' || op[0] == '*') && op[1] == '\0')
            {
                if (!(lhs_mask & TYPE_MASK_ARITH))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected arithmetic type in first argument but got '%.*s'\n",
                                   &orig_lhs);
                    *rty = s_type_literal_int;
                }
                if (!(rhs_mask & TYPE_MASK_ARITH))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected arithmetic type in second argument but got '%.*s'\n",
                                   &orig_rhs);
                }
            }
            else if (op[0] == '!' && op[1] == '\0')
            {
                if (!(lhs_mask & TYPE_MASK_SCALAR))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected scalar type in first argument but got '%.*s'\n",
                                   &orig_lhs);
                }
                *rty = s_type_literal_int;
            }
            else if (op[0] == '+' && op[1] == '\0')
            {
                if (!(lhs_mask & TYPE_MASK_SCALAR))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected scalar type in first argument but got '%.*s'\n",
                                   &orig_lhs);
                    *rty = s_type_unknown;
                }
                if (!(rhs_mask & TYPE_MASK_SCALAR))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected scalar type in second argument but got '%.*s'\n",
                                   &orig_rhs);
                }
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
            }
            else if (op[0] == '-' && op[1] == '\0')
            {
                if (!(lhs_mask & TYPE_MASK_SCALAR))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected scalar type in first argument but got '%.*s'\n",
                                   &orig_lhs);
                    *rty = s_type_unknown;
                }
                if (!(rhs_mask & TYPE_MASK_SCALAR))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected scalar type in second argument but got '%.*s'\n",
                                   &orig_rhs);
                }
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
            }
            else if (operator_is_relation(op))
            {
                if (!(lhs_mask & TYPE_MASK_SCALAR))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected scalar type in first argument but got '%.*s'\n",
                                   &orig_lhs);
                }
                if (!(rhs_mask & TYPE_MASK_SCALAR))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected scalar type in second argument but got '%.*s'\n",
                                   &orig_rhs);
                }
                *rty = s_type_literal_int;
            }
            else if ((op[0] == '&' || op[0] == '|') && op[1] == op[0])
            {
                if (!(lhs_mask & TYPE_MASK_SCALAR))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected scalar type in first argument but got '%.*s'\n",
                                   &orig_lhs);
                }
                if (!(rhs_mask & TYPE_MASK_SCALAR))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected scalar type in second argument but got '%.*s'\n",
                                   &orig_rhs);
                }
                *rty = s_type_literal_int;
            }
            else if (op[0] == '=' && op[1] == '\0')
            {
                if (!typestr_match(&orig_lhs, &orig_rhs))
                {
                    typestr_error2(
                        &e->tok->rc, elab->types, "error: expected type '%.*s' but got '%.*s'\n", &orig_lhs, &orig_rhs);
                    *rty = s_type_unknown;
                }
            }
            else if ((op[0] == '+' || op[0] == '-') && op[1] == op[0])
            {
                typestr_error1(&e->tok->rc, elab->types, "error: '++' and '--' not implemented on '%.*s'\n", rty);
            }
            else if ((op[0] == '+' || op[0] == '-') && op[1] == '=')
            {
                typestr_error1(&e->tok->rc, elab->types, "error: '+=' and '-=' not implemented on '%.*s'\n", rty);
            }
            else if (op[0] == '&' && op[1] == '\0')
            {
                if (!(lhs_mask & TYPE_FLAGS_INT))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected integer type in first argument but got '%.*s'\n",
                                   &orig_lhs);
                    *rty = s_type_literal_int;
                }
                if (!(rhs_mask & TYPE_FLAGS_INT))
                {
                    typestr_error1(&e->tok->rc,
                                   elab->types,
                                   "error: expected integer type in second argument but got '%.*s'\n",
                                   &orig_rhs);
                }
            }
            else if (op[0] == '[' && op[1] == '\0')
            {
                typestr_error1(&e->tok->rc, elab->types, "error: unimplemented '[' on '%.*s'\n", rty);
                *rty = s_type_unknown;
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
            if (d->init)
            {
                struct TypeStr decl_ty;
                typestr_from_decltype(elab->types, &decl_ty, d->type);
                elaborate_expr(elab, ctx, d->init, rty);
                const struct TypeStr orig_rhs = *rty;
                typestr_decay(rty);
                if (!typestr_match(&decl_ty, rty))
                {
                    typestr_error2(
                        &d->id->rc, elab->types, "error: expected type '%.*s' but got '%.*s'\n", &decl_ty, &orig_rhs);
                }
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

static long long eval_constant(struct Elaborator* elab, struct Expr* e)
{
    switch (e->kind)
    {
        case EXPR_LIT:
        {
            struct ExprLit* lit = (struct ExprLit*)e;
            if (lit->tok->type == LEX_NUMBER)
            {
                return atoll(lit->text);
            }
            parser_ferror(expr_to_rc(e), "error: expected integer constant expression\n");
            return 0;
        }
        default: parser_ferror(expr_to_rc(e), "error: expected integer constant expression\n"); return 0;
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
                    parser_tok_error(d->tok, "error: incomplete type\n");
                    return 1;
                }
                return inner_decl->align;
            }

            switch (d->tok->type)
            {
                case LEX_VOID: parser_tok_error(d->tok, "error: cannot align field of type void\n"); return 1;
                case LEX_LONG: return 4;
                case LEX_INT: return 4;
                case LEX_SHORT: return 2;
                case LEX_CHAR: return 1;
                default: abort();
            }
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
                parser_tok_error(d->tok, "error: array definition requires explicit size\n");
                return 1;
            }
            long long numeric_arity = eval_constant(elab, d->arity);
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
                    parser_tok_error(d->tok, "error: incomplete type\n");
                    return 1;
                }
                return get_decl_size(elab, &inner_decl->kind);
            }

            switch (d->tok->type)
            {
                case LEX_VOID: parser_tok_error(d->tok, "error: cannot declare field of type void\n"); return 1;
                case LEX_LONG: return 4;
                case LEX_INT: return 4;
                case LEX_SHORT: return 2;
                case LEX_CHAR: return 1;
                default: abort();
            }
        }
        default: parser_ferror(expr_to_rc(e), "error: cannot calculate size of decl\n"); return 1;
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
    else
    {
        // SUE definition
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
                if (field->init)
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

        size_t i = tt_find_insert_null(elab->types, decl->name);
        struct Decl** d = tt_get_decl(elab->types, i);
        if (*d != NULL)
        {
            UNWRAP(parser_tok_error(decl->id, "error: redefinition of structure/union/enumeration '%s'\n", decl->name));
        }
        *d = decl;
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
