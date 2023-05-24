#include "cg.h"

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "errors.h"
#include "stdlibe.h"
#include "tac.h"
#include "tok.h"
#include "unwrap.h"

void cg_init(struct CodeGen* cg)
{
    memset(cg, 0, sizeof(struct CodeGen));
#ifdef __APPLE__
    cg->target = CG_TARGET_MACOS_GAS;
#else
    cg->target = CG_TARGET_LINUX_GAS;
#endif
}
void cg_destroy(struct CodeGen* cg)
{
    array_destroy(&cg->const_);
    array_destroy(&cg->data);
    array_destroy(&cg->code);
    sm_destroy(&cg->file_numbers);
}

enum
{
    TACA_VOID_IS_MEMORY = 0,
    TACA_LITERAL_IS_MEMORY = 0,
    TACA_IMM_IS_MEMORY = 0,
    TACA_NAME_IS_MEMORY = 1,
    TACA_LNAME_IS_MEMORY = 1,
    TACA_FRAME_IS_MEMORY = 1,
    TACA_REF_IS_MEMORY = 1,
    TACA_PARAM_IS_MEMORY = 1,
    TACA_CONST_IS_MEMORY = 1,
    TACA_ARG_IS_MEMORY = 1,
    TACA_ALABEL_IS_MEMORY = 0,
    TACA_LLABEL_IS_MEMORY = 1,
    TACA_REG_IS_MEMORY = 0,
    TACA_THROUGH_REG_IS_MEMORY = 1,
};

#define Y_IS_MEMORY(Z) Z##_IS_MEMORY,
static const char s_table_taca_is_memory[TACA_KIND_COUNT] = {X_TACA_KIND(Y_IS_MEMORY)};
#undef Y_IS_MEMORY

__forceinline static int taca_is_memory(const struct TACAddress* addr)
{
    return !addr->is_addr && s_table_taca_is_memory[addr->kind];
}

static void cg_debug(struct CodeGen* cg, const char* fmt, ...)
{
    if (cg->fdebug)
    {
        va_list argp;
        va_start(argp, fmt);
        vfprintf(cg->fdebug, fmt, argp);
        va_end(argp);
    }
}

void cg_declare_extern(struct CodeGen* cg, const char* sym)
{
    cg_debug(cg, "   : %s\n", sym);
    if (cg->target == CG_TARGET_WIN_MASM)
        array_appendf(&cg->code, "extern %s:proc\n", sym);
    else
        ;
}

static void cg_mangle_sym(CodeGen* cg, Array* arr, const char* sym)
{
    if (cg->target == CG_TARGET_MACOS_GAS) array_push_byte(arr, '_');
    array_appends(arr, sym);
}

static void cg_mangle_label(CodeGen* cg, Array* arr, size_t n)
{
    if (cg->target == CG_TARGET_LINUX_GAS) array_push_byte(arr, '.');
    array_appendf(arr, "L$%zu", n);
}

static void cg_mangle_const(CodeGen* cg, Array* arr, size_t n)
{
    if (cg->target == CG_TARGET_LINUX_GAS) array_push_byte(arr, '.');
    array_appendf(arr, "L_.S%zu", n);
}

void cg_declare_public(struct CodeGen* cg, const char* sym)
{
    static const char* const public_fmt[] = {
        [CG_TARGET_WIN_MASM] = "public ",
        [CG_TARGET_LINUX_GAS] = ".globl ",
        [CG_TARGET_MACOS_GAS] = ".globl ",
    };
    cg_debug(cg, "   : %s\n", sym);
    array_appendf(&cg->code, public_fmt[cg->target], sym);
    cg_mangle_sym(cg, &cg->code, sym);
    array_push_byte(&cg->code, '\n');
}

void cg_start_function(struct CodeGen* cg, const char* sym) { cg_mark_label(cg, sym); }

void cg_mark_label(struct CodeGen* cg, const char* sym)
{
    cg_debug(cg, "   : %s\n", sym);
    cg_mangle_sym(cg, &cg->code, sym);
    array_appends(&cg->code, ":\n");
}

void cg_mark_alabel(struct CodeGen* cg, size_t n)
{
    cg_debug(cg, "   : L$%zu\n", n);
    cg_mangle_label(cg, &cg->code, n);
    array_appends(&cg->code, ":\n");
}

size_t cg_next_alabel(struct CodeGen* cg) { return cg->next_label++; }

static int needs_escape(char ch) { return !(ch >= 32 && ch < 127 && ch != '"' && ch != '\\'); }

void cg_string_constant(struct CodeGen* cg, size_t cidx, const char* str, size_t sz)
{
    // cg_debug(cg, "   : strconst %d: %s\n", cidx, str);
    cg_mangle_const(cg, &cg->const_, cidx);
    array_appends(&cg->const_, ": .asciz \"");
    for (size_t i = 0; i < sz; ++i)
    {
        if (needs_escape(str[i]))
        {
            array_appendf(&cg->const_, "\\%03o", (unsigned char)str[i]);
        }
        else
        {
            array_push_byte(&cg->const_, str[i]);
        }
    }
    array_appends(&cg->const_, "\"\n");
}
void cg_reserve_data(struct CodeGen* cg, const char* name, const char* data, const TACAddress* const* bases, size_t sz)
{
    if (sz == 0) abort();
    array_appends(&cg->data, ".p2align 3\n");
    cg_mangle_sym(cg, &cg->data, name);
    array_appends(&cg->data, ":\n");
    size_t zeroes = 0;
    size_t i = 0;
    size_t j = 0;
    const size_t sz8 = sz >> 3;
    for (; j < sz8; ++j, i = j * 8)
    {
        size_t offset;
        memcpy(&offset, data + i, 8);
        const TACAddress* base = bases[j];
        if (!base && !offset)
        {
            zeroes += 8;
            continue;
        }
        if (zeroes)
        {
            array_appendf(&cg->data, ".space %zu\n", zeroes);
            zeroes = 0;
        }
        if (base && base->kind == TACA_CONST)
        {
            array_appends(&cg->data, ".quad ");
            cg_mangle_const(cg, &cg->data, base->const_idx);
            array_appendf(&cg->data, " + %zu\n", offset);
        }
        else if (base && (base->kind == TACA_LNAME || base->kind == TACA_NAME))
        {
            array_appends(&cg->data, ".quad ");
            cg_mangle_sym(cg, &cg->data, base->name);
            array_appendf(&cg->data, " + %zu\n", offset);
        }
        else if (base)
        {
            abort();
        }
        else
        {
            array_appendf(&cg->data,
                          ".byte %u, %u, %u, %u, %u, %u, %u, %u\n",
                          (unsigned char)data[i],
                          (unsigned char)data[i + 1],
                          (unsigned char)data[i + 2],
                          (unsigned char)data[i + 3],
                          (unsigned char)data[i + 4],
                          (unsigned char)data[i + 5],
                          (unsigned char)data[i + 6],
                          (unsigned char)data[i + 7]);
        }
    }
    if (zeroes)
    {
        array_appendf(&cg->data, ".space %zu\n", zeroes);
        zeroes = 0;
    }
    if (i != sz)
    {
        array_appendf(&cg->data, ".byte %u", (unsigned char)data[i]);
        ++i;
        for (; i < sz; ++i)
        {
            array_appendf(&cg->data, ", %u", (unsigned char)data[i]);
        }
        array_push_byte(&cg->data, '\n');
    }
    array_push_byte(&cg->data, '\n');
}
void cg_reserve_zeroes(struct CodeGen* cg, const char* name, size_t sz)
{
    if (sz == 0) abort();
    cg_mangle_sym(cg, &cg->data, name);
    array_appendf(&cg->data, ": .skip %zu\n", sz);
}

static const char* const s_reg_names[] = {
    "%rax", "%rbx", "%rcx", "%rdx", "%rdi", "%rsi", "%r8", "%r9", "%r10", "%r11", "%rsp", "%rbp"};
static const char* const s_reg_names_4[] = {
    "%eax", "%ebx", "%ecx", "%edx", "%edi", "%esi", "%r8d", "%r9d", "%r10d", "%r11d", "%esp", "%ebp"};
static const char* const s_reg_names_2[] = {
    "%ax", "%bx", "%cx", "%dx", "%di", "%si", "%r8w", "%r9w", "%r10w", "%r11w", "%sp", "%bp"};
static const char* const s_reg_names_1[] = {
    "%al", "%bl", "%cl", "%dl", "%dil", "%sil", "%r8b", "%r9b", "%r10b", "%r11b", "%spl", "%bpl"};

typedef struct ActivationRecord
{
    unsigned char* frame_slots;
    size_t total_frame_size;
    size_t locals_offset;
    size_t temp_offset;
    size_t arg_offset;

    char reg_usage[REG_COUNT];
} ActivationRecord;

static void ar_reg_clearall(ActivationRecord* frame) { memset(&frame->reg_usage, 0, sizeof(frame->reg_usage)); }
static void ar_reg_check(ActivationRecord* frame, int reg)
{
    if (frame->reg_usage[reg])
    {
        fprintf(stderr, "ERROR: ATTEMPTED TO REUSE REGISTER %s\n", register_to_string(reg));
        fflush(NULL);
        abort();
    }
}
static void ar_reg_use(ActivationRecord* frame, int reg)
{
    ar_reg_check(frame, reg);
    frame->reg_usage[reg] = 1;
}
static int ar_tmp_reg(ActivationRecord* frame)
{
    int reg;
    if (frame->reg_usage[REG_R11] == 0)
    {
        reg = REG_R11;
    }
    else if (frame->reg_usage[REG_R10] == 0)
    {
        reg = REG_R10;
    }
    else if (frame->reg_usage[REG_RAX] == 0)
    {
        reg = REG_RAX;
    }
    else
    {
        reg = REG_RCX;
    }
    ar_reg_check(frame, reg);
    frame->reg_usage[reg] = 1;
    return reg;
}
static void ar_reg_free(ActivationRecord* frame, int reg) { frame->reg_usage[reg] = 0; }

static void cg_gen_taca_reg(struct CodeGen* cg, int reg, Sizing sizing)
{
    switch (sizing.width)
    {
        case 8: array_appends(&cg->code, s_reg_names[reg]); break;
        case 4: array_appends(&cg->code, s_reg_names_4[reg]); break;
        case 2: array_appends(&cg->code, s_reg_names_2[reg]); break;
        case 1: array_appends(&cg->code, s_reg_names_1[reg]); break;
        default: abort();
    }
}

static void cg_gen_taca(struct CodeGen* cg, struct TACAddress addr, struct ActivationRecord* frame)
{
    switch (addr.kind)
    {
        case TACA_NAME:
            cg_mangle_sym(cg, &cg->code, addr.name);
            array_appends(&cg->code, "@GOTPCREL");
            if (addr.offset) array_appendf(&cg->code, "+%zu", addr.offset);
            array_appends(&cg->code, "(%rip)");
            break;
        case TACA_LNAME:
            cg_mangle_sym(cg, &cg->code, addr.name);
            if (addr.offset) array_appendf(&cg->code, "+%zu", addr.offset);
            array_appends(&cg->code, "(%rip)");
            break;
        case TACA_LITERAL:
            if (addr.offset) goto offset_unsupported;
            array_appends(&cg->code, addr.literal);
            break;
        case TACA_IMM:
            if (addr.offset) goto offset_unsupported;
            array_appendf(&cg->code, "$%zu", addr.imm);
            break;
        case TACA_ALABEL:
            if (addr.offset) goto offset_unsupported;
            cg_mangle_label(cg, &cg->code, addr.alabel);
            break;
        case TACA_LLABEL:
            cg_mangle_label(cg, &cg->code, cg->cur_fn_lbl_prefix);
            array_appendf(&cg->code, "_%s", addr.literal);
            break;
        case TACA_REG:
            if (addr.offset) goto offset_unsupported;
            cg_gen_taca_reg(cg, addr.reg, addr.sizing);
            break;
        case TACA_THROUGH_REG:
            if (addr.offset) array_appendf(&cg->code, "%zu", addr.offset);
            array_push_byte(&cg->code, '(');
            array_appends(&cg->code, s_reg_names[addr.reg]);
            array_push_byte(&cg->code, ')');
            break;
        case TACA_CONST:
            cg_mangle_const(cg, &cg->code, addr.const_idx);
            if (addr.offset) array_appendf(&cg->code, "+%zu", addr.offset);
            array_appendf(&cg->code, "(%%rip)");
            break;
        case TACA_REF:
            if (frame->temp_offset + frame->frame_slots[addr.ref] * 8 + addr.offset >= frame->total_frame_size) abort();
            array_appendf(&cg->code, "%zu(%%rsp)", addr.offset + frame->temp_offset + frame->frame_slots[addr.ref] * 8);
            break;
        case TACA_PARAM:
            if (addr.offset >= frame->locals_offset) abort();
            array_appendf(&cg->code, "%zu(%%rsp)", addr.offset);
            break;
        case TACA_FRAME:
            if (frame->temp_offset <= frame->locals_offset + addr.offset) abort();
            array_appendf(&cg->code, "%zu(%%rsp)", frame->locals_offset + addr.offset);
            break;
        case TACA_ARG: array_appendf(&cg->code, "%zu(%%rsp)", 8 + frame->total_frame_size + addr.offset); break;
        default: parser_ferror(NULL, "error: unimplemented TACA: %s\n", taca_to_string(addr.kind)); break;
    }
    return;

offset_unsupported:
    parser_ferror(NULL, "error: unimplemented offset TACA: %s\n", taca_to_string(addr.kind));
    return;
}

enum InstKind
{
    MOV,
    MOVQ,
    MOVL,
    MOVW,
    MOVB,
    MOVSB,
    MOVZB,
    MOVSW,
    MOVZW,
    MOVSL,
    MOVSD,
    MOVSQ,
    MOVSX,
    MOVZX,
    REP_MOVSB,
    REP_STOSB,
    LEAQ,
    SHL,
    SHR,
    CLD,
    INST_OR,
    INST_ORQ,
    INST_AND,
    INST_ANDQ,
    INST_XOR,
    INST_XORQ,
    INST_ADD,
    INST_ADDQ,
    INST_CMP,
    INST_SETL,
    INST_SETLE,
    INST_SETB,
    INST_SETBE,
    INST_SETE,
    INST_SETNE,
    INST_JNZ,
    INST_JZ,
    INST_JMP,
    INST_RET,
    INST_CQTO,
    INST_IDIVQ,
    INST_DIVQ,
    INST_SUBQ,
    INST_IMUL,
    INST_NOT,
    INST_CALLQ,
    INST_CALLQ_INDIRECT,
    BSWAP,
};

enum InstArgKind
{
    IA_NONE,
    IA_U,
    IA_I,
    IA_REG,
    IA_REG_D,
    IA_NAME,
    IA_LNAME,
    IA_LITERAL,
    IA_SYM,
    IA_SYM_PLT,
    IA_LABEL,
    IA_CONST,
};

typedef struct InstArg
{
    enum InstArgKind kind;
    union
    {
        struct
        {
            unsigned reg;
            unsigned regw;
        };
        size_t u8;
        long long i8;
        const char* s;
        size_t cnst;
    };
    union
    {
        size_t offset;
        size_t lbl;
    };
} InstArg;

typedef struct Instruction
{
    enum InstKind kind;
    InstArg a1, a2;
} Instruction;

#define IA_I(x)                                                                                                        \
    {                                                                                                                  \
        .kind = IA_I, .i8 = (x)                                                                                        \
    }

#define IA_U(x)                                                                                                        \
    {                                                                                                                  \
        .kind = IA_U, .u8 = (x)                                                                                        \
    }

#define IA_REG(x)                                                                                                      \
    {                                                                                                                  \
        .kind = IA_REG, .reg = (x), .regw = 8,                                                                         \
    }

// static __forceinline InstArg ia_reg(unsigned reg)
// {
//     InstArg x = IA_REG(reg);
//     return x;
// }

#define IA_REG_W(x, w)                                                                                                 \
    {                                                                                                                  \
        .kind = IA_REG, .reg = (x), .regw = (w)                                                                        \
    }

#define IA_REG_D(x, o)                                                                                                 \
    {                                                                                                                  \
        .kind = IA_REG_D, .reg = (x), .offset = (o)                                                                    \
    }

static __forceinline InstArg ia_reg_d(unsigned reg, size_t offset)
{
    InstArg x = IA_REG_D(reg, offset);
    return x;
}

#define IA_RSP(o) IA_REG_D(REG_RSP, o)

#define IA_NAME(x, o)                                                                                                  \
    {                                                                                                                  \
        .kind = IA_NAME, .s = (x), .offset = (o)                                                                       \
    }
#define IA_LNAME(x, o)                                                                                                 \
    {                                                                                                                  \
        .kind = IA_LNAME, .s = (x), .offset = (o)                                                                      \
    }
#define IA_LITERAL(x)                                                                                                  \
    {                                                                                                                  \
        .kind = IA_LITERAL, .s = (x)                                                                                   \
    }
#define IA_SYM(x)                                                                                                      \
    {                                                                                                                  \
        .kind = IA_SYM, .s = (x)                                                                                       \
    }
#define IA_SYM_PLT(x)                                                                                                  \
    {                                                                                                                  \
        .kind = IA_SYM_PLT, .s = (x)                                                                                   \
    }
#define IA_LABEL(x, l)                                                                                                 \
    {                                                                                                                  \
        .kind = IA_LABEL, .s = (l), .lbl = (x)                                                                         \
    }
#define IA_CONST(x, o)                                                                                                 \
    {                                                                                                                  \
        .kind = IA_CONST, .cnst = (x), .offset = (o)                                                                   \
    }

static const char s_op_neumon[][10] = {
    [MOV] = "mov",
    [MOVQ] = "movq",
    [MOVL] = "movl",
    [MOVW] = "movw",
    [MOVB] = "movb",
    [MOVSB] = "movsb",
    [MOVZB] = "movzb",
    [MOVSW] = "movsw",
    [MOVZW] = "movzw",
    [MOVSL] = "movsl",
    [MOVSD] = "movsd",
    [MOVSQ] = "movsq",
    [MOVSX] = "movsx",
    [MOVZX] = "movzx",
    [REP_MOVSB] = "rep movsb",
    [REP_STOSB] = "rep stosb",
    [LEAQ] = "leaq",
    [SHL] = "shl",
    [SHR] = "shr",
    [CLD] = "cld",
    [INST_ADD] = "add",
    [INST_ADDQ] = "addq",
    [INST_OR] = "or",
    [INST_ORQ] = "orq",
    [INST_AND] = "and",
    [INST_ANDQ] = "andq",
    [INST_XOR] = "xor",
    [INST_XORQ] = "xorq",
    [INST_CMP] = "cmp",
    [INST_SETL] = "setl",
    [INST_SETLE] = "setle",
    [INST_SETB] = "setb",
    [INST_SETBE] = "setbe",
    [INST_SETE] = "sete",
    [INST_SETNE] = "setne",
    [INST_JNZ] = "jnz",
    [INST_JZ] = "jz",
    [INST_JMP] = "jmp",
    [INST_RET] = "ret",
    [INST_CQTO] = "cqto",
    [INST_IDIVQ] = "idivq",
    [INST_DIVQ] = "divq",
    [INST_SUBQ] = "subq",
    [INST_IMUL] = "imul",
    [INST_NOT] = "not",
    [INST_CALLQ] = "callq",
    [INST_CALLQ_INDIRECT] = "callq",
    [BSWAP] = "bswap",
};

static void cg_push_instarg(CodeGen* cg, const InstArg* a)
{
    switch (a->kind)
    {
        case IA_I: array_appendf(&cg->code, "$%zd", a->i8); break;
        case IA_U: array_appendf(&cg->code, "$%zu", a->u8); break;
        case IA_REG:;
            const char* const* names = s_reg_names;
            if (a->regw == 4)
                names = s_reg_names_4;
            else if (a->regw == 2)
                names = s_reg_names_2;
            else if (a->regw == 1)
                names = s_reg_names_1;
            array_appendf(&cg->code, "%s", names[a->reg]);
            break;
        case IA_REG_D:
            if (a->offset || a->reg == REG_RSP) array_appendf(&cg->code, "%zu", a->offset);
            array_appendf(&cg->code, "(%s)", s_reg_names[a->reg]);
            break;
        case IA_LITERAL: array_appendf(&cg->code, "%s", a->s); break;
        case IA_SYM: cg_mangle_sym(cg, &cg->code, a->s); break;
        case IA_SYM_PLT:
            cg_mangle_sym(cg, &cg->code, a->s);
            array_appends(&cg->code, "@PLT");
            break;
        case IA_NAME:
            cg_mangle_sym(cg, &cg->code, a->s);
            if (a->offset) array_appendf(&cg->code, "+%zu", a->offset);
            array_appends(&cg->code, "@GOTPCREL");
            array_appends(&cg->code, "(%rip)");
            break;
        case IA_LNAME:
            cg_mangle_sym(cg, &cg->code, a->s);
            if (a->offset) array_appendf(&cg->code, "+%zu", a->offset);
            array_appends(&cg->code, "(%rip)");
            break;
        case IA_LABEL:
            cg_mangle_label(cg, &cg->code, a->lbl);
            if (a->s) array_appendf(&cg->code, "_%s", a->s);
            break;
        case IA_CONST:
            cg_mangle_const(cg, &cg->code, a->cnst);
            if (a->offset) array_appendf(&cg->code, "+%zu", a->offset);
            array_appends(&cg->code, "(%rip)");
            break;
        default: abort();
    }
}

static InstArg taca_to_ia(const CodeGen* cg, const TACAddress addr, const ActivationRecord* frame)
{
    switch (addr.kind)
    {
        case TACA_NAME:
        {
            InstArg a = IA_NAME(addr.name, addr.offset);
            return a;
        }
        case TACA_LNAME:
        {
            InstArg a = IA_LNAME(addr.name, addr.offset);
            return a;
        }
        case TACA_LITERAL:
        {
            if (addr.offset) goto offset_unsupported;
            InstArg a = IA_LITERAL(addr.name);
            return a;
        }
        case TACA_IMM:
            if (addr.offset) goto offset_unsupported;
            if (addr.sizing.is_signed)
            {
                InstArg a = IA_I((long long)addr.imm);
                return a;
            }
            else
            {
                InstArg a = IA_U(addr.imm);
                return a;
            }
        case TACA_ALABEL:
        {
            if (addr.offset) goto offset_unsupported;
            InstArg a = IA_LABEL(addr.alabel, NULL);
            return a;
        }
        case TACA_LLABEL:
        {
            if (addr.offset) goto offset_unsupported;
            InstArg a = IA_LABEL(cg->cur_fn_lbl_prefix, addr.literal);
            return a;
        }
        case TACA_REG:
            if (addr.offset) goto offset_unsupported;
            InstArg a = IA_REG_W(addr.reg, addr.sizing.width);
            return a;
        case TACA_THROUGH_REG:
        {
            InstArg a = IA_REG_D(addr.reg, addr.offset);
            return a;
        }
        case TACA_CONST:
        {
            InstArg a = IA_CONST(addr.const_idx, addr.offset);
            return a;
        }
        case TACA_REF:
        {
            if (frame->temp_offset + frame->frame_slots[addr.ref] * 8 + addr.offset >= frame->total_frame_size) abort();
            InstArg a = IA_RSP(addr.offset + frame->temp_offset + frame->frame_slots[addr.ref] * 8);
            return a;
        }
        case TACA_PARAM:
        {
            if (addr.offset >= frame->locals_offset) abort();
            InstArg a = IA_RSP(addr.offset);
            return a;
        }
        case TACA_FRAME:
        {
            if (frame->temp_offset <= frame->locals_offset + addr.offset) abort();
            InstArg a = IA_RSP(addr.offset + frame->locals_offset);
            return a;
        }
        case TACA_ARG:
        {
            InstArg a = IA_RSP(addr.offset + 8 + frame->total_frame_size);
            return a;
        }
        default: parser_ferror(NULL, "error: unimplemented TACA: %s\n", taca_to_string(addr.kind)); break;
    }

offset_unsupported:
    parser_ferror(NULL, "error: unimplemented offset TACA: %s\n", taca_to_string(addr.kind));
    InstArg a = {0};
    return a;
}

static void cg_push_insts(CodeGen* cg, const Instruction* inst, size_t n)
{
    for (size_t i = 0; i < n; ++i)
    {
        array_appends(&cg->code, "    ");
        array_appends(&cg->code, s_op_neumon[inst[i].kind]);
        if (inst[i].a1.kind != IA_NONE)
        {
            array_push_byte(&cg->code, ' ');
            if (inst[i].kind == INST_CALLQ_INDIRECT) array_push_byte(&cg->code, '*');
            cg_push_instarg(cg, &inst[i].a1);
            if (inst[i].a2.kind != IA_NONE)
            {
                array_appends(&cg->code, ", ");
                cg_push_instarg(cg, &inst[i].a2);
            }
        }
        array_push_byte(&cg->code, '\n');
    }
}
static void cg_push_inst(CodeGen* cg, Instruction inst) { return cg_push_insts(cg, &inst, 1); }

static void cg_gen_inst_a(struct CodeGen* cg, enum InstKind k, struct TACAddress addr, struct ActivationRecord* frame)
{
    Instruction i = {k, taca_to_ia(cg, addr, frame)};
    cg_push_inst(cg, i);
}

static void cg_gen_load(struct CodeGen* cg, struct TACAddress addr, int reg, struct ActivationRecord* frame)
{
    if (addr.kind == TACA_REG && !addr.is_addr && addr.reg == reg) return;

    if (addr.kind == TACA_IMM)
    {
        Instruction i = {.kind = MOV, .a2 = IA_REG(reg)};
        if (addr.sizing.is_signed)
        {
            InstArg a = IA_I((long long)addr.imm);
            i.a1 = a;
        }
        else
        {
            InstArg a = IA_U(addr.imm);
            i.a1 = a;
        }
        cg_push_inst(cg, i);
        return;
    }
    if (addr.is_addr)
    {
        Instruction i = {.kind = LEAQ, .a1 = taca_to_ia(cg, addr, frame), .a2 = IA_REG(reg)};
        cg_push_inst(cg, i);
        return;
    }

    Instruction i = {.a1 = taca_to_ia(cg, addr, frame), .a2 = IA_REG(reg)};

    size_t offset = 0;
    if (addr.sizing.width > 8)
    {
        abort();
    }
    else if (addr.sizing.width == 8)
    {
        i.kind = MOV;
        offset += 8;
    }
    else if (addr.sizing.width >= 4)
    {
        if (addr.sizing.is_signed)
        {
            i.kind = MOVSL;
        }
        else
        {
            i.kind = MOV;
            InstArg a2 = IA_REG_W(reg, 4);
            i.a2 = a2;
        }
        offset += 4;
    }
    else if (addr.sizing.width >= 2)
    {
        i.kind = addr.sizing.is_signed ? MOVSW : MOVZW;
        offset += 2;
    }
    else if (addr.sizing.width >= 1)
    {
        i.kind = addr.sizing.is_signed ? MOVSB : MOVZB;
        offset += 1;
    }
    else
        abort();
    cg_push_inst(cg, i);

    if (addr.sizing.width - offset >= 2)
    {
        TACAddress addr2 = addr;
        addr2.offset += offset;
        const int tmp = ar_tmp_reg(frame);
        Instruction i[3] = {
            {MOVZW, taca_to_ia(cg, addr2, frame), IA_REG(tmp)},
            {SHL, IA_U(offset * 8), IA_REG(tmp)},
            {INST_OR, IA_REG(tmp), IA_REG(reg)},
        };
        cg_push_insts(cg, i, 3);
        ar_reg_free(frame, tmp);
        offset += 2;
    }
    if (addr.sizing.width - offset >= 1)
    {
        TACAddress addr2 = addr;
        addr2.offset += offset;
        const int tmp = ar_tmp_reg(frame);
        Instruction i[3] = {
            {MOVZB, taca_to_ia(cg, addr2, frame), IA_REG(tmp)},
            {SHL, IA_U(offset * 8), IA_REG(tmp)},
            {INST_OR, IA_REG(tmp), IA_REG(reg)},
        };
        cg_push_insts(cg, i, 3);
        ar_reg_free(frame, tmp);
        offset += 1;
    }
    if (offset != addr.sizing.width) abort();
}

struct FreeFrameSlots
{
    // interpret value as (pos + v) % 256
    unsigned char freestack[255];
    unsigned char next_free;
    unsigned char max_used;
};

static unsigned char ffs_pop(struct FreeFrameSlots* ffs)
{
    unsigned char i = ffs->next_free++;
    if (i == 0xFF) abort();
    if (i + 1 > ffs->max_used) ffs->max_used = i + 1;
    return (unsigned char)(ffs->freestack[i] + i);
}

static void ffs_push(struct FreeFrameSlots* ffs, unsigned char s)
{
    unsigned char i = --ffs->next_free;
    ffs->freestack[i] = s - i;
}

static void cg_gen_store(
    struct CodeGen* cg, struct TACAddress addr, int reg, uint32_t bytes, struct ActivationRecord* frame)
{
    if (!addr.is_addr)
    {
        if (addr.kind != TACA_REG)
        {
            Sizing orig = addr.sizing;
            addr.sizing = s_sizing_ptr;
            const int tmp = ar_tmp_reg(frame);
            cg_gen_load(cg, addr, tmp, frame);
            memset(&addr, 0, sizeof(addr));
            addr.reg = tmp;
            addr.sizing = orig;
        }
        addr.kind = TACA_THROUGH_REG;
        addr.is_addr = 1;
        addr.sizing.width = 0;
    }

    size_t offset = 1;
    if (bytes == 8)
        offset = 8;
    else if (bytes >= 4)
        offset = 4;
    else if (bytes >= 2)
        offset = 2;
    Instruction i1 = {MOV, IA_REG_W(reg, offset), taca_to_ia(cg, addr, frame)};
    cg_push_inst(cg, i1);
    if (bytes - offset > 0)
    {
        const int tmp = ar_tmp_reg(frame);
        Instruction i2[] = {
            {MOV, IA_REG(reg), IA_REG(tmp)},
            {SHR, IA_U(offset * 8), IA_REG(tmp)},
        };
        cg_push_insts(cg, i2, 2);

        if (bytes - offset >= 2)
        {
            TACAddress addr_offset = addr;
            addr_offset.offset += offset;

            Instruction i4 = {MOV, IA_REG_W(tmp, 2), taca_to_ia(cg, addr_offset, frame)};
            cg_push_inst(cg, i4);
            offset += 2;
            if (bytes - offset > 0)
            {
                Instruction i5 = {SHR, IA_U(16), IA_REG(tmp)};
                cg_push_inst(cg, i5);
            }
        }
        if (bytes - offset > 0)
        {
            TACAddress addr_offset = addr;
            addr_offset.offset += offset;

            Instruction i4 = {MOV, IA_REG_W(tmp, 1), taca_to_ia(cg, addr_offset, frame)};
            cg_push_inst(cg, i4);
        }
        ar_reg_free(frame, tmp);
    }
}

static void cg_gen_store_frame(struct CodeGen* cg, size_t i, int reg, struct ActivationRecord* frame)
{
    if (frame->frame_slots[i] == 255) return;
    struct TACAddress addr = {
        .kind = TACA_REF,
        .is_addr = 1,
        .ref = i,
    };
    return cg_gen_store(cg, addr, reg, 8, frame);
}

static int is_i32_imm(const TACAddress* a)
{
    return a->kind == TACA_IMM && (a->imm <= INT32_MAX || a->imm >= (size_t)INT32_MIN);
}

static void cg_add(struct CodeGen* cg, size_t i, const struct TACEntry* tace, struct ActivationRecord* frame)
{
    if (tace->arg2.kind == TACA_REF && frame->frame_slots[tace->arg2.ref] == frame->frame_slots[i])
    {
        if (is_i32_imm(&tace->arg1))
        {
            if (tace->arg1.imm != 0)
            {
                Instruction i = {INST_ADDQ, IA_I(tace->arg1.imm), taca_to_ia(cg, tace->arg2, frame)};
                cg_push_inst(cg, i);
            }
        }
        else
        {
            const int t = ar_tmp_reg(frame);
            cg_gen_load(cg, tace->arg1, t, frame);
            Instruction i = {INST_ADDQ, IA_REG(t), taca_to_ia(cg, tace->arg2, frame)};
            cg_push_inst(cg, i);
        }
        return;
    }

    if (tace->arg1.kind == TACA_REF && frame->frame_slots[tace->arg1.ref] == frame->frame_slots[i])
    {
        if (is_i32_imm(&tace->arg2))
        {
            if (tace->arg2.imm != 0)
            {
                Instruction i = {INST_ADDQ, IA_I(tace->arg2.imm), taca_to_ia(cg, tace->arg1, frame)};
                cg_push_inst(cg, i);
            }
        }
        else
        {
            const int t = ar_tmp_reg(frame);
            cg_gen_load(cg, tace->arg2, t, frame);
            Instruction i = {INST_ADDQ, IA_REG(t), taca_to_ia(cg, tace->arg1, frame)};
            cg_push_inst(cg, i);
        }
        return;
    }

    const int t = ar_tmp_reg(frame);
    if (is_i32_imm(&tace->arg1))
    {
        cg_gen_load(cg, tace->arg2, t, frame);
        if (tace->arg1.imm != 0)
        {
            Instruction i = {INST_ADD, IA_I(tace->arg1.imm), IA_REG(t)};
            cg_push_inst(cg, i);
        }
    }
    else
    {
        cg_gen_load(cg, tace->arg1, t, frame);
        if (is_i32_imm(&tace->arg2))
        {
            if (tace->arg2.imm != 0)
            {
                Instruction i = {INST_ADD, IA_I(tace->arg2.imm), IA_REG(t)};
                cg_push_inst(cg, i);
            }
        }
        else
        {
            const int t2 = ar_tmp_reg(frame);
            cg_gen_load(cg, tace->arg2, t2, frame);
            Instruction i = {INST_ADD, IA_REG(t2), IA_REG(t)};
            cg_push_inst(cg, i);
        }
    }
    cg_gen_store_frame(cg, i, t, frame);
}

static int cg_memcpy(
    struct CodeGen* cg, struct TACAddress arg1, struct TACAddress arg2, size_t bytes, struct ActivationRecord* frame)
{
    int rc = 0;
    if (bytes <= 8 && arg1.is_addr && arg2.is_addr)
    {
        arg1.is_addr = 0;
        arg1.sizing.width = bytes;
        arg2.is_addr = 0;
        arg2.sizing.width = bytes;
        const int t = ar_tmp_reg(frame);
        cg_gen_load(cg, arg2, t, frame);
        cg_gen_store(cg, arg1, t, bytes, frame);
        ar_reg_free(frame, t);
        goto fail;
    }
    ar_reg_use(frame, REG_RSI);
    ar_reg_use(frame, REG_RDI);
    cg_gen_load(cg, arg2, REG_RSI, frame);
    cg_gen_load(cg, arg1, REG_RDI, frame);
    Instruction i = {0};
    if (bytes == 8)
    {
        i.kind = MOVSQ;
    }
    else if (bytes == 4)
    {
        i.kind = MOVSD;
    }
    else if (bytes == 2)
    {
        i.kind = MOVSW;
    }
    else if (bytes == 1)
    {
        i.kind = MOVSB;
    }
    else
    {
        ar_reg_use(frame, REG_RCX);
        Instruction i2[] = {
            {MOV, IA_U(bytes), IA_REG(REG_RCX)},
            {CLD},
        };
        cg_push_insts(cg, i2, 2);
        i.kind = REP_MOVSB;
    }
    cg_push_inst(cg, i);
fail:
    return rc;
}

// static void cg_extend_reg(struct CodeGen* cg, int src_reg, Sizing src, int dst_reg, Sizing dst)
// {
//     if (src.width < dst.width)
//     {
//         Instruction i = {
//             src.is_signed ? MOVSX : MOVZX,
//             IA_REG_W(src_reg, src.width),
//             IA_REG_W(dst_reg, dst.width),
//         };
//         cg_push_inst(cg, i);
//     }
// }

static char mov_inst(int width)
{
    switch (width)
    {
        case 8: return MOVQ;
        case 4: return MOVL;
        case 2: return MOVW;
        case 1: return MOVB;
        default: abort();
    }
}

static int is_suffix_size(uint32_t i) { return i == 8 || i == 4 || i == 2 || i == 1; }

static void cg_assign(
    struct CodeGen* cg, const TACAddress arg1, struct TACAddress arg2, uint32_t bytes, struct ActivationRecord* frame)
{
    if (arg1.kind == TACA_REG && arg1.is_addr)
    {
        cg_gen_load(cg, arg2, arg1.reg, frame);
    }
    else if (is_i32_imm(&arg2))
    {
        if (is_suffix_size(bytes))
        {
            InstArg a1;
            if (arg1.is_addr)
            {
                a1 = taca_to_ia(cg, arg1, frame);
            }
            else
            {
                int reg = REG_RDI;
                if (arg1.kind == TACA_REG)
                    reg = arg1.reg;
                else
                {
                    ar_reg_use(frame, REG_RDI);
                    cg_gen_load(cg, arg1, REG_RDI, frame);
                }
                a1 = ia_reg_d(reg, 0);
            }
            Instruction i = {mov_inst(bytes), IA_I((long long)arg2.imm), a1};
            cg_push_inst(cg, i);
        }
        else
        {
            // Storing nonzero immediates of non-power-of-two size is not implemented
            if (arg2.imm != 0) abort();
            ar_reg_use(frame, REG_RDI);
            cg_gen_load(cg, arg1, REG_RDI, frame);
            ar_reg_use(frame, REG_RCX);
            ar_reg_use(frame, REG_RAX);
            Instruction i[] = {
                {MOV, IA_U(bytes), IA_REG(REG_RCX)},
                {INST_XOR, IA_REG(REG_RAX), IA_REG(REG_RAX)},
                {REP_STOSB},
            };
            cg_push_insts(cg, i, 3);
        }
    }
    else if (arg2.kind == TACA_REG || arg2.is_addr || is_suffix_size(bytes))
    {
        int t;
        if (arg2.kind == TACA_REG && bytes <= arg2.sizing.width)
        {
            t = arg2.reg;
        }
        else
        {
            t = ar_tmp_reg(frame);
            cg_gen_load(cg, arg2, t, frame);
        }
        cg_gen_store(cg, arg1, t, bytes, frame);
    }
    else
    {
        arg2.is_addr = 1;
        arg2.sizing.width = 0;
        arg2.sizing.is_signed = 0;
        cg_memcpy(cg, arg1, arg2, bytes, frame);
    }
}

static void cg_gen_tace(struct CodeGen* cg, const struct TACEntry* taces, size_t i, struct ActivationRecord* frame)
{
    enum InstKind instk;
    TACEntry tace = taces[i];
    ar_reg_clearall(frame);
    if (tace.arg1.kind == TACA_REG || tace.arg1.kind == TACA_THROUGH_REG) ar_reg_use(frame, tace.arg1.reg);
    if (tace.arg2.kind == TACA_REG || tace.arg2.kind == TACA_THROUGH_REG) ar_reg_use(frame, tace.arg2.reg);

    // On MacOS, NAMEs go through the GOT. x@GOTPCREL(%rip) is the pointer to the actual object x.
    TACAddress* args[] = {&tace.arg2, &tace.arg1};
    for (size_t i = 0; i < 2; ++i)
    {
        if (args[i]->kind == TACA_NAME)
        {
            if (args[i]->is_addr)
            {
                args[i]->is_addr = 0;
                args[i]->sizing.is_signed = 0;
                args[i]->sizing.width = 8;
            }
            else
            {
                int reg = ar_tmp_reg(frame);
                Instruction in = {MOVQ, IA_NAME(args[i]->name, args[i]->offset), IA_REG(reg)};
                cg_push_inst(cg, in);

                args[i]->kind = TACA_THROUGH_REG;
                args[i]->reg = reg;
            }
        }
    }

    switch (tace.op)
    {
        case TACO_LT:
        case TACO_LTEQ:
        case TACO_LTU:
        case TACO_LTEQU:
        case TACO_EQ:
        case TACO_NEQ:;
            int wid = tace.arg1.sizing.width;
            if (tace.arg2.sizing.width > wid) wid = tace.arg2.sizing.width;
            if (wid < 4) wid = 4;

            ar_reg_use(frame, REG_RAX);
            cg_gen_load(cg, tace.arg1, REG_RAX, frame);
            if (is_i32_imm(&tace.arg2))
            {
                Instruction i = {INST_CMP, IA_I(tace.arg2.imm), IA_REG_W(REG_RAX, wid)};
                cg_push_inst(cg, i);
            }
            else
            {
                ar_reg_use(frame, REG_RDX);
                cg_gen_load(cg, tace.arg2, REG_RDX, frame);
                Instruction i = {INST_CMP, IA_REG_W(REG_RDX, wid), IA_REG_W(REG_RAX, wid)};
                cg_push_inst(cg, i);
            }
            int op;
            switch (tace.op)
            {
                case TACO_LT: op = INST_SETL; break;
                case TACO_LTEQ: op = INST_SETLE; break;
                case TACO_LTU: op = INST_SETB; break;
                case TACO_LTEQU: op = INST_SETBE; break;
                case TACO_EQ: op = INST_SETE; break;
                case TACO_NEQ: op = INST_SETNE; break;
                default: abort();
            }
            Instruction i2[] = {
                {op, IA_REG_W(REG_RAX, 1)},
                {MOVZX, IA_REG_W(REG_RAX, 1), IA_REG(REG_RAX)},
            };
            cg_push_insts(cg, i2, 2);
            cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
        case TACO_SUB: instk = INST_SUBQ; goto simple_binary;
        case TACO_MUL: instk = INST_IMUL; goto simple_binary;
        case TACO_BAND: instk = INST_ANDQ; goto simple_binary;
        case TACO_BOR: instk = INST_ORQ; goto simple_binary;
        case TACO_BXOR: instk = INST_XORQ; goto simple_binary;
        case TACO_DIV:
        case TACO_IDIV:
        case TACO_MOD:
        case TACO_IMOD:
            ar_reg_use(frame, REG_RAX);
            ar_reg_use(frame, REG_RCX);
            ar_reg_use(frame, REG_RDX);
            cg_gen_load(cg, tace.arg1, REG_RAX, frame);
            cg_gen_load(cg, tace.arg2, REG_RCX, frame);
            if (tace.op == TACO_IMOD || tace.op == TACO_IDIV)
            {
                Instruction j[] = {{INST_CQTO}, {INST_IDIVQ, IA_REG(REG_RCX)}};
                cg_push_insts(cg, j, 2);
            }
            else
            {
                Instruction j[] = {{MOV, IA_U(0), IA_REG(REG_RDX)}, {INST_DIVQ, IA_REG(REG_RCX)}};
                cg_push_insts(cg, j, 2);
            }
            if (tace.op == TACO_MOD || tace.op == TACO_IMOD)
                cg_gen_store_frame(cg, i, REG_RDX, frame);
            else
                cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
        case TACO_SHL: instk = SHL; goto shift;
        case TACO_SHR:
            instk = SHR;
        shift:
            ar_reg_use(frame, REG_RAX);
            cg_gen_load(cg, tace.arg1, REG_RAX, frame);
            if (is_i32_imm(&tace.arg2))
            {
                Instruction j = {instk, IA_I(tace.arg2.imm), IA_REG(REG_RAX)};
                cg_push_inst(cg, j);
            }
            else
            {
                ar_reg_use(frame, REG_RCX);
                cg_gen_load(cg, tace.arg2, REG_RCX, frame);
                Instruction j = {instk, IA_REG_W(REG_RCX, 1), IA_REG(REG_RAX)};
                cg_push_inst(cg, j);
            }
            cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
        case TACO_BNOT:
            ar_reg_use(frame, REG_RAX);
            cg_gen_load(cg, tace.arg1, REG_RAX, frame);
            {
                Instruction j = {INST_NOT, IA_REG(REG_RAX)};
                cg_push_inst(cg, j);
            }
            cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
        case TACO_CALL:
            ar_reg_use(frame, REG_RAX);
            {
                Instruction j[] = {
                    {MOVB, IA_U(0), IA_REG_W(REG_RAX, 1)},
                    {INST_CALLQ},
                };
                if (tace.arg1.kind == TACA_LNAME && tace.arg1.is_addr)
                {
                    InstArg a1 = IA_SYM(tace.arg1.name);
                    j[1].a1 = a1;
                }
                else if (tace.arg1.kind == TACA_NAME)
                {
                    InstArg a1 = IA_SYM_PLT(tace.arg1.name);
                    j[1].a1 = a1;
                }
                else
                {
                    if (taca_is_memory(&tace.arg1))
                    {
                        j[1].kind = INST_CALLQ_INDIRECT;
                    }
                    j[1].a1 = taca_to_ia(cg, tace.arg1, frame);
                }
                cg_push_insts(cg, j, 2);
            }
            cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
        case TACO_LOAD:
        {
            tace.arg1.is_addr = 1;
            cg_memcpy(cg, tace.arg1, tace.arg2, tace.arg1.sizing.width, frame);
            break;
        }
        simple_binary:;
            const int tmp = ar_tmp_reg(frame);
            cg_gen_load(cg, tace.arg1, tmp, frame);
            if (is_i32_imm(&tace.arg2))
            {
                Instruction i = {instk, IA_I(tace.arg2.imm), IA_REG(tmp)};
                cg_push_inst(cg, i);
            }
            else
            {
                ar_reg_use(frame, REG_RDX);
                cg_gen_load(cg, tace.arg2, REG_RDX, frame);
                Instruction i = {instk, IA_REG(REG_RDX), IA_REG(tmp)};
                cg_push_inst(cg, i);
            }
            cg_gen_store_frame(cg, i, tmp, frame);
            break;
        case TACO_ADD: cg_add(cg, i, &tace, frame); break;
        case TACO_ASSIGN: cg_assign(cg, tace.arg1, tace.arg2, tace.assign_width, frame); break;
        case TACO_RETURN:
            if (tace.arg1.kind != TACA_VOID)
            {
                if (tace.arg1.sizing.width <= 8)
                {
                    ar_reg_use(frame, REG_RAX);
                    cg_gen_load(cg, tace.arg1, REG_RAX, frame);
                }
                else
                {
                    struct TACAddress arg2 = {.kind = TACA_PARAM, .is_addr = 1};
                    cg_assign(cg, arg2, tace.arg1, tace.arg1.sizing.width, frame);
                }
            }
            {
                Instruction j[] = {
                    {INST_ADDQ, IA_U(frame->total_frame_size), IA_REG(REG_RSP)},
                    INST_RET,
                };
                cg_push_insts(cg, j, 2);
            }
            break;
        case TACO_JUMP: cg_gen_inst_a(cg, INST_JMP, tace.arg1, frame); break;
        case TACO_BRZ:
            if (tace.arg1.kind == TACA_IMM)
            {
                if (tace.arg1.imm == 0) cg_gen_inst_a(cg, INST_JMP, tace.arg2, frame);
            }
            else
            {
                ar_reg_use(frame, REG_RAX);
                cg_gen_load(cg, tace.arg1, REG_RAX, frame);
                Instruction j[] = {
                    {INST_CMP, IA_U(0), IA_REG(REG_RAX)},
                    {INST_JZ, taca_to_ia(cg, tace.arg2, frame)},
                };
                cg_push_insts(cg, j, 2);
            }
            break;
        case TACO_BRNZ:
            if (tace.arg1.kind == TACA_IMM)
            {
                if (tace.arg1.imm != 0) cg_gen_inst_a(cg, INST_JMP, tace.arg2, frame);
            }
            else
            {
                ar_reg_use(frame, REG_RAX);
                cg_gen_load(cg, tace.arg1, REG_RAX, frame);
                Instruction j[] = {
                    {INST_CMP, IA_U(0), IA_REG(REG_RAX)},
                    {INST_JNZ, taca_to_ia(cg, tace.arg2, frame)},
                };
                cg_push_insts(cg, j, 2);
            }
            break;
        case TACO_CTBZ:
        {
            if (tace.arg1.kind != TACA_IMM) abort();
            ar_reg_use(frame, REG_RCX);
            Instruction j[] = {
                {INST_CMP, IA_U(tace.arg1.imm), IA_REG(REG_RCX)},
                {INST_JZ, taca_to_ia(cg, tace.arg2, frame)},
            };
            cg_push_insts(cg, j, 2);
            break;
        }
        case TACO_LABEL:
            cg_gen_taca(cg, tace.arg1, frame);
            array_appends(&cg->code, ":\n");
            break;
        case TACO_BSWAP32:
        case TACO_BSWAP64:
        {
            ar_reg_use(frame, REG_RAX);
            cg_gen_load(cg, tace.arg1, REG_RAX, frame);
            Instruction j = {BSWAP, IA_REG_W(REG_RAX, tace.op == TACO_BSWAP32 ? 4 : 8)};
            cg_push_inst(cg, j);
            cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
        }
        default: parser_ferror(tace.rc, "error: unimplemented TACO: %s\n", taco_to_string(tace.op)); break;
    }
}

static void cg_emit_rc(struct CodeGen* cg, const RowCol* rc, const char* opts)
{
    const char* filename = rc->file;
    size_t num;
    size_t* file_idx = sm_get(&cg->file_numbers, filename);
    if (file_idx)
    {
        num = *file_idx;
    }
    else
    {
        num = ++cg->next_file_num;
        sm_insert(&cg->file_numbers, filename, num);
        array_appendf(&cg->code, "    .file %zu \"%s\"\n", num, filename);
    }

    array_appendf(
        &cg->code, "    .loc %zu %d %d %s ## %s:%d:%d\n", num, rc->row, rc->col, opts, filename, rc->row, rc->col);
}

static __forceinline size_t round_to_alignment(size_t size, size_t align)
{
    size_t n = size + align - 1;
    return n - (n % align);
}

int cg_gen_taces(struct CodeGen* cg, const struct TACEntry* taces, size_t n_taces, size_t locals_size)
{
    struct Array param_stack = {};

    unsigned char* frame_slots = my_malloc(n_taces);
    struct FreeFrameSlots ffs = {};

    ++cg->cur_fn_lbl_prefix;

    size_t max_param_size = 0;

    memset(frame_slots, 0xFF, n_taces);
    for (size_t i = 0; i < n_taces; ++i)
    {
        const size_t j = n_taces - i - 1;
        const struct TACEntry* const tace = taces + j;
        if (frame_slots[j] != 0xFF)
        {
            ffs_push(&ffs, frame_slots[j]);
        }

        if (ffs.max_used >= 254)
        {
            parser_ferror(NULL, "error: stack frame size exceeded\n");
        }

        if (tace->arg1.kind == TACA_REF)
        {
            size_t ref = tace->arg1.ref;
            if (frame_slots[ref] == 0xFF)
            {
                frame_slots[ref] = ffs_pop(&ffs);
            }
        }
        if (tace->arg2.kind == TACA_REF)
        {
            size_t ref = tace->arg2.ref;
            if (frame_slots[ref] == 0xFF)
            {
                frame_slots[ref] = ffs_pop(&ffs);
            }
        }
        if (tace->op == TACO_ASSIGN && tace->arg1.kind == TACA_PARAM && tace->arg1.is_addr)
        {
            if (tace->arg1.offset + tace->assign_width > max_param_size)
                max_param_size = tace->arg1.offset + tace->assign_width;
        }
    }

    struct ActivationRecord frame = {
        .frame_slots = frame_slots,
    };
    frame.locals_offset = round_to_alignment(max_param_size, 8);
    frame.temp_offset = round_to_alignment(frame.locals_offset + locals_size, 8);
    // align stack for calls (32-byte alignment)
    frame.total_frame_size = round_to_alignment(frame.temp_offset + ffs.max_used * 8 + 8, 32) - 8;
    frame.arg_offset = frame.total_frame_size + 8;

    array_appendf(&cg->code,
                  "    .cfi_startproc\n"
                  "    subq $%zu, %%rsp\n"
                  "    .cfi_def_cfa rsp, %zu\n",
                  frame.total_frame_size,
                  frame.total_frame_size + 8);

    for (size_t i = 0; i < n_taces; ++i)
    {
        if (taces[i].rc)
        {
            cg_emit_rc(cg, taces[i].rc, "");
        }
        array_appendf(&cg->code, "    ## TAC %zu: ", i);
        debug_tace(&cg->code, taces + i);
        array_appendf(&cg->code, " -> %d\n", frame_slots[i]);

        cg_gen_tace(cg, taces, i, &frame);
    }

    array_appendf(&cg->code,
                  "    addq $%zu, %%rsp\n"
                  "    .cfi_def_cfa rsp, 8\n"
                  "    ret\n"
                  "    .cfi_endproc\n",
                  frame.total_frame_size);

    my_free(frame_slots);
    array_destroy(&param_stack);
    return parser_has_errors();
}

int cg_emit(struct CodeGen* cg, const char* src_filename, FILE* fout)
{
    int rc = 0;
    cg_debug(cg, "cg_emit():\n");
    const char* prelude;
    if (cg->target == CG_TARGET_WIN_MASM)
    {
        prelude = "option casemap:none\n"
                  "INCLUDELIB msvcrt.lib\n"
                  "INCLUDELIB ucrt.lib\n"
                  "INCLUDELIB vcruntime.lib\n"
                  "INCLUDELIB kernel32.lib\n"
                  "INCLUDELIB legacy_stdio_definitions.lib\n";
    }
    else
    {
        prelude = ".cfi_sections .eh_frame, .debug_frame\n";
    }

    UNWRAP(!fwrite(prelude, strlen(prelude), 1, fout));

    if (cg->const_.sz)
    {
        static const char* const const_section_header[] = {
            [CG_TARGET_WIN_MASM] = "\n.const\n\n",
            [CG_TARGET_LINUX_GAS] = "\n.section        .rodata\n\n",
            [CG_TARGET_MACOS_GAS] = "\n.section __TEXT,__cstring,cstring_literals\n\n",
        };
        UNWRAP(fputs(const_section_header[cg->target], fout) < 0);
        UNWRAP(!fwrite(cg->const_.data, cg->const_.sz, 1, fout));
    }
    if (cg->data.sz)
    {
        static const char* const data_section_header[] = {
            [CG_TARGET_WIN_MASM] = "\n.data\n\n",
            [CG_TARGET_LINUX_GAS] = "\n.data\n\n",
            [CG_TARGET_MACOS_GAS] = "\n.section __DATA,__data\n\n",
        };
        UNWRAP(fputs(data_section_header[cg->target], fout) < 0);
        UNWRAP(!fwrite(cg->data.data, cg->data.sz, 1, fout));
    }
    if (cg->code.sz)
    {
        static const char* const code_section_header[] = {
            [CG_TARGET_WIN_MASM] = "\n.code\n\n",
            [CG_TARGET_LINUX_GAS] = "\n.text\n\n",
            [CG_TARGET_MACOS_GAS] = "\n.section __TEXT,__text,regular,pure_instructions\n\n",
        };
        static const char* const debug_abbrev_section_header[] = {
            [CG_TARGET_WIN_MASM] = ".???",
            [CG_TARGET_LINUX_GAS] = ".section        .debug_abbrev,\"\",@progbits",
            [CG_TARGET_MACOS_GAS] = ".section __DWARF,__debug_abbrev,regular,debug",
        };
        static const char* const debug_info_section_header[] = {
            [CG_TARGET_WIN_MASM] = ".???",
            [CG_TARGET_LINUX_GAS] = ".section        .debug_info,\"\",@progbits",
            [CG_TARGET_MACOS_GAS] = ".section __DWARF,__debug_info,regular,debug",
        };
        UNWRAP(fputs(code_section_header[cg->target], fout) < 0);
        UNWRAP(fputs("Lfunc_begin0:\n", fout) < 0);
        UNWRAP(!fwrite(cg->code.data, cg->code.sz, 1, fout));
        UNWRAP(fputs("Lfunc_end0:\n", fout) < 0);
        UNWRAP(fprintf(fout,
                       "\n%s\n"
                       ".byte   1                               ## Abbreviation Code\n"
                       ".byte	17                              ## DW_TAG_compile_unit\n"
                       ".byte	1                               ## DW_CHILDREN_yes\n"
                       ".byte	19                              ## DW_AT_language\n"
                       ".byte	5                               ## DW_FORM_data2\n"
                       ".byte	3                               ## DW_AT_name\n"
                       ".byte	8                               ## DW_FORM_string\n"
                       ".byte	16                              ## DW_AT_stmt_list\n"
                       ".byte	23                              ## DW_FORM_sec_offset\n"
                       ".byte	17                              ## DW_AT_low_pc\n"
                       ".byte	1                               ## DW_FORM_addr\n"
                       ".byte	18                              ## DW_AT_high_pc\n"
                       ".byte	6                               ## DW_FORM_data4\n"
                       ".byte	0                               ## EOM(1)\n"
                       ".byte	0                               ## EOM(2)\n"
                       "%s\n"
                       ".set Lset0, Ldebug_info_end0-Ldebug_info_start0 ## Length of Unit\n"
                       ".long	Lset0 ## section length\n"
                       "Ldebug_info_start0:\n"
                       ".short 4 ## dwarf v4\n"
                       ".long 0 ## offset into abbrev\n"
                       ".byte 8 ## addr size\n"
                       ".byte 1 ## abbrev[1]: compile unit\n"
                       ".short	12                              ## DW_AT_language\n"
                       ".asciz	\"%s\"                             ## DW_AT_name\n"
                       ".long	0            ## DW_AT_stmt_list\n"
                       ".quad	Lfunc_begin0                    ## DW_AT_low_pc\n"
                       ".set Lset3, Lfunc_end0-Lfunc_begin0     ## DW_AT_high_pc\n"
                       ".long	Lset3\n"
                       ".byte	0                               ## End of Children\n"
                       "Ldebug_info_end0:\n",
                       debug_abbrev_section_header[cg->target],
                       debug_info_section_header[cg->target],
                       src_filename) < 0);
        static const char* const debug_line_section_header[] = {
            [CG_TARGET_WIN_MASM] = ".???",
            [CG_TARGET_LINUX_GAS] = ".section        .debug_line,\"\",@progbits",
            [CG_TARGET_MACOS_GAS] = ".section __DWARF,__debug_line,regular,debug",
        };
        if (cg->target == CG_TARGET_MACOS_GAS) UNWRAP(fputs(".subsections_via_symbols\n", fout) < 0);
        UNWRAP(fprintf(fout,
                       "%s\n"
                       "Lsection_line:\n"
                       "Lline_table_start0:\n",
                       debug_line_section_header[cg->target]) < 0);
    }

    if (cg->target == CG_TARGET_WIN_MASM) UNWRAP(0 > fputs("END\n", fout));

    return 0;

fail:
    perror("error: failed to write output");
    return 1;
}
