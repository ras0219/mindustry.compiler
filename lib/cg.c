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
#ifndef _WIN32
    cg->target = CG_TARGET_MACOS_GAS;
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

void cg_declare_public(struct CodeGen* cg, const char* sym)
{
    cg_debug(cg, "   : %s\n", sym);
    if (cg->target == CG_TARGET_WIN_MASM)
        array_appendf(&cg->code, "public %s\n", sym);
    else
        array_appendf(&cg->code, ".globl _%s\n", sym);
}

void cg_start_function(struct CodeGen* cg, const char* sym) { cg_mark_label(cg, sym); }

void cg_mark_label(struct CodeGen* cg, const char* sym)
{
    cg_debug(cg, "   : %s\n", sym);
    if (cg->target == CG_TARGET_WIN_MASM)
        array_appendf(&cg->code, "%s:\n", sym);
    else
        array_appendf(&cg->code, "_%s:\n", sym);
}

void cg_mark_alabel(struct CodeGen* cg, size_t n)
{
    cg_debug(cg, "   : L$%zu\n", n);
    array_appendf(&cg->code, "L$%zu:\n", n);
}

size_t cg_next_alabel(struct CodeGen* cg) { return cg->next_label++; }

static int needs_escape(char ch) { return !(ch >= 32 && ch < 127 && ch != '"' && ch != '\\'); }

void cg_string_constant(struct CodeGen* cg, size_t cidx, const char* str, size_t sz)
{
    // cg_debug(cg, "   : strconst %d: %s\n", cidx, str);
    array_appendf(&cg->const_, "L_.S%zu: .asciz \"", cidx);
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
    array_appendf(&cg->data, ".p2align 3\n_%s:\n", name);
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
            array_appendf(&cg->data, ".quad L_.S%zu + %zu\n", base->const_idx, offset);
        }
        else if (base && (base->kind == TACA_LNAME || base->kind == TACA_NAME))
        {
            array_appendf(&cg->data, ".quad _%s + %zu\n", base->name, offset);
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
    array_appendf(&cg->data, "_%s: .skip %zu\n", name, sz);
}

static const char* const s_reg_names[] = {"%rax", "%rbx", "%rcx", "%rdx", "%rdi", "%rsi", "%r8", "%r9", "%r10", "%r11"};
static const char* const s_reg_names_4[] = {
    "%eax", "%ebx", "%ecx", "%edx", "%edi", "%esi", "%r8d", "%r9d", "%r10d", "%r11d"};
static const char* const s_reg_names_2[] = {"%ax", "%bx", "%cx", "%dx", "%di", "%si", "%r8w", "%r9w", "%r10w", "%r11w"};
static const char* const s_reg_names_1[] = {
    "%al", "%bl", "%cl", "%dl", "%dil", "%sil", "%r8b", "%r9b", "%r10b", "%r11b"};

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
            array_push_byte(&cg->code, '_');
            array_appends(&cg->code, addr.name);
            array_appends(&cg->code, "@GOTPCREL(%rip)");
            break;
        case TACA_LNAME:
            array_push_byte(&cg->code, '_');
            array_appends(&cg->code, addr.name);
            array_appends(&cg->code, "(%rip)");
            break;
        case TACA_LITERAL: array_appends(&cg->code, addr.literal); break;
        case TACA_IMM: array_appendf(&cg->code, "$%zu", addr.imm); break;
        case TACA_ALABEL: array_appendf(&cg->code, "L$%zu", addr.alabel); break;
        case TACA_LLABEL: array_appendf(&cg->code, "L$%zu_%s", cg->cur_fn_lbl_prefix, addr.literal); break;
        case TACA_REG: cg_gen_taca_reg(cg, addr.reg, addr.sizing); break;
        case TACA_THROUGH_REG:
            array_push_byte(&cg->code, '(');
            array_appends(&cg->code, s_reg_names[addr.reg]);
            array_push_byte(&cg->code, ')');
            break;
        case TACA_CONST: array_appendf(&cg->code, "L_.S%d(%%rip)", addr.const_idx); break;
        case TACA_REF:
            if (frame->temp_offset + frame->frame_slots[addr.ref] * 8 >= frame->total_frame_size) abort();
            array_appendf(&cg->code, "%zu(%%rsp)", frame->temp_offset + frame->frame_slots[addr.ref] * 8);
            break;
        case TACA_PARAM:
            if (addr.param_offset >= frame->locals_offset) abort();
            array_appendf(&cg->code, "%zu(%%rsp)", addr.param_offset);
            break;
        case TACA_FRAME:
            if (frame->temp_offset <= frame->locals_offset + addr.frame_offset) abort();
            array_appendf(&cg->code, "%zu(%%rsp)", frame->locals_offset + addr.frame_offset);
            break;
        case TACA_ARG: array_appendf(&cg->code, "%zu(%%rsp)", 8 + frame->total_frame_size + addr.arg_offset); break;
        default: parser_ferror(NULL, "error: unimplemented TACA: %s\n", taca_to_string(addr.kind)); break;
    }
}

static void cg_gen_taca_offset(struct CodeGen* cg, struct TACAddress addr, int offset, struct ActivationRecord* frame)
{
    switch (addr.kind)
    {
        case TACA_NAME: array_appendf(&cg->code, "_%s@GOTPCREL+%d(%%rip)", addr.name, offset); break;
        case TACA_LNAME: array_appendf(&cg->code, "_%s+%d(%%rip)", addr.name, offset); break;
        case TACA_CONST: array_appendf(&cg->code, "L_.S%d+%d(%%rip)", addr.const_idx, offset); break;
        case TACA_REF:
            if (frame->temp_offset + frame->frame_slots[addr.ref] * 8 >= frame->total_frame_size) abort();
            array_appendf(&cg->code, "%zu(%%rsp)", offset + frame->temp_offset + frame->frame_slots[addr.ref] * 8);
            break;
        case TACA_PARAM:
            if (addr.param_offset >= frame->locals_offset) abort();
            array_appendf(&cg->code, "%zu(%%rsp)", offset + addr.param_offset);
            break;
        case TACA_FRAME:
            if (frame->temp_offset <= frame->locals_offset + addr.frame_offset) abort();
            array_appendf(&cg->code, "%zu(%%rsp)", offset + frame->locals_offset + addr.frame_offset);
            break;
        case TACA_THROUGH_REG: array_appendf(&cg->code, "%d(%s)", offset, s_reg_names[addr.reg]); break;
        case TACA_ARG:
            array_appendf(&cg->code, "%zu(%%rsp)", offset + 8 + frame->total_frame_size + addr.arg_offset);
            break;
        default: parser_ferror(NULL, "error: unimplemented offset TACA: %s\n", taca_to_string(addr.kind)); break;
    }
}

static void cg_gen_inst_a(struct CodeGen* cg, const char* inst, struct TACAddress addr, struct ActivationRecord* frame)
{
    array_appendf(&cg->code, "    %s ", inst);
    cg_gen_taca(cg, addr, frame);
    array_push_byte(&cg->code, '\n');
}

static void cg_gen_load(struct CodeGen* cg, struct TACAddress addr, int reg, struct ActivationRecord* frame)
{
    if (addr.kind == TACA_REG && !addr.is_addr && addr.reg == reg) return;

    if (addr.kind == TACA_IMM)
    {
        if (addr.sizing.is_signed)
            array_appendf(&cg->code, "    mov $%zd, %s\n", (long long)addr.imm, s_reg_names[reg]);
        else
            array_appendf(&cg->code, "    mov $%zu, %s\n", addr.imm, s_reg_names[reg]);
        return;
    }
    if (addr.is_addr)
    {
        array_appends(&cg->code, "    leaq ");
        cg_gen_taca(cg, addr, frame);
        array_appendf(&cg->code, ", %s\n", s_reg_names[reg]);
        return;
    }

    size_t offset = 0;
    switch (addr.sizing.width)
    {
        case 1:
            array_appendf(&cg->code, "    mov%cb ", addr.sizing.is_signed ? 's' : 'z');
            offset += 1;
            break;
        case 2:
        case 3:
            array_appendf(&cg->code, "    mov%cw ", addr.sizing.is_signed ? 's' : 'z');
            offset += 2;
            break;
        case 4:
        case 5:
        case 6:
        case 7:
            offset += 4;
            if (addr.sizing.is_signed)
            {
                array_appends(&cg->code, "    movsl ");
                break;
            }
            else
            {
                array_appends(&cg->code, "    mov ");
                cg_gen_taca(cg, addr, frame);
                array_appendf(&cg->code, ", %s\n", s_reg_names_4[reg]);
                goto skip;
            }
        case 8:
            array_appends(&cg->code, "    mov ");
            offset += 8;
            break;
        default: abort();
    }
    cg_gen_taca(cg, addr, frame);
    array_appendf(&cg->code, ", %s\n", s_reg_names[reg]);
skip:
    if (addr.sizing.width - offset >= 2)
    {
        const int tmp = ar_tmp_reg(frame);
        array_appends(&cg->code, "    movzw ");
        cg_gen_taca_offset(cg, addr, offset, frame);
        array_appendf(&cg->code,
                      ", %s\n"
                      "    shl $%d, %s\n"
                      "    or %s, %s\n",
                      s_reg_names[tmp],
                      offset * 8,
                      s_reg_names[tmp],
                      s_reg_names[tmp],
                      s_reg_names[reg]);
        ar_reg_free(frame, tmp);
        offset += 2;
    }
    if (addr.sizing.width - offset >= 1)
    {
        const int tmp = ar_tmp_reg(frame);
        array_appends(&cg->code, "    movzb ");
        cg_gen_taca_offset(cg, addr, offset, frame);
        array_appendf(&cg->code,
                      ", %s\n"
                      "    shl $%d, %s\n"
                      "    or %s, %s\n",
                      s_reg_names[tmp],
                      offset * 8,
                      s_reg_names[tmp],
                      s_reg_names[tmp],
                      s_reg_names[reg]);
        ar_reg_free(frame, tmp);
        offset += 1;
    }
    if (offset != addr.sizing.width) abort();
}

#if 0
static int cg_gen_memcpy(struct CodeGen* cg, struct TACAddress addr, struct TACAddress addr2, size_t sz)
{
    array_appends(&cg->code, "    memcpy ...\n");
    return 0;
}
#endif

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

static const Sizing s_sizing_u[] = {
    {0, 1},
    {0, 2},
    {0, 4},
    {0, 8},
};

static void cg_gen_store(struct CodeGen* cg, struct TACAddress addr, int reg, struct ActivationRecord* frame)
{
    if (!addr.is_addr)
    {
        if (addr.kind != TACA_REG)
        {
            Sizing orig = addr.sizing;
            addr.sizing = s_sizing_u[3];
            const int tmp = ar_tmp_reg(frame);
            cg_gen_load(cg, addr, tmp, frame);
            addr.reg = tmp;
            addr.sizing = orig;
        }
        addr.kind = TACA_THROUGH_REG;
        addr.is_addr = 1;
    }

    int i = 0;
    if (addr.sizing.width == 8)
        i = 3;
    else if (addr.sizing.width >= 4)
        i = 2;
    else if (addr.sizing.width >= 2)
        i = 1;
    else
        i = 0;
    size_t offset = s_sizing_u[i].width;
    array_appends(&cg->code, "    mov ");
    cg_gen_taca_reg(cg, reg, s_sizing_u[i]);
    array_appends(&cg->code, ", ");
    cg_gen_taca(cg, addr, frame);
    array_push_byte(&cg->code, '\n');
    if (addr.sizing.width - offset > 0)
    {
        const int tmp = ar_tmp_reg(frame);
        array_appends(&cg->code, "    mov ");
        cg_gen_taca_reg(cg, reg, s_sizing_u[3]);
        array_appendf(&cg->code, ", %s\n", s_reg_names[tmp]);
        array_appendf(&cg->code, "    shr $%d, %s\n", offset * 8, s_reg_names[tmp]);
        if (addr.sizing.width - offset >= 2)
        {
            array_appendf(&cg->code, "    mov %s, ", s_reg_names_2[tmp]);
            cg_gen_taca_offset(cg, addr, offset, frame);
            array_push_byte(&cg->code, '\n');
            offset += 2;
            if (addr.sizing.width - offset > 0) array_appendf(&cg->code, "    shr $16, %s\n", s_reg_names[tmp]);
        }
        if (addr.sizing.width - offset > 0)
        {
            array_appendf(&cg->code, "    mov %s, ", s_reg_names_1[tmp]);
            cg_gen_taca_offset(cg, addr, offset, frame);
            array_push_byte(&cg->code, '\n');
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
        .sizing.width = 8,
    };
    return cg_gen_store(cg, addr, reg, frame);
}

static void cg_add(struct CodeGen* cg, size_t i, const struct TACEntry* tace, struct ActivationRecord* frame)
{
    const int t = ar_tmp_reg(frame);
    if (tace->arg1.kind == TACA_IMM && tace->arg1.imm < INT32_MAX)
    {
        cg_gen_load(cg, tace->arg2, t, frame);
        if (tace->arg1.imm != 0) array_appendf(&cg->code, "    add $%zu, %s\n", tace->arg1.imm, s_reg_names[t]);
    }
    else if (tace->arg2.kind == TACA_IMM && tace->arg2.imm < INT32_MAX)
    {
        cg_gen_load(cg, tace->arg1, t, frame);
        if (tace->arg2.imm != 0) array_appendf(&cg->code, "    add $%zu, %s\n", tace->arg2.imm, s_reg_names[t]);
    }
    else
    {
        const int t2 = ar_tmp_reg(frame);
        cg_gen_load(cg, tace->arg1, t, frame);
        cg_gen_load(cg, tace->arg2, t2, frame);
        array_appendf(&cg->code, "    add %s, %s\n", s_reg_names[t2], s_reg_names[t]);
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
        cg_gen_store(cg, arg1, t, frame);
        goto fail;
    }
    ar_reg_use(frame, REG_RSI);
    ar_reg_use(frame, REG_RDI);
    cg_gen_load(cg, arg2, REG_RSI, frame);
    cg_gen_load(cg, arg1, REG_RDI, frame);
    if (bytes == 8)
    {
        array_appendf(&cg->code, "    movsq\n");
    }
    else if (bytes == 4)
    {
        array_appendf(&cg->code, "    movsd\n");
    }
    else if (bytes == 2)
    {
        array_appendf(&cg->code, "    movsw\n");
    }
    else if (bytes == 1)
    {
        array_appendf(&cg->code, "    movsb\n");
    }
    else
    {
        ar_reg_use(frame, REG_RCX);
        array_appendf(&cg->code, "    mov $%zu, %%rcx\n    cld\n    rep movsb\n", bytes);
    }
fail:
    return rc;
}

static void cg_extend_reg(struct CodeGen* cg, int src_reg, Sizing src, int dst_reg, Sizing dst)
{
    if (src.width < dst.width)
    {
        if (src.is_signed)
            array_appends(&cg->code, "    movsx ");
        else
            array_appends(&cg->code, "    movzx ");
        cg_gen_taca_reg(cg, src_reg, src);
        array_appends(&cg->code, ", ");
        cg_gen_taca_reg(cg, dst_reg, dst);
        array_push_byte(&cg->code, '\n');
    }
}

static char sizing_suffix(int width)
{
    switch (width)
    {
        case 8: return 'q';
        case 4: return 'l';
        case 2: return 'w';
        case 1: return 'b';
        default: return '\0';
    }
}

static int is_suffix_size(uint32_t i) { return i == 8 || i == 4 || i == 2 || i == 1; }

static void cg_assign(struct CodeGen* cg,
                      struct TACAddress arg1,
                      struct TACAddress arg2,
                      struct ActivationRecord* frame)
{
    if (arg1.kind == TACA_REG && arg1.is_addr)
    {
        cg_gen_load(cg, arg2, arg1.reg, frame);
    }
    else if (arg2.kind == TACA_REG)
    {
        if (arg2.is_addr) abort();
        if (arg1.sizing.width > arg2.sizing.width)
        {
            const int t = ar_tmp_reg(frame);
            ar_reg_use(frame, t);
            cg_extend_reg(cg, arg2.reg, arg2.sizing, t, arg1.sizing);
            cg_gen_store(cg, arg1, t, frame);
        }
        else
        {
            cg_gen_store(cg, arg1, arg2.reg, frame);
        }
    }
    else if (arg2.kind == TACA_IMM && (arg2.imm <= INT32_MAX || arg2.imm >= (size_t)INT32_MIN))
    {
        const Sizing bytes = arg1.sizing;
        arg1.sizing.width = 8;
        char ch = sizing_suffix(bytes.width);
        if (ch)
        {
            if (arg1.is_addr)
            {
                array_appendf(&cg->code, "    mov%c $%d, ", sizing_suffix(bytes.width), (int)arg2.imm);
                cg_gen_taca(cg, arg1, frame);
                array_appends(&cg->code, "\n");
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
                array_appendf(
                    &cg->code, "    mov%c $%d, (%s)\n", sizing_suffix(bytes.width), (int)arg2.imm, s_reg_names[reg]);
            }
        }
        else
        {
            // Storing immediates of non-power-of-two size is not implemented
            if (arg2.imm != 0) abort();
            ar_reg_use(frame, REG_RDI);
            cg_gen_load(cg, arg1, REG_RDI, frame);
            ar_reg_use(frame, REG_RCX);
            array_appendf(&cg->code, "    mov $%zu, %%rcx\n", bytes.width);
            array_appends(&cg->code,
                          "    xor %rax, %rax\n"
                          "    rep stosb\n");
        }
    }
    else if (arg2.is_addr || is_suffix_size(arg1.sizing.width))
    {
        int reg = ar_tmp_reg(frame);
        cg_gen_load(cg, arg2, reg, frame);
        cg_gen_store(cg, arg1, reg, frame);
    }
    else
    {
        arg2.is_addr = 1;
        size_t bytes = arg1.sizing.width;
        arg1.sizing.width = 8;
        cg_memcpy(cg, arg1, arg2, bytes, frame);
    }
}

static void cg_gen_tace(struct CodeGen* cg, const struct TACEntry* taces, size_t i, struct ActivationRecord* frame)
{
    const char* inst;
    TACEntry tace = taces[i];
    ar_reg_clearall(frame);

    if (tace.arg1.kind == TACA_NAME)
    {
        if (tace.op == TACO_CALL)
        {
            if (!tace.arg1.is_addr)
            {
                array_appendf(&cg->code, "    movq _%s@GOTPCREL(%%rip), %%r11\n", tace.arg1.name);
                tace.arg1.kind = TACA_THROUGH_REG;
                tace.arg1.reg = REG_R11;
            }
            else
            {
                tace.arg1.kind = TACA_LNAME;
            }
        }
        else
        {
            if (tace.arg1.is_addr)
            {
                tace.arg1.is_addr = 0;
                if (tace.op != TACO_ASSIGN)
                {
                    tace.arg1.sizing.is_signed = 0;
                    tace.arg1.sizing.width = 8;
                }
            }
            else
            {
                array_appendf(&cg->code, "    movq _%s@GOTPCREL(%%rip), %%rax\n", tace.arg1.name);
                tace.arg1.kind = TACA_THROUGH_REG;
                tace.arg1.reg = REG_RAX;
            }
        }
    }
    if (tace.arg2.kind == TACA_NAME)
    {
        if (tace.arg2.is_addr)
        {
            tace.arg2.is_addr = 0;
            tace.arg2.sizing.is_signed = 0;
            tace.arg2.sizing.width = 8;
        }
        else
        {
            array_appendf(&cg->code, "    movq _%s@GOTPCREL(%%rip), %%r11\n", tace.arg2.name);
            tace.arg2.kind = TACA_THROUGH_REG;
            tace.arg2.reg = REG_R11;
        }
    }

    if (tace.arg1.kind == TACA_REG || tace.arg1.kind == TACA_THROUGH_REG) ar_reg_use(frame, tace.arg1.reg);
    if (tace.arg2.kind == TACA_REG || tace.arg2.kind == TACA_THROUGH_REG) ar_reg_use(frame, tace.arg2.reg);

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
            if (tace.arg2.kind == TACA_IMM && tace.arg2.imm < INT32_MAX)
            {
                array_appendf(&cg->code, "    cmp $%zu, %%%cax\n", tace.arg2.imm, wid == 4 ? 'e' : 'r');
            }
            else
            {
                ar_reg_use(frame, REG_RDX);
                cg_gen_load(cg, tace.arg2, REG_RDX, frame);
                if (wid == 4)
                    array_appends(&cg->code, "    cmp %edx, %eax\n");
                else
                    array_appends(&cg->code, "    cmp %rdx, %rax\n");
            }
            switch (tace.op)
            {
                case TACO_LT: array_appends(&cg->code, "    setl %al\n"); break;
                case TACO_LTEQ: array_appends(&cg->code, "    setle %al\n"); break;
                case TACO_LTU: array_appends(&cg->code, "    setb %al\n"); break;
                case TACO_LTEQU: array_appends(&cg->code, "    setbe %al\n"); break;
                case TACO_EQ: array_appends(&cg->code, "    sete %al\n"); break;
                case TACO_NEQ: array_appends(&cg->code, "    setne %al\n"); break;
                default: abort();
            }
            array_appends(&cg->code, "    movzx %al, %rax\n");
            cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
        case TACO_SUB: inst = "subq"; goto simple_binary;
        case TACO_MULT: inst = "imul"; goto simple_binary;
        case TACO_DIV:
        case TACO_MOD:
            ar_reg_use(frame, REG_RAX);
            ar_reg_use(frame, REG_RCX);
            ar_reg_use(frame, REG_RDX);
            cg_gen_load(cg, tace.arg1, REG_RAX, frame);
            cg_gen_load(cg, tace.arg2, REG_RCX, frame);
            array_appends(&cg->code, "    movq $0, %rdx\n");
            array_appends(&cg->code, "    idivq %rcx\n");
            if (tace.op == TACO_DIV)
                cg_gen_store_frame(cg, i, REG_RAX, frame);
            else
                cg_gen_store_frame(cg, i, REG_RDX, frame);
            break;
        case TACO_BAND: inst = "andq"; goto simple_binary;
        case TACO_BOR: inst = "orq"; goto simple_binary;
        case TACO_BXOR: inst = "xorq"; goto simple_binary;
        case TACO_SHL: inst = "shl"; goto shift;
        case TACO_SHR:
            inst = "shr";
        shift:
            ar_reg_use(frame, REG_RAX);
            ar_reg_use(frame, REG_RCX);
            cg_gen_load(cg, tace.arg1, REG_RAX, frame);
            cg_gen_load(cg, tace.arg2, REG_RCX, frame);
            array_appendf(&cg->code, "    %s %%cl, %%rax\n", inst);
            cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
        case TACO_BNOT:
            ar_reg_use(frame, REG_RAX);
            cg_gen_load(cg, tace.arg1, REG_RAX, frame);
            array_appends(&cg->code, "    not %rax\n");
            cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
        case TACO_CALL:
            ar_reg_use(frame, REG_RAX);
            array_appends(&cg->code, "    movb $0, %al\n");
            array_appends(&cg->code, "    callq ");
            if ((tace.arg1.kind == TACA_LNAME || tace.arg1.kind == TACA_NAME) && tace.arg1.is_addr)
            {
                array_push_byte(&cg->code, '_');
                array_appends(&cg->code, tace.arg1.name);
            }
            else
            {
                if (taca_is_memory(&tace.arg1))
                {
                    array_push_byte(&cg->code, '*');
                }
                cg_gen_taca(cg, tace.arg1, frame);
            }
            array_push_byte(&cg->code, '\n');
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
            if (tace.arg2.kind == TACA_IMM && tace.arg2.imm < INT32_MAX)
            {
                array_appendf(&cg->code, "    %s $%zu, %s\n", inst, tace.arg2.imm, s_reg_names[tmp]);
            }
            else
            {
                ar_reg_use(frame, REG_RDX);
                cg_gen_load(cg, tace.arg2, REG_RDX, frame);
                array_appendf(&cg->code, "    %s %%rdx, %s\n", inst, s_reg_names[tmp]);
            }
            cg_gen_store_frame(cg, i, tmp, frame);
            break;
        case TACO_ADD: cg_add(cg, i, &tace, frame); break;
        case TACO_ASSIGN: cg_assign(cg, tace.arg1, tace.arg2, frame); break;
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
                    struct TACAddress arg2 = {
                        .kind = TACA_PARAM,
                        .sizing = tace.arg1.sizing,
                        .is_addr = 1,
                    };
                    cg_assign(cg, arg2, tace.arg1, frame);
                }
            }
            array_appendf(&cg->code, "\n    addq $%zu, %%rsp\n    ret\n", frame->total_frame_size);
            break;
        case TACO_JUMP: cg_gen_inst_a(cg, "jmp", tace.arg1, frame); break;
        case TACO_BRZ:
            if (tace.arg1.kind == TACA_IMM)
            {
                if (tace.arg1.imm == 0) cg_gen_inst_a(cg, "jmp", tace.arg2, frame);
            }
            else
            {
                ar_reg_use(frame, REG_RAX);
                cg_gen_load(cg, tace.arg1, REG_RAX, frame);
                array_appends(&cg->code, "    cmp $0, %rax\n");
                cg_gen_inst_a(cg, "jz ", tace.arg2, frame);
            }
            break;
        case TACO_BRNZ:
            if (tace.arg1.kind == TACA_IMM)
            {
                if (tace.arg1.imm != 0) cg_gen_inst_a(cg, "jmp", tace.arg2, frame);
            }
            else
            {
                ar_reg_use(frame, REG_RAX);
                cg_gen_load(cg, tace.arg1, REG_RAX, frame);
                array_appends(&cg->code, "    cmp $0, %rax\n");
                cg_gen_inst_a(cg, "jnz ", tace.arg2, frame);
            }
            break;
        case TACO_CTBZ:
            if (tace.arg1.kind != TACA_IMM) abort();
            ar_reg_use(frame, REG_RCX);
            array_appendf(&cg->code, "    cmp $%zu, %%rcx\n", tace.arg1.imm);
            cg_gen_inst_a(cg, "jz ", tace.arg2, frame);
            break;
        case TACO_LABEL:
            cg_gen_taca(cg, tace.arg1, frame);
            array_appends(&cg->code, ":\n");
            break;
        case TACO_BSWAP32:
        case TACO_BSWAP64:
            ar_reg_use(frame, REG_RAX);
            cg_gen_load(cg, tace.arg1, REG_RAX, frame);
            array_appendf(&cg->code, "    bswap %%%cax\n", tace.op == TACO_BSWAP32 ? 'e' : 'r');
            cg_gen_store_frame(cg, i, REG_RAX, frame);
            break;
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
        if (tace->arg1.kind == TACA_PARAM && tace->arg1.param_offset + tace->arg1.sizing.width > max_param_size)
        {
            max_param_size = tace->arg1.param_offset + tace->arg1.sizing.width;
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
        if (cg->target == CG_TARGET_WIN_MASM)
            UNWRAP(fputs("\n.const\n\n", fout) < 0);
        else
            UNWRAP(fputs("\n.section __TEXT,__cstring,cstring_literals\n\n", fout) < 0);
        UNWRAP(!fwrite(cg->const_.data, cg->const_.sz, 1, fout));
    }
    if (cg->data.sz)
    {
        if (cg->target == CG_TARGET_WIN_MASM)
            UNWRAP(fputs("\n.data\n\n", fout) < 0);
        else
            UNWRAP(fputs("\n.section __DATA,__data\n\n", fout) < 0);
        UNWRAP(!fwrite(cg->data.data, cg->data.sz, 1, fout));
    }
    if (cg->code.sz)
    {
        if (cg->target == CG_TARGET_WIN_MASM)
            UNWRAP(fputs("\n.code\n\n", fout) < 0);
        else
            UNWRAP(fputs("\n.section __TEXT,__text,regular,pure_instructions\n\n", fout) < 0);
        UNWRAP(fputs("Lfunc_begin0:\n", fout) < 0);
        UNWRAP(!fwrite(cg->code.data, cg->code.sz, 1, fout));
        UNWRAP(fputs("Lfunc_end0:\n", fout) < 0);
        UNWRAP(fprintf(fout,
                       "\n.section __DWARF,__debug_abbrev,regular,debug\n"
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
                       ".section __DWARF,__debug_info,regular,debug\n"
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
                       src_filename) < 0);
        UNWRAP(fprintf(fout,
                       ".subsections_via_symbols\n"
                       ".section __DWARF,__debug_line,regular,debug\n"
                       "Lsection_line:\n"
                       "Lline_table_start0:\n") < 0);
    }

    if (cg->target == CG_TARGET_WIN_MASM) UNWRAP(0 > fputs("END\n", fout));

    return 0;

fail:
    perror("error: failed to write output");
    return 1;
}
