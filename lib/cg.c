#include "cg.h"

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
    array_destroy(&cg->code);
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
    TACA_TEMP_IS_MEMORY = 0,
    TACA_REG_IS_MEMORY = 0,
};

#define Y_IS_MEMORY(Z) Z##_IS_MEMORY,
static const char s_table_taca_is_memory[TACA_KIND_COUNT] = {X_TACA_KIND(Y_IS_MEMORY)};
#undef Y_IS_MEMORY

__forceinline static int taca_is_memory(const struct TACAddress* addr)
{
    return addr->kind == TACA_NAME || (!addr->is_addr && s_table_taca_is_memory[addr->kind]);
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

void cg_string_constant(struct CodeGen* cg, size_t cidx, const char* str)
{
    cg_debug(cg, "   : strconst %d: %s\n", cidx, str);
    array_appendf(&cg->const_, "L_.S%d: .asciz \"%s\"\n", cidx, str);
}
void cg_reserve_data(struct CodeGen* cg, const char* name, const char* data, size_t sz)
{
    if (sz == 0) abort();
    array_appendf(&cg->data, "_%s: .byte ", name);
    array_appendf(&cg->data, "%u", data[0]);
    for (size_t i = 1; i < sz; ++i)
    {
        array_appendf(&cg->data, ",%u", data[i]);
    }
    array_push_byte(&cg->data, '\n');
}

static const char* const s_reg_names[] = {"%rax", "%rbx", "%rcx", "%rdx", "%rdi", "%rsi", "%r8", "%r9", "%r10", "%r11"};
static const char* const s_reg_names_4[] = {"%eax", "%ebx", "%ecx", "%edx", "%edi", "%esi", "%r8d", "%r9d", "%r10d", "%r11d"};
static const char* const s_reg_names_2[] = {"%ax", "%bx", "%cx", "%dx", "%di", "%si", "%r8w", "%r9w", "%r10w", "%r11w"};
static const char* const s_reg_names_1[] = {"%al", "%bl", "%cl", "%dl", "%dil", "%sil", "%r8b", "%r9b", "%r10b", "%r11b"};

struct ActivationRecord
{
    unsigned char* frame_slots;
    size_t total_frame_size;
    size_t locals_offset;
    size_t temp_offset;
    size_t arg_offset;
};

static void cg_gen_taca_reg(struct CodeGen* cg, int reg, int32_t sizing)
{
    switch (sizing)
    {
        case 8:
        case -8: array_appends(&cg->code, s_reg_names[reg]); break;
        case 4:
        case -4: array_appends(&cg->code, s_reg_names_4[reg]); break;
        case 2:
        case -2: array_appends(&cg->code, s_reg_names_2[reg]); break;
        case 1:
        case -1: array_appends(&cg->code, s_reg_names_1[reg]); break;
        default: abort();
    }
}

static int cg_gen_taca(struct CodeGen* cg, struct TACAddress addr, struct ActivationRecord* frame)
{
    int rc = 0;
    switch (addr.kind)
    {
        case TACA_NAME:
            if (cg->target == CG_TARGET_MACOS_GAS)
            {
                array_push_byte(&cg->code, '_');
            }
            array_appends(&cg->code, addr.name);
            array_appends(&cg->code, "@GOTPCREL(%rip)");
            break;
        case TACA_LNAME:
            if (cg->target == CG_TARGET_MACOS_GAS)
            {
                array_push_byte(&cg->code, '_');
            }
            array_appends(&cg->code, addr.name);
            if (!addr.is_addr) array_appends(&cg->code, "(%rip)");
            break;
        case TACA_LITERAL: array_appends(&cg->code, addr.literal); break;
        case TACA_IMM: array_appendf(&cg->code, "$%zu", addr.imm); break;
        case TACA_ALABEL: array_appendf(&cg->code, "L$%zu", addr.alabel); break;
        case TACA_LLABEL: array_appendf(&cg->code, "L$%zu_%s", cg->cur_fn_lbl_prefix, addr.literal); break;
        case TACA_TEMP: array_appends(&cg->code, "%rcx"); break;
        case TACA_REG: cg_gen_taca_reg(cg, addr.reg, addr.sizing); break;
        case TACA_CONST: array_appendf(&cg->code, "L_.S%d(%%rip)", addr.const_idx); break;
        case TACA_REF:
            array_appendf(&cg->code, "%zu(%%rsp)", frame->temp_offset + frame->frame_slots[addr.ref] * 8);
            break;
        case TACA_PARAM: array_appendf(&cg->code, "%zu(%%rsp)", addr.param_offset); break;
        case TACA_FRAME: array_appendf(&cg->code, "%zu(%%rsp)", frame->locals_offset + addr.frame_offset); break;
        case TACA_ARG:
            array_appendf(&cg->code, "%zu(%%rsp)", frame->locals_offset + frame->total_frame_size + addr.arg_offset);
            break;
        default: parser_ferror(NULL, "error: unimplemented TACA: %s\n", taca_to_string(addr.kind)); break;
    }
    return rc;
}

static int cg_gen_inst_a(struct CodeGen* cg, const char* inst, struct TACAddress addr, struct ActivationRecord* frame)
{
    int rc = 0;
    array_appendf(&cg->code, "    %s ", inst);
    rc = cg_gen_taca(cg, addr, frame);
    array_push_byte(&cg->code, '\n');
    return rc;
}

static int cg_gen_load(struct CodeGen* cg, struct TACAddress addr, int reg, struct ActivationRecord* frame)
{
    if (addr.kind == TACA_IMM)
    {
        array_appendf(&cg->code, "    mov $%zu, %s\n", addr.imm, s_reg_names[reg]);
        return 0;
    }
    int rc = 0;
    if (!addr.is_addr)
    {
        goto not_address;
    }
    addr.is_addr = 0;
    array_appends(&cg->code, "    leaq ");
    rc = cg_gen_taca(cg, addr, frame);
    array_appendf(&cg->code, ", %s\n", s_reg_names[reg]);
    return rc;

not_address:;
    const char* dst;
    switch (addr.sizing)
    {
        case 8:
        case -8: dst = s_reg_names[reg]; break;
        case 4:
        case -4: dst = s_reg_names_4[reg]; break;
        case 2:
        case -2: dst = s_reg_names_2[reg]; break;
        case 1:
        case -1: dst = s_reg_names_1[reg]; break;
        default: abort();
    }
    if (addr.kind == TACA_NAME)
    {
        addr.is_addr = 1;
        array_appends(&cg->code, "    movq ");
        rc = cg_gen_taca(cg, addr, frame);
        array_appendf(&cg->code, ", %s\n", s_reg_names[reg]);
        array_appendf(&cg->code, "    mov (%s), %s\n", s_reg_names[reg], dst);
    }
    else
    {
        array_appends(&cg->code, "    mov ");
        rc = cg_gen_taca(cg, addr, frame);
        array_appendf(&cg->code, ", %s\n", dst);
    }
    if (addr.sizing != 8 && addr.sizing != -8 && addr.sizing != 4)
        array_appendf(&cg->code, "    mov%cx %s, %s\n", addr.sizing < 0 ? 's' : 'z', dst, s_reg_names[reg]);
    return rc;
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

static int cg_gen_store(struct CodeGen* cg, struct TACAddress addr, int reg, struct ActivationRecord* frame)
{
    array_appends(&cg->code, "    mov ");
    cg_gen_taca_reg(cg, reg, addr.sizing);
    array_appends(&cg->code, ", ");
    int rc = cg_gen_taca(cg, addr, frame);
    array_push_byte(&cg->code, '\n');
    return rc;
}

static int cg_gen_store_frame(struct CodeGen* cg, size_t i, int reg, struct ActivationRecord* frame)
{
    if (frame->frame_slots[i] == 255) return 0;
    struct TACAddress addr = {
        .kind = TACA_REF,
        .ref = i,
        .sizing = 8,
    };
    return cg_gen_store(cg, addr, reg, frame);
}

static int cg_add(struct CodeGen* cg, size_t i, const struct TACEntry* taces, struct ActivationRecord* frame)
{
    int rc = 0;
    UNWRAP(cg_gen_load(cg, taces[i].arg1, REG_RAX, frame));
    UNWRAP(cg_gen_load(cg, taces[i].arg2, REG_RBX, frame));
    array_appends(&cg->code, "add %rbx, %rax\n");
    UNWRAP(cg_gen_store_frame(cg, i, REG_RAX, frame));
fail:
    return rc;
}

static int cg_memcpy(
    struct CodeGen* cg, struct TACAddress arg1, struct TACAddress arg2, int32_t sizing, struct ActivationRecord* frame)
{
    int rc = 0;
    if (sizing <= 8 && arg1.is_addr && arg2.is_addr)
    {
        arg1.is_addr = 0;
        arg1.sizing = sizing;
        arg2.is_addr = 0;
        arg2.sizing = sizing;
        UNWRAP(cg_gen_load(cg, arg2, REG_RCX, frame));
        UNWRAP(cg_gen_store(cg, arg1, REG_RCX, frame));
        goto fail;
    }

    UNWRAP(cg_gen_load(cg, arg2, REG_RAX, frame));
    UNWRAP(cg_gen_load(cg, arg1, REG_RBX, frame));
    int32_t j = 0;
    for (; j + 8 < sizing; j += 8)
    {
        array_appendf(&cg->code, "    movq %d(%%rax), %%rcx\n", j);
        array_appendf(&cg->code, "    movq %%rcx, %d(%%rbx)\n", j);
    }
    switch (sizing - j)
    {
        case 8:
        case -8:
            array_appendf(&cg->code, "    movq %d(%%rax), %%rcx\n", j);
            array_appendf(&cg->code, "    movq %%rcx, %d(%%rbx)\n", j);
            break;
        case 4:
        case -4:
            array_appendf(&cg->code, "    movl %d(%%rax), %%ecx\n", j);
            array_appendf(&cg->code, "    movl %%ecx, %d(%%rbx)\n", j);
            break;
        case 2:
        case -2:
            array_appendf(&cg->code, "    movw %d(%%rax), %%cx\n", j);
            array_appendf(&cg->code, "    movw %%cx, %d(%%rbx)\n", j);
            break;
        case 1:
        case -1:
            array_appendf(&cg->code, "    movb %d(%%rax), %%cl\n", j);
            array_appendf(&cg->code, "    movb %%cl, %d(%%rbx)\n", j);
            break;
        default: UNWRAP(parser_ferror(NULL, "error: invalid sizing: %d\n", sizing - j));
    }
fail:
    return rc;
}

static int cg_assign(struct CodeGen* cg, struct TACAddress arg1, struct TACAddress arg2, struct ActivationRecord* frame)
{
    int rc = 0;
    if (!arg1.is_addr && arg1.kind == TACA_REG)
    {
        if (arg2.is_addr)
        {
            UNWRAP(cg_gen_load(cg, arg2, REG_R11, frame));
            array_appends(&cg->code, "    mov ");
            cg_gen_taca_reg(cg, REG_R11, 8);
        }
        else
        {
            array_appends(&cg->code, "    mov ");
            UNWRAP(cg_gen_taca(cg, arg2, frame));
        }
        array_appends(&cg->code, ", 0(");
        cg_gen_taca_reg(cg, arg1.reg, arg1.sizing);
        array_appends(&cg->code, ")\n");
        goto fail;
    }
    if (arg1.is_addr && arg1.kind == TACA_REG)
    {
        UNWRAP(cg_gen_load(cg, arg2, arg1.reg, frame));
        goto fail;
    }
    if (!arg2.is_addr)
    {
        if (arg2.kind == TACA_REG)
        {
            // todo: fix cg_gen_store to dereference first argument
            arg1.sizing = arg2.sizing;
            arg1.is_addr = 0;
            UNWRAP(cg_gen_store(cg, arg1, arg2.reg, frame));
        }
        else
        {
            arg2.is_addr = 1;
            UNWRAP(cg_memcpy(cg, arg1, arg2, arg2.sizing, frame));
        }
        goto fail;
    }
    UNWRAP(cg_gen_load(cg, arg2, REG_RAX, frame));
fail:
    return rc;
}

static int cg_gen_tace(struct CodeGen* cg, const struct TACEntry* taces, size_t i, struct ActivationRecord* frame)
{
    int rc = 0;
    const char* inst;

    const struct TACEntry* tace = taces + i;

    switch (tace->op)
    {
        case TACO_LT:
        case TACO_LTEQ:
        case TACO_EQ:
        case TACO_NEQ:
            UNWRAP(cg_gen_load(cg, tace->arg1, REG_RAX, frame));
            UNWRAP(cg_gen_load(cg, tace->arg2, REG_RBX, frame));
            array_appends(&cg->code, "cmp %rbx, %rax\n");
            switch (tace->op)
            {
                case TACO_LT: array_appends(&cg->code, "    setl %al\n"); break;
                case TACO_LTEQ: array_appends(&cg->code, "    setle %al\n"); break;
                case TACO_EQ: array_appends(&cg->code, "    sete %al\n"); break;
                case TACO_NEQ: array_appends(&cg->code, "    setne %al\n"); break;
                default: abort();
            }
            array_appends(&cg->code, "    movzx %al, %rax\n");
            UNWRAP(cg_gen_store_frame(cg, i, REG_RAX, frame));
            break;
        case TACO_SUB: inst = "subq"; goto simple_binary;
        case TACO_MULT: inst = "imul"; goto simple_binary;
        case TACO_DIV:
        case TACO_MOD:
            UNWRAP(cg_gen_load(cg, tace->arg1, REG_RAX, frame));
            UNWRAP(cg_gen_load(cg, tace->arg2, REG_RBX, frame));
            array_appends(&cg->code, "    movq $0, %rdx\n");
            array_appends(&cg->code, "    idivq %rbx\n");
            if (tace->op == TACO_DIV)
                UNWRAP(cg_gen_store_frame(cg, i, REG_RAX, frame));
            else
                UNWRAP(cg_gen_store_frame(cg, i, REG_RBX, frame));
            break;
        case TACO_BAND: inst = "and"; goto simple_binary;
        case TACO_BOR: inst = "or"; goto simple_binary;
        case TACO_BXOR: inst = "xor"; goto simple_binary;
        case TACO_SHL: inst = "shl"; goto shift;
        case TACO_SHR:
            inst = "shr";
        shift:
            if (tace->arg2.kind != TACA_IMM)
            {
                UNWRAP(cg_gen_load(cg, tace->arg1, REG_RAX, frame));
                UNWRAP(cg_gen_load(cg, tace->arg2, REG_RCX, frame));
                array_appends(&cg->code, "    shl %cl, %rax\n");
                UNWRAP(cg_gen_store_frame(cg, i, REG_RAX, frame));
            }
            break;
        case TACO_BNOT:
            UNWRAP(cg_gen_load(cg, tace->arg1, REG_RAX, frame));
            array_appends(&cg->code, "    not %rax\n");
            UNWRAP(cg_gen_store_frame(cg, i, REG_RAX, frame));
            break;
        case TACO_CALL:
            array_appends(&cg->code, "    movb $0, %al\n");
            array_appends(&cg->code, "    callq ");
            if (taca_is_memory(&tace->arg1))
            {
                array_push_byte(&cg->code, '*');
            }
            UNWRAP(cg_gen_taca(cg, tace->arg1, frame));
            array_push_byte(&cg->code, '\n');
            UNWRAP(cg_gen_store_frame(cg, i, REG_RAX, frame));
            break;
        case TACO_LOAD:
        {
            struct TACAddress arg1 = tace->arg1;
            arg1.is_addr = 1;
            UNWRAP(cg_memcpy(cg, arg1, tace->arg2, arg1.sizing, frame));
            break;
        }
        simple_binary:
            UNWRAP(cg_gen_load(cg, tace->arg2, REG_RBX, frame));
            UNWRAP(cg_gen_load(cg, tace->arg1, REG_RAX, frame));
            array_appendf(&cg->code, "    %s %%rbx, %%rax\n", inst);
            UNWRAP(cg_gen_store_frame(cg, i, REG_RAX, frame));
            break;
        case TACO_ADD: UNWRAP(cg_add(cg, i, taces, frame)); break;
        case TACO_ASSIGN: UNWRAP(cg_assign(cg, tace->arg1, tace->arg2, frame)); break;
        case TACO_RETURN:
            if (tace->arg1.kind != TACA_VOID)
            {
                if (tace->arg1.sizing <= 8)
                {
                    UNWRAP(cg_gen_load(cg, tace->arg1, REG_RAX, frame));
                }
                else
                {
                    struct TACAddress arg1 = tace->arg1;
                    arg1.is_addr = 1;
                    struct TACAddress arg2 = {
                        .kind = TACA_PARAM,
                        .is_addr = 1,
                    };
                    UNWRAP(cg_memcpy(cg, arg1, arg2, arg1.sizing, frame));
                }
            }
            array_appendf(&cg->code, "\n    addq $%zu, %%rsp\n    ret\n", frame->total_frame_size);
            break;
        case TACO_JUMP: UNWRAP(cg_gen_inst_a(cg, "jmp", tace->arg1, frame)); break;
        case TACO_BRZ:
            if (tace->arg1.kind == TACA_IMM)
            {
                if (tace->arg1.imm == 0) UNWRAP(cg_gen_inst_a(cg, "jmp", tace->arg2, frame));
            }
            else
            {
                UNWRAP(cg_gen_load(cg, tace->arg1, REG_RAX, frame));
                array_appends(&cg->code, "cmp $0, %rax\n");
                UNWRAP(cg_gen_inst_a(cg, "jz ", tace->arg2, frame));
            }
            break;
        case TACO_BRNZ:
            if (tace->arg1.kind == TACA_IMM)
            {
                if (tace->arg1.imm != 0) UNWRAP(cg_gen_inst_a(cg, "jmp", tace->arg2, frame));
            }
            else
            {
                UNWRAP(cg_gen_load(cg, tace->arg1, REG_RAX, frame));
                array_appends(&cg->code, "cmp $0, %rax\n");
                UNWRAP(cg_gen_inst_a(cg, "jnz ", tace->arg2, frame));
            }
            break;
        case TACO_CTBZ:
            if (tace->arg1.kind != TACA_IMM) abort();
            array_appendf(&cg->code, "cmp $%zu, %%rcx\n", tace->arg1.imm);
            UNWRAP(cg_gen_inst_a(cg, "jz ", tace->arg2, frame));
            break;
        case TACO_LABEL:
            UNWRAP(cg_gen_taca(cg, tace->arg1, frame));
            array_appends(&cg->code, ":\n");
            break;
        case TACO_PHI: break;
        default: parser_ferror(NULL, "error: unimplemented TACO: %s\n", taco_to_string(tace->op)); break;
    }
fail:
    return rc;
}

int cg_gen_taces(struct CodeGen* cg, const struct TACEntry* taces, size_t n_taces, size_t locals_size)
{
    int rc = 0;

    struct Array param_stack = {};

    unsigned char* frame_slots = NULL;
    struct FreeFrameSlots ffs = {};

    ++cg->cur_fn_lbl_prefix;

    size_t num_args = 0;
    size_t max_params = 0;

    frame_slots = (unsigned char*)my_malloc(n_taces);
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
            UNWRAP(parser_ferror(NULL, "error: stack frame size exceeded\n"));
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
        if (tace->op == TACO_ARG)
        {
            ++num_args;
        }
        // if (tace->arg2.kind == TACA_PARAM && tace->arg2.param_idx > max_params)
        // {
        //     max_params = tace->arg2.param_idx;
        // }
    }

    size_t frame_size = locals_size + ffs.max_used * 8;

    if (cg->target == CG_TARGET_MACOS_GAS)
    {
        // reserve space for incoming reg params and outgoing stack params
        frame_size += max_params > 6 ? max_params * 8 : 6 * 8;
    }
    else
    {
        // TODO: ms-abi
        abort();
    }

    // align stack for calls (32-byte alignment)
    frame_size += 31 - (frame_size + 7) % 32;

    struct ActivationRecord frame = {
        .frame_slots = frame_slots,
        .total_frame_size = frame_size,
    };
    frame.arg_offset = cg->target == CG_TARGET_MACOS_GAS ? 6 * 8 : frame_size + 8;
    frame.locals_offset = max_params > 6 ? max_params * 8 : 6 * 8;
    frame.temp_offset = frame.locals_offset + locals_size;

    array_appendf(&cg->code, "    subq $%zu, %%rsp\n", frame_size);

    for (size_t i = 0; i < n_taces; ++i)
    {
        if (taces[i].rc)
        {
            array_appendf(&cg->code,
                          //                          "    .file 1 \"\" \"%s\"\n"
                          "    .loc 1 %d %d ## %s:%d:%d\n",
                          //                          taces[i].rc->file,
                          taces[i].rc->row,
                          taces[i].rc->col,
                          taces[i].rc->file,
                          taces[i].rc->row,
                          taces[i].rc->col);
        }
        array_appendf(&cg->code,
                      "    ## TAC %zu: (%s, %s, %s) -> %d\n",
                      i,
                      taco_to_string(taces[i].op),
                      taca_to_string(taces[i].arg1.kind),
                      taca_to_string(taces[i].arg2.kind),
                      frame_slots[i]);

        UNWRAP(cg_gen_tace(cg, taces, i, &frame));
    }

    array_appendf(&cg->code, "    addq $%zu, %%rsp\n    ret\n", frame_size);

fail:
    my_free(frame_slots);
    array_destroy(&param_stack);
    return rc || parser_has_errors();
}

int cg_emit(struct CodeGen* cg, FILE* fout)
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
        prelude = "\n";
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
        UNWRAP(!fwrite(cg->code.data, cg->code.sz, 1, fout));
    }

    if (cg->target == CG_TARGET_WIN_MASM) UNWRAP(0 > fputs("END\n", fout));

    return 0;

fail:
    perror("error: failed to write output");
    return 1;
}
