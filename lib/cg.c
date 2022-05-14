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

static const char* const s_reg_names[] = {"%rax", "%rbx", "%rcx", "%rdx", "%rdi", "%rsi", "%r8", "%r9"};
static const char* const s_reg_names_4[] = {"%eax", "%ebx", "%ecx", "%edx", "%edi", "%esi", "%r8d", "%r9d"};
static const char* const s_reg_names_2[] = {"%ax", "%bx", "%cx", "%dx", "%di", "%si", "%r8w", "%r9w"};
static const char* const s_reg_names_1[] = {"%al", "%bl", "%cl", "%dl", "%dil", "%sil", "%r8b", "%r9b"};

struct ActivationRecord
{
    unsigned char* frame_slots;
    size_t total_frame_size;
    size_t locals_offset;
    size_t temp_offset;
    size_t arg_offset;
};

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
        case TACA_REG:
            switch (addr.sizing)
            {
                case 8:
                case -8: array_appends(&cg->code, s_reg_names[addr.reg]); break;
                case 4:
                case -4: array_appends(&cg->code, s_reg_names_4[addr.reg]); break;
                case 2:
                case -2: array_appends(&cg->code, s_reg_names_2[addr.reg]); break;
                case 1:
                case -1: array_appends(&cg->code, s_reg_names_1[addr.reg]); break;
                default: abort();
            }
            break;
        case TACA_CONST: array_appendf(&cg->code, "L_.S%d(%%rip)", addr.const_idx); break;
        case TACA_REF:
            array_appendf(&cg->code, "%zu(%%rsp)", frame->temp_offset + frame->frame_slots[addr.ref] * 8);
            break;
        case TACA_ARG:
            if (cg->target == CG_TARGET_MACOS_GAS && addr.arg_idx < 6)
            {
                // With System-V abi, the first 6
                array_appendf(&cg->code, "%zu(%%rsp)", addr.arg_idx * 8);
            }
            else
            {
                array_appendf(&cg->code, "%zu(%%rsp)", frame->total_frame_size + 8 + (addr.arg_idx - 6) * 8);
            }
            break;
        case TACA_FRAME: array_appendf(&cg->code, "%zu(%%rsp)", frame->locals_offset + addr.frame_offset); break;
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
    switch (addr.sizing)
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
    if (!arg2.is_addr)
    {
        arg2.is_addr = 1;
        UNWRAP(cg_memcpy(cg, arg1, arg2, arg2.sizing, frame));
        goto fail;
    }
    UNWRAP(cg_gen_load(cg, arg2, REG_RAX, frame));
    UNWRAP(cg_gen_store(cg, arg1, REG_RAX, frame));
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
        if (tace->arg2.kind == TACA_PARAM && tace->arg2.param_idx > max_params)
        {
            max_params = tace->arg2.param_idx;
        }
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

    array_appendf(&cg->code, "    addq $%zu, %%rsp\n    retq\n", frame_size);

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

#if 0
    const struct CodeGenLabel lab = {
        .line = globals_size,
        .str_len = strlen("__stk__"),
        .str_offset = cg->label_strs.sz,
    };
    array_push(&cg->label_strs, "__stk__", lab.str_len);
    array_push(&cg->labels, &lab, sizeof(struct CodeGenLabel));

    if (cg->fdebug) fprintf(cg->fdebug, "\nCode Gen\n--------\n");
    fflush(NULL);

    const char* const text = (const char*)cg->text.data;
    size_t last_emit_point = 0;
    size_t i = 0;
    for (; i < cg->text.sz; ++i)
    {
        if (text[i] == '$')
        {
            if (fwrite(text + last_emit_point, 1, i - last_emit_point, cg->fout) != i - last_emit_point)
            {
                perror("error: failed to write output");
                abort();
            }
            const size_t sym_begin = ++i;
            do
            {
                if (i == cg->text.sz)
                {
                    fprintf(stderr, "error: unterminated symbol in generated code\n");
                    abort();
                }
                if (text[i] == '$') break;
                ++i;
            } while (1);
            const struct CodeGenLabel* label = cg_lookup(cg, text + sym_begin, i - sym_begin);
            if (!label)
            {
                // fprintf(stderr, "error: unresolved symbol: '%.*s'\n", (int)(i - sym_begin), text + sym_begin);
                // abort();
            }
            else
            {
                fprintf(cg->fout, "%zu", label->line);
            }
            last_emit_point = ++i;
        }
    }
    if (fwrite(text + last_emit_point, 1, i - last_emit_point, cg->fout) != i - last_emit_point)
    {
        perror("error: failed to write output");
        abort();
    }
#endif
}

#if 0
static struct RowCol s_unknown_rc = {
    .file = "<unknown>",
    .row = 1,
    .col = 1,
};
void cg_write_push_ret(struct CodeGen* cg, struct FreeVar* ret_addr)
{
    if (ret_addr->buf[0])
    {
        cg_write_inst_set(cg, ret_addr->buf, "ret");
    }
    else
    {
        cg_write_mem(cg, "__stk__", "ret", &s_unknown_rc);
        cg_write_inst_op(cg, "+", "__stk__", "__stk__", "1");
    }
}
void cg_write_prepare_stack(struct CodeGen* cg)
{
    cg_write_mem(cg, "__stk__", "__ebp__", &s_unknown_rc);
    cg_write_inst_set(cg, "__ebp__", "__stk__");
}
void cg_write_epilog(struct CodeGen* cg)
{
    cg_write_inst_op(cg, "-", "__stk__", "__ebp__", "1");
    cg_read_mem(cg, "__ebp__", "__ebp__", &s_unknown_rc);
}
void cg_write_return(struct CodeGen* cg, struct FreeVar* ret_addr)
{
    if (strcmp(ret_addr->buf, "_r_main") == 0)
    {
        cg_write_inst_jump(cg, "0");
    }
    else if (ret_addr->buf[0])
    {
        cg_write_inst_set(cg, "@counter", ret_addr->buf);
    }
    else
    {
        cg_read_mem(cg, "__stk__", "@counter", &s_unknown_rc);
    }
}
void cg_write_inst_set(struct CodeGen* cg, const char* dst, const char* src)
{
    if (strcmp(dst, src) == 0) return;
    char buf[64];
    snprintf(buf, sizeof(buf), "set %s %s", dst, src);
    cg_write_inst(cg, buf);
}
void cg_write_inst_jump(struct CodeGen* cg, const char* dst)
{
    char buf[64];
    snprintf(buf, sizeof(buf), "jump %s always", dst);
    cg_write_inst(cg, buf);
}
static const char* str_for_op(const char* op)
{
    const char* str_op;
    if (op[0] == '=' && op[1] == '=')
        str_op = "equal";
    else if (op[0] == '!' && op[1] == '=')
        str_op = "notEqual";
    else if (op[0] == '>' && op[1] == '\0')
        str_op = "greaterThan";
    else if (op[0] == '<' && op[1] == '\0')
        str_op = "lessThan";
    else if (op[0] == '>' && op[1] == '=')
        str_op = "greaterThanEq";
    else if (op[0] == '<' && op[1] == '=')
        str_op = "lessThanEq";
    else if (op[0] == '+' && op[1] == '\0')
        str_op = "add";
    else if (op[0] == '-' && op[1] == '\0')
        str_op = "sub";
    else if (op[0] == '*' && op[1] == '\0')
        str_op = "mul";
    else if (op[0] == '%' && op[1] == '\0')
        str_op = "mod";
    else if (op[0] == '/' && op[1] == '\0')
        str_op = "idiv";
    else if (op[0] == '|' && op[1] == '|')
        str_op = "or";
    else if (op[0] == '&' && op[1] == '&')
        str_op = "land";
    else
        abort();
    return str_op;
}
void cg_write_inst_jump_op(struct CodeGen* cg, const char* tgt, const char* op, const char* a, const char* b)
{
    if (op[0] == '=' && op[1] == '=' && strcmp(a, "1") == 0 && strcmp(b, "true") == 0)
    {
        return cg_write_inst_jump(cg, tgt);
    }
    const char* str_op = str_for_op(op);
    char buf[128];
    snprintf(buf, sizeof(buf), "jump %s %s %s %s", tgt, str_op, a, b);
    cg_write_inst(cg, buf);
}
void cg_write_inst_add(struct CodeGen* cg, const char* dst, const char* a, int n)
{
    char buf[128];
    snprintf(buf, sizeof(buf), "op add %s %s %d", dst, a, n);
    cg_write_inst(cg, buf);
}
void cg_write_inst_op(struct CodeGen* cg, const char* op, const char* dst, const char* a, const char* b)
{
    const char* str_op = str_for_op(op);
    char buf[128];
    snprintf(buf, sizeof(buf), "op %s %s %s %s", str_op, dst, a, b);
    cg_write_inst(cg, buf);
}
int cg_write_mem(struct CodeGen* cg, const char* addr, const char* val, const struct RowCol* rc)
{
    if (!val || !*val) abort();
    if (!addr || !*addr) abort();
    if (!cg->memory.buf[0])
    {
        // return parser_ferror(rc, "error: no memory bank configured yet -- use #pragma memory <memory1>\n"), 1;
    }
    char buf[128];
    snprintf(buf, sizeof(buf), "write %s %s %s", val, cg->memory.buf, addr);
    cg_write_inst(cg, buf);
    return 0;
}
int cg_read_mem(struct CodeGen* cg, const char* addr, const char* reg, const struct RowCol* rc)
{
    if (!cg->memory.buf[0])
    {
        return parser_ferror(rc, "error: no memory bank configured yet -- use #pragma memory <memory1>\n"), 1;
    }
    char buf[128];
    snprintf(buf, sizeof(buf), "read %s %s %s", reg, cg->memory.buf, addr);
    cg_write_inst(cg, buf);
    return 0;
}
void cg_write_inst(struct CodeGen* cg, const char* inst)
{
    if (cg->fdebug) fprintf(cg->fdebug, "%03zu: %s\n", cg->lines, inst);
    array_push(&cg->text, inst, strlen(inst));
    array_push(&cg->text, "\n", 1);
    ++cg->lines;
}

struct CodeGenLabel
{
    ptrdiff_t str_offset;
    ptrdiff_t str_len;
    size_t line;
};

static const struct CodeGenLabel* cg_lookup(struct CodeGen* cg, const char* sym, size_t sym_len)
{
    const struct CodeGenLabel* const labels = (const struct CodeGenLabel*)cg->labels.data;
    const size_t labels_sz = cg->labels.sz / sizeof(struct CodeGenLabel);
    const char* const strs = (const char*)cg->label_strs.data;
    for (size_t i = 0; i < labels_sz; ++i)
    {
        if (labels[i].str_len == sym_len && memcmp(sym, strs + labels[i].str_offset, sym_len) == 0) return labels + i;
    }
    return NULL;
}

int cg_set_memory_bank(struct CodeGen* cg, const struct RowCol* rc, const char* mem)
{
    size_t n = strlen(mem);
    if (n >= sizeof(cg->memory.buf))
    {
        return parser_ferror(
                   rc, "error: memory bank length must be less than '%lu' characters", sizeof(cg->memory.buf)),
               1;
    }
    strcpy(cg->memory.buf, mem);
    return 0;
}
#endif