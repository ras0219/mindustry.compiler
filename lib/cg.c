#include "cg.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "errors.h"
#include "stdlibe.h"
#include "tac.h"
#include "tok.h"
#include "unwrap.h"

void cg_init(struct CodeGen* cg) { memset(cg, 0, sizeof(struct CodeGen)); }
void cg_destroy(struct CodeGen* cg)
{
    array_destroy(&cg->const_);
    array_destroy(&cg->code);
}

__forceinline void cg_debug(struct CodeGen* cg, const char* fmt, ...)
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
    array_appendf(&cg->code, "extern %s:proc\n", sym);
}

void cg_declare_public(struct CodeGen* cg, const char* sym)
{
    cg_debug(cg, "   : %s\n", sym);
    array_appendf(&cg->code, "public %s\n", sym);
}

void cg_mark_label(struct CodeGen* cg, const char* sym)
{
    cg_debug(cg, "   : %s\n", sym);
    array_appendf(&cg->code, "%s:\n", sym);
}

void cg_string_constant(struct CodeGen* cg, size_t cidx, const char* str)
{
    cg_debug(cg, "   : strconst %d: %s\n", cidx, str);
    array_appendf(&cg->const_, "@S%d db \"%s\",0\n", cidx, str);
}
static int cg_gen_taca(struct CodeGen* cg, struct TACAddress addr, size_t frame_size, unsigned char* frame_slots)
{
    int rc = 0;
    switch (addr.kind)
    {
        case TACA_NAME: array_appendf(&cg->code, "%s", addr.name); break;
        case TACA_LITERAL: array_appendf(&cg->code, "%s", addr.literal); break;
        case TACA_CONST: array_appendf(&cg->code, "offset @S%d", addr.const_idx); break;
        case TACA_REF: array_appendf(&cg->code, "%zu[RSP]", (size_t)32 + frame_slots[addr.ref] * 8); break;
        case TACA_ARG: array_appendf(&cg->code, "%zu[RSP]", addr.arg_idx * 8 + 8 + frame_size); break;
        case TACA_FRAME: array_appendf(&cg->code, "QWORD PTR [RSP+%zu]", frame_size - 8 - addr.frame_offset); break;
        default: return parser_ferror(NULL, "error: unimplemented TACA: %d\n", addr.kind);
    }
    return rc;
}

static const char* const s_reg_names[4] = {"RCX", "RDX", "R8", "R9"};

// static void cg_emit_label(struct CodeGen* cg, size_t lbl)
//{
//    cg_debug(cg, "   : $L%zu\n", lbl);
//    array_appendf(&cg->code, "$L%zu:\n", lbl);
//}
// static void cg_jump_label(struct CodeGen* cg, size_t lbl) { array_appendf(&cg->code, "    jmp $L%zu\n", lbl); }

struct FreeFrameSlots
{
    // interpret value as (pos + v) % 256
    unsigned char freestack[256];
    unsigned char next_free;
    unsigned char max_used;
};

static unsigned char ffs_pop(struct FreeFrameSlots* ffs)
{
    unsigned char i = ffs->next_free++;
    if (i > ffs->max_used) ffs->max_used = i;
    return (unsigned char)(ffs->freestack[i] + i);
}

static void ffs_push(struct FreeFrameSlots* ffs, unsigned char s)
{
    unsigned char i = --ffs->next_free;
    ffs->freestack[i] = s - i;
}

static void cg_mov_frame_from_rax(struct CodeGen* cg, unsigned char slot)
{
    if (slot == 255) return;
    array_appendf(&cg->code, "    mov QWORD PTR [RSP + %zu], rax\n", 32 + slot);
}

enum
{
    TACA_VOID_IS_MEMORY = 0,
    TACA_LITERAL_IS_MEMORY = 0,
    TACA_IMM_IS_MEMORY = 0,
    TACA_NAME_IS_MEMORY = 0,
    TACA_FRAME_IS_MEMORY = 1,
    TACA_REF_IS_MEMORY = 1,
    TACA_PARAM_IS_MEMORY = 0,
    TACA_CONST_IS_MEMORY = 0,
    TACA_ARG_IS_MEMORY = 1,
};

#define Y_IS_MEMORY(Z) Z##_IS_MEMORY,
static const char s_table_taca_is_memory[TACA_KIND_COUNT] = {X_TACA_KIND(Y_IS_MEMORY)};
#undef Y_IS_MEMORY

__forceinline static int taca_is_memory(enum TACAKind kind) { return s_table_taca_is_memory[kind]; }

static int cg_add(struct CodeGen* cg, size_t i, struct TACEntry* taces, size_t frame_size, unsigned char* frame_slots)
{
    int rc = 0;
    array_appends(&cg->code, "    mov rax, ");
    UNWRAP(cg_gen_taca(cg, taces[i].arg1, frame_size, frame_slots));
    array_appends(&cg->code, "\n");
    if (taces[i].arg2.kind == TACA_REF && frame_slots[taces[i].arg2.ref] == frame_slots[i])
    {
        array_appends(&cg->code, "    add ");
        UNWRAP(cg_gen_taca(cg, taces[i].arg2, frame_size, frame_slots));
        array_appends(&cg->code, ", rax\n");
    }
    else
    {
        array_appends(&cg->code, "    add rax, ");
        UNWRAP(cg_gen_taca(cg, taces[i].arg2, frame_size, frame_slots));
        array_appends(&cg->code, "\n");
        cg_mov_frame_from_rax(cg, frame_slots[i]);
    }
fail:
    return rc;
}

static int cg_assign(
    struct CodeGen* cg, size_t i, struct TACEntry* taces, size_t frame_size, unsigned char* frame_slots)
{
    int rc = 0;
    if (taca_is_memory(taces[i].arg1.kind) && taca_is_memory(taces[i].arg2.kind))
    {
        array_appends(&cg->code, "    mov rax, ");
        UNWRAP(cg_gen_taca(cg, taces[i].arg2, frame_size, frame_slots));
        array_appends(&cg->code, "\n    mov ");
        UNWRAP(cg_gen_taca(cg, taces[i].arg1, frame_size, frame_slots));
        array_appends(&cg->code, ", rax\n");
    }
    else
    {
        array_appends(&cg->code, "    mov ");
        UNWRAP(cg_gen_taca(cg, taces[i].arg1, frame_size, frame_slots));
        array_appends(&cg->code, ", ");
        UNWRAP(cg_gen_taca(cg, taces[i].arg2, frame_size, frame_slots));
        array_appends(&cg->code, "\n");
    }
fail:
    return rc;
}

int cg_gen_taces(struct CodeGen* cg, struct TACEntry* taces, size_t n_taces, size_t frame_size)
{
    int rc = 0;

    struct Array param_stack = {};

    unsigned char* frame_slots = NULL;
    struct FreeFrameSlots ffs = {};

    frame_slots = (unsigned char*)my_malloc(n_taces);
    memset(frame_slots, 0xFF, n_taces);
    for (size_t i = 0; i < n_taces; ++i)
    {
        const size_t j = n_taces - i - 1;
        struct TACEntry* const tace = taces + j;
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
    }

    frame_size += ffs.max_used * 8 + 32;
    // align stack for calls (32-byte alignment)
    frame_size += 31 - (frame_size + 7) % 32;

    array_appendf(&cg->code, "    sub rsp, %zu\n", frame_size);

    for (size_t i = 0; i < n_taces; ++i)
    {
        array_appendf(
            &cg->code, "    ; TAC %zu: (%d, %d, %d)\n", i, taces[i].op, taces[i].arg1.kind, taces[i].arg2.kind);

        switch (taces[i].op)
        {
            case TACO_ARG:
                if (taces[i].arg1.arg_idx > 3)
                {
                    rc = parser_ferror(NULL, "error: only 4 parameters are supported\n");
                    goto fail;
                }
                array_appends(&cg->code, "    mov ");
                UNWRAP(cg_gen_taca(cg, taces[i].arg1, frame_size, frame_slots));
                array_appendf(&cg->code, ", %s\n", s_reg_names[taces[i].arg1.arg_idx]);
                break;
            case TACO_PARAM:
                if (taces[i].arg2.param_idx > 3)
                {
                    rc = parser_ferror(NULL, "error: only 4 parameters are supported\n");
                    goto fail;
                }
                array_appendf(&cg->code, "    mov %s, ", s_reg_names[taces[i].arg2.param_idx]);
                UNWRAP(cg_gen_taca(cg, taces[i].arg1, frame_size, frame_slots));
                array_appends(&cg->code, "\n");
                break;
            case TACO_CALL:
                array_appends(&cg->code, "    call ");
                UNWRAP(cg_gen_taca(cg, taces[i].arg1, frame_size, frame_slots));
                array_appends(&cg->code, "\n");
                cg_mov_frame_from_rax(cg, frame_slots[i]);
                break;
            case TACO_MULT:
                array_appends(&cg->code, "    mov rax, ");
                UNWRAP(cg_gen_taca(cg, taces[i].arg1, frame_size, frame_slots));
                array_appends(&cg->code, "\n");
                array_appends(&cg->code, "    imul rax, ");
                UNWRAP(cg_gen_taca(cg, taces[i].arg2, frame_size, frame_slots));
                array_appends(&cg->code, "\n");
                cg_mov_frame_from_rax(cg, frame_slots[i]);
                break;
            case TACO_ADD: UNWRAP(cg_add(cg, i, taces, frame_size, frame_slots)); break;
            case TACO_ASSIGN: UNWRAP(cg_assign(cg, i, taces, frame_size, frame_slots)); break;
            case TACO_RETURN:
                array_appends(&cg->code, "    mov rax, ");
                UNWRAP(cg_gen_taca(cg, taces[i].arg1, frame_size, frame_slots));
                array_appendf(&cg->code, "\n    add rsp, %zu\n    ret 0\n", frame_size);
                break;
            default: rc = parser_ferror(NULL, "error: unimplemented TACO: %d\n", taces[i].op); goto fail;
        }
    }

    array_appendf(&cg->code, "    add rsp, %zu\n    ret 0\n", frame_size);

fail:
    my_free(frame_slots);
    array_destroy(&param_stack);
    return rc;
}

int cg_emit(struct CodeGen* cg, FILE* fout)
{
    int rc = 0;
    cg_debug(cg, "cg_emit():\n");
    const char prelude[] = "option casemap:none\n"
                           "INCLUDELIB msvcrt.lib\n"
                           "INCLUDELIB ucrt.lib\n"
                           "INCLUDELIB vcruntime.lib\n"
                           "INCLUDELIB kernel32.lib\n"
                           "INCLUDELIB legacy_stdio_definitions.lib\n";
    UNWRAP(!fwrite(prelude, sizeof(prelude) - 1, 1, fout));
    if (cg->const_.sz)
    {
        UNWRAP(fputs("\n.const\n\n", fout) < 0);
        UNWRAP(!fwrite(cg->const_.data, cg->const_.sz, 1, fout));
    }
    if (cg->code.sz)
    {
        UNWRAP(fputs("\n.code\n\n", fout) < 0);
        UNWRAP(!fwrite(cg->code.data, cg->code.sz, 1, fout));
    }

    UNWRAP(0 > fputs("END\n", fout));

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