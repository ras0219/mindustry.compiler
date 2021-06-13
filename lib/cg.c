#include "cg.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tok.h"

void cg_init(struct CodeGen* cg)
{
    cg->lines = 0;
    array_init(&cg->text);
    array_init(&cg->labels);
    array_init(&cg->label_strs);
    memset(&cg->memory, 0, sizeof(cg->memory));
    cg->fdebug = NULL;
    cg->fout = stdout;
}
void cg_destroy(struct CodeGen* cg)
{
    array_destroy(&cg->text);
    array_destroy(&cg->labels);
    array_destroy(&cg->label_strs);
}
static struct RowCol s_unknown_rc = {
    .file = "<unknown>",
    .row = 1,
    .col = 1,
};
void cg_write_bin_entry(struct CodeGen* cg)
{
    cg_write_inst(cg, "set __stk__ $__stk__$");
    cg_write_inst(cg, "set ret 0");
    cg_write_inst(cg, "set _r_main 0");
    cg_write_inst(cg, "jump $main$ always");
}
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
        return parser_ferror(rc, "error: no memory bank configured yet -- use #pragma memory <memory1>\n"), 1;
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
    static const char s_nl = '\n';
    array_push(&cg->text, inst, strlen(inst));
    array_push(&cg->text, &s_nl, 1);
    ++cg->lines;
}

struct CodeGenLabel
{
    ptrdiff_t str_offset;
    ptrdiff_t str_len;
    size_t line;
};

void cg_mark_label(struct CodeGen* cg, const char* sym)
{
    if (cg->fdebug) fprintf(cg->fdebug, "   : %s\n", sym);
    const size_t sym_len = strlen(sym);
    if (sym[0] != '$' || sym_len < 2 || sym[sym_len - 1] != '$')
    {
        fprintf(stderr, "internal compiler error\n");
        abort();
    }
    const struct CodeGenLabel lab = {
        .line = cg->lines,
        .str_len = sym_len - 2,
        .str_offset = cg->label_strs.sz,
    };
    array_push(&cg->label_strs, sym + 1, lab.str_len);
    array_push(&cg->labels, &lab, sizeof(struct CodeGenLabel));
}

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

void cg_emit(struct CodeGen* cg, int globals_size)
{
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
                fprintf(stderr, "error: unresolved symbol: '%.*s'\n", (int)(i - sym_begin), text + sym_begin);
                abort();
            }
            fprintf(cg->fout, "%zu", label->line);
            last_emit_point = ++i;
        }
    }
    if (fwrite(text + last_emit_point, 1, i - last_emit_point, cg->fout) != i - last_emit_point)
    {
        perror("error: failed to write output");
        abort();
    }
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
