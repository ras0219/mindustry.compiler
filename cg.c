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
}
void cg_write_bin_entry(struct CodeGen* cg)
{
    cg_write_inst(cg, "set __retstk__ $__fn_return__$");
    cg_write_inst(cg, "set ret 1");
    cg_write_inst(cg, "jump $main$ always");

    cg_mark_label(cg, "$__fn_return__$");
    // jumping directly here is an error, reboot?
    // This must be the exact size of the pop funclet below
    cg_write_inst(cg, "jump 0 always");
    cg_write_inst(cg, "jump 0 always");
    char buf[64];
    for (size_t i = 0; i < 4; ++i)
    {
        // push funclet
        snprintf(buf, sizeof(buf), "set __retstk%lu__ ret", i);
        cg_write_inst(cg, buf);
        cg_write_inst(cg, "op add __retstk__ __retstk__ 5");
        cg_write_inst(cg, "set @counter __push_ret__ret_");

        // pop funclet
        cg_write_inst(cg, "op sub __retstk__ __retstk__ 5");
        snprintf(buf, sizeof(buf), "set @counter __retstk%lu__", i);
        cg_write_inst(cg, buf);
    }
}
void cg_write_push_ret(struct CodeGen* cg)
{
    cg_write_inst(cg, "op add __push_ret__ret_ @counter 1");
    cg_write_inst(cg, "op add @counter __retstk__ 2");
}
void cg_write_return(struct CodeGen* cg) { cg_write_inst(cg, "set @counter __retstk__"); }
void cg_write_inst_set(struct CodeGen* cg, const char* dst, const char* src)
{
    char buf[64];
    snprintf(buf, sizeof(buf), "set %s %s", dst, src);
    cg_write_inst(cg, buf);
}

void cg_write_inst(struct CodeGen* cg, const char* inst)
{
    static const char s_nl = '\n';
    array_push(&cg->text, inst, strlen(inst));
    array_push(&cg->text, &s_nl, 1);
    ++cg->lines;
}
void cg_mark_label(struct CodeGen* cg, const char* sym)
{
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

void cg_emit(struct CodeGen* cg)
{
    const char* const text = (const char*)cg->text.data;
    size_t last_emit_point = 0;
    size_t i = 0;
    for (; i < cg->text.sz; ++i)
    {
        if (text[i] == '$')
        {
            if (fwrite(text + last_emit_point, 1, i - last_emit_point, stdout) != i - last_emit_point)
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
            fprintf(stdout, "%lu", label->line);
            last_emit_point = ++i;
        }
    }
    if (fwrite(text + last_emit_point, 1, i - last_emit_point, stdout) != i - last_emit_point)
    {
        perror("error: failed to write output");
        abort();
    }
}