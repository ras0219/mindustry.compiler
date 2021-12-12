#pragma once

#include <stddef.h>
#include <stdio.h>

#include "array.h"
#include "fwd.h"

struct CodeGen
{
    FILE* fdebug;

    struct Array const_;
    struct Array code;
};

void cg_init(struct CodeGen* cg);
void cg_destroy(struct CodeGen* cg);
void cg_declare_extern(struct CodeGen* cg, const char* sym);
void cg_declare_public(struct CodeGen* cg, const char* sym);
void cg_mark_label(struct CodeGen* cg, const char* sym);
void cg_string_constant(struct CodeGen* cg, size_t cidx, const char* str);
int cg_gen_taces(struct CodeGen* cg, struct TACEntry* taces, size_t n_taces);
int cg_emit(struct CodeGen* cg, FILE* fout);

#if 0
void cg_write_push_ret(struct CodeGen* cg, struct FreeVar* ret_addr);
void cg_write_return(struct CodeGen* cg, struct FreeVar* ret_addr);
void cg_write_prepare_stack(struct CodeGen* cg);
void cg_write_epilog(struct CodeGen* cg);
void cg_write_inst_jump(struct CodeGen* cg, const char* dst);
void cg_write_inst_jump_op(struct CodeGen* cg, const char* tgt, const char* op, const char* a, const char* b);
void cg_write_inst_set(struct CodeGen* cg, const char* dst, const char* src);
void cg_write_inst_op(struct CodeGen* cg, const char* op, const char* dst, const char* a, const char* b);
void cg_write_inst_add(struct CodeGen* cg, const char* dst, const char* a, int n);
void cg_write_inst(struct CodeGen* cg, const char* inst);
int cg_set_memory_bank(struct CodeGen* cg, const struct RowCol* rc, const char* mem);
int cg_read_mem(struct CodeGen* cg, const char* addr, const char* reg, const struct RowCol* rc);
int cg_write_mem(struct CodeGen* cg, const char* addr, const char* val, const struct RowCol* rc);
int cg_store(struct CodeGen* cg, int offset, const char* val, const struct RowCol* rc);
int cg_load(struct CodeGen* cg, int offset, const char* dst, const struct RowCol* rc);
#endif
