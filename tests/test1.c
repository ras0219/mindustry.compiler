#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "be.h"
#include "cg.h"
#include "elaborator.h"
#include "errors.h"
#include "parse.h"
#include "preproc.h"
#include "stdlibe.h"
#include "symbol.h"
#include "tac.h"
#include "unittest.h"
#include "unwrap.h"

int test_preproc(struct TestState* state, struct Preprocessor** pp, const char* text)
{
    parser_clear_errors();
    *pp = preproc_alloc("");
    REQUIREZ(preproc_text(*pp, text));
    REQUIREZ(parser_has_errors());
    return 0;

fail:
    if (parser_has_errors())
    {
        parser_print_errors(stderr);
    }
    if (*pp)
    {
        preproc_free(*pp);
        *pp = NULL;
    }
    return 1;
}

int test_preproc_pass(struct TestState* state, const char* text)
{
    int rc = 1;
    struct Preprocessor* pp;
    SUBTEST(test_preproc(state, &pp, text));
    rc = 0;
fail:
    if (pp)
    {
        preproc_free(pp);
        pp = NULL;
    }
    return rc;
}

int test_parse(struct TestState* state, struct Parser** parser, struct Preprocessor** pp, const char* text)
{
    parser_clear_errors();
    *parser = NULL;
    *pp = preproc_alloc("");
    REQUIREZ(preproc_text(*pp, text));
    *parser = my_malloc(sizeof(struct Parser));
    parser_init(*parser);

    REQUIREZ(parser_parse(*parser, preproc_tokens(*pp), preproc_stringpool(*pp)));
    parser_debug_check(*parser);
    REQUIREZ(parser_has_errors());
    return 0;

fail:
    if (parser_has_errors()) parser_print_msgs(stderr), parser_clear_errors();
    if (*parser)
    {
        parser_destroy(*parser);
        my_free(*parser);
        *parser = NULL;
    }
    if (*pp)
    {
        preproc_free(*pp);
        *pp = NULL;
    }
    return 1;
}

int test_parse_fail(struct TestState* state, const char* text)
{
    parser_clear_errors();
    struct Preprocessor* const pp = preproc_alloc("");
    Parser* parser = NULL;
    REQUIREZ(preproc_text(pp, text));
    parser = my_malloc(sizeof(struct Parser));
    parser_init(parser);

    REQUIRE(parser_parse(parser, preproc_tokens(pp), preproc_stringpool(pp)));
    parser_debug_check(parser);
    REQUIRE(parser_has_errors());

fail:
    if (parser) parser_destroy(parser), my_free(parser);
    preproc_free(pp);
    return 1;
}

int test_elaborate_fail(struct TestState* state, const char* text)
{
    parser_clear_errors();
    struct Preprocessor* const pp = preproc_alloc("");
    Parser* parser = NULL;
    Elaborator* elab = NULL;
    REQUIREZ(preproc_text(pp, text));
    parser = my_malloc(sizeof(struct Parser));
    parser_init(parser);

    REQUIREZ(parser_parse(parser, preproc_tokens(pp), preproc_stringpool(pp)));
    elab = my_malloc(sizeof(Elaborator));
    elaborator_init(elab, parser);
    int e = elaborate(elab);
    parser_clear_errors();
    REQUIRE_IMPL(e, "elaborate(elab)");
    return 0;
fail:
    if (parser_has_errors()) parser_print_msgs(stderr), parser_clear_errors();
    if (elab) elaborator_destroy(elab), my_free(elab);
    if (parser) parser_destroy(parser), my_free(parser);
    preproc_free(pp);
    return 1;
}

typedef struct StandardTest
{
    struct Parser* parser;
    struct Preprocessor* pp;
    struct Elaborator* elab;
} StandardTest;

typedef struct BETest
{
    StandardTest base;
    struct BackEnd* be;
    struct CodeGen* cg;
} BETest;

static int stdtest_run(struct TestState* state, struct StandardTest* test, const char* text)
{
    test->elab = NULL;
    SUBTEST(test_parse(state, &test->parser, &test->pp, text));
    test->elab = my_malloc(sizeof(Elaborator));
    elaborator_init(test->elab, test->parser);
    REQUIREZ(elaborate(test->elab));

    return 0;
fail:
    return 1;
}

static void stdtest_destroy(struct StandardTest* test)
{
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (test->elab)
    {
        elaborator_destroy(test->elab);
        my_free(test->elab);
    }
    if (test->parser) parser_destroy(test->parser), my_free(test->parser);
    if (test->pp) preproc_free(test->pp);
}

static int betest_run(struct TestState* state, BETest* test, const char* text)
{
    test->be = NULL;
    test->cg = NULL;
    if (stdtest_run(state, &test->base, text)) return 1;
    test->be = my_malloc(sizeof(struct BackEnd));
    test->cg = my_malloc(sizeof(struct CodeGen));
    be_init(test->be, test->base.parser, test->base.elab, test->cg);
    cg_init(test->cg);

    struct Expr** exprs = test->base.parser->expr_seqs.data;
    REQUIRE(0 < test->base.parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test->base.parser->top->offset + test->base.parser->top->extent - 1])
    {
        REQUIRE(0 < decls->extent);
        REQUIRE_EXPR(Decl, decl, exprs[decls->offset + decls->extent - 1])
        {
            return be_compile_decl(test->be, decl);
            ;
        }
    }
    return 0;
fail:
    return 1;
}

static void betest_destroy(struct BETest* test)
{
    stdtest_destroy(&test->base);
    if (test->be) be_destroy(test->be), my_free(test->be);
    if (test->cg) cg_destroy(test->cg), my_free(test->cg);
}

int parse_main(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    SUBTEST(test_parse(state, &parser, &pp, "int main() {}"));

    REQUIRE_EQ(1, parser->top->extent);
    struct Expr** const exprs = parser->expr_seqs.data;
    struct StmtDecls* decls = (struct StmtDecls*)exprs[parser->top->offset];
    REQUIRE_EQ(STMT_DECLS, decls->kind);
    REQUIRE_EQ(1, decls->extent);
    struct Decl* main = (struct Decl*)((struct Expr**)parser->expr_seqs.data)[decls->offset];
    REQUIRE_EQ(AST_DECL, main->kind);
    REQUIRE_STR_EQ("main", main->sym->name);
    struct DeclFn* mainfn = (struct DeclFn*)main->type;
    REQUIRE_EQ(AST_DECLFN, mainfn->kind);
    struct DeclSpecs* mainrty = (struct DeclSpecs*)mainfn->type;
    REQUIRE_EQ(AST_DECLSPEC, mainrty->kind);
    REQUIRE_NULL(mainrty->name);
    REQUIRE_NULL(mainrty->sym);
    REQUIREZ(mainrty->is_long);
    REQUIREZ(mainrty->is_short);
    REQUIREZ(mainrty->is_enum);
    REQUIREZ(mainrty->is_const);
    REQUIREZ(mainrty->is_extern);
    REQUIREZ(mainrty->is_inline);
    REQUIREZ(mainrty->is_typedef);
    REQUIRE(mainrty->tok);
    REQUIRE_STR_EQ("int", token_str(parser, mainrty->tok));
    REQUIRE(main->init);
    REQUIRE_EQ(STMT_BLOCK, main->init->kind);
    struct StmtBlock* init = (void*)main->init;
    REQUIREZ(init->extent);

    rc = 0;
fail:
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_typedef(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    SUBTEST(test_parse(state, &parser, &pp, "typedef unsigned long size_t;\ntypedef unsigned long size_t;"));

    REQUIRE_EQ(2, parser->top->extent);
    struct Expr** const exprs = parser->expr_seqs.data;
    struct StmtDecls* decls = (struct StmtDecls*)exprs[parser->top->offset];
    REQUIRE_EQ(STMT_DECLS, decls->kind);
    REQUIRE_EQ(1, decls->extent);
    struct Decl* def = (struct Decl*)((struct Expr**)parser->expr_seqs.data)[decls->offset];
    REQUIRE_EQ(AST_DECL, def->kind);
    REQUIRE_STR_EQ("size_t", def->sym->name);
    REQUIRE(def->type);
    struct DeclSpecs* defspecs = (struct DeclSpecs*)def->type;
    REQUIRE_EQ(AST_DECLSPEC, defspecs->kind);
    REQUIRE_NULL(defspecs->name);
    REQUIRE_NULL(defspecs->sym);
    REQUIRE(defspecs->is_long);
    REQUIRE(defspecs->is_unsigned);
    REQUIRE(defspecs->is_typedef);
    REQUIREZ(defspecs->is_signed);
    REQUIREZ(defspecs->is_short);
    REQUIREZ(defspecs->is_enum);
    REQUIREZ(defspecs->is_const);
    REQUIREZ(defspecs->is_extern);
    REQUIREZ(defspecs->is_inline);
    REQUIRE(defspecs->tok);
    REQUIRE_NULL(def->init);

    rc = 0;
fail:
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_struct(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    SUBTEST(test_parse(state,
                       &parser,
                       &pp,
                       "typedef unsigned long size_t;\n"
                       "struct Array\n"
                       "{\n"
                       "  size_t sz;\n"
                       "  size_t cap;\n"
                       "  void* data;\n"
                       "};\n"));

    struct Expr** const exprs = parser->expr_seqs.data;
    REQUIRE_EQ(2, parser->top->extent);
    struct StmtDecls* decls = (struct StmtDecls*)exprs[parser->top->offset + 1];
    REQUIRE_EQ(STMT_DECLS, decls->kind);
    REQUIRE_EQ(0, decls->extent);
    REQUIRE(decls->specs);
    struct DeclSpecs* def = decls->specs;
    REQUIRE_STR_EQ("Array", def->name);
    REQUIRE(def->is_struct);
    REQUIRE(def->suinit);
    struct StmtBlock* blk = def->suinit;
    REQUIRE_EQ(3, blk->extent);
    // "size_t sz;"
    REQUIRE(exprs[blk->offset] && exprs[blk->offset]->kind == STMT_DECLS);
    struct StmtDecls* mem1 = (struct StmtDecls*)exprs[blk->offset];
    REQUIRE(mem1->extent == 1);
    REQUIRE(exprs[mem1->offset] && exprs[mem1->offset]->kind == AST_DECL);
    struct Decl* mem1def = (struct Decl*)exprs[mem1->offset];
    REQUIRE_STR_EQ("sz", mem1def->sym->name);
    REQUIRE(mem1def->type && mem1def->type->kind == AST_DECLSPEC);
    struct DeclSpecs* mem1specs = (struct DeclSpecs*)mem1def->type;
    REQUIRE_STR_EQ("size_t", mem1specs->name);
    REQUIRE(mem1specs->_typedef);

    rc = 0;
fail:
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int preproc_ternary(struct TestState* state)
{
    int rc = 1;
    SUBTEST(test_preproc_pass(state,
                              "#if 1 ? 0 : 1\n#error 'should not execute'\n#endif\n"
                              "#if 0 ? 1 : 0\n#error 'should not execute'\n#endif\n"
                              "#if 1 ? 1 ? 0 : 1 : 1\n#error 'should not execute'\n#endif\n"
                              "#if 1 ? 0 ? 1 : 0 : 1\n#error 'should not execute'\n#endif\n"
                              ""));

    rc = 0;
fail:
    return rc;
}

int parse_initializer(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    SUBTEST(test_parse(state,
                       &parser,
                       &pp,
                       "struct Array"
                       "{"
                       "  int sz;"
                       "  int cap;"
                       "  int data;"
                       "};"
                       "struct Array arr = { 1, 2, 3 };"
                       "struct Array arr2 = { .sz = 1 };"
                       "struct Array arr3 = { .cap = 1, 4 };"));

    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE_EQ(4, parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decl1, exprs[parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decl1->extent);
        REQUIRE_EXPR(Decl, def, exprs[decl1->offset])
        {
            REQUIRE_STR_EQ("arr", def->sym->name);
            REQUIRE_AST(AstInit, init, def->init)
            {
                REQUIRE(init->next);
                REQUIRE(init->next->next);
                REQUIRE(init->next->next->next);
                REQUIRE_NULL(init->next->next->next->next);
            }
        }
    }
    REQUIRE_EXPR(StmtDecls, decl1, exprs[parser->top->offset + 2])
    {
        REQUIRE_EQ(1, decl1->extent);
        REQUIRE_EXPR(Decl, def, exprs[decl1->offset])
        {
            REQUIRE_AST(AstInit, init, def->init)
            {
                REQUIRE(init->next);
                REQUIRE_NULL(init->next->init);
                REQUIRE_EQ(1, init->designator_extent);
                struct Designator* designators = parser->designators.data;
                REQUIRE_STR_EQ("sz", designators[init->designator_offset].field);
            }
        }
    }

    REQUIRE_EXPR(StmtDecls, decl1, exprs[parser->top->offset + 3])
    {
        REQUIRE_EQ(1, decl1->extent);
        REQUIRE_EXPR(Decl, def, exprs[decl1->offset])
        {
            REQUIRE_AST(AstInit, init, def->init)
            {
                REQUIRE(init->next);
                REQUIRE(init->next->next);
                REQUIRE_NULL(init->next->next->init);
                REQUIRE_EQ(1, init->designator_extent);
                struct Designator* designators = parser->designators.data;
                REQUIRE_STR_EQ("cap", designators[init->designator_offset].field);
                REQUIRE_EQ(0, init->next->designator_extent);
            }
        }
    }

    rc = 0;
fail:
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_nested_struct(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    struct Elaborator* elab = NULL;
    // from https://en.cppreference.com/w/c/language/struct_initialization
    SUBTEST(test_parse(state,
                       &parser,
                       &pp,
                       "struct example {\n"
                       "    struct addr_t {\n"
                       "       int port;\n"
                       "    } addr;\n"
                       "    union {\n"
                       "       char a8[4];\n"
                       "       short a16[2];\n"
                       "    } in_u;\n"
                       "};\n"));

    elab = my_malloc(sizeof(Elaborator));
    elaborator_init(elab, parser);
    REQUIREZ(elaborate(elab));

    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE(0 < parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset])
    {
        REQUIRE(decls->specs);
        REQUIRE(decls->specs->suinit);
        REQUIRE_EQ(2, decls->specs->suinit->extent);
        REQUIRE_EXPR(StmtDecls, addr_t_decls, exprs[decls->specs->suinit->offset])
        {
            REQUIRE_EQ(1, addr_t_decls->extent);
            REQUIRE_EXPR(Decl, addr_t_decl, exprs[addr_t_decls->offset])
            {
                REQUIRE_PTR_EQ(addr_t_decl->sym, decls->specs->sym->first_member);
                REQUIRE_EQ(4, addr_t_decl->sym->align);
                REQUIRE_EQ(4, addr_t_decl->sym->size);
            }
        }
        REQUIRE(decls->specs->sym->first_member->next_field);
        REQUIRE_EXPR(StmtDecls, in_u_decls, exprs[decls->specs->suinit->offset + 1])
        {
            REQUIRE_EQ(1, in_u_decls->extent);
            REQUIRE_EXPR(Decl, in_u_decl, exprs[in_u_decls->offset])
            {
                REQUIRE_PTR_EQ(in_u_decl->sym, decls->specs->sym->first_member->next_field);
                REQUIRE_EQ(2, in_u_decl->sym->align);
                REQUIRE_EQ(4, in_u_decl->sym->size);
            }
        }
        REQUIRE_NULL(decls->specs->sym->first_member->next_field->next_field);

        REQUIRE_EQ(4, decls->specs->sym->align);
        REQUIRE_EQ(8, decls->specs->sym->size);
    }

    rc = 0;
fail:
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (elab)
    {
        elaborator_destroy(elab);
        my_free(elab);
    }
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_initializer_struct(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    struct Elaborator* elab = NULL;
    // from https://en.cppreference.com/w/c/language/struct_initialization
    SUBTEST(test_parse(state,
                       &parser,
                       &pp,
                       "struct a_t {\n"
                       "    int a;\n"
                       "    short b;\n"
                       "    int c;\n"
                       "};\n"
                       "struct a_t a = { 1, 2, 3 };\n"));

    elab = my_malloc(sizeof(Elaborator));
    elaborator_init(elab, parser);
    REQUIREZ(elaborate(elab));

    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE_EQ(2, parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, a, exprs[decls->offset])
        {
            REQUIRE_AST(AstInit, a_init, a->init)
            {
                REQUIRE_EQ(-4, a_init->sizing);
                REQUIRE_EQ(0, a_init->offset);
                REQUIRE(a_init->next);
                REQUIRE_EQ(-2, a_init->next->sizing);
                REQUIRE_EQ(4, a_init->next->offset);
                REQUIRE(a_init->next->next);
                REQUIRE_EQ(-4, a_init->next->next->sizing);
                REQUIRE_EQ(8, a_init->next->next->offset);
            }
        }
    }

    rc = 0;
fail:
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (elab)
    {
        elaborator_destroy(elab);
        my_free(elab);
    }
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_initializer_union(struct TestState* state)
{
    int rc = 1;

    struct Parser* parser = NULL;
    struct Preprocessor* pp = NULL;
    struct Elaborator* elab = NULL;

    SUBTEST(test_elaborate_fail(state,
                                "union a_t {\n"
                                "    int a;\n"
                                "    short b;\n"
                                "    int c;\n"
                                "};\n"
                                "union a_t a = { 1, 2 };\n"));

    // from https://en.cppreference.com/w/c/language/struct_initialization
    SUBTEST(test_parse(state,
                       &parser,
                       &pp,
                       "union a_t {\n"
                       "    int a;\n"
                       "    short b;\n"
                       "    int c;\n"
                       "};\n"
                       "union a_t a = { 1 };\n"));

    elab = my_malloc(sizeof(Elaborator));
    elaborator_init(elab, parser);
    REQUIREZ(elaborate(elab));

    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE_EQ(2, parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, a, exprs[decls->offset])
        {
            REQUIRE_AST(AstInit, a_init, a->init)
            {
                REQUIRE_EQ(-4, a_init->sizing);
                REQUIRE_EQ(0, a_init->offset);
            }
        }
    }

    rc = 0;
fail:
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (elab)
    {
        elaborator_destroy(elab);
        my_free(elab);
    }
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_initializer_array(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser = NULL;
    struct Preprocessor* pp = NULL;
    struct Elaborator* elab = NULL;
    // from https://en.cppreference.com/w/c/language/array_initialization
    SUBTEST(test_elaborate_fail(state, "int a[3] = { 1, 2, 3, 4 };\n"));
    SUBTEST(test_parse(state, &parser, &pp, "int a[3] = { 1, 2, 3 };\n"));

    elab = my_malloc(sizeof(Elaborator));
    elaborator_init(elab, parser);
    REQUIREZ(elaborate(elab));

    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE(0 < parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, a, exprs[decls->offset])
        {
            REQUIRE_AST(AstInit, a_init, a->init)
            {
                REQUIRE_EQ(-4, a_init->sizing);
                REQUIRE_EQ(0, a_init->offset);
                REQUIRE(a_init->next);
                REQUIRE_EQ(-4, a_init->next->sizing);
                REQUIRE_EQ(4, a_init->next->offset);
                REQUIRE(a_init->next->next);
                REQUIRE_EQ(-4, a_init->next->next->sizing);
                REQUIRE_EQ(8, a_init->next->next->offset);
            }
        }
    }

    rc = 0;
fail:
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (elab)
    {
        elaborator_destroy(elab);
        my_free(elab);
    }
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_initializer2b(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    struct Elaborator* elab = NULL;
    // from https://en.cppreference.com/w/c/language/struct_initialization
    SUBTEST(test_parse(state,
                       &parser,
                       &pp,
                       "struct example {\n"
                       "    struct addr_t {\n"
                       "       int port;\n"
                       "    } addr;\n"
                       "    union {\n"
                       "       char a8[4];\n"
                       "       short a16[2];\n"
                       "    } in_u;\n"
                       "};\n"
                       "struct addr_t ex0 = { 80 };\n"
                       "struct example ex1 = {\n"
                       "    { 80 },\n"
                       "    { {127,0,0,1} },\n"
                       "};\n"
                       "struct example ex2 = {80, 127, 0, 0, 1};"));

    elab = my_malloc(sizeof(Elaborator));
    elaborator_init(elab, parser);
    REQUIREZ(elaborate(elab));

    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE_EQ(4, parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, a, exprs[decls->offset])
        {
            REQUIRE_AST(AstInit, a_init, a->init)
            {
                REQUIRE_EQ(-4, a_init->sizing);
                REQUIRE_EQ(0, a_init->offset);
            }
        }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset + 2])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, a, exprs[decls->offset])
        {
            REQUIRE_AST(AstInit, a_init, a->init)
            {
                REQUIRE_AST(AstInit, b_init, a_init->init)
                {
                    REQUIRE_EQ(-4, b_init->sizing);
                    REQUIRE_EQ(0, b_init->offset);
                }
                REQUIRE(a_init->next);
                REQUIRE_AST(AstInit, b_init, a_init->next->init)
                {
                    REQUIRE_AST(AstInit, c_init, b_init->init)
                    {
                        REQUIRE_EQ(-1, c_init->sizing);
                        REQUIRE_EQ(4, c_init->offset);
                        REQUIRE(c_init->next);
                        REQUIRE_EQ(-1, c_init->next->sizing);
                        REQUIRE_EQ(5, c_init->next->offset);
                    }
                }
            }
        }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset + 3])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, a, exprs[decls->offset])
        {
            REQUIRE_AST(AstInit, a_init, a->init)
            {
                REQUIRE_EQ(-4, a_init->sizing);
                REQUIRE_EQ(0, a_init->offset);
                REQUIRE(a_init->next);
                REQUIRE_EQ(-1, a_init->next->sizing);
                REQUIRE_EQ(4, a_init->next->offset);
                REQUIRE(a_init->next->next);
                REQUIRE_EQ(-1, a_init->next->next->sizing);
                REQUIRE_EQ(5, a_init->next->next->offset);
            }
        }
    }

    rc = 0;
fail:
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (elab)
    {
        elaborator_destroy(elab);
        my_free(elab);
    }
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_initializer_designated(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    struct Elaborator* elab = NULL;
    // from https://en.cppreference.com/w/c/language/initialization
    SUBTEST(test_parse(state,
                       &parser,
                       &pp,
                       "struct {int a[3], b;} w[] = {\n"
                       "    [0].a = {1},\n"
                       "    [1].a[1] = 2,\n"
                       "    [1].b = 7\n"
                       "};"));

    elab = my_malloc(sizeof(Elaborator));
    elaborator_init(elab, parser);
    REQUIREZ(elaborate(elab));

    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE_EQ(1, parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            REQUIRE_AST(AstInit, w_init, w->init)
            {
                REQUIRE_AST(AstInit, init2, w_init->init)
                {
                    REQUIRE_EQ(-4, init2->sizing);
                    REQUIRE_EQ(0, init2->offset);
                }
                REQUIRE(w_init->next);
                REQUIRE_EQ(-4, w_init->next->sizing);
                REQUIRE_EQ(20, w_init->next->offset);
                REQUIRE(w_init->next->next);
                REQUIRE_EQ(-4, w_init->next->next->sizing);
                REQUIRE_EQ(28, w_init->next->next->offset);
            }
            REQUIRE_EQ(32, w->sym->size);
        }
    }

    rc = 0;
fail:
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (elab)
    {
        elaborator_destroy(elab);
        my_free(elab);
    }
    if (parser) parser_destroy(parser), my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_initializer_expr_designated(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "struct A {int x;};\n"
                        "struct A foo();\n"
                        "struct B {struct A a, b;};\n"
                        "void bar()\n"
                        "{ struct B a = { foo() }; }\n"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_initializer_expr_sue(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "struct A {int x;};\n"
                        "struct A foo();\n"
                        "void bar()\n"
                        "{ struct A a = foo(); }\n"));
    SUBTEST(stdtest_run(state,
                        &test,
                        "union A {int x;};\n"
                        "union A foo();\n"
                        "void bar()\n"
                        "{ union A a = foo(); }\n"));
    SUBTEST(stdtest_run(state,
                        &test,
                        "enum A {first};\n"
                        "enum A foo();\n"
                        "void bar()\n"
                        "{ enum A a = foo(); }\n"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_unk_array(struct TestState* state)
{
    int rc = 1;
    struct StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "char txt6[] = \"hello\";\n"
                        "struct Point { int x, y; } points5[] = {[3] = 1,2,3};\n"));
    Parser* const parser = test.parser;

    // from https://en.cppreference.com/w/c/language/initialization
    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE_EQ(2, parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset]) { REQUIRE_EQ(6, w->sym->size); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset]) { REQUIRE_EQ(5 * 8, w->sym->size); }
    }

    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_anon_decls(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "struct __darwin_pthread_handler_rec {\n"
                        "void (*__routine)(void*);\n"
                        "};\n"
                        "struct anon_pad {\n"
                        "int x : 1, : 2, y : 3;\n"
                        "};"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_decls_and_defs(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "extern int i;\n"
                        "int i;\n"
                        "void foo();\n"
                        "void foo() {}\n"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_uuva_list(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "void array_appendf(struct Array* arr, const char* fmt, ...)\n"
                        "{\n"
                        "__builtin_va_list argp;\n"
                        "__builtin_va_start(argp, fmt);\n"
                        "__builtin_va_list args2;\n"
                        "__builtin_va_copy(args2, argp);\n"
                        "__builtin_va_end(argp);\n"
                        "}\n"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_shadow(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "int i;\n"
                        "int main() {\n"
                        "int i;\n"
                        "{ int i; }\n"
                        "}\n"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_body(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "int i;\n"
                        "int main() {\n"
                        "int i;\n"
                        "{ int i; }\n"
                        "}\n"
                        "typedef unsigned long size_t;\n"
                        "size_t array_size(const struct Array* arr, size_t sz);\n"
                        "static size_t findstr(const char* str, const char* const* heap, size_t heap_size);\n"
                        "static __forceinline long tt_find_insert_null(struct TypeTable* tt, const char* str)\n"
                        "{\n"
                        "    const size_t n = array_size((void*)0, sizeof(const char*));\n"
                        "    size_t offset = findstr(str, (const char* const*)0, n);\n"
                        "    return offset;\n"
                        "}\n"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_fn_ptr_conversion(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "int foo();\n"
                        "int main() {\n"
                        "int (*i)() = foo;\n"
                        "int (*j)() = &foo;\n"
                        "}\n"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_expr1(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "int foo();\n"
                        "struct Arr { int y; };\n"
                        "const struct Arr arr[] = {1,2,3};\n"
                        "extern const struct Arr arr[];\n"
                        "struct Foo { int x, int y; };\n"
                        "int main() {\n"
                        "((Foo*)0)->y;\n"
                        "struct Arr* p = arr, q=&arr;\n"
                        "p=arr;\n"
                        "p=&arr;\n"
                        "sizeof(arr);\n"
                        "}\n"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_enums(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "enum A { a1 = 5, a2, a3 };\n"
                        "typedef struct { enum A a; } W;"
                        "int main() {\n"
                        "enum A x = a3;\n"
                        "int y = (x == a2);\n"
                        "if ((unsigned int)y == a1)\n"
                        "  sizeof(enum A);\n"
                        "W w, *pw = &w;\n"
                        "pw->a = a1;\n"
                        "}\n"));
    rc = 0;

    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(3, test.parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset])
    {
        REQUIRE_EQ(0, decls->extent);
        REQUIRE(decls->specs->enum_init);
        REQUIRE_EQ(3, decls->specs->enum_init->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->specs->enum_init->offset])
        {
            REQUIRE_EQ(5, w->sym->enum_value);
            REQUIRE_PTR_EQ(w, w->sym->def);
            REQUIRE_EQ(4, w->sym->size);
        }
    }

fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_aggregates(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "struct A {\n"
                        "struct {\n"
                        "long l; char c;\n"
                        "};\n"
                        "};"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_ptrconvert(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "typedef int Int;"
                        "int bar(int* i);\n"
                        "int foo(const Int* i);\n"
                        "int main() {\n"
                        "int *x = (void*)0;\n"
                        "foo(x);\n"
                        "bar(x);\n"
                        "const int *y = (void*)0;\n"
                        "foo(y);\n"
                        "}\n"));
    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

#define REQUIRE_ENUM(expected, actual, stringify)                                                                      \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        int _expr_a = (expected);                                                                                      \
        int _expr_b = (actual);                                                                                        \
        if (_expr_a != _expr_b)                                                                                        \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            fprintf(stderr,                                                                                            \
                    "%s:%d: error: '%s == %s' was '%s == %s'\n",                                                       \
                    __FILE__,                                                                                          \
                    __LINE__,                                                                                          \
                    STRINGIFY(expected),                                                                               \
                    STRINGIFY(actual),                                                                                 \
                    (stringify)(_expr_a),                                                                              \
                    (stringify)(_expr_b));                                                                             \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define REQUIRE_TACO(expected, actual) REQUIRE_ENUM(expected, actual, taco_to_string)
#define REQUIRE_TACA(expected, actual) REQUIRE_ENUM(expected, actual, taca_to_string)

void array_append_taca(Array* arr, struct TACAddress* addr)
{
    array_push_byte(arr, '{');
    array_appends(arr, taca_to_string(addr->kind));
    if (addr->is_addr) array_appends(arr, ", .is_addr = 1");
    if (addr->sizing != 0) array_appendf(arr, ", .sizing = %d", addr->sizing);
    switch (addr->kind)
    {
        case TACA_FRAME: array_appendf(arr, ", .frame_offset = %d}", addr->frame_offset); break;
        case TACA_IMM: array_appendf(arr, ", .imm = %zu}", addr->imm); break;
        case TACA_ARG: array_appendf(arr, ", .arg_idx = %zu}", addr->arg_idx); break;
        case TACA_REG: array_appendf(arr, ", .reg = %s}", register_to_string(addr->reg)); break;
        case TACA_VOID: array_push_byte(arr, '}'); break;
        default: array_appendf(arr, ", unimplemented = %p}", addr); break;
    }
}

int require_taca(TestState* state, struct TACAddress* expected, struct TACAddress* actual)
{
    struct Array buf_expected = {0};
    struct Array buf_actual = {0};
    array_append_taca(&buf_expected, expected);
    array_push_byte(&buf_expected, 0);
    array_append_taca(&buf_actual, actual);
    array_push_byte(&buf_actual, 0);
    REQUIRE_STR_EQ(buf_expected.data, buf_actual.data);
    return 0;
fail:
    return 1;
}

int require_tace(TestState* state, struct TACEntry* expected, struct TACEntry* actual)
{
    int rc = 0;
    REQUIRE_TACO(expected->op, actual->op);
    INFO("arg1\n", 1) { rc |= require_taca(state, &expected->arg1, &actual->arg1); }
    INFO("arg2\n", 2) { rc |= require_taca(state, &expected->arg2, &actual->arg2); }
    return 0;
fail:
    return 1;
}

#define REQUIRE_TACES()                                                                                                \
    do                                                                                                                 \
    {                                                                                                                  \
        struct TACEntry* data = test.be->code.data;                                                                    \
        const size_t actual_sz = array_size(&test.be->code, sizeof(struct TACEntry));                                  \
        const size_t expected_sz = sizeof(expected) / sizeof(expected[0]);                                             \
        size_t sz = actual_sz;                                                                                         \
        if (sz > expected_sz) sz = expected_sz;                                                                        \
        for (size_t i = 0; i < sz; ++i)                                                                                \
        {                                                                                                              \
            INFO("index %zu\n", i) { SUBTEST(require_tace(state, expected + i, data + i)); }                           \
        }                                                                                                              \
        REQUIRE_EQ(expected_sz, actual_sz);                                                                            \
    } while (0)

int test_be_simple(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state, &test, "int main() { return 42; }"));

    struct TACEntry expected[] = {
        {.op = TACO_ASSIGN,
         .arg1 = {TACA_REG, .sizing = -4, .reg = REG_RAX},
         .arg2 = {TACA_IMM, .sizing = -4, .imm = 42}},
        {.op = TACO_RETURN},
    };

    REQUIRE_TACES();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_simple2(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state, &test, "int main(int argc, char** argv) { return argc; }"));

    struct TACEntry expected[] = {
        {.op = TACO_ASSIGN,
         .arg1 = {TACA_FRAME, .sizing = 4, .frame_offset = 0},
         .arg2 = {TACA_REG, .sizing = 4, .reg = REG_RDI}},
        {.op = TACO_ASSIGN,
         .arg1 = {TACA_FRAME, .sizing = 8, .frame_offset = 8},
         .arg2 = {TACA_REG, .sizing = 8, .reg = REG_RSI}},
        {.op = TACO_ASSIGN,
         .arg1 = {TACA_REG, .sizing = -4, .reg = REG_RAX},
         .arg2 = {TACA_FRAME, .sizing = -4, .frame_offset = 0}},
        {.op = TACO_RETURN},
    };
    REQUIRE_TACES();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_memory_ret(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "struct A { char buf[32]; };\n"
                       "struct A main(int n, void* v, struct A m) { return m; }"));

    struct TACEntry expected[] = {
        {.op = TACO_ASSIGN,
         .arg1 = {TACA_FRAME, .sizing = 8, .frame_offset = 0},
         .arg2 = {TACA_REG, .sizing = 8, .reg = REG_RDI}},
        {.op = TACO_ASSIGN,
         .arg1 = {TACA_FRAME, .sizing = 4, .frame_offset = 8},
         .arg2 = {TACA_REG, .sizing = 4, .reg = REG_RSI}},
        {.op = TACO_ASSIGN,
         .arg1 = {TACA_FRAME, .sizing = 8, .frame_offset = 16},
         .arg2 = {TACA_REG, .sizing = 8, .reg = REG_RDX}},
        {.op = TACO_ASSIGN,
         .arg1 = {TACA_FRAME, .is_addr = 1, .sizing = 8, .frame_offset = 0},
         .arg2 = {TACA_FRAME, .sizing = 32, .frame_offset = -40}},
        {.op = TACO_ASSIGN,
         .arg1 = {TACA_REG, .sizing = 8, .reg = REG_RAX},
         .arg2 = {TACA_FRAME, .sizing = 8, .frame_offset = 0}},
        {.op = TACO_RETURN},
    };
    REQUIRE_TACES();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int main()
{
    struct TestState _state = {.colorsuc = "", .colorerr = "", .colorreset = ""};
    struct TestState* state = &_state;

    const char* const clicolorforce = getenv("CLICOLOR_FORCE");
    const char* const clicolor = getenv("CLICOLOR");

    if (clicolorforce && 0 != strcmp(clicolorforce, "0") || !clicolor || 0 != strcmp(clicolor, "0"))
    {
        _state.colorsuc = "\033[32;1m";
        _state.colorerr = "\033[31;1m";
        _state.colorreset = "\033[m";
    }

    RUN_TEST(preproc_ternary);
    RUN_TEST(parse_main);
    RUN_TEST(parse_body);
    RUN_TEST(parse_typedef);
    RUN_TEST(parse_struct);
    RUN_TEST(parse_initializer);
    RUN_TEST(parse_nested_struct);
    RUN_TEST(parse_initializer_struct);
    RUN_TEST(parse_initializer_union);
    RUN_TEST(parse_initializer_array);
    RUN_TEST(parse_initializer2b);
    RUN_TEST(parse_initializer_designated);
    RUN_TEST(parse_unk_array);
    RUN_TEST(parse_anon_decls);
    RUN_TEST(parse_decls_and_defs);
    RUN_TEST(parse_uuva_list);
    RUN_TEST(parse_shadow);
    RUN_TEST(parse_initializer_expr_sue);
    RUN_TEST(parse_initializer_expr_designated);
    RUN_TEST(parse_fn_ptr_conversion);
    RUN_TEST(parse_enums);
    RUN_TEST(parse_aggregates);
    RUN_TEST(parse_ptrconvert);
    RUN_TEST(test_be_simple);
    RUN_TEST(test_be_simple2);
    RUN_TEST(test_be_memory_ret);

    const char* color = (state->testfails + state->assertionfails == 0) ? _state.colorsuc : _state.colorerr;

    printf("%s%d tests. %d failed. %d assertions. %d failed.%s\n",
           color,
           state->tests,
           state->testfails,
           state->assertions,
           state->assertionfails,
           _state.colorreset);

    return state->testfails > 0 || state->assertionfails > 0;
}
