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
#include "token.h"
#include "unittest.h"
#include "unwrap.h"

#define REQUIRE_SIZING_EQ(expected, actual)                                                                            \
    do                                                                                                                 \
    {                                                                                                                  \
        const Sizing a = (actual);                                                                                     \
        const Sizing e = (expected);                                                                                   \
        REQUIRE_EQ_IMPL(__FILE__, __LINE__, e.width, #expected ".width", a.width, #actual ".width");                   \
        REQUIRE_EQ_IMPL(__FILE__, __LINE__, e.is_signed, #expected ".is_signed", a.is_signed, #actual ".is_signed");   \
    } while (0)

static const Sizing s_sizing_int = {.width = 4, .is_signed = 1};
static const Sizing s_sizing_uint = {.width = 4};
static const Sizing s_sizing_schar = {.width = 1, .is_signed = 1};
static const Sizing s_sizing_uchar = {.width = 1};
static const Sizing s_sizing_ptr = {.width = 8};
static const Sizing s_sizing_sptr = {.width = 8, .is_signed = 1};

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
    if (elaborate(test->elab))
    {
        if (parser_has_errors()) parser_print_msgs(stderr), parser_clear_errors();
        REQUIRE_FAIL("%s", "elaborate failed, see above messages");
    }

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
    for (size_t i = 0; i < test->base.parser->top->extent; ++i)
    {
        StmtDecls* decls = (StmtDecls*)exprs[test->base.parser->top->offset + i];
        if (decls->specs->is_typedef) continue;
        for (size_t j = 0; j < decls->extent; ++j)
        {
            Decl* decl = (Decl*)exprs[decls->offset + j];
            REQUIRE_EQ(0, be_compile_toplevel_decl(test->be, decl));
        }
    }
    return 0;
fail:
    return 1;
}

static int betest_run_cg(struct TestState* state, BETest* test, const char* text)
{
    test->be = NULL;
    test->cg = NULL;
    if (stdtest_run(state, &test->base, text)) return 1;
    test->be = my_malloc(sizeof(struct BackEnd));
    test->cg = my_malloc(sizeof(struct CodeGen));
    be_init(test->be, test->base.parser, test->base.elab, test->cg);
    cg_init(test->cg);

    REQUIRE_EQ(0, be_compile(test->be));
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

int parse_strings(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "#define f(x) #x\n"
                        "char str[] = f(==) \"hello\\n\";\n"));

    rc = 0;

    // from https://en.cppreference.com/w/c/language/initialization
    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(1, test.parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            REQUIRE_EQ(9, w->sym->size.width);
            REQUIRE_AST(ExprLit, e, w->init) { REQUIRE_MEM_EQ("==hello\n", 9, e->text, e->tok->tok_len + 1); }
        }
    }

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
                REQUIRE_EQ(4, addr_t_decl->sym->size.width);
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
                REQUIRE_EQ(4, in_u_decl->sym->size.width);
            }
        }
        REQUIRE_NULL(decls->specs->sym->first_member->next_field->next_field);

        REQUIRE_EQ(4, decls->specs->sym->align);
        REQUIRE_EQ(8, decls->specs->sym->size.width);
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
                REQUIRE_SIZING_EQ(s_sizing_int, a_init->sizing);
                REQUIRE_EQ(0, a_init->offset);
                REQUIRE(a_init->next);
                REQUIRE_EQ(2, a_init->next->sizing.width);
                REQUIRE_EQ(1, a_init->next->sizing.is_signed);
                REQUIRE_EQ(4, a_init->next->offset);
                REQUIRE(a_init->next->next);
                REQUIRE_SIZING_EQ(s_sizing_int, a_init->next->next->sizing);
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
                REQUIRE_EQ(4, a_init->sizing.width);
                REQUIRE_EQ(1, a_init->sizing.is_signed);
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
                REQUIRE_EQ(4, a_init->sizing.width);
                REQUIRE_EQ(1, a_init->sizing.is_signed);
                REQUIRE_EQ(0, a_init->offset);
                REQUIRE(a_init->next);
                REQUIRE_EQ(4, a_init->next->sizing.width);
                REQUIRE_EQ(1, a_init->next->sizing.is_signed);
                REQUIRE_EQ(4, a_init->next->offset);
                REQUIRE(a_init->next->next);
                REQUIRE_EQ(4, a_init->next->next->sizing.width);
                REQUIRE_EQ(1, a_init->next->next->sizing.is_signed);
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
                REQUIRE_SIZING_EQ(s_sizing_int, a_init->sizing);
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
                REQUIRE_SIZING_EQ(s_sizing_uint, a_init->sizing);
                REQUIRE_AST(AstInit, b_init, a_init->init)
                {
                    REQUIRE_SIZING_EQ(s_sizing_int, b_init->sizing);
                    REQUIRE_EQ(0, b_init->offset);
                }
                REQUIRE(a_init->next);
                REQUIRE_SIZING_EQ(s_sizing_uint, a_init->next->sizing);
                REQUIRE_EQ(0, a_init->offset);
                REQUIRE_AST(AstInit, b_init, a_init->next->init)
                {
                    REQUIRE_SIZING_EQ(s_sizing_uint, b_init->sizing);
                    REQUIRE_EQ(4, b_init->offset);
                    REQUIRE_AST(AstInit, c_init, b_init->init)
                    {
                        REQUIRE_SIZING_EQ(s_sizing_schar, c_init->sizing);
                        REQUIRE_EQ(4, c_init->offset);
                        REQUIRE(c_init->next);
                        REQUIRE_SIZING_EQ(s_sizing_schar, c_init->next->sizing);
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
                REQUIRE_SIZING_EQ(s_sizing_int, a_init->sizing);
                REQUIRE_EQ(0, a_init->offset);
                REQUIRE(a_init->next);
                REQUIRE_SIZING_EQ(s_sizing_schar, a_init->next->sizing);
                REQUIRE_EQ(4, a_init->next->offset);
                REQUIRE(a_init->next->next);
                REQUIRE_SIZING_EQ(s_sizing_schar, a_init->next->next->sizing);
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
                    REQUIRE_SIZING_EQ(s_sizing_int, init2->sizing);
                    REQUIRE_EQ(0, init2->offset);
                }
                REQUIRE(w_init->next);
                REQUIRE_SIZING_EQ(s_sizing_int, w_init->next->sizing);
                REQUIRE_EQ(20, w_init->next->offset);
                REQUIRE(w_init->next->next);
                REQUIRE_SIZING_EQ(s_sizing_int, w_init->next->next->sizing);
                REQUIRE_EQ(28, w_init->next->next->offset);
            }
            REQUIRE_EQ(32, w->sym->size.width);
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
        REQUIRE_EXPR(Decl, w, exprs[decls->offset]) { REQUIRE_EQ(6, w->sym->size.width); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            REQUIRE_EQ(5 * 8, w->sym->size.width);
            REQUIRE_AST(AstInit, i, w->init)
            {
                REQUIRE_EQ(24, i->offset);
                REQUIRE(i->next);
                REQUIRE_EQ(28, i->next->offset);
                REQUIRE(i->next->next);
                REQUIRE_EQ(32, i->next->next->offset);
            }
        }
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
                        "typedef __builtin_va_list __gnu_va_list;\n"
                        "typedef __gnu_va_list va_list;\n"
                        "void array_appendf(struct Array* arr, const char* fmt, ...)\n"
                        "{\n"
                        "__builtin_va_list argp;\n"
                        "va_list args2;\n"
                        "__builtin_va_start(argp, fmt);\n"
                        "__builtin_va_copy(args2, argp);\n"
                        "__builtin_va_end(argp);\n"
                        "}\n"));
    rc = 0;

    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(3, test.parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 2])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            REQUIRE_AST(StmtBlock, body, w->init)
            {
                REQUIRE_EQ(5, body->extent);
                REQUIRE_EXPR(StmtDecls, decls, exprs[body->offset])
                {
                    REQUIRE_EQ(1, decls->extent);
                    REQUIRE_EXPR(Decl, v, exprs[decls->offset])
                    {
                        REQUIRE_EQ(24, v->sym->size.width);
                        REQUIRE_EQ('_', v->sym->type.buf[1]);
                    }
                }
                REQUIRE_EXPR(StmtDecls, decls, exprs[body->offset + 1])
                {
                    REQUIRE_EQ(1, decls->extent);
                    REQUIRE_EXPR(Decl, v, exprs[decls->offset])
                    {
                        REQUIRE_EQ(24, v->sym->size.width);
                        REQUIRE_EQ('_', v->sym->type.buf[1]);
                    }
                }
            }
        }
    }

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

int parse_sizeof(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "char d1[sizeof(int)];\n"
                        "char d2[sizeof(d1)];\n"
                        "char d3[] = \"hello.\";\n"
                        "char d4[sizeof(d3)];\n"
                        "void main() { char d3[256]; int x = sizeof(d3); }"));
    rc = 0;

    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(5, test.parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, d, exprs[decls->offset]) { REQUIRE_SIZING_EQ(s_sizing_uint, d->sym->size); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, d, exprs[decls->offset]) { REQUIRE_SIZING_EQ(s_sizing_uint, d->sym->size); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 2])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, d, exprs[decls->offset]) { REQUIRE_EQ(7, d->sym->size.width); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 3])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, d, exprs[decls->offset]) { REQUIRE_EQ(7, d->sym->size.width); }
    }

fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_constants(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "char d1['\\\\'];"
                        "void f() {"
                        "1;"
                        "0x7fffffff;"
                        "0x80000000;"
                        "0x7fffffffLL;"
                        "0x7fffffffLLU;\n"
                        "'\\n';"
                        "'\\t';"
                        "'\\b';"
                        "'\\r';"
                        "'\\0';"
                        "'\\x20';"
                        "}\n"));
    rc = 0;

    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(2, test.parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, d, exprs[decls->offset]) { REQUIRE_EQ('\\', d->sym->size.width); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, d, exprs[decls->offset])
        {
            REQUIRE_AST(StmtBlock, b, d->init)
            {
                REQUIRE_EQ(11, b->extent);
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset])
                {
                    REQUIRE_EQ(1, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE_DECIMAL, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 1])
                {
                    REQUIRE_EQ(0x7fffffff, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 2])
                {
                    REQUIRE_EQ(0x80000000, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_uint, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 3])
                {
                    REQUIRE_EQ(0x7fffffff, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_LL, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_sptr, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 4])
                {
                    REQUIRE_EQ(0x7fffffff, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_LLU, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_ptr, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 5])
                {
                    REQUIRE_EQ('\n', e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 6])
                {
                    REQUIRE_EQ('\t', e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 7])
                {
                    REQUIRE_EQ('\b', e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 8])
                {
                    REQUIRE_EQ('\r', e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 9])
                {
                    REQUIRE_EQ(0, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->offset + 10])
                {
                    REQUIRE_EQ(32, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
            }
        }
    }

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

int parse_implicit_conversion(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "int main(char c, unsigned char hu, unsigned int u, long long ll, unsigned long long ull) {\n"
                        "int*x = 0;\n"
                        "x = 1-1;\n"

                        "c + hu;\n"
                        "ll + 10;\n"
                        "10 + ll;\n"
                        "c + hu;\n"
                        "}\n"));
    rc = 0;

    struct Ast** const asts = test.parser->expr_seqs.data;
    REQUIRE_EQ(1, test.parser->top->extent);
    REQUIRE_AST(StmtDecls, decls, asts[test.parser->top->offset])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_AST(Decl, w, asts[decls->offset])
        {
            REQUIRE_AST(StmtBlock, body, w->init)
            {
                REQUIRE_EQ(6, body->extent);

                REQUIRE_AST(ExprBinOp, e, asts[body->offset + 2])
                REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);

                REQUIRE_AST(ExprBinOp, e, asts[body->offset + 3])
                REQUIRE_SIZING_EQ(s_sizing_sptr, e->sizing);

                REQUIRE_AST(ExprBinOp, e, asts[body->offset + 4])
                REQUIRE_SIZING_EQ(s_sizing_sptr, e->sizing);
            }
        }
    }
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
            REQUIRE_SIZING_EQ(s_sizing_int, w->sym->size);
        }
    }

fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_typedefs(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "typedef int X;\n"
                        "typedef X Y;"
                        "int main() {\n"
                        "Y y = 10;\n"
                        "}\n"));
    rc = 0;

    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(3, test.parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset]) { REQUIRE_EQ(1, w->sym->type.buf[0]); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset]) { REQUIRE_EQ(1, w->sym->type.buf[0]); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 2])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            REQUIRE_AST(StmtBlock, body, w->init)
            {
                REQUIRE_EQ(1, body->extent);
                REQUIRE_EXPR(StmtDecls, decls, exprs[body->offset])
                {
                    REQUIRE_EQ(1, decls->extent);
                    REQUIRE_EXPR(Decl, w, exprs[decls->offset])
                    {
                        REQUIRE_EQ(1, w->sym->type.buf[0]);
                        REQUIRE_SIZING_EQ(s_sizing_int, w->sym->size);
                    }
                }
            }
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
                        "int y;"
                        "struct {\n"
                        "int l; char c;\n"
                        "};\n"
                        "};\n"
                        "int foo() {"
                        " struct A a = {.l=1, .y=0, {2}};"
                        " a.l;"
                        "}"));
    rc = 0;

    struct Ast** const asts = test.parser->expr_seqs.data;
    REQUIRE_EQ(2, test.parser->top->extent);
    REQUIRE_AST(StmtDecls, decls, asts[test.parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_AST(Decl, w, asts[decls->offset])
        REQUIRE_AST(StmtBlock, body, w->init)
        {
            REQUIRE_EQ(2, body->extent);
            REQUIRE_AST(StmtDecls, a, asts[body->offset])
            {
                REQUIRE_EQ(1, a->extent);
                REQUIRE_AST(Decl, d, asts[a->offset])
                REQUIRE_AST(AstInit, i, d->init)
                {
                    REQUIRE_EQ(4, i->offset);

                    AstInit* n = i->next;
                    REQUIRE(n && n->init);
                    REQUIRE_EQ(0, n->offset);

                    AstInit* nn = n->next;
                    REQUIRE(nn);
                    REQUIRE_EQ(4, nn->offset);
                    REQUIRE_AST(AstInit, ii, nn->init) { REQUIRE_EQ(4, ii->offset); }
                }
            }
            REQUIRE_AST(ExprField, f, asts[body->offset + 1])
            {
                REQUIRE_EQ(0, f->is_arrow);
                REQUIRE(f->sym);
                REQUIRE(f->sym->name);
                REQUIRE_STR_EQ("l", f->sym->name);
                REQUIRE_EQ(0, f->sym->field_offset);
                REQUIRE_EQ(4, f->field_offset);
            }
        }
    }

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

int parse_params(struct TestState* state)
{
    int rc = 1;
    StandardTest test;
    SUBTEST(stdtest_run(state,
                        &test,
                        "int arr(int i[1]);\n"
                        "int arru(int i[]);\n"
                        "int valist(__builtin_va_list v);\n"
                        "int ptr(int *i);\n"
                        "int m(int i[1]) {\n"
                        "int x[] = {1,2};\n"
                        "int *p = x;\n"
                        "ptr(x);\n"
                        "arru(x);\n"
                        "arr(x);\n"

                        "ptr(p);\n"
                        "arru(p);\n"
                        "arr(p);\n"

                        "ptr(i);\n"
                        "arru(i);\n"
                        "arr(i);\n"
                        "}\n"));

    Array arr = {0};
    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(5, test.parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            array_clear(&arr);
            typestr_fmt(test.elab->types, &w->sym->type, &arr);
            array_push_byte(&arr, 0);
            REQUIRE_STR_EQ("function (pointer to int) returning int", arr.data);
        }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            array_clear(&arr);
            typestr_fmt(test.elab->types, &w->sym->type, &arr);
            array_push_byte(&arr, 0);
            REQUIRE_STR_EQ("function (pointer to int) returning int", arr.data);
        }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 2])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            array_clear(&arr);
            typestr_fmt(test.elab->types, &w->sym->type, &arr);
            array_push_byte(&arr, 0);
            REQUIRE_STR_EQ("function (pointer to __builtin_va_list) returning int", arr.data);
        }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->offset + 4])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            array_clear(&arr);
            typestr_fmt(test.elab->types, &w->sym->type, &arr);
            array_push_byte(&arr, 0);
            REQUIRE_STR_EQ("function (pointer to int) returning int", arr.data);
            REQUIRE_AST(DeclFn, fn, w->type)
            {
                REQUIRE_EQ(1, fn->extent);
                REQUIRE_EXPR(StmtDecls, arg1s, exprs[fn->offset])
                {
                    REQUIRE_EQ(1, arg1s->extent);
                    REQUIRE_EXPR(Decl, arg1, exprs[arg1s->offset])
                    {
                        array_clear(&arr);
                        typestr_fmt(test.elab->types, &arg1->sym->type, &arr);
                        array_push_byte(&arr, 0);
                        REQUIRE_STR_EQ("pointer to int", arr.data);
                    }
                }
            }
        }
    }

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

int require_taco(TestState* state,
                 int expected,
                 const char* expected_str,
                 int actual,
                 const char* actual_str,
                 const char* file,
                 int line)
{
    if (expected != actual)
    {
        REQUIRE_FAIL_IMPL(file,
                          line,
                          "'%s == %s' was '%s == %s'",
                          expected_str,
                          actual_str,
                          taco_to_string(expected),
                          taco_to_string(actual));
    }
    return 0;
fail:
    return 1;
}

int require_taca(TestState* state, struct TACAddress* expected, struct TACAddress* actual, const char* file, int line)
{
    struct Array buf_expected = {0};
    struct Array buf_actual = {0};
    debug_taca(&buf_expected, expected);
    debug_taca(&buf_actual, actual);
    REQUIRE_MEM_EQ_IMPL(file, line, buf_expected.data, buf_expected.sz, buf_actual.data, buf_actual.sz);
    return 0;
fail:
    return 1;
}

int require_tace(TestState* state, struct TACEntry* expected, struct TACEntry* actual, const char* file, int line)
{
    int rc = 0;
    rc |= require_taco(state, expected->op, "expected", actual->op, "actual", file, line - 3);
    INFO("arg1\n", 1) { rc |= require_taca(state, &expected->arg1, &actual->arg1, file, line - 2); }
    INFO("arg2\n", 2) { rc |= require_taca(state, &expected->arg2, &actual->arg2, file, line - 1); }
    return rc;
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
            INFO("index %zu\n", i) { SUBTEST(require_tace(state, expected + i, data + i, __FILE__, __LINE__)); }       \
        }                                                                                                              \
        REQUIRE_EQ(expected_sz, actual_sz);                                                                            \
    } while (0)

#define REQUIRE_NEXT_TACE(...)                                                                                         \
    do                                                                                                                 \
    {                                                                                                                  \
        if (index < array_size(&test.be->code, sizeof(struct TACEntry)))                                               \
        {                                                                                                              \
            struct TACEntry expected = __VA_ARGS__;                                                                    \
            UNWRAP(require_tace(state, &expected, (TACEntry*)test.be->code.data + index++, __FILE__, __LINE__));       \
        }                                                                                                              \
        else                                                                                                           \
        {                                                                                                              \
            REQUIRE_FAIL("expected additional TACEntry (found %zu)", (size_t)index);                                   \
        }                                                                                                              \
    } while (0)

#define REQUIRE_END_TACE() REQUIRE_ZU_EQ(index, array_size(&test.be->code, sizeof(struct TACEntry)))

static size_t end_of_line(size_t offset, const char* const data, const size_t data_len)
{
    const char* const eol = memchr(data + offset, '\n', data_len - offset);
    return eol ? eol - data : data_len;
}

static size_t after_end_of_line(size_t offset, const char* const data, const size_t data_len)
{
    const char* const eol = memchr(data + offset, '\n', data_len - offset);
    return eol ? eol - data + 1 : data_len;
}

static size_t start_of_line_text(size_t offset, const char* const data, const size_t data_len)
{
    do
    {
        while (offset < data_len && (data[offset] == ' ' || data[offset] == '\n'))
            ++offset;
        if (offset < data_len && (data[offset] == '#' || data[offset] == '.'))
            offset = after_end_of_line(offset, data, data_len);
        else
            return offset;
    } while (1);
}

#define REQUIRE_NEXT_TEXT(...)                                                                                         \
    do                                                                                                                 \
    {                                                                                                                  \
        index = start_of_line_text(index, test.cg->code.data, test.cg->code.sz);                                       \
        if (index < test.cg->code.sz)                                                                                  \
        {                                                                                                              \
            const char* expected = __VA_ARGS__;                                                                        \
            size_t expected_len = strlen(expected);                                                                    \
            const char* actual = (const char*)test.cg->code.data + index;                                              \
            size_t actual_len = end_of_line(index, test.cg->code.data, test.cg->code.sz) - index;                      \
            REQUIRE_MEM_EQ(expected, expected_len, actual, actual_len);                                                \
            index += actual_len;                                                                                       \
            if (index != test.cg->code.sz) ++index;                                                                    \
        }                                                                                                              \
        else                                                                                                           \
        {                                                                                                              \
            REQUIRE_FAIL("%s", "expected more text.");                                                                 \
        }                                                                                                              \
    } while (0)

#define REQUIRE_END_TEXT()                                                                                             \
    do                                                                                                                 \
    {                                                                                                                  \
        index = start_of_line_text(index, test.cg->code.data, test.cg->code.sz);                                       \
        if (index < test.cg->code.sz)                                                                                  \
        {                                                                                                              \
            const char* actual = (const char*)test.cg->code.data + index;                                              \
            size_t actual_len = end_of_line(index, test.cg->code.data, test.cg->code.sz) - index;                      \
            REQUIRE_FAIL("expected end of text but found \"%.*s\"", (int)actual_len, actual);                          \
        }                                                                                                              \
    } while (0)

int test_be_simple(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run_cg(state, &test, "int main() { return 42; }"));

    size_t index = 0;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_int, .reg = REG_RAX},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 42},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});

    REQUIRE_END_TACE();

    index = 0;
    REQUIRE_NEXT_TEXT("_main:");
    REQUIRE_NEXT_TEXT("subq $24, %rsp");
    REQUIRE_NEXT_TEXT("mov $42, %rax");
    REQUIRE_NEXT_TEXT("addq $24, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_NEXT_TEXT("addq $24, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_simple2(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run_cg(state, &test, "int main(int argc, char** argv) { return argc; }"));

    size_t index = 0;

    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 0},
        {TACA_REG, .sizing = s_sizing_int, .reg = REG_RDI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 8},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RSI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_int, .reg = REG_RAX},
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});

    REQUIRE_END_TACE();

    index = 0;
    REQUIRE_NEXT_TEXT("_main:");
    REQUIRE_NEXT_TEXT("subq $24, %rsp");
    REQUIRE_NEXT_TEXT("mov %edi, 0(%rsp)");
    REQUIRE_NEXT_TEXT("mov %rsi, 8(%rsp)");
    REQUIRE_NEXT_TEXT("movsl 0(%rsp), %rax");
    REQUIRE_NEXT_TEXT("addq $24, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_NEXT_TEXT("addq $24, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_relations(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run_cg(state,
                          &test,
                          "int is_ascii_alphu(int ch) { return ('a' <= ch && 'z' >= ch) || ('A' < ch && 'Z' > ch) || "
                          "ch == '_' || ch != ' '; }"));

    size_t index = 0;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 0},
        {TACA_REG, .sizing = 1, 4, .reg = REG_RDI},
    });
    REQUIRE_NEXT_TACE({
        TACO_LTEQ,
        {TACA_IMM, .sizing = 1, 4, .imm = 97},
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 16},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_BRZ,
        {TACA_REF, .sizing = 1, 4, .ref = 1},
        {TACA_ALABEL, .alabel = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LTEQ,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
        {TACA_IMM, .sizing = 1, 4, .imm = 122},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 16},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 1},
        {TACA_VOID},
    });
    REQUIRE_NEXT_TACE({
        TACO_NEQ,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 16},
        {TACA_IMM, .sizing = 1, 4, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 12},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_BRNZ,
        {TACA_REF, .sizing = 1, 4, .ref = index - 2},
        {TACA_ALABEL, .alabel = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_LT,
        {TACA_IMM, .sizing = 1, 4, .imm = 65},
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 20},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_BRZ,
        {TACA_REF, .sizing = 1, 4, .ref = index - 2},
        {TACA_ALABEL, .alabel = 3},
    });
    REQUIRE_NEXT_TACE({
        TACO_LT,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
        {TACA_IMM, .sizing = 1, 4, .imm = 90},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 20},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 3},
        {TACA_VOID},
    });
    REQUIRE_NEXT_TACE({
        TACO_NEQ,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 20},
        {TACA_IMM, .sizing = 1, 4, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 12},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 2},
        {TACA_VOID},
    });
    REQUIRE_NEXT_TACE({
        TACO_NEQ,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 12},
        {TACA_IMM, .sizing = 1, 4, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 8},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_BRNZ,
        {TACA_REF, .sizing = 1, 4, .ref = index - 2},
        {TACA_ALABEL, .alabel = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_EQ,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
        {TACA_IMM, .sizing = 1, 4, .imm = 95},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 8},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 4},
        {TACA_VOID},
    });
    REQUIRE_NEXT_TACE({
        TACO_NEQ,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 8},
        {TACA_IMM, .sizing = 1, 4, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 4},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_BRNZ,
        {TACA_REF, .sizing = 1, 4, .ref = index - 2},
        {TACA_ALABEL, .alabel = 5},
    });
    REQUIRE_NEXT_TACE({
        TACO_NEQ,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
        {TACA_IMM, .sizing = 1, 4, .imm = 32},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 4},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 5},
        {TACA_VOID},
    });
    REQUIRE_NEXT_TACE({
        TACO_NEQ,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 4},
        {TACA_IMM, .sizing = 1, 4, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = 1, 4, .reg = REG_RAX},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});

    REQUIRE_END_TACE();

    // index = 0;
    // REQUIRE_NEXT_TEXT("_main:");
    // REQUIRE_NEXT_TEXT("subq $24, %rsp");
    // REQUIRE_NEXT_TEXT("mov $42, %rax");
    // REQUIRE_NEXT_TEXT("addq $24, %rsp");
    // REQUIRE_NEXT_TEXT("ret");
    // REQUIRE_NEXT_TEXT("addq $24, %rsp");
    // REQUIRE_NEXT_TEXT("ret");
    // REQUIRE_END_TEXT();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_arithmetic(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run_cg(state,
                          &test,
                          "void f(int* ch, int i) {\n"
                          " ch += 1;\n"
                          " ch -= 1;\n"
                          " int* z = ++ch; int* w = ch++;\n"
                          " --ch; ch--;\n"
                          " i - 1; i -= 1;\n"
                          " i + 1; i += 1;\n"
                          " i % 1; i %= 1;\n"
                          " i / 1; i /= 1;\n"
                          "}"));

    size_t index = 0;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 0, 8, .frame_offset = 0},
        {TACA_REG, .sizing = 0, 8, .reg = REG_RDI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 8},
        {TACA_REG, .sizing = 1, 4, .reg = REG_RSI},
    });
    // ch += 1;
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = 0, 8, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 0, 8, .frame_offset = 0},
        {TACA_REF, .sizing = 0, 8, .ref = index - 1},
    });
    // ch -= 1;
    REQUIRE_NEXT_TACE({
        TACO_SUB,
        {TACA_FRAME, .sizing = 0, 8, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 0, 8, .frame_offset = 0},
        {TACA_REF, .sizing = 0, 8, .ref = index - 1},
    });
    // int* z = ++ch;
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = 0, 8, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 0, 8, .frame_offset = 0},
        {TACA_REF, .sizing = 0, 8, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 0, 8, .frame_offset = 16},
        {TACA_REF, .sizing = 0, 8, .ref = index - 2},
    });
    // int* w = ch++;
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = 0, 8, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = 0, 8, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 0, 8, .frame_offset = 0},
        {TACA_REF, .sizing = 0, 8, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 0, 8, .frame_offset = 24},
        {TACA_REF, .sizing = 0, 8, .ref = index - 3},
    });

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}
int test_be_bitmath(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run_cg(state,
                          &test,
                          "void f(int i, int j) {\n"
                          " int k = i & j;\n"
                          " int l = i | j;\n"
                          " int m = i ^ j;\n"
                          " int n = ~i;\n"
                          "}"));

    size_t index = 0;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 0},
        {TACA_REG, .sizing = 1, 4, .reg = REG_RDI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 4},
        {TACA_REG, .sizing = 1, 4, .reg = REG_RSI},
    });
    // i & j
    REQUIRE_NEXT_TACE({
        TACO_BAND,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 8},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    // i | j
    REQUIRE_NEXT_TACE({
        TACO_BOR,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 12},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    // i ^ j
    REQUIRE_NEXT_TACE({
        TACO_BXOR,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 16},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    // ~i
    REQUIRE_NEXT_TACE({
        TACO_BNOT,
        {TACA_FRAME, .sizing = 1, 4, .frame_offset = 0},
        0,
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 1, 4, .frame_offset = 20},
        {TACA_REF, .sizing = 1, 4, .ref = index - 1},
    });
    REQUIRE_END_TACE();

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

    size_t index = 0;

    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        .arg1 = {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 0},
        .arg2 = {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RDI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        .arg1 = {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 8},
        .arg2 = {TACA_REG, .sizing = s_sizing_int, .reg = REG_RSI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        .arg1 = {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 16},
        .arg2 = {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RDX},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        .arg1 = {TACA_FRAME, .sizing.width = 32, .frame_offset = 0},
        .arg2 = {TACA_ARG, .sizing.width = 32, .arg_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        .arg1 = {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RAX},
        .arg2 = {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});

    REQUIRE_END_TACE();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_cast(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "void main() {"
                       " char ch;"
                       " int x = (int)ch++;"
                       " int y = 2 + (int)ch;"
                       "}"));
    int index = 0;
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_schar, .param_offset = 0},
        {TACA_REF, .sizing = s_sizing_schar, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .param_offset = 4},
        {TACA_REF, .sizing = s_sizing_schar, .ref = index - 3},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .param_offset = 8},
        {TACA_REF, .sizing = s_sizing_int, .ref = index - 1},
    });
    REQUIRE_END_TACE();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_call(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "struct A { char buf[32]; };\n"
                       "struct A mm_call(struct A a);"
                       "struct A mi_call(int a);"
                       "int im_call(struct A a);"
                       "int ii_call(int a);"
                       "void main() { ii_call(10); im_call(mm_call(mi_call(5))); }"));
    int index = 0;

    // ii_call(10);
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDI},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 10},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .is_addr = 1, .name = "ii_call"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });

    // im_call(mm_call(mi_call(5)));
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDI},
        {TACA_FRAME, .is_addr = 1, .frame_offset = 32},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RSI},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 5},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .is_addr = 1, .name = "mi_call"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_PARAM, .is_addr = 1, .sizing.width = 32, .param_offset = 0},
        {TACA_FRAME, .sizing.width = 32, .frame_offset = 32},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDI},
        {TACA_FRAME, .is_addr = 1, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .is_addr = 1, .name = "mm_call"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_PARAM, .is_addr = 1, .sizing.width = 32, .param_offset = 0},
        {TACA_FRAME, .sizing.width = 32, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .is_addr = 1, .name = "im_call"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });
    REQUIRE_END_TACE();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_call2(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "void cg_debug(struct CodeGen* cg, const char* fmt, ...);"
                       "void cg_declare_extern(struct CodeGen* cg, const char* sym)"
                       "{"
                       "cg_debug(cg, \"   : %s\\n\", sym);"
                       "}"));
    int index = 0;

    // prologue;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 0},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RDI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 8},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RSI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDI},
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RSI},
        {TACA_CONST, .is_addr = 1, .const_idx = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDX},
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 8},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .is_addr = 1, .name = "cg_debug"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 3},
    });
    REQUIRE_END_TACE();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_call3(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "struct Y { char buf[20]; };"
                       "void f(int x[1], struct Y y);"
                       "void g(int x[1], struct Y y1)"
                       "{"
                       " struct Y y;"
                       " f(x, y);"
                       "}"));
    int index = 0;

    // prologue;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 0},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RDI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_PARAM, .is_addr = 1, .sizing.width = 20, .param_offset = 0},
        {TACA_FRAME, .sizing.width = 20, .frame_offset = 8},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDI},
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .is_addr = 1, .name = "f"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_END_TACE();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_va_args(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "typedef __builtin_va_list __gnu_va_list;\n"
                       "typedef __gnu_va_list va_list;\n"
                       "void g(const char* fmt, __builtin_va_list w);\n"
                       "void f(const char* fmt, ...) {\n"
                       "va_list v;\n"
                       "__builtin_va_start(v, fmt);\n"
                       "g(fmt, v);\n"
                       "int x = __builtin_va_arg(v, int);"
                       "__builtin_va_end(v);"
                       "f(0, 1, 2, 2, 2, 2, 2);"
                       "}"));
    int index = 0;

    // prologue;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 0},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RDI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 8},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RSI},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 16},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RDX},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 24},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RCX},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 32},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_R8},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 40},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_R9},
    });
    // va_start
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_uint, .frame_offset = 48},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 8},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_uint, .frame_offset = 52},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 48},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 56},
        {TACA_ARG, .is_addr = 1, .arg_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 64},
        {TACA_FRAME, .is_addr = 1, .frame_offset = 0},
    });
    // g(v);
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDI},
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RSI},
        {TACA_FRAME, .is_addr = 1, .frame_offset = 48},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .is_addr = 1, .name = "g"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    // va_arg(, int)
    REQUIRE_NEXT_TACE({
        TACO_LT,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 48},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 48},
    });
    REQUIRE_NEXT_TACE({
        TACO_BRZ,
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
        {TACA_ALABEL, .alabel = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 64},
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 48},
    });
    REQUIRE_NEXT_TACE({
        TACO_LOAD,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 76},
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 48},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 8},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 48},
        {TACA_REF, .sizing = s_sizing_int, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_JUMP,
        {TACA_ALABEL, .alabel = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_LOAD,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 76},
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 56},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 56},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 8},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 56},
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 72},
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 76},
    });
    // f(0, 1, 2);
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_PARAM, .is_addr = 1, .sizing = s_sizing_ptr, .param_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDI},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RSI},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDX},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RCX},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_R8},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_R9},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .is_addr = 1, .name = "f"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
    });
    REQUIRE_END_TACE();

    rc = cg_gen_taces(test.cg, test.be->code.data, array_size(&test.be->code, sizeof(TACEntry)), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    index = 0;

    REQUIRE_NEXT_TEXT("_f:");
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("mov %rdi, 8(%rsp)");
    REQUIRE_NEXT_TEXT("mov %rsi, 16(%rsp)");
    REQUIRE_NEXT_TEXT("mov %rdx, 24(%rsp)");
    REQUIRE_NEXT_TEXT("mov %rcx, 32(%rsp)");
    REQUIRE_NEXT_TEXT("mov %r8, 40(%rsp)");
    REQUIRE_NEXT_TEXT("mov %r9, 48(%rsp)");

    REQUIRE_NEXT_TEXT("movl $8, 56(%rsp)");
    REQUIRE_NEXT_TEXT("movl $48, 60(%rsp)");
    REQUIRE_NEXT_TEXT("leaq 128(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 64(%rsp)");
    REQUIRE_NEXT_TEXT("leaq 8(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 72(%rsp)");

    REQUIRE_NEXT_TEXT("mov 8(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("leaq 56(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _g");

    REQUIRE_NEXT_TEXT("movsl 56(%rsp), %rax");
    REQUIRE_NEXT_TEXT("mov $48, %rdx");
    REQUIRE_NEXT_TEXT("cmp %edx, %eax");
    REQUIRE_NEXT_TEXT("setl %al");
    REQUIRE_NEXT_TEXT("movzx %al, %rax");
    REQUIRE_NEXT_TEXT("mov %rax, 112(%rsp)");
    REQUIRE_NEXT_TEXT("mov 112(%rsp), %rax");
    REQUIRE_NEXT_TEXT("cmp $0, %rax");
    REQUIRE_NEXT_TEXT("jz  L$2");
    REQUIRE_NEXT_TEXT("mov 72(%rsp), %r10");
    REQUIRE_NEXT_TEXT("movsl 56(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add %r11, %r10");
    REQUIRE_NEXT_TEXT("mov %r10, 112(%rsp)");

    REQUIRE_NEXT_TEXT("mov 112(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("leaq 84(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movsd");

    REQUIRE_NEXT_TEXT("movsl 56(%rsp), %r10");
    REQUIRE_NEXT_TEXT("mov $8, %r11");
    REQUIRE_NEXT_TEXT("add %r11, %r10");
    REQUIRE_NEXT_TEXT("mov %r10, 112(%rsp)");
    REQUIRE_NEXT_TEXT("movsl 112(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11d, 56(%rsp)");
    REQUIRE_NEXT_TEXT("jmp L$1");
    REQUIRE_NEXT_TEXT("L$2:");
    REQUIRE_NEXT_TEXT("mov 64(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("leaq 84(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movsd");

    REQUIRE_NEXT_TEXT("mov 64(%rsp), %r10");
    REQUIRE_NEXT_TEXT("mov $8, %r11");
    REQUIRE_NEXT_TEXT("add %r11, %r10");
    REQUIRE_NEXT_TEXT("mov %r10, 112(%rsp)");
    REQUIRE_NEXT_TEXT("mov 112(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 64(%rsp)");
    REQUIRE_NEXT_TEXT("L$1:");

    REQUIRE_NEXT_TEXT("movsl 84(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11d, 80(%rsp)");
    REQUIRE_NEXT_TEXT("movq $2, 0(%rsp)");
    REQUIRE_NEXT_TEXT("mov $0, %rdi");
    REQUIRE_NEXT_TEXT("mov $1, %rsi");
    REQUIRE_NEXT_TEXT("mov $2, %rdx");
    REQUIRE_NEXT_TEXT("mov $2, %rcx");
    REQUIRE_NEXT_TEXT("mov $2, %r8");
    REQUIRE_NEXT_TEXT("mov $2, %r9");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _f");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_conversions(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "void cg_declare_extern()"
                       "{"
                       "char x = 10;"
                       "x = 11;"
                       "int y = x;"
                       "y = x;"
                       "x += 1;"
                       "x++;"
                       "char h[] = \"he\\\\\\\\\\\\\";"
                       "char *p = h;"
                       "unsigned long long z = x * 2;"
                       "unsigned long long w = z * 2;"
                       "}"));
    int index = 0;

    // prologue;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 10},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_schar, .imm = 11},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 4},
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 4},
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
    });

    // x += 1;
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_REF, .sizing = s_sizing_schar, .ref = index - 1},
    });
    // x++;
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_REF, .sizing = s_sizing_schar, .ref = index - 1},
    });
    // h[] = "hello"
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = 0, 6, .frame_offset = 8},
        {TACA_CONST, .sizing = 0, 6, .const_idx = 0},
    });
    // p = h;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 16},
        {TACA_FRAME, .is_addr = 1, .frame_offset = 8},
    });
    // z = x * 2;
    REQUIRE_NEXT_TACE({
        TACO_MULT,
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 24},
        {TACA_REF, .sizing = s_sizing_int, .ref = index - 1},
    });
    // w = z * 2;
    REQUIRE_NEXT_TACE({
        TACO_MULT,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 24},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 32},
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
    });
    REQUIRE_END_TACE();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_init(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "struct S {"
                       "int x, y, z, w;"
                       "};"
                       "struct A {"
                       " struct S s1, s2;"
                       "};"
                       "void main()"
                       "{"
                       "struct A s1 = {0};"
                       "}"));
    int index = 0;

    // prologue;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing.width = 32, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_END_TACE();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_switch(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "int f(int x) {\n"
                       "switch (x) {"
                       "case 1: return 10;"
                       "case 2: return 20;"
                       "default: return 30;"
                       "}}"));
    int index = 0;

    // prelude
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 0},
        {TACA_REG, .sizing = s_sizing_int, .reg = REG_RDI},
    });
    // switch (x)
    REQUIRE_NEXT_TACE({
        TACO_JUMP,
        {TACA_ALABEL, .alabel = 2},
    });
    // case 1:
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 3},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_int, .reg = REG_RAX},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 10},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});
    // case 2:
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_int, .reg = REG_RAX},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 20},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});
    // default:
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 5},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_int, .reg = REG_RAX},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 30},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});
    // implicit jump to end
    REQUIRE_NEXT_TACE({
        TACO_JUMP,
        {TACA_ALABEL, .alabel = 1},
    });

    // jump table
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RCX},
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_CTBZ,
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        {TACA_ALABEL, .alabel = 3},
    });
    REQUIRE_NEXT_TACE({
        TACO_CTBZ,
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
        {TACA_ALABEL, .alabel = 4},
    });
    REQUIRE_NEXT_TACE({
        TACO_JUMP,
        {TACA_ALABEL, .alabel = 5},
    });

    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 1},
    });

    REQUIRE_END_TACE();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_ternary(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "int f(int x) {\n"
                       "return x == 1 ? 10 : 20;"
                       "}"));
    int index = 0;

    // prelude
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 0},
        {TACA_REG, .sizing = s_sizing_int, .reg = REG_RDI},
    });
    REQUIRE_NEXT_TACE({
        TACO_EQ,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_BRZ,
        {TACA_REF, .sizing = s_sizing_int, .ref = index - 1},
        {TACA_ALABEL, .alabel = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 4},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 10},
    });
    REQUIRE_NEXT_TACE({
        TACO_JUMP,
        {TACA_ALABEL, .alabel = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 4},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 20},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_int, .reg = REG_RAX},
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 4},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});

    REQUIRE_END_TACE();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_be_fnstatic(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run_cg(state, &test, "int main() { static const int x = 42; return x; }"));

    size_t index = 0;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_int, .reg = REG_RAX},
        {TACA_LNAME, .sizing = s_sizing_int, .name = "x"},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});

    REQUIRE_END_TACE();

    // index = 0;
    // REQUIRE_NEXT_TEXT("_main:");
    // REQUIRE_NEXT_TEXT("subq $24, %rsp");
    // REQUIRE_NEXT_TEXT("mov $42, %rax");
    // REQUIRE_NEXT_TEXT("addq $24, %rsp");
    // REQUIRE_NEXT_TEXT("ret");
    // REQUIRE_NEXT_TEXT("addq $24, %rsp");
    // REQUIRE_NEXT_TEXT("ret");
    // REQUIRE_END_TEXT();

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

typedef struct CGTest
{
    struct CodeGen* cg;
} CGTest;

int test_cg_assign(TestState* state)
{
    int rc = 1;
    CGTest test = {
        .cg = my_malloc(sizeof(struct CodeGen)),
    };
    cg_init(test.cg);
    TACEntry taces[] = {
        // stores from reg
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 0},
            {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RDI},
        },
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_uint, .frame_offset = 0},
            {TACA_REG, .sizing = s_sizing_uint, .reg = REG_RDI},
        },
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing.width = 2, .frame_offset = 0},
            {TACA_REG, .sizing.width = 2, .reg = REG_RDI},
        },
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_uchar, .frame_offset = 0},
            {TACA_REG, .sizing = s_sizing_uchar, .reg = REG_RDI},
        },
        // loads to reg
        {
            TACO_ASSIGN,
            {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RDI},
            {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 0},
        },
        {
            TACO_ASSIGN,
            {TACA_REG, .is_addr = 1, .sizing = s_sizing_uint, .reg = REG_RDI},
            {TACA_FRAME, .sizing = s_sizing_uint, .frame_offset = 0},
        },
        // memory load+store
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_uint, .frame_offset = 20},
            {TACA_FRAME, .sizing = s_sizing_uint, .frame_offset = 24},
        },
        // leaq
        {
            TACO_ASSIGN,
            {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RAX},
            {TACA_FRAME, .is_addr = 1, .frame_offset = 24},
        },
        // assign through: mov %ebx, 0(%rax)
        {
            TACO_ASSIGN,
            {TACA_REG, .sizing = s_sizing_uint, .reg = REG_RAX},
            {TACA_REG, .sizing = s_sizing_uint, .reg = REG_RBX},
        },
        // store address
        {
            TACO_ASSIGN,
            {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RAX},
            {TACA_FRAME, .is_addr = 1, .frame_offset = 1},
        },
        // calc address
        {
            TACO_ASSIGN,
            {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RAX},
            {TACA_ARG, .is_addr = 1, .arg_offset = 0},
        },
        {
            TACO_ASSIGN,
            {TACA_PARAM, .is_addr = 1, .sizing = s_sizing_ptr, .arg_offset = 0},
            {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RAX},
        },
        // memset 0
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing.width = 32, .frame_offset = 0},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
        },
        // memcpy
        {
            TACO_ASSIGN,
            {TACA_FRAME, .sizing.width = 32, .frame_offset = 0},
            {TACA_FRAME, .sizing.width = 32, .frame_offset = 8},
        },
        // assign through ref
        {
            TACO_ASSIGN,
            {TACA_REF, .sizing = s_sizing_int, .ref = 0},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        },
        // mixed sizes
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .param_offset = 4},
            {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        },
    };
    rc = cg_gen_taces(test.cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("mov %rdi, 8(%rsp)");
    REQUIRE_NEXT_TEXT("mov %edi, 8(%rsp)");
    REQUIRE_NEXT_TEXT("mov %di, 8(%rsp)");
    REQUIRE_NEXT_TEXT("mov %dil, 8(%rsp)");

    REQUIRE_NEXT_TEXT("mov 8(%rsp), %rdi");

    REQUIRE_NEXT_TEXT("mov 8(%rsp), %edi");

    REQUIRE_NEXT_TEXT("mov 32(%rsp), %r11d");
    REQUIRE_NEXT_TEXT("mov %r11d, 28(%rsp)");

    REQUIRE_NEXT_TEXT("leaq 32(%rsp), %rax");

    REQUIRE_NEXT_TEXT("mov %ebx, 0(%rax)");

    REQUIRE_NEXT_TEXT("leaq 9(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 0(%rax)");

    REQUIRE_NEXT_TEXT("leaq 128(%rsp), %rax");
    REQUIRE_NEXT_TEXT("mov %rax, 0(%rsp)");

    // memset 0
    REQUIRE_NEXT_TEXT("leaq 8(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("mov $32, %rcx");
    REQUIRE_NEXT_TEXT("xor %rax, %rax");
    REQUIRE_NEXT_TEXT("rep stosb");

    // memcpy
    REQUIRE_NEXT_TEXT("leaq 16(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("mov 8(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("mov $32, %rcx");
    REQUIRE_NEXT_TEXT("cld");
    REQUIRE_NEXT_TEXT("rep movsb");

    // assign through ref
    REQUIRE_NEXT_TEXT("mov 112(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movl $1, (%rdi)");

    // mixed sizes
    REQUIRE_NEXT_TEXT("movsb 8(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11d, 12(%rsp)");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    cg_destroy(test.cg);
    my_free(test.cg);
    return rc;
}

int test_cg_refs(TestState* state)
{
    int rc = 1;
    CGTest test = {
        .cg = my_malloc(sizeof(struct CodeGen)),
    };
    cg_init(test.cg);
    TACEntry taces[] = {
        {
            TACO_ADD,
            {TACA_LNAME, .is_addr = 1, .name = "a"},
            {TACA_LNAME, .sizing = s_sizing_int, .name = "b"},
        },
        {
            TACO_ADD,
            {TACA_NAME, .is_addr = 1, .name = "a"},
            {TACA_NAME, .sizing = s_sizing_int, .name = "b"},
        },
    };
    rc = cg_gen_taces(test.cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("leaq _a(%rip), %r10");
    REQUIRE_NEXT_TEXT("movsl _b(%rip), %r11");
    REQUIRE_NEXT_TEXT("add %r11, %r10");

    REQUIRE_NEXT_TEXT("movq _a@GOTPCREL(%rip), %r10");
    REQUIRE_NEXT_TEXT("movq _b@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("movsl (%r11), %r11");
    REQUIRE_NEXT_TEXT("add %r11, %r10");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    cg_destroy(test.cg);
    my_free(test.cg);
    return rc;
}

int test_be_static_init(TestState* state)
{
    int rc = 1;
    BETest test;
    SUBTEST(betest_run(state,
                       &test,
                       "enum {v1 = 2};"
                       "static int data[] = {1,v1,3};\n"
                       "static const char* const s_reg_names[] = {\"%rax\", \"%rbx\"};\n"));

    struct Expr** const exprs = (struct Expr**)test.base.parser->expr_seqs.data;
    REQUIRE_EQ(3, test.base.parser->top->extent);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.base.parser->top->offset + 1])
    {
        REQUIRE_EQ(1, decls->extent);
        REQUIRE_EXPR(Decl, w, exprs[decls->offset])
        {
            REQUIRE_AST(AstInit, w_init, w->init)
            {
                REQUIRE_SIZING_EQ(s_sizing_int, w_init->sizing);
                REQUIRE_EQ(0, w_init->offset);

                REQUIRE(w_init->next);
                REQUIRE_SIZING_EQ(s_sizing_int, w_init->next->sizing);
                REQUIRE_EQ(4, w_init->next->offset);

                REQUIRE(w_init->next->next);
                REQUIRE_SIZING_EQ(s_sizing_int, w_init->next->next->sizing);
                REQUIRE_EQ(8, w_init->next->next->offset);

                REQUIRE(w_init->next->next->next);
                REQUIRE_NULL(w_init->next->next->next->init);
            }
            REQUIRE_EQ(12, w->sym->size.width);
        }
    }

    array_push_byte(&test.cg->data, '\0');

    REQUIRE_LINES(test.cg->data.data)
    {
        REQUIRE_LINE("_data:");
        REQUIRE_LINE(".byte 1, 0, 0, 0, 2, 0, 0, 0");
        REQUIRE_LINE(".byte 3, 0, 0, 0");
        REQUIRE_LINE("");
        REQUIRE_LINE("_s_reg_names:");
        REQUIRE_LINE(".quad L_.S0 + 0");
        REQUIRE_LINE(".quad L_.S1 + 0");
        REQUIRE_LINE("");
    }

    rc = 0;
fail:
    betest_destroy(&test);
    return rc;
}

int test_cg_call(TestState* state)
{
    int rc = 1;
    CGTest test = {
        .cg = my_malloc(sizeof(struct CodeGen)),
    };
    cg_init(test.cg);
    TACEntry taces[] = {
        {
            TACO_CALL,
            {TACA_NAME, .is_addr = 1, .name = "f"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_CALL,
            {TACA_NAME, .sizing = s_sizing_ptr, .name = "f"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_CALL,
            {TACA_LNAME, .is_addr = 1, .name = "f"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_CALL,
            {TACA_LNAME, .sizing = s_sizing_ptr, .name = "f"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
    };
    rc = cg_gen_taces(test.cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _f");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *_f@GOTPCREL(%rip)");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _f");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *_f(%rip)");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    cg_destroy(test.cg);
    my_free(test.cg);
    return rc;
}

int test_cg_add(TestState* state)
{
    int rc = 1;
    CGTest test = {
        .cg = my_malloc(sizeof(struct CodeGen)),
    };
    cg_init(test.cg);
    TACEntry taces[] = {
        {
            TACO_ADD,
            {TACA_FRAME, .is_addr = 1, .frame_offset = 0},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_ADD,
            {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 0},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_ADD,
            {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
    };
    rc = cg_gen_taces(test.cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("leaq 0(%rsp), %r10");
    REQUIRE_NEXT_TEXT("mov $7, %r11");
    REQUIRE_NEXT_TEXT("add %r11, %r10");

    REQUIRE_NEXT_TEXT("movsl 0(%rsp), %r10");
    REQUIRE_NEXT_TEXT("mov $7, %r11");
    REQUIRE_NEXT_TEXT("add %r11, %r10");

    REQUIRE_NEXT_TEXT("movsb 0(%rsp), %r10");
    REQUIRE_NEXT_TEXT("mov $7, %r11");
    REQUIRE_NEXT_TEXT("add %r11, %r10");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    cg_destroy(test.cg);
    my_free(test.cg);
    return rc;
}

int test_cg_bitmath(TestState* state)
{
    int rc = 1;
    CGTest test = {
        .cg = my_malloc(sizeof(struct CodeGen)),
    };
    cg_init(test.cg);
    TACEntry taces[] = {
        {
            TACO_BAND,
            {TACA_FRAME, .is_addr = 1, .frame_offset = 0},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_BOR,
            {TACA_REF, .sizing = s_sizing_int, .ref = 0},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_BXOR,
            {TACA_REF, .sizing = s_sizing_int, .ref = 1},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_BNOT,
            {TACA_REF, .sizing = s_sizing_int, .ref = 2},
            0,
        },
    };
    rc = cg_gen_taces(test.cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("mov $7, %rdx");
    REQUIRE_NEXT_TEXT("leaq 0(%rsp), %rax");
    REQUIRE_NEXT_TEXT("and %rdx, %rax");
    REQUIRE_NEXT_TEXT("mov %rax, 104(%rsp)");

    REQUIRE_NEXT_TEXT("mov $7, %rdx");
    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %rax");
    REQUIRE_NEXT_TEXT("or %rdx, %rax");
    REQUIRE_NEXT_TEXT("mov %rax, 104(%rsp)");

    REQUIRE_NEXT_TEXT("mov $7, %rdx");
    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %rax");
    REQUIRE_NEXT_TEXT("xor %rdx, %rax");
    REQUIRE_NEXT_TEXT("mov %rax, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %rax");
    REQUIRE_NEXT_TEXT("not %rax");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    cg_destroy(test.cg);
    my_free(test.cg);
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

    RUN_TEST(parse_initializer_struct);
    RUN_TEST(preproc_ternary);
    RUN_TEST(parse_strings);
    RUN_TEST(parse_main);
    RUN_TEST(parse_body);
    RUN_TEST(parse_sizeof);
    RUN_TEST(parse_constants);
    RUN_TEST(parse_typedef);
    RUN_TEST(parse_struct);
    RUN_TEST(parse_initializer);
    RUN_TEST(parse_nested_struct);
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
    RUN_TEST(parse_implicit_conversion);
    RUN_TEST(parse_params);
    RUN_TEST(parse_enums);
    RUN_TEST(parse_typedefs);
    RUN_TEST(parse_aggregates);
    RUN_TEST(parse_ptrconvert);
    RUN_TEST(test_be_simple);
    RUN_TEST(test_be_simple2);
    RUN_TEST(test_be_relations);
    RUN_TEST(test_be_arithmetic);
    RUN_TEST(test_be_bitmath);
    RUN_TEST(test_be_memory_ret);
    RUN_TEST(test_be_cast);
    RUN_TEST(test_be_call);
    RUN_TEST(test_be_call2);
    RUN_TEST(test_be_call3);
    RUN_TEST(test_be_va_args);
    RUN_TEST(test_be_conversions);
    RUN_TEST(test_be_init);
    RUN_TEST(test_be_switch);
    RUN_TEST(test_be_ternary);
    RUN_TEST(test_be_static_init);
    RUN_TEST(test_be_fnstatic);
    RUN_TEST(test_cg_assign);
    RUN_TEST(test_cg_call);
    RUN_TEST(test_cg_add);
    RUN_TEST(test_cg_bitmath);
    RUN_TEST(test_cg_refs);

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
