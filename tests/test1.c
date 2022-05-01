#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "cg.h"
#include "elaborator.h"
#include "errors.h"
#include "parse.h"
#include "preproc.h"
#include "stdlibe.h"
#include "symbol.h"
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
    REQUIRE_NULL(mainrty->def);
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
    SUBTEST(test_parse(state, &parser, &pp, "typedef unsigned long size_t;"));

    REQUIRE_EQ(1, parser->top->extent);
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
    REQUIRE_NULL(defspecs->def);
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
                       "struct Array"
                       "{"
                       "  size_t sz;"
                       "  size_t cap;"
                       "  void* data;"
                       "};"));

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
                REQUIRE_PTR_EQ(addr_t_decl, decls->specs->first_member);
                REQUIRE_EQ(4, addr_t_decl->sym->align);
                REQUIRE_EQ(4, addr_t_decl->sym->size);
            }
        }
        REQUIRE(decls->specs->first_member->sym->next_field);
        REQUIRE_EXPR(StmtDecls, in_u_decls, exprs[decls->specs->suinit->offset + 1])
        {
            REQUIRE_EQ(1, in_u_decls->extent);
            REQUIRE_EXPR(Decl, in_u_decl, exprs[in_u_decls->offset])
            {
                REQUIRE_PTR_EQ(in_u_decl, decls->specs->first_member->sym->next_field);
                REQUIRE_EQ(2, in_u_decl->sym->align);
                REQUIRE_EQ(4, in_u_decl->sym->size);
            }
        }
        REQUIRE_NULL(decls->specs->first_member->sym->next_field->sym->next_field);

        REQUIRE_EQ(4, decls->specs->align);
        REQUIRE_EQ(8, decls->specs->size);
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

int main()
{
    struct TestState _state = {};
    struct TestState* state = &_state;
    RUN_TEST(preproc_ternary);
    RUN_TEST(parse_main);
    RUN_TEST(parse_typedef);
    RUN_TEST(parse_struct);
    RUN_TEST(parse_initializer);
    RUN_TEST(parse_nested_struct);
    RUN_TEST(parse_initializer_struct);
    RUN_TEST(parse_initializer_union);
    RUN_TEST(parse_initializer_array);
    RUN_TEST(parse_initializer2b);
    RUN_TEST(parse_initializer_designated);

    const char* const clicolorforce = getenv("CLICOLOR_FORCE");
    const char* const clicolor = getenv("CLICOLOR");
    const char* color = "";

    if (clicolorforce && 0 != strcmp(clicolorforce, "0") || !clicolor || 0 != strcmp(clicolor, "0"))
    {
        if (state->testfails + state->assertionfails == 0)
            color = "\033[32;1m";
        else
            color = "\033[31;1m";
    }
    printf("%s%d tests. %d failed. %d assertions. %d failed.\033[m\n",
           color,
           state->tests,
           state->testfails,
           state->assertions,
           state->assertionfails);

    return state->testfails > 0 || state->assertionfails > 0;
}
