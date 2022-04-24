#include <stdio.h>
#include <string.h>

#include "cg.h"
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
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (*parser)
    {
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

int parse_main(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    SUBTEST(test_parse(state, &parser, &pp, "int main() {}"));

    REQUIRE_EQ(1, array_size(&parser->arr_exprs, sizeof(struct Expr*)));
    struct StmtDecls* decls = (struct StmtDecls*)*(struct Expr**)parser->arr_exprs.data;
    REQUIRE_EQ(STMT_DECLS, decls->kind.kind);
    REQUIRE_EQ(1, decls->extent);
    struct Decl* main = (struct Decl*)((struct Expr**)parser->expr_seqs.data)[decls->offset];
    REQUIRE_EQ(AST_DECL, main->kind.kind);
    REQUIRE_STR_EQ("main", token_str(parser, main->id));
    struct DeclFn* mainfn = (struct DeclFn*)main->type;
    REQUIRE_EQ(AST_DECLFN, mainfn->kind.kind);
    struct DeclSpecs* mainrty = (struct DeclSpecs*)mainfn->type;
    REQUIRE_EQ(AST_DECLSPEC, mainrty->kind.kind);
    REQUIRE_NULL(mainrty->name);
    REQUIRE_NULL(mainrty->type);
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
    if (parser) my_free(parser);
    if (pp) preproc_free(pp);
    return rc;
}

int parse_typedef(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    SUBTEST(test_parse(state, &parser, &pp, "typedef unsigned long size_t;"));

    REQUIRE_EQ(1, array_size(&parser->arr_exprs, sizeof(struct Expr*)));
    struct StmtDecls* decls = (struct StmtDecls*)((struct Expr**)parser->arr_exprs.data)[0];
    REQUIRE_EQ(STMT_DECLS, decls->kind.kind);
    REQUIRE_EQ(1, decls->extent);
    struct Decl* def = (struct Decl*)((struct Expr**)parser->expr_seqs.data)[decls->offset];
    REQUIRE_EQ(AST_DECL, def->kind.kind);
    REQUIRE_STR_EQ("size_t", token_str(parser, def->id));
    REQUIRE(def->type);
    struct DeclSpecs* defspecs = (struct DeclSpecs*)def->type;
    REQUIRE_EQ(AST_DECLSPEC, defspecs->kind.kind);
    REQUIRE_NULL(defspecs->name);
    REQUIRE_NULL(defspecs->type);
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
    if (parser) my_free(parser);
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
    REQUIRE_EQ(2, array_size(&parser->arr_exprs, sizeof(struct Expr*)));
    struct StmtDecls* decls = (struct StmtDecls*)((struct Expr**)parser->arr_exprs.data)[1];
    REQUIRE_EQ(STMT_DECLS, decls->kind.kind);
    REQUIRE_EQ(1, decls->extent);
    struct Decl* def = (struct Decl*)exprs[decls->offset];
    REQUIRE_EQ(AST_DECL, def->kind.kind);
    REQUIRE_STR_EQ("Array", token_str(parser, def->id));
    REQUIRE_NULL(def->type);
    REQUIRE(def->init && def->init->kind == STMT_BLOCK);
    struct StmtBlock* blk = (struct StmtBlock*)def->init;
    REQUIRE_EQ(3, blk->extent);
    // "size_t sz;"
    REQUIRE(exprs[blk->offset] && exprs[blk->offset]->kind == STMT_DECLS);
    struct StmtDecls* mem1 = (struct StmtDecls*)exprs[blk->offset];
    REQUIRE(mem1->extent == 1);
    REQUIRE(exprs[mem1->offset] && exprs[mem1->offset]->kind == AST_DECL);
    struct Decl* mem1def = (struct Decl*)exprs[mem1->offset];
    REQUIRE_STR_EQ("sz", token_str(parser, mem1def->id));
    REQUIRE(mem1def->type && mem1def->type->kind == AST_DECLSPEC);
    struct DeclSpecs* mem1specs = (struct DeclSpecs*)mem1def->type;
    REQUIRE(mem1specs->type);
    REQUIRE_STR_EQ("size_t", token_str(parser, mem1specs->type->id));

    rc = 0;
fail:
    if (parser) my_free(parser);
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
                       "struct Array arr2 = { .sz = 1 };"));

    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE_EQ(3, array_size(&parser->arr_exprs, sizeof(struct Expr*)));
    {
        struct StmtDecls* decls = (struct StmtDecls*)((struct Expr**)parser->arr_exprs.data)[1];
        REQUIRE_EQ(STMT_DECLS, decls->kind.kind);
        REQUIRE_EQ(1, decls->extent);
        struct Decl* def = (struct Decl*)exprs[decls->offset];
        REQUIRE_EQ(AST_DECL, def->kind.kind);
        REQUIRE_STR_EQ("arr", token_str(parser, def->id));
        REQUIRE(def->init && def->init->kind == AST_INIT);
        struct ASTInit* init = (struct ASTInit*)def->init;
        REQUIRE_EQ(3, init->extent);
        REQUIRE_EQ(EXPR_LIT, exprs[init->offset]->kind);
    }
    {
        struct StmtDecls* decls = (struct StmtDecls*)((struct Expr**)parser->arr_exprs.data)[2];
        struct Decl* def = (struct Decl*)exprs[decls->offset];
        REQUIRE(def->init && def->init->kind == AST_INIT);
        struct ASTInit* init = (struct ASTInit*)def->init;
        REQUIRE_EQ(1, init->extent);
        REQUIRE_EQ(EXPR_LIT, exprs[init->offset]->kind);
    }

    rc = 0;
fail:
    if (parser) my_free(parser);
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

    printf("%d tests. %d failed. %d assertions. %d failed.\n",
           state->tests,
           state->testfails,
           state->assertions,
           state->assertionfails);

    return state->testfails > 0 || state->assertionfails > 0;
}
