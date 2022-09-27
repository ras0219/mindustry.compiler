#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "be.h"
#include "cg.h"
#include "dirent.h"
#include "elaborator.h"
#include "errors.h"
#include "lexstate.h"
#include "parse.h"
#include "parse_macros.h"
#include "path.h"
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
        const Sizing _require_s_e = (expected);                                                                        \
        const Sizing _require_s_a = (actual);                                                                          \
        REQUIRE_EQ_IMPL(                                                                                               \
            __FILE__, __LINE__, _require_s_e.width, #expected ".width", _require_s_a.width, #actual ".width");         \
        REQUIRE_EQ_IMPL(__FILE__,                                                                                      \
                        __LINE__,                                                                                      \
                        _require_s_e.is_signed,                                                                        \
                        #expected ".is_signed",                                                                        \
                        _require_s_a.is_signed,                                                                        \
                        #actual ".is_signed");                                                                         \
    } while (0)

static const Sizing s_sizing_int = {.width = 4, .is_signed = 1};
static const Sizing s_sizing_uint = {.width = 4};
static const Sizing s_sizing_schar = {.width = 1, .is_signed = 1};
static const Sizing s_sizing_uchar = {.width = 1};
static const Sizing s_sizing_ptr = {.width = 8};
static const Sizing s_sizing_sptr = {.width = 8, .is_signed = 1};

int unittest_require_typestr_eq_impl(struct TestState* state,
                                     const char* file,
                                     int line,
                                     const char* expected_str,
                                     const char* actual_str,
                                     TypeStr a,
                                     TypeStr b,
                                     const struct TypeTable* tt)
{
    int rc = 1;
    ++state->assertions;

    struct Array buf = {0};
    typestr_fmt(tt, &a, &buf);
    const size_t sep = buf.sz;
    typestr_fmt(tt, &b, &buf);

    REQUIRE_MEM_EQ_IMPL(
        file, line, expected_str, (char*)buf.data, sep, actual_str, (char*)buf.data + sep, buf.sz - sep);

    rc = 0;
fail:
    array_destroy(&buf);
    return rc;
}

#define REQUIRE_TYPESTR_EQ_IMPL(file, line, expected, expected_str, actual, actual_str)                                \
    do                                                                                                                 \
    {                                                                                                                  \
        if (unittest_require_typestr_eq_impl(                                                                          \
                state, file, line, expected_str, actual_str, (expected), (actual), test.elab->types))                  \
            goto fail;                                                                                                 \
    } while (0)

#define REQUIRE_TYPESTR_EQ(expected, actual)                                                                           \
    REQUIRE_TYPESTR_EQ_IMPL(__FILE__, __LINE__, expected, #expected, actual, #actual)

int test_preproc(struct TestState* state, struct Preprocessor* pp, const char* text)
{
    parser_clear_errors();
    REQUIREZ(preproc_text(pp, text));
    REQUIREZ(parser_has_errors());
    return 0;

fail:
    if (parser_has_errors())
    {
        parser_print_errors(stderr);
    }
    return 1;
}

int test_preproc_pass(struct TestState* state, const char* text)
{
    int rc = 1;
    struct Preprocessor* pp = preproc_alloc();
    SUBTEST(test_preproc(state, pp, text));
    rc = 0;
fail:
    preproc_free(pp);
    return rc;
}

int test_parse(struct TestState* state, struct Parser** parser, struct Preprocessor** pp, const char* text)
{
    parser_clear_errors();
    *parser = NULL;
    *pp = preproc_alloc();
    REQUIREZ(preproc_text(*pp, text));
    *parser = my_malloc(sizeof(struct Parser));
    parser_init(*parser);

    REQUIREZ(parser_parse(*parser, preproc_tokens(*pp), preproc_stringpool(*pp)));
    parser_debug_check(*parser);
    REQUIREZ(parser_has_errors());
    return 0;

fail:
    if (parser_has_errors()) parser_print_msgs(stderr), parser_clear_errors();
    return 1;
}

int test_parse_fail(struct TestState* state, const char* text)
{
    parser_clear_errors();
    struct Preprocessor* const pp = preproc_alloc();
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
    struct Preprocessor* const pp = preproc_alloc();
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
    struct BackEnd* be;
    struct CodeGen* cg;
} StandardTest;

static int stdtest_parse(struct TestState* state, StandardTest* test, const char* text)
{
    SUBTEST(test_parse(state, &test->parser, &test->pp, text));
    return 0;
fail:
    return 1;
}

static int stdtest_run(struct TestState* state, StandardTest* test, const char* text)
{
    int rc = 1;
    if (stdtest_parse(state, test, text)) goto fail;
    test->elab = my_malloc(sizeof(Elaborator));
    elaborator_init(test->elab, test->parser);
    if (elaborate(test->elab))
    {
        if (parser_has_errors()) parser_print_msgs(stderr), parser_clear_errors();
        REQUIRE_FAIL("%s", "elaborate failed, see above messages");
    }

    rc = 0;
fail:
    return rc;
}

static void stdtest_destroy(StandardTest* test)
{
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (test->elab)
    {
        elaborator_destroy(test->elab);
        my_free(test->elab);
    }
    if (test->parser) parser_destroy(test->parser), my_free(test->parser);
    if (test->pp) preproc_free(test->pp);
    if (test->be) be_destroy(test->be), my_free(test->be);
    if (test->cg) cg_destroy(test->cg), my_free(test->cg);
}

static int betest_run(struct TestState* state, StandardTest* test, const char* text)
{
    if (stdtest_run(state, test, text)) goto fail;
    test->be = my_malloc(sizeof(struct BackEnd));
    test->cg = my_malloc(sizeof(struct CodeGen));
    be_init(test->be, test->parser, test->elab, test->cg);
    cg_init(test->cg);

    void** seqs = test->parser->expr_seqs.data;
    SeqView top = test->parser->top->seq;
    REQUIRE(0 < top.ext);
    StmtDecls** top_seq = (void*)(seqs + top.off);
    for (size_t i = 0; i < top.ext; ++i)
    {
        StmtDecls* decls = top_seq[i];
        if (decls->specs->is_typedef) continue;
        for (size_t j = 0; j < decls->seq.ext; ++j)
        {
            Decl* decl = (Decl*)seqs[decls->seq.off + j];
            REQUIRE_EQ(0, be_compile_toplevel_decl(test->be, decl));
        }
    }
    return 0;
fail:
    return 1;
}

static int betest_run_cg(struct TestState* state, StandardTest* test, const char* text)
{
    if (stdtest_run(state, test, text)) return 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
    test->be = my_malloc(sizeof(struct BackEnd));
    be_init(test->be, test->parser, test->elab, test->cg);

    REQUIRE_EQ(0, be_compile(test->be));
    return 0;
fail:
    return 1;
}

int parse_main(struct TestState* state)
{
    int rc = 1;
    StandardTest test = {0};
    SUBTEST(stdtest_parse(state, &test, "int main() {}"));
    Parser* parser = test.parser;

    REQUIRE_EQ(1, parser->top->seq.ext);
    struct Expr** const exprs = parser->expr_seqs.data;
    struct StmtDecls* decls = (struct StmtDecls*)exprs[parser->top->seq.ext];
    REQUIRE_EQ(STMT_DECLS, decls->kind);
    REQUIRE_EQ(1, decls->seq.ext);
    struct Decl* main = (struct Decl*)((struct Expr**)parser->expr_seqs.data)[decls->seq.off];
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
    REQUIREZ(init->seq.ext);

    rc = 0;
fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_typedef(struct TestState* state)
{
    int rc = 1;
    struct Parser* parser;
    struct Preprocessor* pp;
    SUBTEST(test_parse(state, &parser, &pp, "typedef unsigned long size_t;\ntypedef unsigned long size_t;"));

    REQUIRE_EQ(2, parser->top->seq.ext);
    struct Expr** const exprs = parser->expr_seqs.data;
    struct StmtDecls* decls = (struct StmtDecls*)exprs[parser->top->seq.off];
    REQUIRE_EQ(STMT_DECLS, decls->kind);
    REQUIRE_EQ(1, decls->seq.ext);
    struct Decl* def = (struct Decl*)((struct Expr**)parser->expr_seqs.data)[decls->seq.off];
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
                       "} const a;\n"));

    struct Expr** const exprs = parser->expr_seqs.data;
    REQUIRE_EQ(2, parser->top->seq.ext);
    struct StmtDecls* decls = (struct StmtDecls*)exprs[parser->top->seq.off + 1];
    REQUIRE_EQ(STMT_DECLS, decls->kind);
    REQUIRE_EQ(1, decls->seq.ext);
    REQUIRE(decls->specs);
    struct DeclSpecs* def = decls->specs;
    REQUIRE_STR_EQ("Array", def->name);
    REQUIRE(def->is_struct);
    REQUIRE(def->suinit);
    REQUIRE(def->is_const);
    struct StmtBlock* blk = def->suinit;
    REQUIRE_EQ(3, blk->seq.ext);
    // "size_t sz;"
    REQUIRE(exprs[blk->seq.off] && exprs[blk->seq.off]->kind == STMT_DECLS);
    struct StmtDecls* mem1 = (struct StmtDecls*)exprs[blk->seq.off];
    REQUIRE(mem1->seq.ext == 1);
    REQUIRE(exprs[mem1->seq.off] && exprs[mem1->seq.off]->kind == AST_DECL);
    struct Decl* mem1def = (struct Decl*)exprs[mem1->seq.off];
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

int parse_strings(struct TestState* state)
{
    int rc = 1;
    StandardTest test = {0};
    SUBTEST(stdtest_run(state,
                        &test,
                        "#define f(x) #x\n"
                        "char str[] = f(==) \"hello\\n\";\n"));

    // from https://en.cppreference.com/w/c/language/initialization
    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(1, test.parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
        {
            REQUIRE_EQ(9, w->sym->size.width);
            REQUIRE_AST(ExprLit, e, w->init) { REQUIRE_MEM_EQ("==hello\n", 9, e->text, e->tok->tok_len + 1); }
        }
    }

    rc = 0;
fail:
    stdtest_destroy(&test);
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
    REQUIRE_EQ(4, parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decl1, exprs[parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decl1->seq.ext);
        REQUIRE_EXPR(Decl, def, exprs[decl1->seq.off])
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
    REQUIRE_EXPR(StmtDecls, decl1, exprs[parser->top->seq.off + 2])
    {
        REQUIRE_EQ(1, decl1->seq.ext);
        REQUIRE_EXPR(Decl, def, exprs[decl1->seq.off])
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

    REQUIRE_EXPR(StmtDecls, decl1, exprs[parser->top->seq.off + 3])
    {
        REQUIRE_EQ(1, decl1->seq.ext);
        REQUIRE_EXPR(Decl, def, exprs[decl1->seq.off])
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
    REQUIRE(0 < parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off])
    {
        REQUIRE(decls->specs);
        REQUIRE(decls->specs->suinit);
        REQUIRE_EQ(2, decls->specs->suinit->seq.ext);
        REQUIRE_EXPR(StmtDecls, addr_t_decls, exprs[decls->specs->suinit->seq.off])
        {
            REQUIRE_EQ(1, addr_t_decls->seq.ext);
            REQUIRE_EXPR(Decl, addr_t_decl, exprs[addr_t_decls->seq.off])
            {
                REQUIRE_PTR_EQ(addr_t_decl->sym, decls->specs->sym->first_member);
                REQUIRE_EQ(4, addr_t_decl->sym->align);
                REQUIRE_EQ(4, addr_t_decl->sym->size.width);
            }
        }
        REQUIRE(decls->specs->sym->first_member->next_field);
        REQUIRE_EXPR(StmtDecls, in_u_decls, exprs[decls->specs->suinit->seq.off + 1])
        {
            REQUIRE_EQ(1, in_u_decls->seq.ext);
            REQUIRE_EXPR(Decl, in_u_decl, exprs[in_u_decls->seq.off])
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
    REQUIRE_EQ(2, parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, a, exprs[decls->seq.off])
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
    REQUIRE_EQ(2, parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, a, exprs[decls->seq.off])
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
    SUBTEST(test_parse(state, &parser, &pp, "int a[3] = { 1, 2, 3 };\n"));

    elab = my_malloc(sizeof(Elaborator));
    elaborator_init(elab, parser);
    REQUIREZ(elaborate(elab));

    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE(0 < parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, a, exprs[decls->seq.off])
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
    REQUIRE_EQ(4, parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, a, exprs[decls->seq.off])
        {
            REQUIRE_AST(AstInit, a_init, a->init)
            {
                REQUIRE_SIZING_EQ(s_sizing_int, a_init->sizing);
                REQUIRE_EQ(0, a_init->offset);
            }
        }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off + 2])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, a, exprs[decls->seq.off])
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
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off + 3])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, a, exprs[decls->seq.off])
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
    REQUIRE_EQ(1, parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
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

int parse_unk_array(struct TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(stdtest_run(state,
                        test,
                        "char txt6[] = \"hello\";\n"
                        "struct Point { int x, y; } points5[] = {[3] = 1,2,3};\n"
                        "char txt7[] = {\"hello\"};\n"
                        "static const char mode[][5] = { \"EPRT\", \"PORT\" };\n"
                        "struct Foo { int a; int last[]; };\n"));
    Parser* const parser = test->parser;

    // from https://en.cppreference.com/w/c/language/initialization
    struct Expr** const exprs = (struct Expr**)parser->expr_seqs.data;
    REQUIRE_EQ(5, parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off]) { REQUIRE_EQ(6, w->sym->size.width); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
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
    return rc;
}

int parse_anon_decls(struct TestState* state)
{
    int rc = 1;
    StandardTest test = {0};
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
    StandardTest test = {0};
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
    StandardTest test = {0};
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
    REQUIRE_EQ(3, test.parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 2])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
        {
            REQUIRE_AST(StmtBlock, body, w->init)
            {
                REQUIRE_EQ(5, body->seq.ext);
                REQUIRE_EXPR(StmtDecls, decls, exprs[body->seq.off])
                {
                    REQUIRE_EQ(1, decls->seq.ext);
                    REQUIRE_EXPR(Decl, v, exprs[decls->seq.off])
                    {
                        REQUIRE_EQ(24, v->sym->size.width);
                        REQUIRE_EQ('_', v->sym->type.buf.buf[1]);
                    }
                }
                REQUIRE_EXPR(StmtDecls, decls, exprs[body->seq.off + 1])
                {
                    REQUIRE_EQ(1, decls->seq.ext);
                    REQUIRE_EXPR(Decl, v, exprs[decls->seq.off])
                    {
                        REQUIRE_EQ(24, v->sym->size.width);
                        REQUIRE_EQ('_', v->sym->type.buf.buf[1]);
                    }
                }
            }
        }
    }

fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_body(struct TestState* state)
{
    int rc = 1;
    StandardTest test = {0};
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
    StandardTest test = {0};
    SUBTEST(stdtest_run(state,
                        &test,
                        "char d1[sizeof(int)];\n"
                        "char d2[sizeof(d1)];\n"
                        "char d3[] = \"hello.\";\n"
                        "char d4[sizeof(d3)];\n"
                        "void main() { char d3[256]; int x = sizeof(d3); }"));
    rc = 0;

    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(5, test.parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, d, exprs[decls->seq.off]) { REQUIRE_SIZING_EQ(s_sizing_uint, d->sym->size); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, d, exprs[decls->seq.off]) { REQUIRE_SIZING_EQ(s_sizing_uint, d->sym->size); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 2])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, d, exprs[decls->seq.off]) { REQUIRE_EQ(7, d->sym->size.width); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 3])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, d, exprs[decls->seq.off]) { REQUIRE_EQ(7, d->sym->size.width); }
    }

fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_constants(struct TestState* state)
{
    int rc = 1;
    StandardTest test = {0};
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
                        "0L;"
                        "010;"
                        "1234'5678;"
                        "}\n"));
    rc = 0;

    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(2, test.parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, d, exprs[decls->seq.off]) { REQUIRE_EQ('\\', d->sym->size.width); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, d, exprs[decls->seq.off])
        {
            REQUIRE_AST(StmtBlock, b, d->init)
            {
                REQUIRE_EQ(14, b->seq.ext);
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off])
                {
                    REQUIRE_EQ(1, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE_DECIMAL, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 1])
                {
                    REQUIRE_EQ(0x7fffffff, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 2])
                {
                    REQUIRE_EQ(0x80000000, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_uint, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 3])
                {
                    REQUIRE_EQ(0x7fffffff, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_LL, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_sptr, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 4])
                {
                    REQUIRE_EQ(0x7fffffff, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_LLU, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_ptr, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 5])
                {
                    REQUIRE_EQ('\n', e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 6])
                {
                    REQUIRE_EQ('\t', e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 7])
                {
                    REQUIRE_EQ('\b', e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 8])
                {
                    REQUIRE_EQ('\r', e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 9])
                {
                    REQUIRE_EQ(0, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 10])
                {
                    REQUIRE_EQ(32, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 11])
                {
                    REQUIRE_EQ(0, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_L, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_sptr, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 12])
                {
                    REQUIRE_EQ(8, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
                REQUIRE_EXPR(ExprLit, e, exprs[b->seq.off + 13])
                {
                    REQUIRE_EQ(12345678, e->numeric);
                    REQUIRE_EQ(LIT_SUFFIX_NONE_DECIMAL, e->suffix);
                    REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);
                }
            }
        }
    }

fail:
    stdtest_destroy(&test);
    return rc;
}

int parse_implicit_conversion(struct TestState* state)
{
    int rc = 1;
    StandardTest test = {0};
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
    REQUIRE_EQ(1, test.parser->top->seq.ext);
    REQUIRE_AST(StmtDecls, decls, asts[test.parser->top->seq.off])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_AST(Decl, w, asts[decls->seq.off])
        {
            REQUIRE_AST(StmtBlock, body, w->init)
            {
                REQUIRE_EQ(6, body->seq.ext);

                REQUIRE_AST(ExprBinOp, e, asts[body->seq.off + 2])
                REQUIRE_SIZING_EQ(s_sizing_int, e->sizing);

                REQUIRE_AST(ExprBinOp, e, asts[body->seq.off + 3])
                REQUIRE_SIZING_EQ(s_sizing_sptr, e->sizing);

                REQUIRE_AST(ExprBinOp, e, asts[body->seq.off + 4])
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
    StandardTest test = {0};
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
    StandardTest test = {0};
    SUBTEST(stdtest_run(state,
                        &test,
                        "enum A { a1 = 5, a2, a3 = a2 + 20 };\n"
                        "typedef struct { enum A a; } W;\n"
                        "int main() {\n"
                        "enum A x = a3;\n"
                        "int y = (x == a2);\n"
                        "if ((unsigned int)y == a1)\n"
                        "  sizeof(enum A);\n"
                        "W w, *pw = &w;\n"
                        "pw->a = a1;\n"
                        "}\n"
                        "struct N { enum { N1 = 4 } e; };\n"
                        "char ch[N1];"));
    rc = 0;

    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(5, test.parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off])
    {
        REQUIRE_EQ(0, decls->seq.ext);
        REQUIRE(decls->specs->enum_init);
        REQUIRE_EQ(3, decls->specs->enum_init->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->specs->enum_init->seq.off])
        {
            REQUIRE_EQ(5, w->sym->enum_value);
            REQUIRE_PTR_EQ(w, w->sym->def);
            REQUIRE_SIZING_EQ(s_sizing_int, w->sym->size);
        }
        REQUIRE_EXPR(Decl, w, exprs[decls->specs->enum_init->seq.off + 1])
        {
            REQUIRE_EQ(6, w->sym->enum_value);
            REQUIRE_PTR_EQ(w, w->sym->def);
            REQUIRE_SIZING_EQ(s_sizing_int, w->sym->size);
        }
        REQUIRE_EXPR(Decl, w, exprs[decls->specs->enum_init->seq.off + 2])
        {
            REQUIRE_EQ(26, w->sym->enum_value);
            REQUIRE_PTR_EQ(w, w->sym->def);
            REQUIRE_SIZING_EQ(s_sizing_int, w->sym->size);
        }
    }

fail:
    stdtest_destroy(&test);
    return rc;
}

#define REQUIRE_TYPE_EQ(expected, actual)

int parse_typedefs(struct TestState* state)
{
    int rc = 1;
    StandardTest test = {0};
    SUBTEST(stdtest_run(state,
                        &test,
                        "typedef int X;\n"
                        "typedef X Y;"
                        "int main() {\n"
                        "Y y = 10;\n"
                        "}\n"));
    rc = 0;

    const TypeStr i = {.buf = {1, TYPE_BYTE_INT}};

    struct Expr** const exprs = (struct Expr**)test.parser->expr_seqs.data;
    REQUIRE_EQ(3, test.parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off]) { REQUIRE_TYPESTR_EQ(i, w->sym->type); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off]) { REQUIRE_TYPESTR_EQ(i, w->sym->type); }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 2])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
        {
            REQUIRE_AST(StmtBlock, body, w->init)
            {
                REQUIRE_EQ(1, body->seq.ext);
                REQUIRE_EXPR(StmtDecls, decls, exprs[body->seq.off])
                {
                    REQUIRE_EQ(1, decls->seq.ext);
                    REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
                    {
                        REQUIRE_TYPESTR_EQ(i, w->sym->type);
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
    StandardTest test = {0};
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
    REQUIRE_EQ(2, test.parser->top->seq.ext);
    REQUIRE_AST(StmtDecls, decls, asts[test.parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_AST(Decl, w, asts[decls->seq.off])
        REQUIRE_AST(StmtBlock, body, w->init)
        {
            REQUIRE_EQ(2, body->seq.ext);
            REQUIRE_AST(ExprField, f, asts[body->seq.off + 1])
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

typedef struct AstChecker
{
    TestState* const state;
    const Preprocessor* const pp;
    const Parser* const p;
    const Elaborator* const e;
    const char* const filename;
} AstChecker;

static const Token* expect_str(AstChecker* ctx, const Token* cur_tok, const char* id)
{
    const char* tokstr = preproc_stringpool(ctx->pp) + cur_tok->sp_offset;
    if (strcmp(tokstr, id) == 0)
        ++cur_tok;
    else
        PARSER_FAIL("error: expected token '%s' but got '%s'\n", id, tokstr);
fail:
    return cur_tok;
}
static const Token* expect_number(AstChecker* ctx, const Token* cur_tok, unsigned long long n)
{
    if (cur_tok->type != LEX_NUMBER) PARSER_FAIL("error: expected number '%llu'\n", n);
    const char* tokstr = preproc_stringpool(ctx->pp) + cur_tok->sp_offset;
    unsigned long long m = strtoull(tokstr, NULL, 10);
    if (n != m) PARSER_FAIL("error: expected '%llu' but got '%llu'\n", n, m);
    ++cur_tok;
fail:
    return cur_tok;
}

static const Token* test_ast_ast(AstChecker* ctx, const Token* cur_tok, const Ast* ast)
{
    if (cur_tok->type == TOKEN_SYM1('?'))
    {
        ++cur_tok;
    }
    else if (cur_tok->type == TOKEN_SYM1('('))
    {
        ++cur_tok;
        PARSER_DO(expect_str(ctx, cur_tok, ast_kind_to_string(ast->kind)));
        switch (ast->kind)
        {
            case STMT_BLOCK:
            {
                const struct StmtBlock* a = (void*)ast;
                const Ast* const* const asts = (const Ast**)ctx->p->expr_seqs.data + a->seq.off;
                for (size_t i = 0; i < a->seq.ext; ++i)
                {
                    PARSER_DO(test_ast_ast(ctx, cur_tok, asts[i]));
                }
                break;
            }
            case STMT_DECLS:
            {
                const StmtDecls* a = (void*)ast;
                PARSER_DO(test_ast_ast(ctx, cur_tok, &a->specs->ast));
                const Ast* const* const asts = (const Ast**)ctx->p->expr_seqs.data + a->seq.off;
                for (size_t i = 0; i < a->seq.ext; ++i)
                {
                    PARSER_DO(test_ast_ast(ctx, cur_tok, asts[i]));
                }
                break;
            }
            case AST_DECLSPEC:
            {
                const DeclSpecs* a = (void*)ast;
                if (a->tok) PARSER_DO(expect_str(ctx, cur_tok, token_str(ctx->p, a->tok)));
                if (a->name) PARSER_DO(expect_str(ctx, cur_tok, a->name));
                if (a->suinit)
                {
                    PARSER_DO(expect_str(ctx, cur_tok, "su"));
                    PARSER_DO(test_ast_ast(ctx, cur_tok, &a->suinit->ast));
                }
                if (a->enum_init)
                {
                    PARSER_DO(expect_str(ctx, cur_tok, "enum"));
                    PARSER_DO(test_ast_ast(ctx, cur_tok, &a->enum_init->ast));
                }
                break;
            }
            case AST_DECL:
            {
                const Decl* a = (void*)ast;
                if (a->tok) PARSER_DO(expect_str(ctx, cur_tok, token_str(ctx->p, a->tok)));
                if (a->type) PARSER_DO(test_ast_ast(ctx, cur_tok, &a->type->ast));
                if (a->init) PARSER_DO(test_ast_ast(ctx, cur_tok, a->init));
                break;
            }
            case AST_DECLARR:
            {
                const DeclArr* a = (void*)ast;
                if (a->arity) PARSER_DO(test_ast_ast(ctx, cur_tok, &a->arity->ast));
                PARSER_DO(test_ast_ast(ctx, cur_tok, &a->type->ast));
                break;
            }
            case AST_DECLFN:
            {
                const DeclFn* a = (void*)ast;
                if (a->is_param_list)
                {
                    PARSER_DO(expect_str(ctx, cur_tok, "param"));
                }
                else
                {
                    const Ast* const* const asts = (const Ast**)ctx->p->expr_seqs.data + a->seq.off;
                    for (size_t i = 0; i < a->seq.ext; ++i)
                    {
                        PARSER_DO(test_ast_ast(ctx, cur_tok, asts[i]));
                    }
                }
                if (a->is_varargs) PARSER_DO(expect_str(ctx, cur_tok, "..."));
                PARSER_DO(test_ast_ast(ctx, cur_tok, &a->type->ast));
                break;
            }
            case AST_DECLPTR:
            {
                const DeclPtr* a = (void*)ast;
                PARSER_DO(test_ast_ast(ctx, cur_tok, &a->type->ast));
                break;
            }
            case EXPR_LIT:
            {
                const ExprLit* a = (void*)ast;
                if (a->sym)
                {
                    // string literal
                }
                else
                {
                    PARSER_DO(expect_number(ctx, cur_tok, a->numeric));
                    if (a->suffix) PARSER_DO(expect_number(ctx, cur_tok, a->suffix));
                }
                break;
            }
            case EXPR_FIELD:
            {
                const ExprField* a = (void*)ast;
                if (a->tok) PARSER_DO(expect_str(ctx, cur_tok, token_str(ctx->p, a->tok)));
                if (a->fieldname) PARSER_DO(expect_str(ctx, cur_tok, a->fieldname));
                if (a->lhs) PARSER_DO(test_ast_ast(ctx, cur_tok, &a->lhs->ast));
                break;
            }
            case EXPR_REF:
            {
                const ExprRef* a = (void*)ast;
                PARSER_DO(expect_str(ctx, cur_tok, token_str(ctx->p, a->tok)));
                break;
            }
            case EXPR_CALL:
            {
                const ExprCall* a = (void*)ast;
                PARSER_DO(test_ast_ast(ctx, cur_tok, &a->fn->ast));
                const CallParam* const b = (const CallParam*)ctx->p->callparams.data + a->param_offset;
                for (size_t i = 0; i < a->param_extent; ++i)
                {
                    PARSER_DO(test_ast_ast(ctx, cur_tok, &b[i].expr->ast));
                }
                break;
            }
            case AST_INIT:
            {
                const AstInit* a = (void*)ast;
                while (a->init != NULL)
                {
                    PARSER_DO(expect_number(ctx, cur_tok, a->designator_offset));
                    PARSER_DO(expect_number(ctx, cur_tok, a->designator_extent));
                    PARSER_DO(test_ast_ast(ctx, cur_tok, a->init));
                    a = a->next;
                }
                break;
            }
            default: PARSER_FAIL("error: unknown ast type: %s\n", ast_kind_to_string(ast->kind));
        }
        if (cur_tok->type != TOKEN_SYM1(')'))
        {
            PARSER_FAIL("error: expected ')'\n");
        }
        ++cur_tok;
    }
    else
    {
        PARSER_FAIL("error: expected '(' %s\n", ast_kind_to_string(ast->kind));
    }
fail:
    return cur_tok;
}
static const Token* test_ast_top(AstChecker* ctx, const Token* cur_tok, const Ast* ast)
{
    PARSER_DO(test_ast_ast(ctx, cur_tok, ast));
    if (cur_tok->type != LEX_EOF)
    {
        PARSER_FAIL("error: expected eof\n");
    }
fail:
    return cur_tok;
}

static int test_ast(
    struct TestState* state, const Preprocessor* pp, Parser* parser, Elaborator* elab, const char* filename)
{
    AstChecker ctx = {.state = state, .pp = pp, .p = parser, .e = elab, .filename = filename};
    const Token* start = preproc_tokens(pp);
    const Token* cur_tok = test_ast_top(&ctx, start, &parser->top->ast);
    if (cur_tok == NULL)
    {
        parser_print_errors(stderr);
        REQUIRE_FAIL_IMPL(filename, 999, "failed to parse");
    }
    REQUIREZ(parser_has_errors());
    return 0;
fail:
    parser_clear_errors();
    return 1;
}

static int test_file(struct TestState* state, const char* path)
{
    int rc = 1;
    Array astfile = {0}, buf = {0};
    Preprocessor *pp = NULL, *ast_pp = NULL;
    Parser parser = {0};
    Elaborator elab = {0};
    FILE* f = fopen(path, "r");
    FILE* f2 = NULL;
    if (!f) REQUIRE_FAIL("failed to open test file %s", path);

    parser_clear_errors();
    pp = preproc_alloc();
    if (preproc_file(pp, f, path))
    {
        REQUIRE_FAIL_IMPL(path, 1, "%s", "failed to preprocess");
    }
    REQUIREZ(parser_parse(&parser, preproc_tokens(pp), preproc_stringpool(pp)));
    parser_debug_check(&parser);
    if (parser_has_errors())
    {
        REQUIRE_FAIL_IMPL(path, 1, "%s", "failed to parse");
    }

    elaborator_init(&elab, &parser);
    if (elaborate(&elab) || parser_has_errors())
    {
        REQUIRE_FAIL_IMPL(path, 1, "%s", "failed to elaborate");
    }

    array_appends(&astfile, path);
    array_push(&astfile, ".ast", 5);
    if (f2 = fopen(astfile.data, "rb"))
    {
        ast_pp = preproc_alloc();
        if (preproc_file(ast_pp, f2, astfile.data))
        {
            REQUIRE_FAIL_IMPL((char*)astfile.data, 1, "%s", "failed to preprocess");
        }
        if (test_ast(state, ast_pp, &parser, &elab, astfile.data)) goto fail;
    }
    rc = 0;
fail:
    if (parser_has_errors()) parser_print_msgs(stderr), parser_clear_errors();
    elaborator_destroy(&elab);
    parser_destroy(&parser);
    if (ast_pp) preproc_free(ast_pp);
    if (pp) preproc_free(pp);
    if (f2) fclose(f2);
    if (f) fclose(f);
    array_destroy(&buf);
    array_destroy(&astfile);
    return rc;
}

static int test_file_fail(struct TestState* state, const char* path)
{
    int rc = 1;
    Preprocessor* pp = NULL;
    Parser parser = {0};
    Elaborator elab = {0};
    FILE* f = fopen(path, "r");
    if (!f) REQUIRE_FAIL("failed to open test file %s", path);

    parser_clear_errors();
    pp = preproc_alloc();
    if (preproc_file(pp, f, path))
    {
        rc = 0;
        goto fail;
    }
    if (parser_parse(&parser, preproc_tokens(pp), preproc_stringpool(pp)))
    {
        rc = 0;
        goto fail;
    }
    parser_debug_check(&parser);
    if (parser_has_errors())
    {
        rc = 0;
        goto fail;
    }

    elaborator_init(&elab, &parser);
    if (elaborate(&elab))
    {
        rc = 0;
        goto fail;
    }
    REQUIRE_FAIL_IMPL(path, 1, "was expected to fail, but passed");

fail:
    parser_clear_errors();
    elaborator_destroy(&elab);
    parser_destroy(&parser);
    if (pp) preproc_free(pp);
    if (f) fclose(f);
    return rc;
}

static void foreach_c_file(struct TestState* state,
                           const char* subdir,
                           int (*cb)(struct TestState* state, const char* path))
{
    Array filebuf = {0};
    Array arr = {0};
    assign_path_join(&arr, g_datadir, g_datadir_sz, subdir, strlen(subdir));
    DIR* dir = opendir(arr.data);
    FILE* f = NULL;
    if (!dir)
    {
        fprintf(stderr, "error: opendir(): ");
        perror(arr.data);
        exit(1);
    }
    const size_t base_sz = arr.sz - 1;
    struct dirent* ent;
    while (ent = readdir(dir))
    {
#ifdef __APPLE__
        size_t len = ent->d_namlen;
#else
        size_t len = strlen(ent->d_name);
#endif
        if (len < 2) continue;

        // Only test files ending in .c
        if (ent->d_name[len - 2] != '.' || ent->d_name[len - 1] != 'c') continue;

        array_shrink(&arr, base_sz, 1);
        path_combine(&arr, ent->d_name, len);
        array_push_byte(&arr, '\0');

        ++state->tests;
        if (cb(state, arr.data)) ++state->testfails;
    }

    closedir(dir);
    if (f) fclose(f);
    array_destroy(&arr);
    array_destroy(&filebuf);
}

int parse_params(struct TestState* state)
{
    int rc = 1;
    StandardTest test = {0};
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
    REQUIRE_EQ(5, test.parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
        {
            array_clear(&arr);
            typestr_fmt(test.elab->types, &w->sym->type, &arr);
            array_push_byte(&arr, 0);
            REQUIRE_STR_EQ("function of (pointer to int) returning int", arr.data);
        }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
        {
            array_clear(&arr);
            typestr_fmt(test.elab->types, &w->sym->type, &arr);
            array_push_byte(&arr, 0);
            REQUIRE_STR_EQ("function of (pointer to int) returning int", arr.data);
        }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 2])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
        {
            array_clear(&arr);
            typestr_fmt(test.elab->types, &w->sym->type, &arr);
            array_push_byte(&arr, 0);
            REQUIRE_STR_EQ("function of (pointer to __builtin_va_list) returning int", arr.data);
        }
    }
    REQUIRE_EXPR(StmtDecls, decls, exprs[test.parser->top->seq.off + 4])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
        {
            array_clear(&arr);
            typestr_fmt(test.elab->types, &w->sym->type, &arr);
            array_push_byte(&arr, 0);
            REQUIRE_STR_EQ("function of (pointer to int) returning int", arr.data);
            REQUIRE_AST(DeclFn, fn, &w->type->ast)
            {
                REQUIRE_EQ(1, fn->seq.ext);
                REQUIRE_EXPR(StmtDecls, arg1s, exprs[fn->seq.off])
                {
                    REQUIRE_EQ(1, arg1s->seq.ext);
                    REQUIRE_EXPR(Decl, arg1, exprs[arg1s->seq.off])
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
    array_destroy(&arr);
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
    int rc = 1;
    struct Array buf_expected = {0};
    struct Array buf_actual = {0};
    debug_taca(&buf_expected, expected);
    debug_taca(&buf_actual, actual);
    REQUIRE_MEM_EQ_IMPL(
        file, line, "expected", buf_expected.data, buf_expected.sz, "actual", buf_actual.data, buf_actual.sz);
    rc = 0;
fail:
    array_destroy(&buf_expected);
    array_destroy(&buf_actual);
    return rc;
}

int require_tace(TestState* state, struct TACEntry* expected, struct TACEntry* actual, const char* file, int line)
{
    int rc = 0;
    rc |= require_taco(state, expected->op, "expected", actual->op, "actual", file, line - 3);
    INFO("arg1\n", 1) { rc |= require_taca(state, &expected->arg1, &actual->arg1, file, line - 2); }
    INFO("arg2\n", 2) { rc |= require_taca(state, &expected->arg2, &actual->arg2, file, line - 1); }
    return rc;
}
int require_tace2(
    TestState* state, StandardTest* test, struct TACEntry* expected, const char* file, int line, size_t* index)
{
    int rc = 0;
    if (*index < array_size(&test->be->code, sizeof(struct TACEntry)))
    {
        UNWRAP(require_tace(state, expected, (TACEntry*)test->be->code.data + (*index)++, file, line));
    }
    else
    {
        REQUIRE_FAIL("expected additional TACEntry (found %zu)", (size_t)*index);
    }
fail:
    return rc;
}

#define REQUIRE_NEXT_TACE(...)                                                                                         \
    do                                                                                                                 \
    {                                                                                                                  \
        struct TACEntry expected = __VA_ARGS__;                                                                        \
        if (require_tace2(state, test, &expected, __FILE__, __LINE__, &index)) goto fail;                              \
    } while (0)

#define REQUIRE_END_TACE() REQUIRE_ZU_EQ(index, array_size(&test->be->code, sizeof(struct TACEntry)))

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

static int require_next_text(TestState* state,
                             StandardTest* test,
                             const char* file,
                             unsigned line,
                             size_t* index,
                             const char* expected,
                             const char* expected_str)
{
    *index = start_of_line_text(*index, test->cg->code.data, test->cg->code.sz);
    if (*index < test->cg->code.sz)
    {
        size_t expected_len = strlen(expected);
        const char* actual = (const char*)test->cg->code.data + *index;
        size_t actual_len = end_of_line(*index, test->cg->code.data, test->cg->code.sz) - *index;
        REQUIRE_MEM_EQ_IMPL(file, line, expected_str, expected, expected_len, "actual", actual, actual_len);
        *index += actual_len;
        if (*index != test->cg->code.sz) ++*index;
    }
    else
    {
        REQUIRE_FAIL_IMPL(file, line, "%s", "expected more text.");
    }
    return 0;
fail:
    return 1;
}

#define REQUIRE_NEXT_TEXT(...)                                                                                         \
    do                                                                                                                 \
    {                                                                                                                  \
        if (require_next_text(state, test, __FILE__, __LINE__, &index, (__VA_ARGS__), #__VA_ARGS__)) goto fail;        \
    } while (0)

#define REQUIRE_END_TEXT()                                                                                             \
    do                                                                                                                 \
    {                                                                                                                  \
        index = start_of_line_text(index, test->cg->code.data, test->cg->code.sz);                                     \
        if (index < test->cg->code.sz)                                                                                 \
        {                                                                                                              \
            const char* actual = (const char*)test->cg->code.data + index;                                             \
            size_t actual_len = end_of_line(index, test->cg->code.data, test->cg->code.sz) - index;                    \
            REQUIRE_FAIL("expected end of text but found \"%.*s\"", (int)actual_len, actual);                          \
        }                                                                                                              \
    } while (0)

int test_be_simple(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run_cg(state, test, "int main() { return 42; }"));

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
    return rc;
}

int test_be_simple2(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run_cg(state, test, "int main(int argc, char** argv) { return argc; }"));

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
    return rc;
}

int test_be_simple3(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run_cg(state, test, "void main() { }"));

    size_t index = 0;
    REQUIRE_NEXT_TACE({TACO_RETURN});
    REQUIRE_END_TACE();

    index = 0;
    REQUIRE_NEXT_TEXT("_main:");
    REQUIRE_NEXT_TEXT("subq $24, %rsp");
    REQUIRE_NEXT_TEXT("addq $24, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_NEXT_TEXT("addq $24, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_be_relations(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run_cg(state,
                          test,
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

    return rc;
}

int test_be_arithmetic(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run_cg(state,
                          test,
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

    return rc;
}
int test_be_bitmath(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run_cg(state,
                          test,
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

    return rc;
}

int test_be_memory_ret(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
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

    return rc;
}

int test_be_nested_array(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "static const char mode[][5] = { \"AAAA\", \"BBBB\" };\n"
                       "const char* main() { return mode[0]; }"));

    size_t index = 0;

    REQUIRE_NEXT_TACE({
        TACO_ADD,
        .arg1 = {TACA_LNAME, .is_addr = 1, .name = "mode"},
        .arg2 = {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_LOAD,
        .arg1 = {TACA_FRAME, .sizing = 0, 8, .frame_offset = 0},
        .arg2 = {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        .arg1 = {TACA_REG, .is_addr = 1, .sizing = s_sizing_ptr, .reg = REG_RAX},
        .arg2 = {TACA_FRAME, .sizing = 0, 8, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({TACO_RETURN});

    REQUIRE_END_TACE();

    rc = 0;
fail:

    return rc;
}

int test_be_cast(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "void main() {"
                       " char ch;"
                       " int x = (int)ch++;"
                       " int y = 2 + (int)ch;"
                       " unsigned z = (unsigned char)(char)-1;"
                       "}"));
    size_t index = 0;
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
        TACO_ADD,
        {TACA_REF, .sizing = s_sizing_schar, .ref = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .param_offset = 4},
        {TACA_REF, .sizing = s_sizing_int, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
        {TACA_REF, .sizing = s_sizing_int, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .param_offset = 8},
        {TACA_REF, .sizing = s_sizing_int, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_SUB,
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_uint, .param_offset = 12},
        {TACA_REF, .sizing = s_sizing_uchar, .ref = index - 1},
    });
    REQUIRE_END_TACE();

    rc = cg_gen_taces(test->cg, test->be->code.data, array_size(&test->be->code, sizeof(TACEntry)), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    index = 0;
    REQUIRE_NEXT_TEXT("_main:");
    REQUIRE_NEXT_TEXT("subq $120, %rsp");
    REQUIRE_NEXT_TEXT("movsb 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsb 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $1, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 112(%rsp)");

    REQUIRE_NEXT_TEXT("movsb 112(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11b, 0(%rsp)");

    REQUIRE_NEXT_TEXT("movsb 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11d, 4(%rsp)");

    REQUIRE_NEXT_TEXT("movsb 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $2, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11d, 8(%rsp)");

    REQUIRE_NEXT_TEXT("mov $0, %r11");
    REQUIRE_NEXT_TEXT("subq $1, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movzb 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11d, 12(%rsp)");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:

    return rc;
}

int test_be_call(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "struct A { char buf[32]; };\n"
                       "struct A mm_call(struct A a);"
                       "struct A mi_call(int a);"
                       "int im_call(struct A a);"
                       "int ii_call(int a);\n"
                       "extern void (*f)();"
                       "void main() {\n"
                       " ii_call(10);\n"
                       " im_call(mm_call(mi_call(5)));\n"
                       " f();\n"
                       "}"));
    size_t index = 0;

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
    // f();
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .sizing = 0, 8, .name = "f"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_END_TACE();

    rc = 0;
fail:

    return rc;
}

int test_be_call2(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "void cg_debug(struct CodeGen* cg, const char* fmt, ...);"
                       "void cg_declare_extern(struct CodeGen* cg, const char* sym)"
                       "{"
                       "cg_debug(cg, \"   : %s\\n\", sym);"
                       "}"));
    size_t index = 0;

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

    return rc;
}

int test_be_call3(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "struct Y { char buf[20]; };"
                       "void f(int x[1], struct Y y);"
                       "void g(int x[1], struct Y y1)"
                       "{"
                       " struct Y y;"
                       " f(x, y);"
                       "}"));
    size_t index = 0;

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

    return rc;
}

int test_be_got(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "void (*g)();\n"
                       "extern void h();\n"
                       "extern void (*i)();\n"
                       "void f() {\n"
                       " f();\n"
                       " g();\n"
                       " h();\n"
                       " i();\n"
                       "\n"
                       " g = f;\n"
                       " g = g;\n"
                       " g = h;\n"
                       " g = i;\n"
                       "}\n"));
    size_t index = 0;

    // prologue;
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_LNAME, .is_addr = 1, .name = "f"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_LNAME, .sizing = 0, 8, .name = "g"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .is_addr = 1, .name = "h"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_CALL,
        {TACA_NAME, .sizing = 0, 8, .name = "i"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
    });

    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_LNAME, .is_addr = 1, .sizing = 0, 8, .name = "g"},
        {TACA_LNAME, .is_addr = 1, .name = "f"},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_LNAME, .is_addr = 1, .sizing = 0, 8, .name = "g"},
        {TACA_LNAME, .sizing = 0, 8, .name = "g"},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_LNAME, .is_addr = 1, .sizing = 0, 8, .name = "g"},
        {TACA_NAME, .is_addr = 1, .name = "h"},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_LNAME, .is_addr = 1, .sizing = 0, 8, .name = "g"},
        {TACA_NAME, .sizing = 0, 8, .name = "i"},
    });
    REQUIRE_END_TACE();

    rc = 0;
fail:

    return rc;
}

int test_be_va_args(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
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
                       "}\n"));
    size_t index = 0;

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
        {TACA_LNAME, .is_addr = 1, .name = "f"},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
    });
    REQUIRE_END_TACE();

    rc = cg_gen_taces(test->cg, test->be->code.data, array_size(&test->be->code, sizeof(TACEntry)), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    index = 0;

    REQUIRE_NEXT_TEXT("_f:");
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    // prologue
    REQUIRE_NEXT_TEXT("mov %rdi, 8(%rsp)");
    REQUIRE_NEXT_TEXT("mov %rsi, 16(%rsp)");
    REQUIRE_NEXT_TEXT("mov %rdx, 24(%rsp)");
    REQUIRE_NEXT_TEXT("mov %rcx, 32(%rsp)");
    REQUIRE_NEXT_TEXT("mov %r8, 40(%rsp)");
    REQUIRE_NEXT_TEXT("mov %r9, 48(%rsp)");

    // va_start
    REQUIRE_NEXT_TEXT("movl $8, 56(%rsp)");
    REQUIRE_NEXT_TEXT("movl $48, 60(%rsp)");
    REQUIRE_NEXT_TEXT("leaq 128(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 64(%rsp)");
    REQUIRE_NEXT_TEXT("leaq 8(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 72(%rsp)");

    // g(v,)
    REQUIRE_NEXT_TEXT("mov 8(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("leaq 56(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _g");

    // va_arg
    REQUIRE_NEXT_TEXT("movsl 56(%rsp), %rax");
    REQUIRE_NEXT_TEXT("cmp $48, %eax");
    REQUIRE_NEXT_TEXT("setl %al");
    REQUIRE_NEXT_TEXT("movzx %al, %rax");
    REQUIRE_NEXT_TEXT("mov %rax, 112(%rsp)");
    REQUIRE_NEXT_TEXT("mov 112(%rsp), %rax");
    REQUIRE_NEXT_TEXT("cmp $0, %rax");
    REQUIRE_NEXT_TEXT("jz  L$2");
    REQUIRE_NEXT_TEXT("mov 72(%rsp), %r11");
    REQUIRE_NEXT_TEXT("movsl 56(%rsp), %r10");
    REQUIRE_NEXT_TEXT("add %r10, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 112(%rsp)");

    REQUIRE_NEXT_TEXT("mov 112(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("leaq 84(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movsd");

    REQUIRE_NEXT_TEXT("movsl 56(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $8, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 112(%rsp)");
    REQUIRE_NEXT_TEXT("movsl 112(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11d, 56(%rsp)");
    REQUIRE_NEXT_TEXT("jmp L$1");
    REQUIRE_NEXT_TEXT("L$2:");
    REQUIRE_NEXT_TEXT("mov 64(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("leaq 84(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movsd");

    REQUIRE_NEXT_TEXT("mov 64(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $8, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 112(%rsp)");
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

    return rc;
}

int test_be_va_args2(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "void f(__builtin_va_list v) {\n"
                       "int x = __builtin_va_arg(v, int);"
                       "}\n"));
    size_t index = 0;

    // prologue;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 0},
        {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RDI},
    });

    // va_arg(, int)
    REQUIRE_NEXT_TACE({
        TACO_LOAD,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 16},
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 0},
    });
    REQUIRE_NEXT_TACE({
        TACO_LT,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 16},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 48},
    });
    REQUIRE_NEXT_TACE({
        TACO_BRZ,
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
        {TACA_ALABEL, .alabel = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .frame_offset = 16},
    });
    REQUIRE_NEXT_TACE({
        TACO_LOAD,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 24},
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 24},
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 16},
    });
    REQUIRE_NEXT_TACE({
        TACO_LOAD,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 12},
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 16},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 8},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 0},
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
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 8},
    });
    REQUIRE_NEXT_TACE({
        TACO_LOAD,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 32},
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LOAD,
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 12},
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 32},
    });
    REQUIRE_NEXT_TACE({
        TACO_ADD,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 32},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 8},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 4},
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_LABEL,
        {TACA_ALABEL, .alabel = 1},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 8},
        {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 12},
    });
    REQUIRE_END_TACE();

    rc = cg_gen_taces(test->cg, test->be->code.data, array_size(&test->be->code, sizeof(TACEntry)), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    index = 0;

    REQUIRE_NEXT_TEXT("_f:");
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    // prologue
    REQUIRE_NEXT_TEXT("mov %rdi, 0(%rsp)");

    // va_arg
    REQUIRE_NEXT_TEXT("mov 0(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("leaq 16(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movsd");

    REQUIRE_NEXT_TEXT("movsl 16(%rsp), %rax");
    REQUIRE_NEXT_TEXT("cmp $48, %eax");
    REQUIRE_NEXT_TEXT("setl %al");
    REQUIRE_NEXT_TEXT("movzx %al, %rax");
    REQUIRE_NEXT_TEXT("mov %rax, 104(%rsp)");
    REQUIRE_NEXT_TEXT("mov 104(%rsp), %rax");
    REQUIRE_NEXT_TEXT("cmp $0, %rax");
    REQUIRE_NEXT_TEXT("jz  L$2");
    REQUIRE_NEXT_TEXT("mov 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $16, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("mov 104(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("leaq 24(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movsq");

    REQUIRE_NEXT_TEXT("mov 24(%rsp), %r11");
    REQUIRE_NEXT_TEXT("movsl 16(%rsp), %r10");
    REQUIRE_NEXT_TEXT("add %r10, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");
    REQUIRE_NEXT_TEXT("mov 104(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("leaq 12(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movsd");

    REQUIRE_NEXT_TEXT("movsl 16(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $8, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");
    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov 0(%rsp), %r10");
    REQUIRE_NEXT_TEXT("mov %r11d, (%r10)");
    REQUIRE_NEXT_TEXT("jmp L$1");
    REQUIRE_NEXT_TEXT("L$2:");

    REQUIRE_NEXT_TEXT("mov 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $8, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("mov 104(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("leaq 32(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movsq");

    REQUIRE_NEXT_TEXT("mov 32(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("leaq 12(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movsd");

    REQUIRE_NEXT_TEXT("mov 32(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $8, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 112(%rsp)");
    REQUIRE_NEXT_TEXT("mov 112(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov 104(%rsp), %r10");
    REQUIRE_NEXT_TEXT("mov %r11, (%r10)");
    REQUIRE_NEXT_TEXT("L$1:");

    REQUIRE_NEXT_TEXT("movsl 12(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11d, 8(%rsp)");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:

    return rc;
}

int test_be_conversions(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "void cg_declare_extern()"
                       "{"
                       "char x = 10;"
                       "x = 11;"
                       "int y = x;"
                       "y = x;"
                       "char* px = &(x += 1);"
                       "x++;"
                       "char h[] = \"he\\\\\\\\\\\\\";"
                       "char *p = h;"
                       "unsigned long long z = x * 2;"
                       "unsigned long long w = z * 2;"
                       "}"));
    size_t index = 0;

    // char x = 10;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 10},
    });
    // x = 11;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 11},
    });
    // int y = x;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 4},
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
    });
    // y = x;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 4},
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
    });
    // char* px = &(x += 1);
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
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 8},
        {TACA_FRAME, .is_addr = 1, .frame_offset = 0},
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
        {TACA_FRAME, .is_addr = 1, .sizing = 0, 6, .frame_offset = 16},
        {TACA_CONST, .sizing = 0, 6, .const_idx = 0},
    });
    // p = h;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 24},
        {TACA_FRAME, .is_addr = 1, .frame_offset = 16},
    });
    // z = x * 2;
    REQUIRE_NEXT_TACE({
        TACO_MULT,
        {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 32},
        {TACA_REF, .sizing = s_sizing_int, .ref = index - 1},
    });
    // w = z * 2;
    REQUIRE_NEXT_TACE({
        TACO_MULT,
        {TACA_FRAME, .sizing = s_sizing_ptr, .frame_offset = 32},
        {TACA_IMM, .sizing = s_sizing_int, .imm = 2},
    });
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 40},
        {TACA_REF, .sizing = s_sizing_ptr, .ref = index - 1},
    });
    REQUIRE_END_TACE();

    rc = 0;
fail:

    return rc;
}

int test_be_init(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
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
    size_t index = 0;

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

    return rc;
}

int test_be_switch(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "int f(int x) {\n"
                       "switch (x) {"
                       "case 1: return 10;"
                       "case 2: return 20;"
                       "default: return 30;"
                       "}}"));
    size_t index = 0;

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

    return rc;
}

int test_be_ternary(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "int f(int x) {\n"
                       "return x == 1 ? 10 : 20;"
                       "}"));
    size_t index = 0;

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

    return rc;
}

int test_be_fnstatic(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run_cg(state, test, "int main() { static const int x = 42; return x; }"));

    size_t index = 0;
    REQUIRE_NEXT_TACE({
        TACO_ASSIGN,
        {TACA_REG, .is_addr = 1, .sizing = s_sizing_int, .reg = REG_RAX},
        {TACA_LNAME, .sizing = s_sizing_int, .name = "x$1"},
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

    return rc;
}

int test_cg_assign(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
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
            {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_int, .frame_offset = 4},
            {TACA_FRAME, .sizing = s_sizing_schar, .frame_offset = 0},
        },
        // small odd size load
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing = s_sizing_ptr, .frame_offset = 4},
            {TACA_FRAME, .sizing = 0, 3, .frame_offset = 0},
        },
        // small odd size store
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .sizing = 0, 3, .frame_offset = 4},
            {TACA_FRAME, .sizing = 0, 8, .frame_offset = 0},
        },
        // high reg use
        {
            TACO_ASSIGN,
            {TACA_NAME, .sizing = 0, 3, .name = "out"},
            {TACA_NAME, .sizing = 0, 3, .name = "in"},
        },
        // high reg use 2
        {
            TACO_ASSIGN,
            {TACA_NAME, .is_addr = 1, .sizing = 0, 3, .name = "out"},
            {TACA_NAME, .sizing = 0, 3, .name = "in"},
        },
    };
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
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

    REQUIRE_NEXT_TEXT("mov %ebx, (%rax)");

    REQUIRE_NEXT_TEXT("leaq 9(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, (%rax)");

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

    // small odd size load
    REQUIRE_NEXT_TEXT("movzw 8(%rsp), %r11");
    REQUIRE_NEXT_TEXT("movzb 10(%rsp), %r10");
    REQUIRE_NEXT_TEXT("shl $16, %r10");
    REQUIRE_NEXT_TEXT("or %r10, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 12(%rsp)");

    // small odd size store
    REQUIRE_NEXT_TEXT("movzw 8(%rsp), %r11");
    REQUIRE_NEXT_TEXT("movzb 10(%rsp), %r10");
    REQUIRE_NEXT_TEXT("shl $16, %r10");
    REQUIRE_NEXT_TEXT("or %r10, %r11");

    REQUIRE_NEXT_TEXT("mov 12(%rsp), %r10");
    REQUIRE_NEXT_TEXT("mov %r11w, (%r10)");
    REQUIRE_NEXT_TEXT("mov %r11, %rax");
    REQUIRE_NEXT_TEXT("shr $16, %rax");
    REQUIRE_NEXT_TEXT("mov %al, 2(%r10)");

    // high reg use
    REQUIRE_NEXT_TEXT("movq _out@GOTPCREL(%rip), %rax");
    REQUIRE_NEXT_TEXT("movq _in@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("leaq (%r11), %rsi");
    REQUIRE_NEXT_TEXT("mov (%rax), %rdi");
    REQUIRE_NEXT_TEXT("mov $3, %rcx");
    REQUIRE_NEXT_TEXT("cld");
    REQUIRE_NEXT_TEXT("rep movsb");

    // high reg use
    REQUIRE_NEXT_TEXT("movq _in@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("leaq (%r11), %rsi");
    REQUIRE_NEXT_TEXT("mov _out@GOTPCREL(%rip), %rdi");
    REQUIRE_NEXT_TEXT("mov $3, %rcx");
    REQUIRE_NEXT_TEXT("cld");
    REQUIRE_NEXT_TEXT("rep movsb");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_cg_refs(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
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
        {
            TACO_ASSIGN,
            {TACA_NAME, .is_addr = 1, .sizing = 0, 8, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 10},
        },
        {
            TACO_ASSIGN,
            {TACA_NAME, .is_addr = 1, .sizing = 0, 8, .name = "a"},
            {TACA_FRAME, .sizing = s_sizing_int, .frame_offset = 0},
        },
        {
            TACO_CALL,
            {TACA_NAME, .is_addr = 1, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        },
        {
            TACO_CALL,
            {TACA_LNAME, .is_addr = 1, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        },
        {
            TACO_CALL,
            {TACA_NAME, .sizing = 0, 8, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        },
        {
            TACO_CALL,
            {TACA_LNAME, .sizing = 0, 8, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        },
    };
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("leaq _a(%rip), %r11");
    REQUIRE_NEXT_TEXT("movsl _b(%rip), %r10");
    REQUIRE_NEXT_TEXT("add %r10, %r11");

    REQUIRE_NEXT_TEXT("movq _b@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("mov _a@GOTPCREL(%rip), %r10");
    REQUIRE_NEXT_TEXT("movsl (%r11), %rax");
    REQUIRE_NEXT_TEXT("add %rax, %r10");

    REQUIRE_NEXT_TEXT("mov _a@GOTPCREL(%rip), %rdi");
    REQUIRE_NEXT_TEXT("movq $10, (%rdi)");

    REQUIRE_NEXT_TEXT("movsl 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov _a@GOTPCREL(%rip), %r10");
    REQUIRE_NEXT_TEXT("mov %r11, (%r10)");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _a");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _a");
    REQUIRE_NEXT_TEXT("movq _a@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *(%r11)");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *_a(%rip)");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_cg_regalloc(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
    TACEntry taces[] = {
        {
            TACO_BAND,
            {TACA_NAME, .sizing = 1, 8, .name = "a"},
            {TACA_REF, .sizing = s_sizing_int, .ref = 0},
        },
    };
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("movq _a@GOTPCREL(%rip), %rax");
    REQUIRE_NEXT_TEXT("mov (%rax), %r11");
    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %rdx");
    REQUIRE_NEXT_TEXT("andq %rdx, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_be_static_init(TestState* state, StandardTest* test)
{
    int rc = 1;
    SUBTEST(betest_run(state,
                       test,
                       "enum {v1 = 2};"
                       "static int data[] = {1,v1,3};\n"
                       "static const char* const s_reg_names[] = {\"%rax\", \"%rbx\"};\n"
                       "extern void bar() {}\n"
                       "static void foo() {}\n"
                       "static void* ptrs[] = { foo, bar };\n"));

    struct Expr** const exprs = (struct Expr**)test->parser->expr_seqs.data;
    REQUIRE_EQ(6, test->parser->top->seq.ext);
    REQUIRE_EXPR(StmtDecls, decls, exprs[test->parser->top->seq.off + 1])
    {
        REQUIRE_EQ(1, decls->seq.ext);
        REQUIRE_EXPR(Decl, w, exprs[decls->seq.off])
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

    array_push_byte(&test->cg->data, '\0');

    REQUIRE_LINES(test->cg->data.data)
    {
        REQUIRE_LINE(".p2align 3");
        REQUIRE_LINE("_data:");
        REQUIRE_LINE(".byte 1, 0, 0, 0, 2, 0, 0, 0");
        REQUIRE_LINE(".byte 3, 0, 0, 0");
        REQUIRE_LINE("");
        REQUIRE_LINE(".p2align 3");
        REQUIRE_LINE("_s_reg_names:");
        REQUIRE_LINE(".quad L_.S0 + 0");
        REQUIRE_LINE(".quad L_.S1 + 0");
        REQUIRE_LINE("");
        REQUIRE_LINE(".p2align 3");
        REQUIRE_LINE("_ptrs:");
        REQUIRE_LINE(".quad _foo + 0");
        REQUIRE_LINE(".quad _bar + 0");
        REQUIRE_LINE("");
    }

    rc = 0;
fail:

    return rc;
}

int test_cg_call(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
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
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _f");

    REQUIRE_NEXT_TEXT("movq _f@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *(%r11)");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _f");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *_f(%rip)");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_cg_add(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
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
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("leaq 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $7, %r11");

    REQUIRE_NEXT_TEXT("movsl 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $7, %r11");

    REQUIRE_NEXT_TEXT("movsb 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $7, %r11");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_cg_bitmath(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
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
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("leaq 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("andq $7, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("orq $7, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("xorq $7, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %rax");
    REQUIRE_NEXT_TEXT("not %rax");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int main(int argc, char** argv)
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

    if (argc == 0)
        abort();
    else if (argc == 1)
    {
        fprintf(stderr, "No data dir specified.\n The first parameter to %s must be the data dir.\n", argv[0]);
        exit(1);
    }
    else
    {
        g_datadir = argv[1];
        g_datadir_sz = strlen(argv[1]);
    }

    typedef int (*stdtest_t)(struct TestState*, struct StandardTest*);

    static const stdtest_t stdtests[] = {
        parse_unk_array,   test_be_static_init, test_be_simple,      test_be_simple2,    test_be_simple3,
        test_be_relations, test_be_arithmetic,  test_be_bitmath,     test_be_memory_ret, test_be_nested_array,
        test_be_cast,      test_be_call,        test_be_call2,       test_be_call3,      test_be_got,
        test_be_va_args,   test_be_va_args2,    test_be_conversions, test_be_init,       test_be_switch,
        test_be_ternary,   test_be_fnstatic,    test_cg_assign,      test_cg_call,       test_cg_add,
        test_cg_bitmath,   test_cg_refs,        test_cg_regalloc,
    };

    static int (*const othertests[])(struct TestState*) = {
        parse_initializer_struct,
    };

    for (size_t i = 0; i < sizeof(stdtests) / sizeof(stdtests[0]); ++i)
    {
        struct StandardTest test = {0};
        state->tests++;
        if (stdtests[i](state, &test))
        {
            state->testfails++;
        }
        stdtest_destroy(&test);
    }
    for (size_t i = 0; i < sizeof(othertests) / sizeof(othertests[0]); ++i)
    {
        state->tests++;
        if (othertests[i](state))
        {
            state->testfails++;
        }
    }

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
    RUN_TEST(parse_anon_decls);
    RUN_TEST(parse_decls_and_defs);
    RUN_TEST(parse_uuva_list);
    RUN_TEST(parse_implicit_conversion);
    RUN_TEST(parse_params);
    RUN_TEST(parse_enums);
    RUN_TEST(parse_typedefs);
    RUN_TEST(parse_aggregates);

    foreach_c_file(state, "tests/pass", test_file);
    foreach_c_file(state, "tests/fail", test_file_fail);

    const char* color = (state->testfails + state->assertionfails == 0) ? _state.colorsuc : _state.colorerr;

    printf("%s%d tests. %d failed. %d assertions. %d failed.%s\n",
           color,
           state->tests,
           state->testfails,
           state->assertions,
           state->assertionfails,
           _state.colorreset);

    array_destroy(&state->info);
    array_destroy(&state->stack);

    return state->testfails > 0 || state->assertionfails > 0;
}
