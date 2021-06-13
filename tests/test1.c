#define _GNU_SOURCE 1

#include <stdio.h>
#include <string.h>

#include "fe.h"

int require_same_lines(const char* buf, size_t buf1sz, const char* str)
{
    int different = 0;
    for (int x = 0;; ++x)
    {
        const char* p = memchr(buf, '\n', buf1sz);
        const char* p2 = strchr(str, '\n');
        if (!p && !p2) return different;
        if (p && !p2)
        {
            int llen = p - buf;
            printf("%3d  Left: %.*s\n", x, llen, buf);
            different = 1;
        }
        if (p2 && !p)
        {
            int rlen = p2 - str;
            printf("%3d Right: %.*s\n", x, rlen, str);
            different = 1;
        }
        if (p2 && p)
        {
            int llen = p - buf;
            int rlen = p2 - str;
            if (llen != rlen || 0 != memcmp(buf, str, llen))
            {
                printf("%3d  Both: %.*s ||| %.*s\n", x, llen, buf, rlen, str);
                different = 1;
            }
        }
        if (p)
        {
            buf1sz -= p - buf + 1;
            buf = p + 1;
        }
        if (p2)
        {
            str = p2 + 1;
        }
    }
}

int test_compile(const char* testname, char* source, const char* binary)
{
    printf("Test: %s\n", testname);
    char outbuf[1024];
    FILE* fout = fmemopen(outbuf, sizeof(outbuf), "w");
    parser_clear_errors();
    FrontEnd fe;
    fe_init(&fe);
    fe.parser.cg.fdebug = NULL;
    fe.parser.cg.fout = fout;
    FILE* f = fmemopen(source, strlen(source), "r");
    int rc = fe_lex_file_opened(&fe, testname, f);
    fe_destroy(&fe);
    fclose(f);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    fflush(fout);
    long n = ftell(fout);
    rc = rc || require_same_lines(outbuf, n, binary);
    fclose(fout);
    return rc;
}

int main()
{
    test_compile("tests/test1.c",

                 "int main() { return 1; }",

                 "set reg1 1\n"
                 "jump 0 always\n");

    test_compile("tests/test2.c",

                 "int x[2];\n"
                 "#pragma memory memory1\n"
                 "void main() { x[0] = 10; x[1] = 11; }",

                 "write 10 memory1 0\n"
                 "write 11 memory1 1\n"
                 "jump 0 always\n");

    test_compile("tests/test3.c",

                 "#pragma memory memory1\n"
                 "int main() { int x; int *y = &x; return 5; }",

                 "set __stk__ 0\n"
                 "write __ebp__ memory1 __stk__\n"
                 "set __ebp__ __stk__\n"
                 "op add _3_y __ebp__ 1\n"
                 "set reg1 5\n"
                 "op sub __stk__ __ebp__ 1\n"
                 "read __ebp__ memory1 __ebp__\n"
                 "jump 0 always\n");

    test_compile("tests/test3b.c",

                 "#pragma memory memory1\n"
                 "int x;\n"
                 "int main() { int *y = &x; return 5; }",

                 "set _2_y 0\n"
                 "set reg1 5\n"
                 "jump 0 always\n");

    test_compile("tests/test4.c",

                 "#pragma memory memory1\n"
                 "int f(int x, int y) {\n"
                 "  return f(f(x-1, x-2), f(y-1, y-2));\n"
                 "}\n"
                 "void main() { f(0,0); }",

                 "set __stk__ 0\n"
                 // main
                 "set reg1 0\n"
                 "set reg2 0\n"
                 "op add ret 1 @counter\n"
                 "jump 6 always\n"
                 "jump 0 always\n"
                 // f(x,y)
                 "write ret memory1 __stk__\n"
                 "op add __stk__ __stk__ 1\n"
                 "write __ebp__ memory1 __stk__\n"
                 "set __ebp__ __stk__\n"
                 "op add _3 __ebp__ 1\n"
                 "set _2_x reg1\n"
                 "op add _5 __ebp__ 2\n"
                 "set _4_y reg2\n"
                 "op add _7 __ebp__ 3\n"
                 "op sub _6 _4_y 2\n"
                 "op sub reg1 _4_y 1\n"
                 "set reg2 _6\n"
                 "write _4_y memory1 _5\n"
                 "write _2_x memory1 _3\n"
                 "op add ret 1 @counter\n"
                 "jump 6 always\n"
                 "op add _9 __ebp__ 4\n"
                 "set _8 reg1\n"
                 "write _8 memory1 _9\n"
                 "op add _3 __ebp__ 1\n"
                 "read _2_x memory1 _3\n"
                 "op add _11 __ebp__ 5\n"
                 "op sub _10 _2_x 2\n"
                 "op sub reg1 _2_x 1\n"
                 "set reg2 _10\n"
                 "op add ret 1 @counter\n"
                 "jump 6 always\n"
                 "op add _9 __ebp__ 4\n"
                 "read reg2 memory1 _9\n"
                 "op add ret 1 @counter\n"
                 "jump 6 always\n"
                 "op sub __stk__ __ebp__ 1\n"
                 "read __ebp__ memory1 __ebp__\n"
                 "read @counter memory1 __stk__\n");
    return 0;
}
