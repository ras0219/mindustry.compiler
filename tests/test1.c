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

                 "set __stk__ 0\n"
                 "set ret 0\n"
                 "set _r_main 0\n"
                 "jump 4 always\n"
                 "set eax 1\n"
                 "jump 0 always\n");

    test_compile("tests/test2.c",

                 "int x[2];\n"
                 "#pragma memory memory1\n"
                 "void main() { x[0] = 10; x[1] = 11; }",

                 "set __stk__ 0\n"
                 "set ret 0\n"
                 "set _r_main 0\n"
                 "jump 4 always\n"
                 "write 10 memory1 0\n"
                 "write 11 memory1 1\n"
                 "jump 0 always\n");

    test_compile("tests/test2.c",

                 "#pragma memory memory1\n"
                 "int main() { int x; int *y = &x; return 5; }",

                 "set __stk__ 0\n"
                 "set ret 0\n"
                 "set _r_main 0\n"
                 "jump 4 always\n"
                 "write __ebp__ memory1 __stk__\n"
                 "set __ebp__ __stk__\n"
                 "op add _3_y __ebp__ 1\n"
                 "set eax 5\n"
                 "op sub __stk__ __ebp__ 1\n"
                 "read __ebp__ memory1 __ebp__\n"
                 "jump 0 always\n");
    return 0;
}
