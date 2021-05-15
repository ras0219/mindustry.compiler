#include <stdio.h>
#include <string.h>
#include <stddef.h>

#include "tok.h"

int usage()
{
    printf("Usage: mindustry.compiler <file.mlogp>\n");
}

char s_error_buffer[1024];

typedef struct
{
    Lexer lexer;
    Parser parser;
} FrontEnd;

int fe_on_token(Lexer *l)
{
    FrontEnd *self = (FrontEnd *)((char *)l - offsetof(FrontEnd, lexer));
    printf("TOK: %s\n", self->lexer.tok);
    return parse(&self->parser, &self->lexer);
}

void init_front_end(FrontEnd *fe)
{
    init_lexer(&fe->lexer, &fe_on_token);
    init_parser(&fe->parser);
}

static int lex_file(FILE *f, Lexer *l)
{
    s_error_buffer[0] = 0;
    int rc = 0;
    Buffer buf;
    while (1)
    {
        buf.sz = fread(buf.buf, 1, sizeof(buf.buf), f);
        if (!buf.sz)
            break;
        if (rc = lex(l, &buf))
            return rc;
    }
    return end_lex(l);
}

int main(int argc, const char *const *argv)
{
    if (argc != 2)
    {
        return usage();
    }
    FILE *f = fopen(argv[1], "r");
    if (!f)
    {
        char buf[128];
        snprintf(buf, 128, "%s: failed to open", argv[1]);
        perror(buf);
        return 1;
    }

    FrontEnd fe;
    init_front_end(&fe);
    int rc = lex_file(f, &fe.lexer);
    if (rc)
    {
        fprintf(stderr, "%s:%s", argv[1], s_error_buffer);
    }
    return rc;
}