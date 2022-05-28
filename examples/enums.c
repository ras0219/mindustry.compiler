#include "stdarg.h"
#include "stdio.h"

enum E
{
    A,
    B,
    C = 5
};

struct A
{
    unsigned int z : 1;
    unsigned int s : 8;
    unsigned int y : 1;
};

int main()
{
    int n = B;
    switch (n)
    {
        case A: puts("A"); break;
        case B: puts("B"); break;
        case C: puts("C"); break;
        default: puts("default\n"); break;
    }

    printf("hello, world! %d %d %d\n", A, B, C);
    fflush(NULL);

    struct A a, *p;
    p = &a;
    a.s = B;

    printf("hello, world! %d %d\n", a.s, p->s);
    fflush(NULL);
    return 0;
}
