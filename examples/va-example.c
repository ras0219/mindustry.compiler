#include "stdarg.h"
#include "stdio.h"

void h(const char* fmt, va_list v) { vprintf(fmt, v); }

void f(const char* fmt, va_list v3)
{
    printf("f()\n");
    fflush(NULL);
    printf("(%d,%d,%p,%p)\n", ((int*)v3)[0], ((int*)v3)[1], ((void**)v3)[1], ((void**)v3)[2]);
    printf("va_arg()=%p\n", va_arg(v3, char*));
    printf("(%d,%d,%p,%p)\n", ((int*)v3)[0], ((int*)v3)[1], ((void**)v3)[1], ((void**)v3)[2]);
    printf("va_arg()=%p\n", va_arg(v3, char*));
    printf("(%d,%d,%p,%p)\n", ((int*)v3)[0], ((int*)v3)[1], ((void**)v3)[1], ((void**)v3)[2]);
    printf("va_arg()=%d\n", va_arg(v3, int));
    printf("(%d,%d,%p,%p)\n", ((int*)v3)[0], ((int*)v3)[1], ((void**)v3)[1], ((void**)v3)[2]);
}

void g(const char* fmt, ...)
{
    va_list v;
    va_start(v, fmt);
    vprintf(fmt, v);
    va_end(v);

    va_list v2;
    va_start(v2, fmt);
    h(fmt, v2);
    va_end(v2);

    va_list v3;
    va_start(v3, fmt);
    printf("(%d,%d,%p,%p)\n", ((int*)v3)[0], ((int*)v3)[1], ((void**)v3)[1], ((void**)v3)[2]);
    printf("va_arg()=%p\n", va_arg(v3, char*));
    printf("(%d,%d,%p,%p)\n", ((int*)v3)[0], ((int*)v3)[1], ((void**)v3)[1], ((void**)v3)[2]);
    printf("va_arg()=%p\n", va_arg(v3, char*));
    printf("(%d,%d,%p,%p)\n", ((int*)v3)[0], ((int*)v3)[1], ((void**)v3)[1], ((void**)v3)[2]);
    printf("va_arg()=%d\n", va_arg(v3, int));
    printf("(%d,%d,%p,%p)\n", ((int*)v3)[0], ((int*)v3)[1], ((void**)v3)[1], ((void**)v3)[2]);
    va_end(v3);

    va_list v4;
    va_start(v4, fmt);
    f(fmt, v4);
    va_end(v4);
}

int main()
{
    printf("hello, world! %d\n", sizeof(int[10]));
    fflush(NULL);
    printf("hello, world! %d %c %zu\n", 10, '?', (size_t)-1);
    fflush(NULL);
    g("%s, %s, %d\n", "hello", "world", 5);
    fflush(NULL);
    return 0;
}
