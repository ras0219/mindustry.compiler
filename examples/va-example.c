#include "stdarg.h"
#include "stdio.h"

void h(const char* fmt, va_list v) { vprintf(fmt, v); }

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
