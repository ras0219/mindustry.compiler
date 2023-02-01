typedef __builtin_va_list __gnu_va_list;
typedef __gnu_va_list va_list;
void g(const char* fmt, __builtin_va_list w) { int x = __builtin_va_arg(w, int); }
void f(const char* fmt, ...)
{
    va_list v;
    __builtin_va_start(v, fmt);
    g(fmt, v);
    int x = __builtin_va_arg(v, int);
    __builtin_va_end(v);
    f(0, 1, 2, 2, 2, 2, 2);
}