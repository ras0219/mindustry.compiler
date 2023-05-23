typedef __builtin_va_list __gnu_va_list;
typedef __gnu_va_list va_list;

void h(const char* fmt, ...)
{
    va_list v, w;
    __builtin_va_start(v, fmt);
    __builtin_va_copy(w, v);
    __builtin_va_end(v);
}