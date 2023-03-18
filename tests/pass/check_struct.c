#define UINT32_MAX 2147483647U
#define INT32_MAX 2147483647
#define INT32_MIN (-INT32_MAX - 1)

#ifndef __ras0219_cc__
#define _Out_
#define _In_
#define _In_opt_
#define _Out_opt_
#endif

struct Foo
{
    int a;
    int b;
};

int foo(const struct Foo* f)
{
    if (f) return f->a;
    return 0;
}

int main()
{
    struct Foo f = {0};
    return foo(&f);
}
