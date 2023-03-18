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

void bar(struct Foo* f) __attribute__((nonnull))
{
    struct Foo g;
    f->a = 5;
    __prove(f->b);
}

int main()
{
    struct Foo f = {0};
    bar(&f);
    return foo(&f);
}
