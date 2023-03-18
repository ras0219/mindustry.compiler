#define UINT32_MAX 2147483647U
#define INT32_MAX 2147483647
#define INT32_MIN (-INT32_MAX - 1)

void init_foo(int* f) __attribute__((nonnull))
{
    unsigned int i = 0;
    *f = 1;
    __prove(0 == i);
}

int bar(const int* f) __attribute__((nonnull)) { return *f; }

int foo(const int* f)
{
    if (f) return *f;
    return 0;
}

int main()
{
    const char* x = "hello";
    __prove(x);
    int f = 0;
    return foo(&f);
}
