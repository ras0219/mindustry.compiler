#define UINT32_MAX 2147483647U
#define INT32_MAX 2147483647
#define INT32_MIN (-INT32_MAX - 1)

void init_foo(int* f) __attribute__((nonnull)) { *f = 1; }

int bar(const int* f) __attribute__((nonnull)) { return *f; }

int foo(const int* f)
{
    if (f) return *f;
    return 0;
}

int main()
{
    // bar(0);
    int f = 0;
    return foo(&f);
}
