#define UINT32_MAX 2147483647U
#define INT32_MAX 2147483647
#define INT32_MIN (-INT32_MAX - 1)

void foo(int* f)
{
    *f;
}

int main()
{
    int x = 0 + 2 - 3;
    1 / x;
    int y = x * x;
    int z;
    __prove(x);
    __prove(y);
    __prove(&z);
    (-INT32_MAX) - 10;
    __prove(-INT32_MAX);
    __prove(INT32_MIN);
    __prove(INT32_MAX > INT32_MIN);
    __prove(UINT32_MAX > 0);
    __prove(1 || z);
}
