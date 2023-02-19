struct A
{
    int w[2];
};

int main()
{
    int x = 1 + -1 * 3;
    const int y = 3 + 1;
    int z = y + y;
    struct A a;
    int w[2];
    const int* p = &w;
    p = &a.w;

    const int q = y / 2;
    const int r = 1 == 1 != 1 || 1 && 1;
    if (x && 0)
    {
        int m = 5;
    }
    if (1 || 0)
    {
        int m = 5;
    }
}