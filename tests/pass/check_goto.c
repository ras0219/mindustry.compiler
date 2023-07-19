void foo(int* p)
{
    int x = 0;
a:
    __prove(x >= 0);
    if (x < 10)
    {
        x = x + 1;
        p = 0;
        goto a;
    }
    else
    {
        x = x - 1;
        __prove(x >= 1);
    }
    __prove(x >= 1);
}
