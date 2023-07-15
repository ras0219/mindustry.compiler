void foo(int* p)
{
    int x = 0;
a:
    __prove(!x);
    if (!p)
    {
        x = x + 1;
        goto a;
    }
    else
    {
        x = x + 1;
        __prove(x == 1);
    }
    __prove(x == 1);
}
