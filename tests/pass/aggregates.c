struct A
{
    int y;
    struct
    {
        int l;
        char c;
    };
};
int foo()
{
    struct A a = {.l = 1, .y = 0, {2}};
    a.l;
    (&a)->l;
    a.l = 5;
    (&a)->l = 8;
}
