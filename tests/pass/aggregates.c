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
}
