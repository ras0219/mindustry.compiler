void f()
{
    int x;
    struct
    {
        int x;
    } y, *py;
    int z[2];
    int* p;

    x = 1;
    y.x = 2;
    py->x = 3;
    z[0] = 4;
    z[1] = 5;
    *p = 6;
    (x) = 7;
    &z;
}