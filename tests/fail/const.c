void foo()
{
    const int z = 5;
    const int *p = &z;
    z = 4;
    *p = 4;
    int * const q = &z;
    *q = 5;
}