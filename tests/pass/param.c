int arr(int i[1]);
int arru(int i[]);
int valist(__builtin_va_list v);
int ptr(int* i);
int m(int i[1])
{
    int x[] = {1, 2};
    int* p = x;
    ptr(x);
    arru(x);
    arr(x);
    ptr(p);
    arru(p);
    arr(p);
    ptr(i);
    arru(i);
    arr(i);
}
