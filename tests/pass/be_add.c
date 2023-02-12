void main(void)
{
    long long ll = 0;
    int i = 0;
    short s = 0;
    signed char c = 0;

    i = ll + 8;
    i = i + 8;
    i = s + 8;
    i = c + 8;

    signed char a[10];
    void* r = a + 7;
}
