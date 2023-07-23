
int main()
{
    int i = 0;
top:
    if (i < 10)
    {
        i = i + 1;
        __prove(i >= 1 && i <= 10);
        goto top;
    }
    __prove(i >= 10);
    return i;
}
