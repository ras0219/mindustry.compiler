
int main()
{
    int i = 0;
top:
    if (i < 10)
    {
        i = i + 1;
        __prove(i);
        goto top;
    }
    return i;
}
