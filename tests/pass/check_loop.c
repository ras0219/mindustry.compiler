
int main()
{
    int i = 0;
top:
    i = i + 1;
    __prove(i);
    if (i < 10) goto top;
    return i;
}
