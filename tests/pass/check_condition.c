int main()
{
    int x = 0;
    int* p = &x;
    if (p)
    {
        __prove(p);
    }
    else
    {
        __prove(!p);
    }
}
