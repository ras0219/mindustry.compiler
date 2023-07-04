int main()
{
    int x = 0;
    int* p = &x;
    if (p)
    {
        *p;
    }
    else
    {
        __prove(!p);
    }
}
