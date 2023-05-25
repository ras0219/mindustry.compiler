void f(void);

void g(void)
{
    int x = (f(), 5), y;
    (y = x + 2, y = x + 3);
}
