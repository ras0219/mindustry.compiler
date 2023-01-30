struct Y
{
    char buf[20];
};
void f(int x[1], struct Y y);
void g(int x[1], struct Y y1)
{
    struct Y y;
    f(x, y);
}
