int main()
{
    __prove(1);
    const int x = 1;
    __prove(x);
    __prove(&x);
    __prove(x > 0);
}