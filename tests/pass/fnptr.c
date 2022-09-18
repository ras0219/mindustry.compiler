int foo();
int main()
{
    int (*i)() = foo;
    int (*j)() = &foo;
}
