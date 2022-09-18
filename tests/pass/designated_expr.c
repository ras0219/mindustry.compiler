struct A
{
    int x;
};
struct A foo();
struct B
{
    struct A a, b;
};
void bar() { struct B a = {foo()}; }
