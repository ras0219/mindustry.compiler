// struct
struct A
{
    int x;
};
struct A foo();
void bar() { struct A a = foo(); }

// union
union B
{
    int x;
};
union B fooB();
void barB() { union B a = fooB(); }

// enum
enum C
{
    first
};
enum C fooC();
void barC() { enum C a = fooC(); }
