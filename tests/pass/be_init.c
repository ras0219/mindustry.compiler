struct S
{
    int x, y, z, w;
};
struct A
{
    struct S s1, s2;
};
void main() { struct A s1 = {0}; }
