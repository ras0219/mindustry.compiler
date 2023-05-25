struct abc
{
    int a, b, c;
};

struct ab
{
    int a, b;
};

struct ab g(void);

struct abc f();

int main()
{
    int y;
    y = f().b;
    struct abc* init;
    init->a = g().b;
    return f().c;
}
