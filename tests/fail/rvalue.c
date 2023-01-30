struct abc
{
    int a, b, c;
};

struct abc get_abc();

int main() { &(get_abc().a); }