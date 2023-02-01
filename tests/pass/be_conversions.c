void cg_declare_extern()
{
    char x = 10;
    x = 11;
    int y = x;
    y = x;
    char* px = &(x += 1);
    x++;
    char h[] = "he\\\\\\";
    char* p = h;
    unsigned long long z = x * 2;
    unsigned long long w = z * 2;
}
