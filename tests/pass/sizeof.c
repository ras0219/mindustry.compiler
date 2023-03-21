char d1[sizeof(int)];
char d2[sizeof(d1)];
char d3[] = "hello.";
char d4[sizeof(d3)];
void main()
{
    char d5[256];
    int x = sizeof(d5);

    char a1[sizeof(d1) == 4];
    char a2[sizeof(d2) == 4];
    char a3[sizeof(d3) == 7];
    char a4[sizeof(d4) == 7];
    char a5[sizeof(d5) == 256];
}
