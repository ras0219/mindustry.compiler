void f(int* ch, int i)
{
    ch += 1;
    ch -= 1;
    int* z = ++ch;
    int* w = ch++;
    --ch;
    ch--;
    i = i - 1;
    i -= 1;
    i = i + 1;
    i += 1;
    i = i * 2;
    i *= 2;
    i = i % 1;
    i %= 1;
    i = i / 1;
    i /= 1;

    unsigned j = 1;
    j = j / 2;
    j /= 2;
    j = j * 2;
    j *= 2;
    j = j % 2;
    j %= 2;
}
