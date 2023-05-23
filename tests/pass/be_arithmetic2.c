void f(int* ch, int i)
{
    unsigned j = 1;
    j <<= 1;
    j = j << 1;

    j >>= 1;
    j = j >> 1;

    unsigned k = 1;
    j <<= k;
    j = j << k;
    j >>= k;
    j = j >> k;
}
