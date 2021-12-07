#include <stdio.h>
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

FILE* fmemopen(void* buf, size_t size, const char* mode)
{
    char temppath[MAX_PATH - 13];
    if (0 == GetTempPathA(sizeof(temppath), temppath)) return NULL;

    char filename[MAX_PATH + 1];
    if (0 == GetTempFileNameA(temppath, "SC", 0, filename)) return NULL;

    FILE* f = fopen(filename, "wb");
    if (NULL == f) return NULL;

    fwrite(buf, size, 1, f);
    fclose(f);

    return fopen(filename, mode);
}
