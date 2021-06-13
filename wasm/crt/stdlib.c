#include <stdint.h>
#include <stdlib.h>

#include "Imports.h"

#define MEMORYDEBUG 0

static const int bytes_per_megabyte = 1024000; // 1 MB
static const int max_memory = 2 * bytes_per_megabyte;

extern char __heap_base;
char* memory = &__heap_base;
static int end_of_memory = 0;

struct malloc_header
{
#if MEMORYDEBUG
    int magic = 0xdeadbeef;
#endif
    unsigned int used : 1;
    unsigned int size : 31;
};

void memscan()
{
#if MEMORYDEBUG
    int local_offset = 0;
    while (local_offset < end_of_memory)
    {
        malloc_header* header = (malloc_header*)(memory + local_offset);

        if (header->magic != 0xdeadbeef)
        {
            char buf[100];
            char* b = buf;
            b = strcpy(b, "memscan corruption @");
            b = writebytes(b, local_offset);
            b = strcpy(b, " H");
            b = writebytes(b, *header);
            b = strcpy(b, " M");
            b = writebytes(b, 0xdeadbeef);
            imports::log(buf);
            abort("Memory corruption during scan");
        }

        if (!header->used)
        {
            char* char_buffer = (char*)header + sizeof(malloc_header);
            for (int i = 0; i < header->size; ++i)
            {
                if (char_buffer[i] != 'e')
                {
                    abort("Use after free");
                }
            }
        }

        local_offset += header->size + sizeof(malloc_header);
    }
#endif
}

void* realloc(void* ptr, size_t size)
{
    if (!ptr) return malloc(size);
    struct malloc_header* hdr = ptr - sizeof(struct malloc_header);
    if (hdr->size >= size) return ptr;
    void* ret = malloc(size);
    memcpy(ret, ptr, hdr->size);
    free(ptr);
    return ret;
}

void* malloc(const size_t requested_size)
{
    // 4-byte align requested size
    const size_t size = ((requested_size + 3) >> 2) << 2;
    const int total_size = size + sizeof(struct malloc_header);

    int local_offset = 0;
    while (local_offset < end_of_memory)
    {
        struct malloc_header* header = (struct malloc_header*)(memory + local_offset);

#if MEMORYDEBUG
        if (header->magic != 0xdeadbeef)
        {
            char buf[100];
            char* b = buf;
            b = strcpy(b, "malloc corruption @");
            b = writebytes(b, local_offset);
            b = strcpy(b, " H");
            b = writebytes(b, *header);
            b = strcpy(b, " M");
            b = writebytes(b, 0xdeadbeef);
            b = strcpy(b, " R");
            b = writebytes(b, size);
            imports::log(buf);
            abort("Memory corruption (header mismatch in malloc)");
        }
#endif

        if (!header->used && size <= header->size)
        {
            char* buffer = (char*)header + sizeof(struct malloc_header);

            header->used = 1;

#if MEMORYDEBUG
            char buf[100];
            char* b = buf;
            b = strcpy(b, "re-use alloc @");
            b = writebytes(b, (int)((char*)header - memory));
            b = strcpy(b, " S");
            b = writebytes(b, header->size);
            b = strcpy(b, " R");
            b = writebytes(b, size);
            imports::log(buf);
            memset(buffer, 0, header->size);
#endif

            return buffer;
        }

        local_offset += header->size + sizeof(struct malloc_header);
    }

    if ((end_of_memory + total_size) > max_memory)
    {
        abort_js("Out of memory");
    }

    struct malloc_header* header = (void*)(memory + end_of_memory);
#if MEMORYDEBUG
    header->magic = 0xdeadbeef;
#endif

    header->size = size;
    header->used = 1;

    char* buffer = memory + end_of_memory + sizeof(struct malloc_header);

#if MEMORYDEBUG
    char buf[100];
    char* b = buf;
    b = strcpy(b, "alloc @");
    b = writebytes(b, (int)((char*)header - memory));
    b = strcpy(b, " R");
    b = writebytes(b, size);
    imports::log(buf);
#endif

    end_of_memory += total_size;

    return buffer;
}

void free(void* buffer)
{
    if (!buffer) return;

    struct malloc_header* header = (struct malloc_header*)((char*)buffer - sizeof(struct malloc_header));

#if MEMORYDEBUG
    if (header->magic != 0xdeadbeef || header->size == 0)
    {
        char buf[100];
        char* b = buf;
        b = strcpy(b, "free corruption @");
        b = writebytes(b, (int)((char*)header - memory));
        b = strcpy(b, " H");
        b = writebytes(b, *header);
        b = strcpy(b, " S");
        b = writebytes(b, header->size);
        imports::log(buf);
        abort("Memory corruption (header mismatch in free)");
    }
#endif

    if (!header->used) abort_js("Double free");
    header->used = 0;

#if MEMORYDEBUG
    char buf[100];
    char* b = buf;
    b = strcpy(b, "free @");
    b = writebytes(b, (int)((char*)header - memory));
    b = strcpy(b, " H");
    b = writebytes(b, *header);
    b = strcpy(b, " S");
    b = writebytes(b, header->size);
    b = strcpy(b, " U");
    b = writebytes(b, (char*)buffer - memory);
    b = strcpy(b, " N");
    b = writebytes(b, ((char*)buffer - memory) + header->size);
    imports::log(buf);

    memset(buffer, 'e', header->size);
#endif
}
