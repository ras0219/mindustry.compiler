typedef unsigned long size_t;
size_t array_size(const struct Array* arr, size_t sz);
static size_t findstr(const char* str, const char* const* heap, size_t heap_size);
static __forceinline long tt_find_insert_null(struct TypeTable* tt, const char* str)
{
    const size_t n = array_size((void*)0, sizeof(const char*));
    size_t offset = findstr(str, (const char* const*)0, n);
    return offset;
}
