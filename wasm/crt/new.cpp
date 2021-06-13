#include "new.h"

#include "stdlib.h"

void* operator new[](size_t sz) { return malloc(sz); }

void operator delete[](void* p) { return free(p); }

void* operator new(size_t sz) { return malloc(sz); }

void operator delete(void* p) { return free(p); }

// Ignore all static destructors
extern "C" int __cxa_atexit(void __attribute__((unused)) (*function)(void), void*, void*) { return 0; }
