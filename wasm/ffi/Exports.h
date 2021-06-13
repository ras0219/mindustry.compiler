#pragma once

#if defined(__cplusplus)
extern "C"
{
#endif
    typedef void (*func_t)(void*);
    typedef void (*callback_t)(char*);

    void call(func_t func, void* context);
    void callback(callback_t callback, char* value);

    volatile int* get_callback_buffer();
#if defined(__cplusplus)
}
#endif
