#include "Exports.h"

static int callback_buffer[10];
volatile int* get_callback_buffer() { return callback_buffer; }

void call(func_t func, void* context) { func(context); }
void callback(callback_t callback, char* value) { callback(value); }
