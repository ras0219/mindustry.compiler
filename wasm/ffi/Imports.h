#pragma once

#if defined(__cplusplus)
extern "C"
{
#endif
    typedef void (*func_t)(void*);
    typedef void (*callback_t)(char*);

    // Fires a javascript alert. Note: does not actually abort execution
    void abort_js(const char*);

    // Writes to "standard out"
    void fwrite_js(const char* data, size_t bytes, int fd);

    // Logs a string to the javascript console
    void log_js(const char*);

    // Logs an object to the javascript console
    void object_log(int id);

    // `JSON.stringify(obj[id])`
    int object_to_json(int id);

    // `JSON.stringify(obj[id])`
    int object_from_json(int id);

    // `obj[obj_id][property] = obj[value_id]`
    void object_set_property(int obj_id, const char* property, int value_id);

    // `obj[id][property]`
    int object_get_property(int id, const char* property);

    // `obj[id][property]`
    int object_get_integer_property(int id, const char* property);

    _Bool object_equals(int obj1, int obj2);

    // Frees the javascript object at `id`
    void free_object(int id);
    float random();
#if defined(__cplusplus)
}
#endif
