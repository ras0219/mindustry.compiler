#include "charclass.h"

#include "foreach128.h"

#define CLASSIFY_CHAR(ch)                                                                                              \
    [ch] =                                                                                                             \
        (ch <= '9' && ch >= '0' ? char_class_digit : 0) | (ch <= 'z' && ch >= 'a' ? char_class_alpha : 0) |            \
        (ch <= 'Z' && ch >= 'A' ? char_class_alpha : 0) | (ch == '_' ? char_class_uscore : 0) |                        \
        (ch == ' ' || ch == '\b' || ch == '\n' || ch == '\r' || ch == '\t' || ch == '\v' || ch == '\f' ? char_class_ws \
                                                                                                       : 0) |          \
        (ch == '\n' || ch == '\r' ? char_class_nl : 0) |                                                               \
        (ch == '\n' || ch == '\r' || ch == '\\' || ch == '"' ? char_class_strscan : 0),

const unsigned char s_char_classes[256] = {X_FOREACH_128(CLASSIFY_CHAR)};

// hex value + 1, 0 on error
const char s_hex_map[128] = {
    ['0'] = 1,  ['1'] = 2,  ['2'] = 3,  ['3'] = 4,  ['4'] = 5,  ['5'] = 6,  ['6'] = 7,  ['7'] = 8,
    ['8'] = 9,  ['9'] = 10, ['a'] = 11, ['b'] = 12, ['c'] = 13, ['d'] = 14, ['e'] = 15, ['f'] = 16,
    ['A'] = 11, ['B'] = 12, ['C'] = 13, ['D'] = 14, ['E'] = 15, ['F'] = 16,
};
