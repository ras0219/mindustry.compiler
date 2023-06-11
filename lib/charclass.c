#include "charclass.h"

#define CLASSIFY_CHAR(ch)                                                                                              \
    [ch] =                                                                                                             \
        (ch <= '9' && ch >= '0' ? char_class_digit : 0) | (ch <= 'z' && ch >= 'a' ? char_class_alpha : 0) |            \
        (ch <= 'Z' && ch >= 'A' ? char_class_alpha : 0) | (ch == '_' ? char_class_uscore : 0) |                        \
        (ch == ' ' || ch == '\b' || ch == '\n' || ch == '\r' || ch == '\t' || ch == '\v' || ch == '\f' ? char_class_ws \
                                                                                                       : 0) |          \
        (ch == '\n' || ch == '\r' ? char_class_nl : 0) |                                                               \
        (ch == '\n' || ch == '\r' || ch == '\\' || ch == '"' ? char_class_strscan : 0)

#define CLASSIFY_CHAR8(X)                                                                                              \
    CLASSIFY_CHAR((X + 0)), CLASSIFY_CHAR((X + 1)), CLASSIFY_CHAR((X + 2)), CLASSIFY_CHAR((X + 3)),                    \
        CLASSIFY_CHAR((X + 4)), CLASSIFY_CHAR((X + 5)), CLASSIFY_CHAR((X + 6)), CLASSIFY_CHAR((X + 7))

const unsigned char s_char_classes[256] = {
    CLASSIFY_CHAR8(0),
    CLASSIFY_CHAR8(8),
    CLASSIFY_CHAR8(16),
    CLASSIFY_CHAR8(24),
    CLASSIFY_CHAR8(32),
    CLASSIFY_CHAR8(40),
    CLASSIFY_CHAR8(48),
    CLASSIFY_CHAR8(56),
    CLASSIFY_CHAR8(64),
    CLASSIFY_CHAR8(72),
    CLASSIFY_CHAR8(80),
    CLASSIFY_CHAR8(88),
    CLASSIFY_CHAR8(96),
    CLASSIFY_CHAR8(104),
    CLASSIFY_CHAR8(112),
    CLASSIFY_CHAR8(120),
};

// hex value + 1, 0 on error
const char s_hex_map[128] = {
    ['0'] = 1,  ['1'] = 2,  ['2'] = 3,  ['3'] = 4,  ['4'] = 5,  ['5'] = 6,  ['6'] = 7,  ['7'] = 8,
    ['8'] = 9,  ['9'] = 10, ['a'] = 11, ['b'] = 12, ['c'] = 13, ['d'] = 14, ['e'] = 15, ['f'] = 16,
    ['A'] = 11, ['B'] = 12, ['C'] = 13, ['D'] = 14, ['E'] = 15, ['F'] = 16,
};
