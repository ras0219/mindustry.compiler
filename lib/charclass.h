#pragma once

enum
{
    char_class_digit = 2,
    char_class_uscore = 4,
    char_class_alpha = 8,
    char_class_ws = 16,
    char_class_nl = 32,
    char_class_strscan = 64,

    char_class_alphu = char_class_alpha | char_class_uscore,
    char_class_alnumu = char_class_alphu | char_class_digit,
};

extern const unsigned char s_char_classes[256];

// hex value + 1, 0 on error
extern const char s_hex_map[128];
