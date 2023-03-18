#pragma once

#define X_COMMA(...) __VA_ARGS__,
#define X_COMMA_BEFORE(...) , __VA_ARGS__
#define X_PLUS_ONES(...) +1
#define X_CASE_STRING(E)                                                                                               \
    case E:                                                                                                            \
#E;
#define X_DINIT_STRING(E) [E] = #E,
