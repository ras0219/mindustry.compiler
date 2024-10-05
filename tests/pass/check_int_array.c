typedef unsigned long long uint64_t;
typedef unsigned int uint32_t;
typedef long long int64_t;
typedef int int32_t;

void abort(void);

#define UINT8_MAX 0xFFU
#define UINT16_MAX 0xFFFFU
#define UINT32_MAX 0xFFFFFFFFU
#define UINT64_MAX 0xFFFFFFFFFFFFFFFFULL

#define INT8_MAX 0x7F
#define INT16_MAX 0x7FFF
#define INT32_MAX 0x7FFFFFFF
#define INT64_MAX 0x7FFFFFFFFFFFFFFFLL

// 9223372036854775807

#define INT8_MIN (-INT8_MAX - 1)
#define INT16_MIN (-INT16_MAX - 1)
#define INT32_MIN (-INT32_MAX - 1)
#define INT64_MIN (-INT64_MAX - 1)

typedef struct Sizing
{
    uint32_t is_signed : 1, width : 31;
} Sizing;

/// @brief example: 0xFFFFFFFFFFFF8000 for [2]
static const int64_t s_i64_imin_sizing[] = {
    [1] = INT8_MIN,
    [2] = INT16_MIN,
    [4] = INT32_MIN,
    [8] = INT64_MIN,
};

/// @brief example: 0x7FFF for [2]
static const uint64_t s_i64_imax_sizing[] = {
    [1] = INT8_MAX,
    [2] = INT16_MAX,
    [4] = INT32_MAX,
    [8] = INT64_MAX,
};

/// @brief Represents sets of integer intervals for abstract interpretation. Empty set is not representable.
///
/// Ranges are represented as [base, base + extent]. This can wrap around the maximum, producing a set that excludes a
/// middle section. For example, [UINT64_MAX-10, 20] contains (UINT64_MAX-10) and 9 but excludes 10.
///
/// unsigned numbers are based from 0 to UINT??_MAX
/// signed numbers are mapped with a wrap-around at INT??_MAX
/// - [0, INT??_MAX] => [0, INT??_MAX]
/// - [INT_??MIN, -1] => [INT??_MAX + 1, UINT??_MAX]
typedef struct Interval
{
    uint64_t base, maxoff;
    Sizing sz;
} Interval;

int64_t interval_signed_min(Interval i);
int64_t interval_signed_max(Interval i);

int interval_add_ofchk(Interval i, Interval j)
{
    int64_t in = interval_signed_min(i);
    int64_t jn = interval_signed_min(j);
    if (in < 0 && jn < 0 && in < s_i64_imin_sizing[i.sz.width] - jn) return 1;

    int64_t ix = interval_signed_max(i);
    int64_t jx = interval_signed_max(j);
    if (ix > 0 && jx > 0 && ix > s_i64_imax_sizing[i.sz.width] - jx) return 1;
    return 0;
}
