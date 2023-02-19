#include "interval.h"
#include "unittest.h"
#include "unwrap.h"

static const Sizing sz8 = {.width = 8};
static const Sizing sz4 = {.width = 4};
// static const Sizing si8 = {.width = 8, .is_signed = 1};
static const Sizing si4 = {.width = 4, .is_signed = 1};

static int test_interval_add(TestState* state)
{
    int rc = 1;
    {
        Interval i = {.base = 10, .maxoff = 0, .sz = sz8};
        Interval j = {.base = 15, .maxoff = 0, .sz = sz8};
        Interval k = interval_add(i, j);
        REQUIRE_ZU_EQ(25, k.base);
        REQUIRE_ZU_EQ(0, k.maxoff);
    }
    {
        Interval i = {.base = 10, .maxoff = UINT64_MAX - 10, .sz = sz8};
        Interval j = {.base = 15, .maxoff = 0, .sz = sz8};
        Interval k = interval_add(i, j);
        REQUIRE_ZU_EQ(25, k.base);
        REQUIRE_ZU_EQ(UINT64_MAX - 10, k.maxoff);
    }
    {
        Interval i = {.base = UINT64_MAX, .maxoff = 50, .sz = sz8};
        Interval j = {.base = 10, .maxoff = 1, .sz = sz8};
        Interval k = interval_add(i, j);
        REQUIRE_ZU_EQ(9, k.base);
        REQUIRE_ZU_EQ(51, k.maxoff);
    }
    {
        Interval i = {.base = UINT32_MAX, .maxoff = 0, .sz = sz4};
        Interval j = {.base = 1, .maxoff = 0, .sz = sz4};
        Interval k = interval_add(i, j);
        REQUIRE_ZU_EQ(0, k.base);
        REQUIRE_ZU_EQ(0, k.maxoff);
    }
    {
        Interval i = {.base = UINT32_MAX, .maxoff = 0, .sz = sz4};
        Interval j = {.base = UINT32_MAX, .maxoff = 0, .sz = sz4};
        Interval k = interval_add(i, j);
        REQUIRE_ZU_EQ(UINT32_MAX - 1, k.base);
        REQUIRE_ZU_EQ(0, k.maxoff);
    }
    {
        Interval i = {.base = INT32_MAX, .maxoff = 0, .sz = si4};
        Interval j = {.base = 1, .maxoff = 0, .sz = si4};
        REQUIRE_EQ(1, interval_add_ofchk(i, j));
    }
    {
        Interval i = {.base = INT32_MAX, .maxoff = 0, .sz = si4};
        Interval j = {.base = (uint32_t)INT32_MIN, .maxoff = 0, .sz = si4};
        REQUIRE_EQ(0, interval_add_ofchk(i, j));
        Interval k = interval_add(i, j);
        REQUIRE_ZU_EQ((uint32_t)-1, k.base);
        REQUIRE_ZU_EQ(0, k.maxoff);
    }
    rc = 0;
fail:
    return rc;
}

void run_interval_tests(TestState* state) { RUN_TEST(test_interval_add); }
