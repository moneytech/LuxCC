#include "../../sassert.h"

int main(void)
{
    __static_assert(_ASSERT_TYPE, 1L, long);
    __static_assert(_ASSERT_TYPE, 1LL, long long);
    __static_assert(_ASSERT_TYPE, 1U, unsigned);
    __static_assert(_ASSERT_TYPE, 1UL, unsigned long);
    __static_assert(_ASSERT_TYPE, 1ULL, unsigned long long);
    __static_assert(_ASSERT_TYPE, 1LU, unsigned long);
    __static_assert(_ASSERT_TYPE, 1LLU, unsigned long long);

    __static_assert(_ASSERT_TYPE, 1, int);
    __static_assert(_ASSERT_TYPE, 2147483647, int);
#ifdef __LP64__
    __static_assert(_ASSERT_TYPE, 2147483648, long);
#else
    __static_assert(_ASSERT_TYPE, 2147483648, long long);
#endif

#ifdef __LP64__
    __static_assert(_ASSERT_TYPE, 9223372036854775807, long);
#else
    __static_assert(_ASSERT_TYPE, 9223372036854775807, long long);
#endif

    __static_assert(_ASSERT_TYPE, 0xF, int);
    __static_assert(_ASSERT_TYPE, 0x7FFFFFFF, int);
    __static_assert(_ASSERT_TYPE, 0x80000000, unsigned);
    __static_assert(_ASSERT_TYPE, 0xFFFFFFFF, unsigned);
#ifdef __LP64__
    __static_assert(_ASSERT_TYPE, 0x7FFFFFFFFFFFFFFF, long);
    __static_assert(_ASSERT_TYPE, 0x8000000000000000, unsigned long);
    __static_assert(_ASSERT_TYPE, 0xFFFFFFFFFFFFFFFF, unsigned long);
#else
    __static_assert(_ASSERT_TYPE, 0x7FFFFFFFFFFFFFFF, long long);
    __static_assert(_ASSERT_TYPE, 0x8000000000000000, unsigned long long);
    __static_assert(_ASSERT_TYPE, 0xFFFFFFFFFFFFFFFF, unsigned long long);
#endif

    return 0;
}
