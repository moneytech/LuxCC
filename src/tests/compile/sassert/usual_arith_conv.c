#include "../../sassert.h"

int main(void)
{
    char a;
    unsigned char b;
    short c;
    unsigned short d;
    int e;
    unsigned int f;
    long g;
    unsigned long h;
    long long i;
    unsigned long long j;

    __static_assert(_ASSERT_TYPE, a+a, int);
    __static_assert(_ASSERT_TYPE, e+d, int);

    __static_assert(_ASSERT_TYPE, e+g, long);
    __static_assert(_ASSERT_TYPE, e+i, long long);
    __static_assert(_ASSERT_TYPE, g+i, long long);
    __static_assert(_ASSERT_TYPE, f+h, unsigned long);
    __static_assert(_ASSERT_TYPE, f+j, unsigned long long);
    __static_assert(_ASSERT_TYPE, h+j, unsigned long long);

    __static_assert(_ASSERT_TYPE, e+f, unsigned);
    __static_assert(_ASSERT_TYPE, e+h, unsigned long);
    __static_assert(_ASSERT_TYPE, e+j, unsigned long long);
    __static_assert(_ASSERT_TYPE, g+h, unsigned long);
    __static_assert(_ASSERT_TYPE, g+j, unsigned long long);
    __static_assert(_ASSERT_TYPE, i+j, unsigned long long);

    __static_assert(_ASSERT_TYPE, i+f, long long);
#ifdef __LP64__
    __static_assert(_ASSERT_TYPE, g+f, long);
    __static_assert(_ASSERT_TYPE, i+h, unsigned long long);
#else
    __static_assert(_ASSERT_TYPE, g+f, unsigned long);
    __static_assert(_ASSERT_TYPE, i+h, long long);
#endif

    return 0;
}
