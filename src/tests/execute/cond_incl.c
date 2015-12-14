#include <stdio.h>

int main(void)
{
#define T 1
#define F 0
#define U

#ifdef U
    printf("1\n");
#endif
#undef U
#ifndef U
    printf("2\n");
#endif

#if defined U || defined T
    printf("3\n");
#endif
#if defined T && defined U
    printf("4\n");
#else
    printf("5\n");
#endif

#if 1+(T ? 1 : -1)
    printf("6\n");
#endif
#if 1+(F ? 1 : -1)
    printf("7\n");
#endif

#if (10+2*-5+1<<20) && F
    printf("8\n");
#elif T && 3*-1+6 || 1
    printf("9\n");
#endif

#if 1 | 0
    printf("10\n");
#endif
#if 1 ^ 0
    printf("11\n");
#endif
#if 1 & 0
    printf("12\n");
#elif 0 & 1
    printf("13\n");
#else
    printf("14\n");
#endif

#if T == F
    printf("15\n");
#endif
#if T != F
    printf("15\n");
#endif

#if 10 > 20
    printf("15\n");
#elif 20 < 10
    printf("16\n");
#elif 20 <= 10
    printf("17\n");
#elif 10 >= 20
    printf("18\n");
#else
    printf("19\n");
#endif

#if 16777216<<7 == 274877906944>>7
    printf("20\n");
#endif

#if 10+20*2 == 160/2-30
    printf("21\n");
#endif

#if !!-(-5) && ~+0
    printf("22\n");
#endif

#if UNK+UNK2 == UNK3*2
    printf("23\n");
#endif

	return 0;
}
