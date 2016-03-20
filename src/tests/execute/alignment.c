#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

/*
    Test alignment for architectures where long long is 8 byte aligned.
*/

void f0(long long x)
{
    printf("f0: %lld\n", x);
}

void f1(int x, /* pad arg , */ long long y)
{
    printf("f1: %d %lld\n", x, y);
}

void f2(int x, int y, long long z)
{
    printf("f2: %d %d %lld\n", x, y, z);
}

void f3(int x, int y, int z, /* pad arg , */ long long w)
{
    printf("f3: %d %d %d %lld\n", x, y, z, w);
}

typedef struct {
    char x[64];
} A;

A f4(/* return value address , pad arg , */ long long x)
{
    printf("f4: %lld\n", x);
}

void f5(int n, ...)
{
    int i;
    va_list ap;

    printf("f5:");

    va_start(ap, n);
    for (i = 0; i < n; i++)
        printf(" %lld", va_arg(ap, long long));
    va_end(ap);

    printf("\n");
}

void f6(int x, int y, int z, /* pad arg , */ long long w)
{
    printf("f6: %d %d %d %lld\n", x, y, z, w);
}

int f7(long long x)
{
    printf("f7: %lld\n", x);
    return 1234;
}

int main(void)
{
    printf("%ld\n", __alignof__(char));
    printf("%ld\n", __alignof__(short));
    printf("%ld\n", __alignof__(int));
    printf("%ld\n", __alignof__(long));
    /* gcc aligns long long's within structs to 4 bytes,
       yet __alignof__(long long) returns 8. Weird... */
    /*printf("%d\n", __alignof__(long long));*/

    f0(1152921504606846976LL);
    f1(10, 576460752303423488LL);
    f2(20, 30, 288230376151711744LL);
    f3(40, 50, 60, 144115188075855872LL);
    f4(72057594037927936LL);
    f5(3, 36028797018963968LL, 18014398509481984LL, 9007199254740992LL);
    f6(70, 80, f7(4503599627370496LL), 576460752303423488LL);

    return 0;
}
