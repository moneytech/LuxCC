#include <stdio.h>

int main(void)
{
    int i1, i2, i3;
    long long ll1, ll2, ll3;
    unsigned u1, u2, u3;
    unsigned long long ull1, ull2, ull3;

    i1 = 10;
    i2 = -20;
    ll1 = 2199023255552;
    ll2 = 8589934592;
    ll3 = -1;
    u1 = 100;
    u2 = 4294967295;
    ull1 = 1099511627776;
    ull2 = 9223372036854775808ULL;
    ull3 = 18446744073709551615ULL;
    //
    printf("%lld\n", i1+i2+ll1);
    printf("%lld\n", i1+i2+ll2);
    printf("%lld\n", i1+i2+ll3);

    i3 = i1+i2;
    printf("%lld\n", i3+ll1);
    printf("%lld\n", i3+ll2);
    printf("%lld\n", i3+ll3);

    printf("%llu\n", i1+i2+ull1);
    printf("%llu\n", i1+i2+ull2);
    printf("%llu\n", i1+i2+ull3);

    i3 = i1+i2;
    printf("%llu\n", i3+ull1);
    printf("%llu\n", i3+ull2);
    printf("%llu\n", i3+ull3);
    //
    printf("%lld\n", u1+u2+ll1);
    printf("%lld\n", u1+u2+ll2);
    printf("%lld\n", u1+u2+ll3);

    u3 = u1+u2;
    printf("%lld\n", u3+ll1);
    printf("%lld\n", u3+ll2);
    printf("%lld\n", u3+ll3);

    printf("%llu\n", u1+u2+ull1);
    printf("%llu\n", u1+u2+ull2);
    printf("%llu\n", u1+u2+ull3);

    u3 = u1+u2;
    printf("%llu\n", u3+ull1);
    printf("%llu\n", u3+ull2);
    printf("%llu\n", u3+ull3);

    return 0;
}
