#include <stdio.h>

int main(void)
{
    int a;
    long long e;

    a = 10;
    while (a)
        printf("%d", a), a--;

    e = 10;
    while (e)
        printf("%lld\n", e), e--;

    return 0;
}
