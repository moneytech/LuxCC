#include <stdio.h>

int main(void)
{
    int a;
    long long e;

    for (a = 10; a >= 0; a--)
        printf(">%d\n", a);

    for (e = 10; e >= 0; e--)
        printf(">>%lld\n", e);

    for (a = 10; ; a--)
        if (!a)
            break;
        else
            printf(">>>%d\n", a);
    printf(">>>>%d\n", a);

    return 0;
}
