#include <stdio.h>

int main(void)
{
    int a;
    long long e;

    a = 10;
    do
        printf(">%d\n", a);
    while (a--);

    e = 10;
    do
        printf(">>%lld\n", e);
    while (e--);

    a = 10;
    do
        if (!a--)
            break;
        else
            printf(">>>%d\n", a);
    while (1);
    printf(">>>>%d\n", a);

    return 0;
}
