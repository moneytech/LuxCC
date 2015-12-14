#include <stdio.h>

int main(void)
{
    int a;
    long long e;

    a = 0;
    if (a)
        printf("A\n");
    else
        printf("B\n");
    a = 1;
    if (a)
        printf("C\n");
    else
        printf("D\n");

    e = 0;
    if (e)
        printf(">A\n");
    else
        printf(">B\n");
    e = 1;
    if (e)
        printf(">C\n");
    else
        printf(">D\n");

    return 0;
}
