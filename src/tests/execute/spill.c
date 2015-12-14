#include <stdio.h>

int x;

void foo(int *p)
{
    printf("%d\n", *p);
}

void bar(void)
{
    printf("%d\n", x);
}

int main(void)
{
    int y;

    /* Test liveness / register spilling */

    y = 1234;
    foo(&y);
    y = 0;

    x = 9999;
    bar();
    x = 0;

    y = 4321;
    if (1)
        foo(&y);
    y = 0;

    x = 8888;
    if (1)
        bar();
    x = 0;

    return 0;
}
