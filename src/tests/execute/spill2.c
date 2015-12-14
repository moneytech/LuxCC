#include <stdio.h>

int x, *p = &x;

int main(void)
{
    x = 2;
    printf("%d\n", x**p);
    x = 0;

    return 0;
}
