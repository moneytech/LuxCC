#include <stdio.h>

int main(void)
{
    char c1;
    unsigned char c2;

    c1 = -128;
    printf("%d\n", --c1);
    printf("%d\n", c1);

    c1 = -128;
    printf("%d\n", c1++);
    printf("%d\n", c1);

    c2 = 255;
    printf("%d\n", ++c2);
    printf("%d\n", c2);

    c2 = 255;
    printf("%d\n", c2++);
    printf("%d\n", c2);

    return 0;
}
