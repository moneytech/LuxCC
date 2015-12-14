#include <stdio.h>

int main(void)
{
    unsigned x = 0xAABBCCDD;
    unsigned char *p;

    for (p = &x; p < (unsigned char *)&x+4; p++)
        printf("%x\n", *p);

    /* again */
    p = &x;
    printf("%x\n", p[0]);
    printf("%x\n", p[1]);
    printf("%x\n", p[2]);
    printf("%x\n", p[3]);

    return 0;
}
