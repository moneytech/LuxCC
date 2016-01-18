#include <stdio.h>
#include <stdlib.h>

int (*f1(void))[5]
{
    int (*p)[5];

    p = malloc(sizeof(int)*5);
    (*p)[3] = 1234;
    return p;
}

int (*f2(void))[5][10]
{
    int (*p)[5][10];

    p = malloc(sizeof(int)*5*10);
    (*p)[1][2] = 4321;
    return p;
}

int main(void)
{
    int (*r1)[5];
    int (*r2)[5][10];

    r1 = f1();
    printf("%d\n", (*r1)[3]);
    printf("%d\n", r1[0][3]);

    r2 = f2();
    printf("%d\n", (*r2)[1][2]);
    printf("%d\n", r2[0][1][2]);

    free(r1);
    free(r2);

    return 0;
}
