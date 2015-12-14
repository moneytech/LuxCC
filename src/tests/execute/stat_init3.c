#include <stdio.h>

/*
 *  6.6#10
 * The semantic rules for the evaluation of a constant expression are the same as for
 * nonconstant expressions.
 */
int i0 = 2 || 1/0;
int i1 = 0 && 1/0;
int i2 = 2 ? 1 : 1/0;
int i3 = 0 ? 1/0 : 1;

/* address of object: true or false? */
int *p0 = 0 && &i0;
int *p1 = &i0 && 0;
int *p2 = (int *)(1 || &i0);
int *p3 = (int *)(&i0 || 1);

int main(void)
{
    printf("%d\n", i0);
    printf("%d\n", i1);
    printf("%d\n", i2);
    printf("%d\n", i3);

    printf("0x%lx\n", p0);
    printf("0x%lx\n", p1);
    printf("0x%lx\n", p2);
    printf("0x%lx\n", p3);

    return 0;
}
