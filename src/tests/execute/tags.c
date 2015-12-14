#include <stdio.h>

struct A { /* (1) */
    int a[2];
} x;

int main(void)
{
    struct A; /* hides (1) */
    struct A *y;
    struct A {
        int a[3];
    };
    printf("%lu != %lu\n", sizeof(x), sizeof(*y));

    return 0;
}
