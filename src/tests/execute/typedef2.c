#include <stdio.h>

typedef int A[3];

struct S {
    A x, y;
};

int main(void)
{
    struct S s = { {1, 2, 3}, {4, 5, 6} };
    printf("%lu\n", sizeof(s));

    return 0;
}
