#include <stdarg.h>
#include <stdio.h>

typedef struct {
    short x, y, z;
} A;

void foo(int n, ...)
{
    va_list ap;
    A x;

    va_start(ap, n);
    x = va_arg(ap, A);
    va_end(ap);

    printf("x=%d\n", x.x);
    printf("y=%d\n", x.y);
    printf("z=%d\n", x.z);
}

int main(void)
{
    A s = { 15, 25, 35 };
    foo(1, s);

    return 0;
}
