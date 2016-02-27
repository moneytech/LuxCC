#include <stdio.h>

int f(int x1, int x2, int x3, int x4, int x5)
{
    return x1+x2+x3+x4+x5;
}

int main(void)
{
    printf("%d\n", f(f(f(15, 25, 35, 45, 55), 66, 77, 88, 99), f(1000, 2000, 3000, 4000, 5000),
    f(9323, 2504, 7732, 203, 123), f(f(44, 1990, 3003, 101, 102), 843, 12, 1200, 388), 729));

    return 0;
}
