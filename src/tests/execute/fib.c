#include <stdio.h>

int fib(int n)
{
    if (n < 2)
        return n;
    else
        return fib(n-1) + fib(n-2);
}

int fib2(int n) { return (n<2 ? n : fib(n-1)+fib(n-2)); }

int main(void)
{
    int t = 0;

    while(t < 20)
        printf("fib(%d)=%d\n", t, fib(t)), ++t;

    return 0;
}
