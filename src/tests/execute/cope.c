#include <stdio.h>

int fact(int x)
{
    return (x > 1) ? x*fact(x-1) : 1;
}

int ipow(int x, int y)
{
    int p;
    for (--y, p=x; y > 0; --y)
        p *= x;
    return p;
}

/* compute number of permutations (allow repetition) */
int PR(int n, int r)
{
    return ipow(n, r);
}

/* compute number of permutations (don't allow repetition) */
int P(int n, int r)
{
    return fact(n)/fact(n-r);
}

/* compute number of combinations (allow repetition) */
int CR(int n, int r)
{
    return fact(n+r-1)/(r*fact(n-1));
}

/* compute number of combinations (don't allow repetition) */
int C(int n, int r)
{
    return fact(n)/(fact(r)*fact(n-r));
}

int main(void)
{
    printf("PR(3, 2) = %d\n", PR(3, 2));
    printf("P(3, 2)  = %d\n", P(3, 2));
    printf("CR(3, 2) = %d\n", CR(3, 2));
    printf("C(3, 2)  = %d\n", C(3, 2));

    return 0;
}
