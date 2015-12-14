// #include <stdio.h>
int printf(const char *, ...);

int ackermann(int m, int n)
{
    if (m == 0)
        return n+1;
    else if (n == 0)
        return ackermann(m-1, 1);
    else
        return ackermann(m-1, ackermann(m, n-1));
}

int main(void)
{
    printf("ackermann(2, 2) = %d\n", ackermann(2, 2)); // 7
    printf("ackermann(3, 2) = %d\n", ackermann(3, 2)); // 29
    printf("ackermann(3, 3) = %d\n", ackermann(3, 3)); // 61

    return 0;
}
