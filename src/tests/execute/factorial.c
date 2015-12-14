// #include <stdio.h>
int printf(const char *, ...);

int factorial(int x)
{
    int fact;
    if (x > 0) {
        fact = 1;
        do
            fact = fact*x--;
        while (x != 0);
    }
    return fact;
}

int rfactorial(int x)
{
    if (x > 1)
        return x*rfactorial(x-1);
    else
        return 1;
}

int main(void)
{
    printf("%d\n", factorial(5));
    printf("%d\n", rfactorial(5));

    return 0;
}
