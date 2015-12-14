#include <stdio.h>

typedef struct {
    char s[32];
} S;

S f3(void)
{
    S r = { "hello world" };
    return r;
}

S f2(void)
{
    return f3();
}

S f1(void)
{
    return f2();
}

int main(void)
{
    S x = f1();
    printf("%s\n", x.s);

    return 0;
}
