#include <stdio.h>

void foo(void)
{
    printf("foo()\n");
}

void bar(void)
{
    printf("bar()\n");
}

int main(void)
{
    int x;

    x = 0;
    x ? foo():bar();
    x = 1;
    x ? foo():bar();

    return 0;
}
