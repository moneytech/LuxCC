#include <stdio.h>

typedef char Char;

static void panic(const Char *);

static void panic(const Char *a)
{
    printf(a);
}

int main(void)
{
    panic("hello world!\n");
    return 0;
}
