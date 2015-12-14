#include <stdio.h>

void foo();

int main()
{
    void (*fp)() = foo;
    fp();
    return 0;
}

void foo()
{
    printf("Hello and bye!\n");
}
