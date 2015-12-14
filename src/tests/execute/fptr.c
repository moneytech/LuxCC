#include <stdio.h>

void foo(int x) { printf("foo(%d)\n", x); }

int main(void)
{
    foo(1);
    (*foo)(2);
    (**foo)(3);
    (***foo)(4);
    (****foo)(5);
    (***&foo)(6);
    (**&foo)(7);
    (*&foo)(8);
    (&foo)(9);

    return 0;
}
