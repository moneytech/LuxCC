#include <stdio.h>

int main(void)
{
    typedef int A[];
    A a = { 1, 2 }, b = { 3, 4, 5 };

    printf("%lu %lu\n", sizeof(a), sizeof(b));

    return 0;
}
