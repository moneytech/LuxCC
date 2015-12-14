#include <stdio.h>

int main(void)
{
    char x[0xFFFFFFFF / 16777216];
    printf("%lu\n", sizeof(x));

    return 0;
}
