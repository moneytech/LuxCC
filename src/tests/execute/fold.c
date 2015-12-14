#include <stdio.h>

int main(void)
{
    int si;
    unsigned ui;

    si = -1;
    ui = (unsigned)-1;

    printf("%d\n", si*2);
    printf("%u\n", ui*2);

    printf("%d\n", si/2);
    printf("%u\n", ui/2);

    printf("%d\n", si%2);
    printf("%u\n", ui%2);

    return 0;
}
