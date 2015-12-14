#include <stdio.h>

int main(void)
{
    printf("%lld\n", 0 ? 1LL : 2);
    printf("%lld\n", 1 ? 1 : 2LL);

    return 0;
}
