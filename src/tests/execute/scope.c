#include <stdio.h>

int x = 1;

int main(void)
{
    printf("%d\n", x);
    x = 2;
    printf("%d\n", x);
    {
        int x;
        x = 3;
        printf("%d\n", x);
        {
            int x;
            x = 4;
            printf("%d\n", x);
        }
    }
    printf("%d\n", x);

    return 0;
}
