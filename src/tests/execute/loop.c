#include <stdio.h>

int main(void)
{
    /* from tcc test suite */
    int i;
    i = 0;
    while (i < 10)
        printf("%d", i++);
    printf("\n");
    for(i = 0; i < 10; i++)
        printf("%d", i);
    printf("\n");
    i = 0;
    do
        printf("%d", i++);
    while (i < 10);
    printf("\n");

    /* break/continue tests */
    i = 0;
    while (1) {
        if (i == 6)
            break;
        i++;
        if (i == 3)
            continue;
        printf("%d", i);
    }
    printf("\n");
    i = 0;
    do {
        if (i == 6)
            break;
        i++;
        if (i == 3)
            continue;
        printf("%d", i);
    } while(1);
    printf("\n");
    for(i = 1; i < 7; i++) {
        if (i == 3)
            continue;
        printf("%d", i);
    }
    printf("\n");

    return 0;
}
