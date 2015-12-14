#include <stdio.h>

int main(void)
{
    int x = 1;
top:
    switch (x) {
    case 0:
        x = 2;
        goto top;
    case 1:
        x = 0;
        goto top;
    case 2:
        x = -1;
        goto top;
    default:
        break;
    }
    printf("x=%d\n", x);
    for (x = 0; ; x++)
        if (x == 10)
            goto exit;
exit:
    printf("x=%d\n", x);
    for (x = 0; ; ) {
        switch (x) {
        case 0:
            x++;
            continue;
        case 1:
            x++;
            continue;
        case 2:
            break;
        }
        break;
    }
    printf("x=%d\n", x);

    return 0;
}
