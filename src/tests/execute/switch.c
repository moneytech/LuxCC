#include <stdio.h>

int main(void)
{
    int a;
    long long e;

    a = 10;
    switch (a) {
    case 1: printf("BAD\n"); break;
    case 2: printf("BAD\n"); break;
    case 3: printf("BAD\n"); break;
    case 5: printf("BAD\n"); break;
    case 9: printf("BAD\n"); break;
    case 10: printf("GOOD\n"); break;
    case 11: printf("BAD\n"); break;
    }

    switch (a) {
    case 1: printf("BAD\n"); break;
    case 10: printf("GOOD\n"); break;
    case 20: printf("BAD\n"); break;
    case 1000: printf("BAD\n"); break;
    case 2000: printf("BAD\n"); break;
    case 5: printf("BAD\n"); break;
    case 2: printf("BAD\n"); break;
    default: printf("BAD\n"); break;
    }

    e = 8589934592;
    switch (e) {
    case 8589934597: printf("BAD\n"); break;
    case 8589934598: printf("BAD\n"); break;
    case 8589934591: printf("BAD\n"); break;
    case 8589934595: printf("BAD\n"); break;
    case 8589934592: printf("GOOD\n"); break;
    default: printf("BAD\n"); break;
    }

    for (e = 34359738368; ; e <<= 1) {
        switch (e) {
        case 68719476736:   printf("case B\n"); break;
        case 34359738368:   printf("case A\n"); break;
        case 549755813888:  printf("case E\n"); break;
        case 137438953472:  printf("case C\n"); break;
        case 274877906944:  printf("case D\n"); break;
        default:            printf("switch OK!\n"); goto done;
        }
    }
done:
    printf("%lld\n", e);

    return 0;
}
