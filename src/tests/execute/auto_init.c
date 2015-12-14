#include <stdio.h>

int main(void)
{
    struct A {
        int a;
        union {
            char b[5];
            char c;
        } d;
        short e;
    } s = { 1, { "abcd" }, 2 }, q = s;

    int m[3][2][1] = {
        { { 'A' }, { 'B' } },
        { { 'C' }, /*zero*/ },
        /*zero, zero*/
    };

    char msg1[] = "hello world!";
    char msg2[] = { "hello again!" };

    struct {
        char nonz1;
        short nonz2;
        int nonz3;
        char z1;
        short z2;
        int z3;
    } sz = { 1, 1, 1 /*,zero, zero, zero*/ };

    printf("%d %s %c %d\n", q.a, s.d.b, q.d.c, s.e);
    printf("%c %c %c %d\n", m[0][0][0], m[0][1][0], m[1][0][0], m[2][1][0]);
    printf("%s\n", msg1);
    printf("%s\n", msg2);
    printf("%d %d %d %d %d %d\n", sz.nonz1, sz.nonz2, sz.nonz3, sz.z1, sz.z2, sz.z3);

    return 0;
}
