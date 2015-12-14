#include <stdio.h>

struct A {
    int a;
    union {
        char b[5];
        char c;
    } d;
    short e;
} s = { 1, { "abcd" }, 2 };

int m[3][2][1] = {
    { { 'A' }, { 'B' } },
    { { 'C' }, /*zero*/ },
    /*zero, zero*/
}, (*mp)[2][1] = m+1;

char msg[] = "hello world!", *msg2 = msg+6;

struct {
    char nonz1;
    short nonz2;
    int nonz3;
    char z1;
    short z2;
    int z3;
} sz = { 1, 1, 1 /*,zero, zero, zero*/ };

unsigned char x1  = 0xAABBCCDD;
unsigned short x2 = 0xAABBCCDD;
unsigned int x3   = 0xAABBCCDD;

int foo(void) { return 199; }
int (*fp)(void) = foo;

int main(void)
{
    printf("%d %s %c %d\n", s.a, s.d.b, s.d.c, s.e);
    printf("%c %c %c %d\n", m[0][0][0], m[0][1][0], m[1][0][0], m[2][1][0]);
    printf("%c\n", mp[0][0][0]);
    printf("%s\n", msg);
    printf("%lu\n", sizeof(msg));
    printf("%s\n", msg2);
    printf("%d %d %d %d %d %d\n", sz.nonz1, sz.nonz2, sz.nonz3, sz.z1, sz.z2, sz.z3);
    printf("%x %x %x\n", x1, x2, x3);
    printf("%d\n", fp());

    return 0;
}
