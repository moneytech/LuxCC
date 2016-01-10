#include <stdio.h>
#include <stdarg.h>

/* long long static initialization */

long long g0 = 0xabcdef1234;
long long g1[] = { 0x12345678910, 0x10987654321, 0xAAAABBBBCCCC };
struct {
    long long m0, m1;
    long long m2;
} g2 = { 0xabcdefffff, 0x123456789, 0xAAAABBBBCCCCD };

void lloptest2(long long a, long long b)
{
    if (a && b)
        printf("%lld && %lld\n", a, b);
    if (a || b)
        printf("%lld || %lld\n", a, b);
    printf("%lld\n", 0 ? a : b);
    printf("%lld\n", 1 ? a : b);
    printf("%lld\n", 0 ? 0 : 0 ? a : b);
    printf("%lld\n", 0 ? 0 : 1 ? a : b);

    printf("%lld\n", a+b);
    printf("%lld\n", a-b);
    printf("%lld\n", b-a);

    printf("%lld\n", a*b);
    printf("%lld\n", a*-b);

    printf("%lld\n", a / b);
    printf("%lld\n", a % b);
    printf("%lld\n", -a / b);
    printf("%lld\n", -a % b);
    printf("%lld\n", a / -b);
    printf("%lld\n", a % -b);

    printf("%lld\n", a | b);
    printf("%lld\n", a ^ b);
    printf("%lld\n", a & b);

    printf("%lld\n", +a);
    printf("%lld\n", -a);
    printf("%lld\n", ~a);
    printf("%d\n", !a);

    printf("%lld\n", +b);
    printf("%lld\n", -b);
    printf("%lld\n", ~b);
    printf("%d\n", !b);

    printf("%d\n", !~-+a);
    printf("%lld\n", -+~-b);

    printf("%d\n", a==b);
    printf("%d\n", a!=b);
    printf("%d\n", a>b);
    printf("%d\n", a<b);
    printf("%d\n", a>=b);
    printf("%d\n", a<=b);

    printf("%lld\n", a*b/b);
    printf("%lld\n", a*7+b/2-9);

    printf("%lld\n", --a);
    printf("%lld\n", ++a);
    printf("%lld\n", b--);
    printf("%lld\n", b++);
    printf("%lld\n", a+b);

    printf("%lld\n", (a, b, a+1));

    printf("%lld\n", a = b);
    printf("%lld\n", a += 5);
    printf("%lld\n", a -= 5);
}

void lloptest(long long a, long long b)
{
    lloptest2(a, b);
    lloptest2(b, a);
}

void ulloptest2(unsigned long long a, unsigned long long b)
{
    if (a && b)
        printf("%llu && %llu\n", a, b);
    if (a || b)
        printf("%llu || %llu\n", a, b);
    printf("%llu\n", 0 ? a : b);
    printf("%llu\n", 1 ? a : b);
    printf("%llu\n", 0 ? 0 : 0 ? a : b);
    printf("%llu\n", 0 ? 0 : 1 ? a : b);

    printf("%llu\n", a+b);
    printf("%llu\n", a-b);
    printf("%llu\n", b-a);

    printf("%llu\n", a*b);
    printf("%llu\n", a*-b);

    printf("%llu\n", a / b);
    printf("%llu\n", a % b);
    printf("%llu\n", -a / b);
    printf("%llu\n", -a % b);
    printf("%llu / %llu = %llu\n", a, b, a / -b);
    printf("%llu\n", a % -b);

    printf("%llu\n", a | b);
    printf("%llu\n", a ^ b);
    printf("%llu\n", a & b);

    printf("%llu\n", +a);
    printf("%llu\n", -a);
    printf("%llu\n", ~a);
    printf("%d\n", !a);

    printf("%llu\n", +b);
    printf("%llu\n", -b);
    printf("%llu\n", ~b);
    printf("%d\n", !b);

    printf("%d\n", !~-+a);
    printf("%llu\n", -+~-b);

    printf("%d\n", a==b);
    printf("%d\n", a!=b);
    printf("%d\n", a>b);
    printf("%d\n", a<b);
    printf("%d\n", a>=b);
    printf("%d\n", a<=b);

    printf("%llu\n", a*b/b);
    printf("%llu\n", a*7+b/2-9);

    printf("%llu\n", --a);
    printf("%llu\n", ++a);
    printf("%llu\n", b--);
    printf("%llu\n", b++);
    printf("%llu\n", a+b);

    printf("%llu\n", (a, b, a+1));

    printf("%llu\n", a = b);
    printf("%llu\n", a += 5);
    printf("%llu\n", a -= 5);
}

void ulloptest(unsigned long long a, unsigned long long b)
{
    ulloptest2(a, b);
    ulloptest2(b, a);
}

void llshift(long long a, int b)
{
    printf("%llx\n", a<<b);
    printf("%llx\n", a>>b);
    printf("%llx\n", (unsigned long long)a>>b);
}

void llarg(long long a, long long b, unsigned long long c, unsigned long long d)
{
    printf("a=lld\n", a);
    printf("b=lld\n", b);
    printf("c=llu\n", c);
    printf("d=llu\n", d);
}

/* long long and vararg */

void llprint(int n, ...)
{
    va_list ap;

    va_start(ap, n);
    while (n)
        printf("n=%d, %lld\n", n, va_arg(ap, long long)), --n;
    va_end(ap);
}

long long llretval(long long a)
{
    return -a;
}

int main(void)
{
    printf("%llx\n", g0);
    printf("%llx, %llx, %llx\n", g1[0], g1[1], g1[2]);
    printf("%llx, %llx, %llx\n", g2.m0, g2.m1, g2.m2);

    /* long long integer constants */

    printf("lliconsts:\n");
    printf("%lld\n", 274877907043);
    printf("%lld\n", 274877907043LL);
    printf("%lld\n", 274877907043ULL);
    printf("%lld\n", 274877907043U);

    printf("%llo\n", 04000000000143);
    printf("%llo\n", 04000000000143LL);
    printf("%llo\n", 04000000000143ULL);
    printf("%llo\n", 04000000000143U);

    printf("%lld\n", 0x4000000063);
    printf("%lld\n", 0x4000000063LL);
    printf("%lld\n", 0x4000000063U);

    {
        /* long long auto initialization */

        long long x0 = 9223372036854775807, x1 = 0x7FFFFFFFFFFFFFFF;
        unsigned long long x2 = 18446744073709551615U, x3 = 0xFFFFFFFFFFFFFFFF;
        long long a0[] = { 0xabcdef122345678, x0, 773738363243222648 };
        struct {
            long long m0, m1;
            unsigned long long m2;
        } s0 = { 0xabcdef122345678, x1, 0x10000000000 };

        printf("llauto:\n");
        printf("%lld, %llx\n", x0, x1);
        printf("%llu, %llx\n", x2, x3);
        printf("%llx, %lld, %lld\n", a0[0], a0[1], a0[2]);
        printf("%llx, %lld, %llx\n", s0.m0, s0.m1, s0.m2);
    }

    /* long long arguments */

    printf("llarg:\n");
    llarg(9223372036854775807, 1099511627776, 18446744073709551615ULL, 8589934592LL);

    /* long long operations */

    printf("llopts:\n");
    lloptest(1, 1);
    lloptest(1, -1);
    lloptest(34359738368, -255);
    lloptest(2199023255480, 32);
    lloptest(32, 1099511627776);
    lloptest(1099511627776, 1125899906842624);
    lloptest(9223372036854775807, 4611686018427387904);

    printf("ullopts:\n");
    ulloptest(1, 1);
    ulloptest(2147483648, 4294967296);
    ulloptest(8589934592, 17179869184);
    ulloptest(9223372036854775808ULL, 18446744073709551615ULL);

    printf("shifts:\n");
    llshift(0x8000000000000000, 63);
    llshift(0x8000000000000000, 31);
    llshift(0x7FFFFFFFFFFFFFFF, 63);
    llshift(0x7FFFFFFFFFFFFFFF, 10);
    {
        int x = 1;
        long long n = 5;

        x = x<<n;
        printf("%d\n", x);
        x = x>>n;
        printf("%d\n", x);
    }

    /* long long return values */

    printf("ret vals:\n");
    printf("%lld\n", llretval(1));
    printf("%lld\n", llretval(-1));
    printf("%lld\n", llretval(18446744073709551615ULL));
    printf("%lld\n", llretval(18446744073709551615ULL)*llretval(1));

    llprint(3, 274877906944, -1LL, 0x80000000LL);

    /* long long and pointers */
    {
        int x[] = { 11, 22, 33, 44, 55 };
        long long i = 3;

        printf("ptrs:\n");
        printf("%d\n", x[i]);
        printf("%d\n", i[x]);
        printf("%d\n", *(x+i));
        printf("%d\n", *(i+x));
        i = 1;
        printf("%d\n", *(&x[4]-i));
    }

    /* long long post/pre increment */
    {
        long long i = 10;

        printf("post/pre inc:\n");
        printf("%lld\n", i++);
        printf("%lld\n", ++i);
        printf("%lld\n", i--);
        printf("%lld\n", --i);
    }

    return 0;
}
