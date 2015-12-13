/*
    Support library offering 64-bit arithmetic and logical operations.
*/
#include <stdint.h>
#include <string.h>
#include <assert.h>

typedef struct {
    union {
        uint8_t c[8];
        uint16_t s[4];
        uint32_t i[2];
    } d;
} LongLong;

#define CMP_LT 4
#define CMP_EQ 1
#define CMP_GT 2

int __lux_ucmp64(LongLong a, LongLong b)
{
    int i;

    for (i = 1; i >= 0; i--) {
        if (a.d.i[i] > b.d.i[i])
            return CMP_GT;
        else if (a.d.i[i] < b.d.i[i])
            return CMP_LT;
    }
    return CMP_EQ;
}

int __lux_scmp64(LongLong a, LongLong b)
{
    uint32_t sa, sb;

    sa = a.d.i[1]&0x80000000;
    sb = b.d.i[1]&0x80000000;
    if (sa != sb)
        return (sa) ? CMP_LT : CMP_GT;
    else
        return __lux_ucmp64(a, b);
}

long long __lux_shl64(LongLong a, int n)
{
    uint32_t hob;

    if (n <= 0)
        return *(long long *)&a;
    while (n--) {
        hob = !!(a.d.i[0]&0x80000000);
        a.d.i[0] = a.d.i[0]<<1;
        a.d.i[1] = (a.d.i[1]<<1)+hob;
    }
    return *(long long *)&a;
}

long long __lux_ushr64(LongLong a, int n)
{
    uint32_t lob;

    if (n <= 0)
        goto done;
    while (n--) {
        lob = a.d.i[1]&1;
        a.d.i[1] = a.d.i[1]>>1;
        a.d.i[0] = (a.d.i[0]>>1)+(lob<<31);
    }
done:
    return *(long long *)&a;
}

long long __lux_sshr64(LongLong a, int n)
{
    uint32_t lob;

    if (n <= 0)
        goto done;
    while (n--) {
        lob = a.d.i[1]&1;
        a.d.i[1] = (uint32_t)((int32_t)a.d.i[1]>>1);
        a.d.i[0] = (a.d.i[0]>>1)+(lob<<31);
    }
done:
    return *(long long *)&a;
}

long long __lux_mul64(LongLong a, LongLong b)
{
    LongLong r;
    LongLong p1, p2, p3, p4;

    memset(&p1, 0, sizeof(LongLong));
    memset(&p2, 0, sizeof(LongLong));
    memset(&p3, 0, sizeof(LongLong));
    memset(&p4, 0, sizeof(LongLong));
    p1.d.i[0] = a.d.s[0]*b.d.s[0];
    *(uint32_t *)&p2.d.s[1] = a.d.s[0]*b.d.s[1];
    p3.d.i[1] = a.d.s[0]*b.d.s[2];
    p4.d.s[3] = (uint16_t)(a.d.s[0]*b.d.s[3]);
    *(long long *)&r = *(long long *)&p1+*(long long *)&p2+*(long long *)&p3+*(long long *)&p4;

    memset(&p1, 0, sizeof(LongLong));
    memset(&p2, 0, sizeof(LongLong));
    memset(&p3, 0, sizeof(LongLong));
    *(uint32_t *)&p1.d.s[1] = a.d.s[1]*b.d.s[0];
    p2.d.i[1] = a.d.s[1]*b.d.s[1];
    p3.d.s[3] = (uint16_t)(a.d.s[1]*b.d.s[2]);
    *(long long *)&r += *(long long *)&p1+*(long long *)&p2+*(long long *)&p3;

    memset(&p1, 0, sizeof(LongLong));
    memset(&p2, 0, sizeof(LongLong));
    p1.d.i[1] = a.d.s[2]*b.d.s[0];
    p2.d.s[3] = (uint16_t)(a.d.s[2]*b.d.s[1]);
    *(long long *)&r += *(long long *)&p1+*(long long *)&p2;

    memset(&p1, 0, sizeof(LongLong));
    p1.d.s[3] = (uint16_t)(a.d.s[3]*b.d.s[0]);
    *(long long *)&r += *(long long *)&p1;

    return *(long long *)&r;
}

static long long __udivmod64(LongLong a, LongLong b, int retquo)
{
    int i;
    LongLong q, r;

    if (b.d.i[0]==0 && b.d.i[1]==0) {
        i = 0;
        123/i;
    }
    memset(&q, 0, sizeof(LongLong));
    memset(&r, 0, sizeof(LongLong));
    for (i = 63; i >= 0; i--) {
        *(long long *)&r <<= 1;
        r.d.c[0] |= !!(a.d.c[i/8] & (1<<(i%8)));
        if (*(unsigned long long *)&r >= *(unsigned long long *)&b) {
            *(long long *)&r -= *(long long *)&b;
            q.d.c[i/8] |= 1<<(i%8);
        }
    }
    return retquo ? *(long long *)&q : *(long long *)&r;
}

long long __lux_udiv64(LongLong a, LongLong b)
{
    return __udivmod64(a, b, 1);
}

long long __lux_umod64(LongLong a, LongLong b)
{
    return __udivmod64(a, b, 0);
}

static long long __sdivmod64(LongLong a, LongLong b, int retquo)
{
    int i;
    int as, bs;
    LongLong q, r;

    if (b.d.i[0]==0 && b.d.i[1]==0) {
        i = 0;
        123/i;
    }
    as = bs = 0;
    if (a.d.c[7] & 0x80) {
        *(long long *)&a = -*(long long *)&a;
        as = 1;
    }
    if (b.d.c[7] & 0x80) {
        *(long long *)&b = -*(long long *)&b;
        bs = 1;
    }
    memset(&q, 0, sizeof(LongLong));
    memset(&r, 0, sizeof(LongLong));

    for (i = 63; i >= 0; i--) {
        *(long long *)&r <<= 1;
        r.d.c[0] |= !!(a.d.c[i/8] & (1<<(i%8)));
        if (*(long long *)&r >= *(long long *)&b) {
            *(long long *)&r -= *(long long *)&b;
            q.d.c[i/8] |= 1<<(i%8);
        }
    }
    if (retquo) {
        if (as != bs)
            *(long long *)&q = -*(long long *)&q;
        return *(long long *)&q;
    } else {
        if (as)
            *(long long *)&r = -*(long long *)&r;
        return *(long long *)&r;
    }
}

long long __lux_sdiv64(LongLong a, LongLong b)
{
    return __sdivmod64(a, b, 1);
}

long long __lux_smod64(LongLong a, LongLong b)
{
    return __sdivmod64(a, b, 0);
}
