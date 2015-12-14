/*
    From Fabrice Bellard's tcc (http://bellard.org/tcc/).
    Modified to compile with luxcc.
*/

/*
 * TCC auto test program
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdint.h>
#include <stddef.h>

void string_test(void);
void expr_test(void);
void scope_test(void);
void forward_test(void);
void funcptr_test(void);
void loop_test(void);
void goto_test(void);
void enum_test(void);
void typedef_test(void);
void struct_test(void);
void array_test(void);
void expr_ptr_test(void);
void bool_test(void);
void expr2_test(void);
void constant_expr_test(void);
void expr_cmp_test(void);
void char_short_test(void);
void cast_test(void);
void longlong_test(void);
void stdarg_test(void);
void relocation_test(void);
void sizeof_test(void);
void global_data_test(void);
void callsave_test(void);
void builtin_frame_address_test(void);

int fib(int n);
void num(int n);
void forward_ref(void);
int isid(int c);

#define MACRO_NOARGS()

#define AAA 3
#undef AAA
#define AAA 4

#if 1
#define B3 1
#elif 1
#define B3 2
#elif 0
#define B3 3
#else
#define B3 4
#endif

void macro_test(void)
{
    printf("macro:\n");
    printf("aaa=%d\n", AAA);
    printf("B3=%d\n", B3);

#ifdef A
    printf("A defined\n");
#endif
#ifdef B
    printf("B defined\n");
#endif
#ifdef A
    printf("A defined\n");
#else
    printf("A not defined\n");
#endif
#ifdef B
    printf("B defined\n");
#else
    printf("B not defined\n");
#endif

#ifdef A
    printf("A defined\n");
#ifdef B
    printf("B1 defined\n");
#else
    printf("B1 not defined\n");
#endif
#else
    printf("A not defined\n");
#ifdef B
    printf("B2 defined\n");
#else
    printf("B2 not defined\n");
#endif
#endif

#if 1+1
    printf("test true1\n");
#endif
#if 0
    printf("test true2\n");
#endif
#if 1-1
    printf("test true3\n");
#endif
#if defined(A)
    printf("test trueA\n");
#endif
#if defined(B)
    printf("test trueB\n");
#endif

#if 0
    printf("test 0\n");
#elif 0
    printf("test 1\n");
#elif 2
    printf("test 2\n");
#else
    printf("test 3\n");
#endif

    MACRO_NOARGS();
}

int op(int a, int b)
{
    return a / b;
}

int ret(int a)
{
    if (a == 2)
        return 1;
    if (a == 3)
        return 2;
    return 0;
}

void ps(const char *s)
{
    int c;
    while (1) {
        c = *s;
        if (c == 0)
            break;
        printf("%c", c);
        s++;
    }
}

const char foo1_string[] = "\
bar\n\
test\14\
1";

void string_test()
{
    unsigned int b;
    printf("string:\n");
    printf("\141\142" "3\143\n");/* dezdez test */
    printf("\x41\x42\x43\x3a\n");
    printf("c=%c\n", 'r');
    // printf("wc=%C 0x%lx %C\n", L'a', L'\x1234', L'c');
    printf("foo1_string='%s'\n", foo1_string);
#if 0
    printf("wstring=%S\n", L"abc");
    printf("wstring=%S\n", L"abc" L"def" "ghi");
    printf("'\\377'=%d '\\xff'=%d\n", '\377', '\xff');
    printf("L'\\377'=%d L'\\xff'=%d\n", L'\377', L'\xff');
#endif
    ps("test\n");
    b = 32;
    while ((b = b + 1) < 96) {
        printf("%c", b);
    }
    printf("\n");
    printf("fib=%d\n", fib(33));
    b = 262144;
    while (b != 0x80000000) {
        num(b);
        b = b * 2;
    }
}

void loop_test()
{
    int i;
    i = 0;
    while (i < 10)
        printf("%d", i++);
    printf("\n");
    for(i = 0; i < 10;i++)
        printf("%d", i);
    printf("\n");
    i = 0;
    do {
        printf("%d", i++);
    } while (i < 10);
    printf("\n");

#if 0
    char count = 123;
    /* c99 for loop init test */
    for (size_t count = 1; count < 3; count++)
        printf("count=%d\n", count);
    printf("count = %d\n", count);
#endif

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

    /* break/continue tests */
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

    for(i = 0;i < 10;i++) {
        if (i == 3)
            continue;
        printf("%d", i);
    }
    printf("\n");
}

typedef int typedef_and_label;

void goto_test()
{
    int i;
#if 0
    static void *label_table[3] = { &&label1, &&label2, &&label3 };
#endif

    printf("goto:\n");
    i = 0;
    /* This needs to parse as label, not as start of decl.  */
 typedef_and_label:
 s_loop:
    if (i >= 10)
        goto s_end;
    printf("%d", i);
    i++;
    goto s_loop;
 s_end:
    printf("\n");

#if 0
    /* we also test computed gotos (GCC extension) */
    for(i=0;i<3;i++) {
        goto *label_table[i];
    label1:
        printf("label1\n");
        goto next;
    label2:
        printf("label2\n");
        goto next;
    label3:
        printf("label3\n");
    next: ;
    }
#endif
}

enum {
    E0,
    E1 = 2,
    E2 = 4,
    E3,
    E4,
};

enum test {
    E5 = 1000,
};

void enum_test()
{
    enum test b1;
    printf("enum:\n%d %d %d %d %d %d\n",
           E0, E1, E2, E3, E4, E5);
    b1 = 1;
    printf("b1=%d\n", b1);
}

typedef int *my_ptr;

typedef int mytype1;
typedef int mytype2;

void typedef_test()
{
    my_ptr a;
    mytype1 mytype2;
    int b;

    a = &b;
    *a = 1234;
    printf("typedef:\n");
    printf("a=%d\n", *a);
    mytype2 = 2;
    printf("mytype2=%d\n", mytype2);
}

void forward_test()
{
    printf("forward:\n");
    forward_ref();
    forward_ref();
}

void forward_ref(void)
{
    printf("forward ok\n");
}

typedef struct struct1 {
    int f1;
    int f2, f3;
    union union1 {
        int v1;
        int v2;
    } u;
    char str[3];
} struct1;

struct struct2 {
    int a;
    char b;
};

union union2 {
    int w1;
    int w2;
};

struct struct1 st1, st2;

int main(int argc, char **argv)
{
    string_test();
    expr_test();
    scope_test();
    forward_test();
    funcptr_test();
    loop_test();
    goto_test();
    enum_test();
    typedef_test();
    struct_test();
    array_test();
    expr_ptr_test();
    bool_test();
    expr2_test();
    constant_expr_test();
    expr_cmp_test();
    char_short_test();
    cast_test();
    longlong_test();
    stdarg_test();
    relocation_test();
    sizeof_test();
    global_data_test();
    return 0;
}

int tab[3];
int tab2[3][2];

int g;

void f1(int g)
{
    printf("g1=%d\n", g);
}

void scope_test()
{
    printf("scope:\n");
    g = 2;
    f1(1);
    printf("g2=%d\n", g);
    {
        int g;
        g = 3;
        printf("g3=%d\n", g);
        {
            int g;
            g = 4;
            printf("g4=%d\n", g);
        }
    }
    printf("g5=%d\n", g);
}

void array_test()
{
    int i, j, a[4];

    printf("array:\n");
    printf("sizeof(a) = %lu\n", sizeof(a));
    printf("sizeof(\"a\") = %lu\n", sizeof("a"));
#ifdef C99_MACROS
    printf("sizeof(__func__) = %lu\n", sizeof(__func__));
#endif
    printf("sizeof tab %lu\n", sizeof(tab));
    printf("sizeof tab2 %lu\n", sizeof tab2);
    tab[0] = 1;
    tab[1] = 2;
    tab[2] = 3;
    printf("%d %d %d\n", tab[0], tab[1], tab[2]);
    for(i=0;i<3;i++)
        for(j=0;j<2;j++)
            tab2[i][j] = 10 * i + j;
    for(i=0;i<3*2;i++) {
        printf(" %3d", ((int *)tab2)[i]);
    }
    printf("\n");
    printf("sizeof(size_t)=%lu\n", sizeof(size_t));
    printf("sizeof(ptrdiff_t)=%lu\n", sizeof(ptrdiff_t));
}

void expr_test()
{
    int a, b;
    a = 0;
    printf("%d\n", a += 1);
    printf("%d\n", a -= 2);
    printf("%d\n", a *= 31232132);
    printf("%d\n", a /= 4);
    printf("%d\n", a %= 20);
    printf("%d\n", a &= 6);
    printf("%d\n", a ^= 7);
    printf("%d\n", a |= 8);
    printf("%d\n", a >>= 3);
    printf("%d\n", a <<= 4);

    a = 22321;
    b = -22321;
    printf("%d\n", a + 1);
    printf("%d\n", a - 2);
    printf("%d\n", a * 312);
    printf("%d\n", a / 4);
    printf("%d\n", b / 4);
    printf("%d\n", (unsigned)b / 4);
    printf("%d\n", a % 20);
    printf("%d\n", b % 20);
    printf("%d\n", (unsigned)b % 20);
    printf("%d\n", a & 6);
    printf("%d\n", a ^ 7);
    printf("%d\n", a | 8);
    printf("%d\n", a >> 3);
    printf("%d\n", b >> 3);
    printf("%d\n", (unsigned)b >> 3);
    printf("%d\n", a << 4);
    printf("%d\n", ~a);
    printf("%d\n", -a);
    printf("%d\n", +a);

    printf("%d\n", 12 + 1);
    printf("%d\n", 12 - 2);
    printf("%d\n", 12 * 312);
    printf("%d\n", 12 / 4);
    printf("%d\n", 12 % 20);
    printf("%d\n", 12 & 6);
    printf("%d\n", 12 ^ 7);
    printf("%d\n", 12 | 8);
    printf("%d\n", 12 >> 2);
    printf("%d\n", 12 << 4);
    printf("%d\n", ~12);
    printf("%d\n", -12);
    printf("%d\n", +12);
    printf("%d %d %d %d\n",
           isid('a'),
           isid('g'),
           isid('T'),
           isid('('));
}

int isid(int c)
{
    return (c >= 'a' & c <= 'z') | (c >= 'A' & c <= 'Z') | c == '_';
}

/**********************/

int vstack[10], *vstack_ptr;

void vpush(int vt, int vc)
{
    *vstack_ptr++ = vt;
    *vstack_ptr++ = vc;
}

void vpop(int *ft, int *fc)
{
    *fc = *--vstack_ptr;
    *ft = *--vstack_ptr;
}

void expr2_test()
{
    int a, b;

    printf("expr2:\n");
    vstack_ptr = vstack;
    vpush(1432432, 2);
    vstack_ptr[-2] &= ~0xffffff80;
    vpop(&a, &b);
    printf("res= %d %d\n", a, b);
}

void constant_expr_test()
{
    int a;
    printf("constant_expr:\n");
    a = 3;
    printf("%d\n", a * 16);
    printf("%d\n", a * 1);
    printf("%d\n", a + 0);
}

int tab4[10];

void expr_ptr_test()
{
    int *p, *q;
    int i = -1;

    printf("expr_ptr:\n");
    p = tab4;
    q = tab4 + 10;
    printf("diff=%ld\n", q - p);
    p++;
    printf("inc=%ld\n", p - tab4);
    p--;
    printf("dec=%ld\n", p - tab4);
    ++p;
    printf("inc=%ld\n", p - tab4);
    --p;
    printf("dec=%ld\n", p - tab4);
    printf("add=%ld\n", p + 3 - tab4);
    printf("add=%ld\n", 3 + p - tab4);

    /* check if 64bit support is ok */
    q = p = 0;
    q += i;
    printf("0x%lx 0x%lx %ld\n", q, p, p-q);
    printf("%d %d %d %d %d %d\n",
           p == q, p != q, p < q, p <= q, p >= q, p > q);
    i = 0xf0000000;
    p += i;
    printf("0x%lx 0x%lx %ld\n", q, p, p-q);
    printf("%d %d %d %d %d %d\n",
           p == q, p != q, p < q, p <= q, p >= q, p > q);
    p = (int *)((char *)p + 0xf0000000);
    printf("0x%lx 0x%lx %ld\n", q, p, p-q);
    printf("%d %d %d %d %d %d\n",
           p == q, p != q, p < q, p <= q, p >= q, p > q);
    p += 0xf0000000;
    printf("0x%lx 0x%lx %ld\n", q, p, p-q);
    printf("%d %d %d %d %d %d\n",
           p == q, p != q, p < q, p <= q, p >= q, p > q);
    {
        struct size12 {
            int i, j, k;
        };
        struct size12 s[2], *sp = s;
        int i, j;
        sp->i = 42;
        sp++;
        j = -1;
        printf("%d\n", sp[j].i);
    }
}

void expr_cmp_test()
{
    int a, b;
    printf("constant_expr:\n");
    a = -1;
    b = 1;
    printf("%d\n", a == a);
    printf("%d\n", a != a);

    printf("%d\n", a < b);
    printf("%d\n", a <= b);
    printf("%d\n", a <= a);
    printf("%d\n", b >= a);
    printf("%d\n", a >= a);
    printf("%d\n", b > a);

    printf("%d\n", (unsigned)a < b);
    printf("%d\n", (unsigned)a <= b);
    printf("%d\n", (unsigned)a <= a);
    printf("%d\n", (unsigned)b >= a);
    printf("%d\n", (unsigned)a >= a);
    printf("%d\n", (unsigned)b > a);
}

struct aligntest1 {
    char a[10];
};

struct aligntest2 {
    int a;
    char b[10];
};

void struct_test()
{
    struct1 *s;
    union union2 u;

    printf("struct:\n");
    printf("sizes: %lu %lu %lu %lu\n",
           sizeof(struct struct1),
           sizeof(struct struct2),
           sizeof(union union1),
           sizeof(union union2));
    st1.f1 = 1;
    st1.f2 = 2;
    st1.f3 = 3;
    printf("st1: %d %d %d\n",
           st1.f1, st1.f2, st1.f3);
    st1.u.v1 = 1;
    st1.u.v2 = 2;
    printf("union1: %d\n", st1.u.v1);
    u.w1 = 1;
    u.w2 = 2;
    printf("union2: %d\n", u.w1);
    s = &st2;
    s->f1 = 3;
    s->f2 = 2;
    s->f3 = 1;
    printf("st2: %d %d %d\n",
           s->f1, s->f2, s->f3);
    printf("str_addr=%x\n", (int)st1.str - (int)&st1.f1);
}

/* XXX: depend on endianness */
void char_short_test()
{
    int var1, var2;

    printf("char_short:\n");

    var1 = 0x01020304;
    var2 = 0xfffefdfc;
    printf("s8=%d %d\n",
           *(char *)&var1, *(char *)&var2);
    printf("u8=%d %d\n",
           *(unsigned char *)&var1, *(unsigned char *)&var2);
    printf("s16=%d %d\n",
           *(short *)&var1, *(short *)&var2);
    printf("u16=%d %d\n",
           *(unsigned short *)&var1, *(unsigned short *)&var2);
    printf("s32=%d %d\n",
           *(int *)&var1, *(int *)&var2);
    printf("u32=%d %d\n",
           *(unsigned int *)&var1, *(unsigned int *)&var2);
    *(char *)&var1 = 0x08;
    printf("var1=%x\n", var1);
    *(short *)&var1 = 0x0809;
    printf("var1=%x\n", var1);
    *(int *)&var1 = 0x08090a0b;
    printf("var1=%x\n", var1);
}

/******************/

typedef struct Sym {
    int v;
    int t;
    int c;
    struct Sym *next;
    struct Sym *prev;
} Sym;

#define ISLOWER(c) ('a' <= (c) && (c) <= 'z')
#define TOUPPER(c) (ISLOWER(c) ? 'A' + ((c) - 'a') : (c))

static int toupper1(int a)
{
    return TOUPPER(a);
}

void bool_test()
{
    int *s, a, b, t, f, i;

    a = 0;
    s = (void*)0;
    printf("!s=%d\n", !s);

    if (!s || !s[0])
        a = 1;
    printf("a=%d\n", a);

    printf("a=%d %d %d\n", 0 || 0, 0 || 1, 1 || 1);
    printf("a=%d %d %d\n", 0 && 0, 0 && 1, 1 && 1);
    printf("a=%d %d\n", 1 ? 1 : 0, 0 ? 1 : 0);
#if 1 && 1
    printf("a1\n");
#endif
#if 1 || 0
    printf("a2\n");
#endif
#if 1 ? 0 : 1
    printf("a3\n");
#endif
#if 0 ? 0 : 1
    printf("a4\n");
#endif

    a = 4;
    printf("b=%d\n", a + (0 ? 1 : a / 2));

    /* test register spilling */
    a = 10;
    b = 10;
    a = (a + b) * ((a < b) ?
                   ((b - a) * (a - b)): a + b);
    printf("a=%d\n", a);

    /* test complex || or && expressions */
    t = 1;
    f = 0;
    a = 32;
    printf("exp=%d\n", f == (32 <= a && a <= 3));
    printf("r=%d\n", (t || f) + (t && f));
}

/* Actually, my version of gcc (4.7.3) doesn't accept this! */
#if 0
/* GCC accepts that */
static int tab_reinit[];
static int tab_reinit[10];
#endif

//int cinit1; /* a global variable can be defined several times without error ! */
int cinit1;
int cinit1;
int cinit1 = 0;
#if 0
int *cinit2 = (int []){3, 2, 1};
#endif

void num(int n)
{
    char *tab, *p;
    tab = (char*)malloc(20);
    p = tab;
    while (1) {
        *p = 48 + (n % 10);
        p++;
        n = n / 10;
        if (n == 0)
            break;
    }
    while (p != tab) {
        p--;
        printf("%c", *p);
    }
    printf("\n");
    free(tab);
}

/* structure assignment tests */
struct structa1 {
    int f1;
    char f2;
};

struct structa1 ssta1;

void struct_assign_test1(struct structa1 s1, int t, int f)
{
    printf("%d %d %d %d\n", s1.f1, s1.f2, t, f);
}

struct structa1 struct_assign_test2(struct structa1 s1, int t)
{
    s1.f1 += t;
    s1.f2 -= t;
    return s1;
}

void struct_assign_test(void)
{
    struct S {
      struct structa1 lsta1, lsta2;
      int i;
    } s, *ps;

    ps = &s;
    ps->i = 4;
#if 0
    printf("struct_assign_test:\n");

    s.lsta1.f1 = 1;
    s.lsta1.f2 = 2;
    printf("%d %d\n", s.lsta1.f1, s.lsta1.f2);
    s.lsta2 = s.lsta1;
    printf("%d %d\n", s.lsta2.f1, s.lsta2.f2);
#else
    s.lsta2.f1 = 1;
    s.lsta2.f2 = 2;
#endif
    struct_assign_test1(ps->lsta2, 3, 4);

    printf("before call: %d %d\n", s.lsta2.f1, s.lsta2.f2);
    ps->lsta2 = struct_assign_test2(ps->lsta2, ps->i);
    printf("after call: %d %d\n", ps->lsta2.f1, ps->lsta2.f2);

    {
        static struct {
            void (*elem)();
        } t[] = {
            /* XXX: we should allow this even without braces */
            { struct_assign_test }
        };
        printf("%d\n", struct_assign_test == t[0].elem);
    }
}

/* casts to short/char */

void cast1(char a, short b, unsigned char c, unsigned short d)
{
    printf("%d %d %d %d\n", a, b, c, d);
}

char bcast;
short scast;

void cast_test()
{
    int a;
    char c;
    char tab[10];
    unsigned b,d;
    short s;
    char *p = NULL;
    p -= 0x700000000042;

    printf("cast_test:\n");
    a = 0xfffff;
    cast1(a, a, a, a);
    a = 0xffffe;
    printf("%d %d %d %d\n",
           (char)(a + 1),
           (short)(a + 1),
           (unsigned char)(a + 1),
           (unsigned short)(a + 1));
    printf("%d %d %d %d\n",
           (char)0xfffff,
           (short)0xfffff,
           (unsigned char)0xfffff,
           (unsigned short)0xfffff);

    a = (bcast = 128) + 1;
    printf("%d\n", a);
    a = (scast = 65536) + 1;
    printf("%d\n", a);

    printf("sizeof(c) = %lu, sizeof((int)c) = %lu\n", sizeof(c), sizeof((int)c));

    /* test cast from unsigned to signed short to int */
    b = 0xf000;
    d = (short)b;
    printf("((unsigned)(short)0x%08x) = 0x%08x\n", b, d);
    b = 0xf0f0;
    d = (char)b;
    printf("((unsigned)(char)0x%08x) = 0x%08x\n", b, d);

    /* test implicit int casting for array accesses */
    c = 0;
    tab[1] = 2;
    tab[c] = 1;
    printf("%d %d\n", tab[0], tab[1]);

    /* test implicit casting on some operators */
    printf("sizeof(+(char)'a') = %lu\n", sizeof(+(char)'a'));
    printf("sizeof(-(char)'a') = %lu\n", sizeof(-(char)'a'));
    printf("sizeof(~(char)'a') = %lu\n", sizeof(-(char)'a'));

    /* from pointer to integer types */
    printf("%d %d %ld %ld %lld %lld\n",
           (int)p, (unsigned int)p,
           (long)p, (unsigned long)p,
           (long long)p, (unsigned long long)p);

    /* from integers to pointers */
    printf("0x%lx 0x%lx 0x%lx 0x%lx\n",
           (void *)a, (void *)b, (void *)c, (void *)d);
}

/* initializers tests */
struct structinit1 {
    int f1;
    char f2;
    short f3;
    int farray[3];
};

int sinit1 = 2;
int sinit2 = { 3 };
// int sinit3[3] = { 1, 2, {{3}}, };
int sinit4[3][2] = { {1, 2}, {3, 4}, {5, 6} };
// int sinit5[3][2] = { 1, 2, 3, 4, 5, 6 };
int sinit6[] = { 1, 2, 3 };
// int sinit7[] = { [2] = 3, [0] = 1, 2 };
char sinit8[] = "hello" "trala";

struct structinit1 sinit9 = { 1, 2, 3 };

char *sinit12 = "hello world";
char *sinit13[] = {
    "test1",
    "test2",
    "test3",
};
char sinit14[10] = { "abc" };
int sinit15[3] = { sizeof(sinit15), 1, 2 };

struct bar {
        char *s;
        int len;
} sinit17[] = {
        { "a1", 4 },
        { "a2", 1 }
};

int fib(int n)
{
    if (n <= 2)
        return 1;
    else
        return fib(n-1) + fib(n-2);
}

void funcptr_test()
{
    void (*func)(int);
    int a;
    struct {
        int dummy;
        void (*func)(int);
    } st1;

    printf("funcptr:\n");
    func = &num;
    (*func)(12345);
    func = num;
    a = 1;
    a = 1;
    func(12345);
    /* more complicated pointer computation */
    st1.func = num;
    st1.func(12346);
    // printf("sizeof1 = %d\n", sizeof(funcptr_test));
    // printf("sizeof2 = %d\n", sizeof funcptr_test);
    printf("sizeof3 = %lu\n", sizeof(&funcptr_test));
    printf("sizeof4 = %lu\n", sizeof &funcptr_test);
}

void lloptest(long long a, long long b)
{
    unsigned long long ua, ub;

    ua = a;
    ub = b;
    /* arith */
    printf("arith: %lld %lld %lld\n",
           a + b,
           a - b,
           a * b);

    if (b != 0) {
        printf("arith1: %lld %lld\n",
           a / b,
           a % b);
    }

    /* binary */
    printf("bin: %lld %lld %lld\n",
           a & b,
           a | b,
           a ^ b);

    /* tests */
    printf("test: %d %d %d %d %d %d\n",
           a == b,
           a != b,
           a < b,
           a > b,
           a >= b,
           a <= b);

    printf("utest: %d %d %d %d %d %d\n",
           ua == ub,
           ua != ub,
           ua < ub,
           ua > ub,
           ua >= ub,
           ua <= ub);

    /* arith2 */
    a++;
    b++;
    printf("arith2: %lld %lld\n", a, b);
    printf("arith2: %lld %lld\n", a++, b++);
    printf("arith2: %lld %lld\n", --a, --b);
    printf("arith2: %lld %lld\n", a, b);
    b = ub = 0;
    printf("not: %d %d %d %d\n", !a, !ua, !b, !ub);
}

void llshift(long long a, int b)
{
    printf("shift: %lld %lld %lld\n",
           (unsigned long long)a >> b,
           a >> b,
           a << b);
    printf("shiftc: %lld %lld %lld\n",
           (unsigned long long)a >> 3,
           a >> 3,
           a << 3);
    printf("shiftc: %lld %lld %lld\n",
           (unsigned long long)a >> 35,
           a >> 35,
           a << 35);
}

long long llfunc1(int a)
{
    return a * 2;
}

struct S {
    int id;
    char item;
};

long long int value(struct S *v)
{
    return ((long long int)v->item);
}

void longlong_test(void)
{
    long long a, b, c;
    int ia;
    unsigned int ua;
    printf("longlong_test:\n");
    printf("sizeof(long long) = %lu\n", sizeof(long long));
    ia = -1;
    ua = -2;
    a = ia;
    b = ua;
    printf("%lld %lld\n", a, b);
    printf("%lld %lld %lld %llx\n",
           (long long)1,
           (long long)-2,
           1LL,
           0x1234567812345679);
    a = llfunc1(-3);
    printf("%lld\n", a);

    lloptest(1000, 23);
    lloptest(0xff, 0x1234);
    b = 0x72345678 << 10;
    lloptest(-3, b);
    llshift(0x123, 5);
    llshift(-23, 5);
    b = 0x72345678LL << 10;
    llshift(b, 47);

    // llfloat();
#if 1
    b = 0x12345678;
    a = -1;
    c = a + b;
    printf("%llx\n", c);
#endif

    /* long long reg spill test */
    {
          struct S a;

          a.item = 3;
          printf("%lld\n", value(&a));
    }
    lloptest(0x80000000, 0);

    /* another long long spill test */
    {
        long long *p, v;
        v = 1;
        p = &v;
        p[0]++;
        printf("%lld\n", *p);
    }

    a = 68719476720LL;
    b = 4294967295LL;
    printf("%d %d %d %d\n", a > b, a < b, a >= b, a <= b);

    printf("%lld\n", 0x123456789LLU);
}

void vprintf1(const char *fmt, ...)
{
    va_list ap, aq;
    const char *p;
    int c, i;
    long long ll;

    va_start(aq, fmt);
    va_copy(ap, aq);

    p = fmt;
    for(;;) {
        c = *p;
        if (c == '\0')
            break;
        p++;
        if (c == '%') {
            c = *p;
            switch(c) {
            case '\0':
                goto the_end;
            case 'd':
                i = va_arg(ap, int);
                printf("%d", i);
                break;
            case 'l':
                ll = va_arg(ap, long long);
                printf("%lld", ll);
                break;
            }
            p++;
        } else {
            printf("%c", c);
        }
    }
 the_end:
    va_end(aq);
    va_end(ap);
}

struct myspace {
    short int profile;
};

void stdarg_for_struct(struct myspace bob, ...)
{
    struct myspace george, bill;
    va_list ap;
    short int validate;

    va_start(ap, bob);
    bill     = va_arg(ap, struct myspace);
    george   = va_arg(ap, struct myspace);
    validate = va_arg(ap, int);
    printf("stdarg_for_struct: %d %d %d %d\n",
           bob.profile, bill.profile, george.profile, validate);
    va_end(ap);
}

void stdarg_test(void)
{
    struct myspace bob;

    vprintf1("%d %d %d\n", 1, 2, 3);
    bob.profile = 42;
    stdarg_for_struct(bob, bob, bob, bob.profile);
}

int reltab[3] = { 1, 2, 3 };

int *rel1 = &reltab[1];
int *rel2 = &reltab[2];

void relocation_test(void)
{
    printf("*rel1=%d\n", *rel1);
    printf("*rel2=%d\n", *rel2);
}

void sizeof_test(void)
{
    int a;
    int **ptr;

    printf("sizeof(int) = %lu\n", sizeof(int));
    printf("sizeof(unsigned int) = %lu\n", sizeof(unsigned int));
    printf("sizeof(long) = %lu\n", sizeof(long));
    printf("sizeof(unsigned long) = %lu\n", sizeof(unsigned long));
    printf("sizeof(short) = %lu\n", sizeof(short));
    printf("sizeof(unsigned short) = %lu\n", sizeof(unsigned short));
    printf("sizeof(char) = %lu\n", sizeof(char));
    printf("sizeof(unsigned char) = %lu\n", sizeof(unsigned char));
    // printf("sizeof(func) = %lu\n", sizeof sizeof_test());
    a = 1;
    printf("sizeof(a++) = %lu\n", sizeof a++);
    printf("a=%d\n", a);
    ptr = NULL;
    printf("sizeof(**ptr) = %lu\n", sizeof (**ptr));

    /* The type of sizeof should be as large as a pointer, actually
       it should be size_t.  */
    printf("sizeof(sizeof(int) = %lu\n", sizeof(sizeof(int)));
    {
        uintptr_t t = 1;
        uintptr_t t2;
        /* Effectively <<32, but defined also on 32bit machines.  */
        t <<= 16;
        t <<= 16;
        t++;
        /* This checks that sizeof really can be used to manipulate
           uintptr_t objects, without truncation.  */
        t2 = t & -sizeof(uintptr_t);
        printf ("%lu %lu\n", t, t2);
    }
}

struct condstruct {
  int i;
};

int getme (struct condstruct *s, int i)
{
  int i1 = (i == 0 ? 0 : s)->i;
  int i2 = (i == 0 ? s : 0)->i;
#if 0 /* this is a bug in luxcc I'm not willing to fix */
  int i3 = (i == 0 ? (void*)0 : s)->i;
  int i4 = (i == 0 ? s : (void*)0)->i;
#endif
  return i1 + i2 /*+ i3 + i4*/;
}

struct global_data
{
  int a[40];
  int *b[40];
};

struct global_data global_data;

int global_data_getstuff (int *, int);

void global_data_callit (int i)
{
  *global_data.b[i] = global_data_getstuff (global_data.b[i], 1);
}

int global_data_getstuff (int *p, int i)
{
  return *p + i;
}

void global_data_test (void)
{
  global_data.a[0] = 42;
  global_data.b[0] = &global_data.a[0];
  global_data_callit (0);
  printf ("%d\n", global_data.a[0]);
}
