/*
	Test x64 calling conventions.
*/
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

void f1(int a, int b, int c, int d, int e, int f)
{
	printf("%d %d %d %d %d %d\n", a, b, c, d, e, f);
}

void f2(int a, int b, int c, int d, int e, int f, int g, int h, int i)
{
	printf("%d %d %d %d %d %d %d %d %d\n", a, b, c, d, e, f, g, h, i);
}

typedef struct {
	char a[16];
} A1;

void f3(int a, int b, int c, int d, int e, int f, A1 g, A1 h, int i, int j, int k)
{
	printf("%d %d %d %d %d %d %s %s %d %d %d\n", a, b, c, d, e, f, g.a, h.a, i, j, k);
}

typedef struct {
	char a[24];
} A2;

void f4(int a, int b, int c, int d, int e, int f, A2 g, A2 h, int i, int j, int k)
{
	printf("%d %d %d %d %d %d %s %s %d %d %d\n", a, b, c, d, e, f, g.a, h.a, i, j, k);
}

typedef struct {
	char a[32];
} A3;

void f5(int a, A2 b, A3 c, int d)
{
	printf("%d %d\n%s\n%s\n", a, d, b.a, c.a);
}

int f6(int a, int b)
{
    return a+b;
}

long f7(long a, long b)
{
    return a+b;
}

A1 f8(char *s)
{
    A1 x;
    strcpy(x.a, s);
    return x;
}

typedef struct {
	char a[8];
} A4;

A4 f9(char *s)
{
	A4 x;
	strcpy(x.a, s);
	return x;
}

typedef struct {
	char a[15];
} A5;

A5 f10(char *s)
{
	A5 x;
	strcpy(x.a, s);
	return x;
}

A3 f11(char *s1, char *s2)
{
	A3 x;
	strcpy(x.a, s1);
	strcat(x.a, s2);
	return x;
}

void f12(int n, ...)
{
	va_list ap;

	va_start(ap, n);
	while (n--)
		printf("%d ", va_arg(ap, int));
	printf("\n");
}

A3 f13(long a, long b, long c, long d, ...)
{
	va_list ap;
	A3 x;
	A1 tmp;

	va_start(ap, d);
	a += b+c+d;

	/*
     * => x64
	 * expect long (in register), A1 (in stack),
	 * long (in stack (remember the hidden first arg)),
	 * and A1 (in stack).
	 */
	a += va_arg(ap, long);
	tmp = va_arg(ap, A1);
	strcpy(x.a, tmp.a);
	a += va_arg(ap, long);
	tmp = va_arg(ap, A1);
	strcat(x.a, tmp.a);

	printf("a=%ld\n", a);
	return x;
}

int main(void)
{
	f1(1, 2, 3, 4, 5, 6);
	f2(1, 2, 3, 4, 5, 6, 7, 8, 9);
	{
		A1 x0 = { "hello, world!" };
		A1 x1 = { "world, hello!" };
		f3(1, 2, 3, 4, 5, 6, x0, x1, 7, 8, 9);
	}
	{
		A2 x0 = { "ABCDEFGHIJKLMNOPQ" };
		A2 x1 = { "12345678910111213" };
		f4(1, 2, 3, 4, 5, 6, x0, x1, 7, 8, 9);
	}
	{
		A2 x0 = { "<X><X><X><X><X><X><X>" };
		A3 x1 = { "<X><X><X><X><X><X><X><X><X>" };
		f5(10, x0, x1, 20);
	}
    printf("%d\n", f6(f6(10, 20), f6(30, 10)));
    printf("%ld\n", f7(10L, 90L));
    {
		A1 x0 = f8("hello, world!");
		printf("%s\n", x0.a);
	}
	{
		A4 x0 = f9("ABCDEFG");
		printf("%s\n", x0.a);
	}
	{
		A5 x0 = f10("0123456789ABCD");
		printf("%s\n", x0.a);
	}
	{
		A3 x0 = f11("0123456789", "ABCDEFGHIJKLMNOPQR");
		printf("%s\n", x0.a);
	}
	f12(9, 10, 20, 30, 40, 50, 60, 70, 80, 90);
	{
		A1 x0 = { "hello, " };
		A1 x1 = { "world!" };
		A3 x2 = f13(5L, 10L, 20L, 40L, 80L, x0, 160L, x1);
		printf("%s\n", x2.a);
	}
    return 0;
}
