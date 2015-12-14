/* from: http://rosettacode.org/wiki/N-queens_problem#C */
#include <stdio.h>
#include <stdlib.h>

typedef unsigned uint;
uint full, *qs, count = 0, nn;

void solve(uint d, uint c, uint l, uint r)
{
	uint b, a, *s;
	if (!d) {
		count++;
		printf("\nNo. %d\n===========\n", count);
		for (a = 0; a < nn; a++, printf("\n"))
			for (b = 0; b < nn; b++, printf(" "))
				printf("%c", " -QQ"[((b == qs[a])<<1)|((a + b)&1)]);
		return;
	}

	a = (c | (l <<= 1) | (r >>= 1)) & full;
	if (a != full)
		for (*(s = qs + --d) = 0, b = 1; b <= full; (*s)++, b <<= 1)
			if (!(b & a)) solve(d, b|c, b|l, b|r);
}

int main(void)
{
	nn = 8;
	qs = calloc(nn, sizeof(int));
	full = (1U << nn) - 1;

	solve(nn, 0, 0, 0);
	printf("\nSolutions: %d\n", count);
	return 0;
}
