/* from: http://rosettacode.org/wiki/N-queens_problem#C */
#include <stdio.h>

int abs2(int x)
{
    return (x < 0) ? -x : x;
}

int count = 0;
void solve(int n, int col, int *hist)
{
    int i, j;

	if (col == n) {
		printf("\nNo. %d\n-----\n", ++count);
		for (i = 0; i < n; i++, printf("\n"))
			for (j = 0; j < n; j++)
				printf("%c", j == hist[i] ? 'Q' : ((i + j) & 1) ? ' ' : '.');

		return;
	}

#define attack(i, j) (hist[j] == i || abs2(hist[j] - i) == col - j)
	for (i = 0, j = 0; i < n; i++) {
		for (j = 0; j < col && !attack(i, j); j++)
            ;
		if (j < col)
            continue;

		hist[col] = i;
		solve(n, col + 1, hist);
	}
}

int main(void)
{
    int hist[8];
	solve(8, 0, hist);
    return 0;
}
