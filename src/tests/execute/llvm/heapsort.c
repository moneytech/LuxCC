/* -*- mode: c -*-
 * $Id$
 * http://www.bagley.org/~doug/shootout/
 */

#include <stdlib.h>
#include <stdio.h>
#define heapsort benchmark_heapsort

#define IM 139968
#define IA   3877
#define IC  29573

#define SMALL_PROBLEM_SIZE

void
heapsort(int n, long *ra) {
    int i, j;
    int ir = n;
    int l = (n >> 1) + 1;
    long rra;

    for (;;) {
	if (l > 1) {
	    rra = ra[--l];
	} else {
            rra = ra[ir];
	    ra[ir] = ra[1];
	    if (--ir == 1) {
		ra[1] = rra;
		return;
	    }
	}

	i = l;
	j = l << 1;
	while (j <= ir) {
	    if (j < ir && ra[j] < ra[j+1]) {
              ++j;
            }
	    if (rra < ra[j]) {
		ra[i] = ra[j];
		j += (i = j);
	    } else {
		j = ir + 1;
	    }
	}
	ra[i] = rra;
    }
}

int
main(int argc, char *argv[]) {
#ifdef SMALL_PROBLEM_SIZE
#define LENGTH 800000
#else
#define LENGTH 8000000
#endif
    int N = 10; //((argc == 2) ? atoi(argv[1]) : LENGTH);
    // long *ary;
    long ary[] = { 100, 200, 82, 10, 9,
                   72,  49,  17, 24, 12,
                   34 };
    int i;

    /* create an array of N random doubles */
    // ary = (long *)malloc((N+1) * sizeof(long));
    // for (i=1; i<=N; i++) {
	// ary[i] = random();
    // }

    heapsort(N, ary);

    printf("%ld\n", ary[N]);

    // free(ary);
    return(0);
}

