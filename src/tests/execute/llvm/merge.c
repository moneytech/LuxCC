#include <stdio.h>

void merge_sort(int m, int n, int X[], int Y[], int Z[])
{
	int i = 0, j = 0, k = 0; 	/* index variables for X, Y, and Z */
	while ((i < m) && (j < n))
		if (X[i] <= Y[j])		/* find largest of two */
			Z[k++] = X[i++]; 	/* copy and update indices */
		else
			Z[k++] = Y[j++]; 	/* copy and update indices */
	/* copy remainder of input array */
	if (i < m)
		while (i < m)
			Z[k++] = X[i++];
	else
		while (j < n)
			Z[k++] = Y[j++];
}

int main(void)
{
	int a1[5] = {55, 100, 532, 6111, 94343};
	int a2[3] = {4, 777, 7000};
	int a3[8], i;
	merge_sort(5, 3, a1, a2, a3);
	for (i = 0; i < 8; i++)
		printf("%d ", a3[i]);
    printf("\n");
	return 0;
}
