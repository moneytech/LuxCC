#include <stdio.h>

void swap(int v[], int i, int j)
{
	int temp;
	temp = v[i];
	v[i] = v[j];
	v[j] = temp;
}

void quick_sort(int v[], int left, int right)
{
	int i, last;
	if (left >= right)
		return;
	swap(v, left, (left+right)/2);
	last = left;
	for (i = left+1; i <= right; i++)
		if (v[i] < v[left])
			swap(v, ++last, i);
	swap(v, left, last);
	quick_sort(v, left, last - 1);
	quick_sort(v, last+1, right);
}

int main(void)
{
	// int a[5], i;
    // printf("Enter five numbers to sort:\n");
    // for (i = 0; i < 5; i++)
        // scanf("%d", &a[i]);
    int a[5] = { 4, 2, 5, 1, 3 }, i;
	quick_sort(a, 0, 4);
	for (i = 0; i < 5; i++)
		printf("%d ", a[i]);
    printf("\n");

	return 0;
}
