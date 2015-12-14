// #include <stdio.h>
int printf(const char *, ...);

#define SWITCH_LENGTH 8

/*
 * Duff's device loop unrolling.
 */
void duff_device(int *dest, int *src, int len)
{
	int n = (len+SWITCH_LENGTH-1)/SWITCH_LENGTH; /* number of iterations */
	switch (len % SWITCH_LENGTH) {
		case 0:
			do {
				*dest++ = *src++;
		case 7: *dest++ = *src++;
		case 6: *dest++ = *src++;
		case 5: *dest++ = *src++;
		case 4: *dest++ = *src++;
		case 3: *dest++ = *src++;
		case 2: *dest++ = *src++;
		case 1: *dest++ = *src++;
			} while (--n > 0);
	}
}

int main(void)
{
	int s[] = {1, 2, 3, 4, 5}, d[5], i;
	duff_device(d, s, 5);
	for (i = 0; i < 5; i++)
        printf("%d\n", d[i]);

	return 0;
}
