// #include <stdio.h>
int printf(const char *, ...);

void insert(int *a, int n)
{
    int x, i, j;
    i = 1;
    while (i < n) {
        x = a[i];
        j = i-1;
        while ((j>-1) && (x<a[j])) {
            a[j+1] = a[j];
            --j;
        }
        a[j+1] = x;
        ++i;
    }
}

void bubble(int *a, int n)
{
    int done = 0;
    while (!done) {
        int i;
        --n, done = 1;
        for (i = 0; i < n; i++) {
            if (a[i] > a[i+1]) {
                int temp = a[i];
                a[i] = a[i+1];
                a[i+1] = temp;
                done = 0;
            }
        }
    }
}

void select(int *a, int n)
{
    int i, j, x, min;
    for(i = 0; i < n-1; i++) {
        x = a[i];
        min = i;
        for(j = i+1; j < n; j++) {
            if(a[j] < x) {
                min = j;
                x = a[j];
            }
        }
        a[min] = a[i];
        a[i] = x;
    }
}

void init_array(int *a)
{
    a[0] = 4;
    a[1] = 2;
    a[2] = 5;
    a[3] = 1;
    a[4] = 3;
}

void print_array(int *a)
{
    int i;

    for (i = 0; i < 5; i++)
        printf((i!=4)?"%d, ":"%d\n", a[i]);
}

int main(void)
{
    /*int x[5], i;
    printf("Enter five numbers to sort:\n");
    for (i = 0; i < 5; i++)
        scanf("%d", &x[i]);*/
    int x[5];

    init_array(x);
    insert(x, 5);
    print_array(x);

    init_array(x);
    bubble(x, 5);
    print_array(x);

    init_array(x);
    select(x, 5);
    print_array(x);


    return 0;
}
