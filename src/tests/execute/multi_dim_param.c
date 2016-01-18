#include <stdio.h>

void func_1(int a[2][3][5])
{
    printf("%d\n", a[1][2][3]);
}

void func_2(int a[][3][5])
{
    printf("%d\n", a[1][2][3]);
}

void func_3(int (*a)[3][5])
{
    printf("%d\n", a[1][2][3]);
}

int main(void)
{
    int a[2][3][5] = {
        { { 10, 20, 30, 40, 50 } ,
          { 60, 70, 80, 90, 100 } ,
          { 110, 120, 130, 140, 150 } },
        { { 160, 170, 180, 190, 200 } ,
          { 210, 220, 230, 240, 250 } ,
          { 260, 270, 280, 290, 300 } },
    };
    int (*p)[3][5] = a;
    int (*q)[2][3][5] = &a;

    func_1(a);
    func_2(a);
    func_3(a);

    func_1(p);
    func_2(p);
    func_3(p);

    func_1(*q);
    func_2(*q);
    func_3(*q);

    return 0;
}
