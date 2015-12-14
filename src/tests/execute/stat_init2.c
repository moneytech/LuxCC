#include <stdio.h>

typedef struct {
    int x;
    int y[3];
    int z;
} A;

int z0 = ((int)&((A *)0)->y);
int z1 = ((int)&(*((A *)0)).y);
int z2 = (int)&*(int *)5;
int z3 = (int)&((int *)0)[2];

A s = { 11, { 22, 33, 44 }, 55 };
int *z4 = &(&s)->z;
int *z5 = &s.z;
int *z6 = &(&s)->y[1];
int *z7 = &s.y[1];
int *z72 = &*s.y;

A s2[2] = {
    { 11, { 22, 33, 44 }, 55 },
    { 66, { 77, 88, 99 }, 100 }
};
int *z8 = &s2[1].x;
int *z9 = &(s2+1)->x;
int *z10 = &s2[1].y[2];
int *z11 = s2->y;
int *z12 = (*s2).y;
int *z13 = (s2+1)->y+1;
int *z14 = (*(s2+1)).y+1;
int *z142 = s2[1].y;

typedef struct {
    int q;
    A t[3];
    int p;
} B;

B s3[2] = {
    { 12, { { 93, { 94, 95, 96 }, 97 }, { 98, { 99, 100, 101 }, 102 }, { 103, { 104, 105, 106 }, 107 } }, 34 },
    { 56, { { 39, { 40, 41, 42 }, 43 }, { 44, { 45, 46,  47  }, 48 },  { 49,  { 50,  51,  52  }, 53  } }, 78 }
};

int *z15 = &s3[1].t[1].y[1];
int *z16 = &(s3+1)->t[1].z;
int *z17 = ((*((s3+1)->t+2)).y+2);
int *z18 = (((s3+1)->t+2)->y+2);

int main(void)
{
    printf("%d\n", z0);
    printf("%d\n", z1);
    printf("%d\n", z2);
    printf("%d\n", z3);
    printf("%d\n", *z4);
    printf("%d\n", *z5);
    printf("%d\n", *z6);
    printf("%d\n", *z7);
    printf("%d\n", *z72);
    printf("%d\n", *z8);
    printf("%d\n", *z9);
    printf("%d\n", *z10);
    printf("%d\n", *z11);
    printf("%d\n", *z12);
    printf("%d\n", *z13);
    printf("%d\n", *z14);
    printf("%d\n", *z142);
    printf("%d\n", *z15);
    printf("%d\n", *z16);
    printf("%d\n", *z17);
    printf("%d\n", *z18);
    return 0;
}
