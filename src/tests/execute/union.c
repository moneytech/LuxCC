#include <stdio.h>

typedef union Point3 Point3;
union Point3 {
    struct { int x, y, z; } c1;
    int c2[3];
};

Point3 create_point(int x, int y, int z)
{
    Point3 p = { { x, y, z } };
    return p;
}

Point3 add_points3(Point3 a, Point3 b)
{
    a.c2[0] += b.c1.x;
    a.c1.y += b.c2[1];
    a.c2[2] += b.c1.z;
    return a;
}

void modif_union(Point3 *p)
{
    p->c1.x = 10;
    p->c2[1] = 20;
    p->c1.z = 30;
}

typedef union {
    unsigned x;
    unsigned short word[2];
    unsigned char byte[4];
} A;

int main(void)
{
    Point3 a = create_point(15, 27, 32), b, c;
    A n = { 0xAABBCCDD };

    b.c1.x = 5;
    b.c2[1] = 3;
    b.c1.z = 8;
    printf("%d %d %d\n", a.c1.x, a.c1.y, a.c1.z);
    printf("%d %d %d\n", b.c2[0], b.c2[1], b.c2[2]);

    c = add_points3(a, b);
    printf("%d %d %d\n", c.c1.x, c.c1.y, c.c1.z);
    modif_union(&c);
    printf("%d %d %d\n", c.c2[0], c.c2[1], c.c2[2]);

    printf("%d %d %d\n", a.c1.x, a.c1.y, a.c1.z);
    printf("%d %d %d\n", b.c2[0], b.c2[1], b.c2[2]);

    printf("%x\n", n.x);
    printf("%x%x\n", n.word[0], n.word[1]);
    printf("%x%x%x%x\n", n.byte[0], n.byte[1], n.byte[2], n.byte[3]);

    return 0;
}
