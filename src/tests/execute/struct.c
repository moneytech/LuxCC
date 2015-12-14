#include <stdio.h>

struct Point2 { int x, y; };
typedef struct Point2 Point2;

Point2 create_point(int x, int y)
{
    Point2 p = { x, y };
    return p;
}

Point2 add_points2(Point2 a, Point2 b)
{
    // a.x += b.x;
    // a.y += b.y;
    b.x += a.x;
    b.y += a.y;
    // return a;
    return b;
}

typedef struct Point3 Point3;
struct Point3 { Point2 k; int z; };

Point3 add_points3(Point3 a, Point3 b)
{
    a.k.x += b.k.x;
    a.k.y += b.k.y;
    a.z += b.z;
    return a;
}

void modif_struct(Point3 *p)
{
    p->k.x = 10;
    p->k.y = 20;
    p->z = 30;
}

typedef struct {
    char s[32];
} String;

String create_string(char *s)
{
    String n;
    char *p = n.s;

    while (*s != '\0')
        *p++ = *s++;
    *p = '\0';
    return n;
}

typedef struct { char s[5]; } Odd;

void print_odd(Odd a)
{
    printf("%s\n", a.s);
}

int main(void)
{
    Point2 a = create_point(-10, 15);
    Point2 b = create_point(5, -10);
    Point2 c;

    Point3 d = { create_point(256, 128), 100 }, e, f;

    String s1 = { "abcd" };
    String s2 = s1;

    Odd x = { "xyzw" };

    c = add_points2(a, b);
    printf("%d %d\n", c.x, c.y);
    c.x = 0, c.y = 0;

    c.x = add_points2(a, b).x;
    c.y = add_points2(a, b).y;
    printf("%d %d\n", c.x, c.y);

    e.k = create_point(4, 2);
    e.z = 100;

    c = add_points2(d.k, e.k);
    printf("%d %d\n", c.x, c.y);

    f = add_points3(d, e);
    printf("%d %d %d\n", f.k.x, f.k.y, f.z);
    modif_struct(&f);
    printf("%d %d %d\n", f.k.x, f.k.y, f.z);

    printf("%s\n", s2.s);
    s2 = create_string("xyzw");
    printf("%s\n", s2.s);
    /* Google 'printing member of returned struct' for more info */
    // printf("%s\n", create_string("xyzw").s);

    /* all original values should remain unmodified */
    printf("%d %d\n", a.x, a.y);
    printf("%d %d\n", b.x, b.y);
    printf("%d %d %d\n", d.k.x, d.k.y, d.z);

    print_odd(x);

    return 0;
}
