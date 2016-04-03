#include <stdio.h>

typedef struct A A;

struct A {
    int x;
    int y;
};

extern A g;         /* install_external_id() is called */
A g = { 10, 20 };   /* analyze_init_declarator() should update the
                       previously installed entry */

int main(void)
{
    printf("%d %d\n", g.x, g.y);
    return 0;
}
