#include "../../sassert.h"

int main(void)
{
    {
        int a;
        int *b;
        int c[10];
        int d(void);
        extern struct A e, f;
        struct B {
            int x;
        } g;

        a++;
        b++;
        *b++;
        c[0]++;
        __static_assert(_ASSERT_IMMUTABLE, c);
        __static_assert(_ASSERT_IMMUTABLE, d);
        __static_assert(_ASSERT_IMMUTABLE, d());
        __static_assert(_ASSERT_IMMUTABLE, e);
        g.x++;
    }
    {
        const int a;
        int *const b;
        const int c[10];
        const int d2(void);
        struct A {
            const int x;
        } e, e2, *f2(void);
        const struct B {
            int x;
        } g;
        struct C {
            struct D {
                struct A x;
            } y;
        } h, i;
        struct E {
            int *const* x[10][20];
        } j, k;
        struct F {
            int **const x[10][20];
        } l, m;

        __static_assert(_ASSERT_IMMUTABLE, a);
        __static_assert(_ASSERT_IMMUTABLE, b);
        __static_assert(_ASSERT_IMMUTABLE, c[0]);
        __static_assert(_ASSERT_IMMUTABLE, d2());
        __static_assert(_ASSERT_IMMUTABLE, e.x);
        __static_assert(_ASSERT_IMMUTABLE, e);
        __static_assert(_ASSERT_IMMUTABLE, f2()->x);
        __static_assert(_ASSERT_IMMUTABLE, g.x);
        __static_assert(_ASSERT_IMMUTABLE, h);
        __static_assert(_ASSERT_IMMUTABLE, l);
        j = k;
    }

    return 0;
}
