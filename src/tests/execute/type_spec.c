/*
 * analyze_decl_specs() was forgetting to copy pointers
 * and this led to a crash further in the processing.
 */

typedef int int2;
struct A { int a; long b; };
union B { int a; long b; };
enum C { ONE, TWO };
struct D {
    char a;
    int b;
} const s;

int main(int argc, char *argv[])
{
    int2 const *x0;
    struct A const *x1;
    union B const x2;
    enum C const x3;
    struct D x;

    return 0;
}
