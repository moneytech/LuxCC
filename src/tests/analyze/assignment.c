int main(void)
{
    char a;
    short b;
    int c;
    unsigned d;
    long e;

    void *f;

    struct A {
        int x;
    } g;
    struct B {
        int x;
    } h;
    union C {
        int x;
    } i;

    char *k;
    int *l;
    short (*m)(void);

    const int *n;
    int *const*o;
    const void *p;
    const int (*q)[10];
    const volatile long *r;
    const long *t;

    a = b; // @ warning "precision: `short' to `char'"
    b = c; // @ warning "precision: `int' to `short'"
    c = e;
    a = d; // @ warning "precision: `unsigned' to `char'"
    c = d; // @ warning "signedness: `unsigned' to `int'"

    c = f; // @ warning "pointer to integer conversion without a cast"
    f = c; // @ warning "integer to pointer conversion without a cast"

    g = h; // @ error "incompatible types"
    i = g; // @ error "incompatible types"
    {
        struct A {
            int x;
        } j;
        j = g; // @ error "incompatible types"
    }

    k = l; // @ warning "assignment from incompatible pointer type"
    m = k; // @ warning "assignment from incompatible pointer type"
    f = k;
    k = f;
    m = f; // @ warning "void pointer implicitly converted to function pointer"
    f = m; // @ warning "function pointer implicitly converted to void pointer"
    k = 0;
    m = 0;

    f = n; // @ warning "assignment discards `const' qualifier from pointer target type"
    p = n;
    o = p;
    q = p; // @ warning "assignment discards `const' qualifier from pointer target type"
    t = r; // @ warning "assignment discards `volatile' qualifier from pointer target type"
}
