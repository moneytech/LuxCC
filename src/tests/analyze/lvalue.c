int main(void)
{
    int a;
    int *b;
    int c[10];
    struct A {
        int k;
    } d, e(void), *f(void);

    &"abc";

    &123; // @ error "invalid operand to &"

    &a;
    &(a);

    &*b;

    &c;
    &c[0];

    d.k++;

    e().k++; // @ error "expression is not modifiable"

    f()->k++;

    return 0;
}
