struct A1 {
    int x;
};
void f0(void)
{
    struct A1; // new incomplete type
    struct A1 x; // @ error "`x' has no linkage and incomplete type"
}


struct A2 {
    int x;
};
void f1(void)
{
    struct A2 x; // no new type, references the previous
}


void f2(void)
{
    struct A3 x; // @ error "`x' has no linkage and incomplete type"
}


struct A4; // new incomplete type
struct A4 {
    int x;
}; // type completed
struct A4; // no new type
struct A4 a4s; // OK


struct A5 {
    int x;
};
void f3(void)
{
    struct A5 x;
    struct A5; // new incomplete type
    struct A5 x2; // @ error "`x2' has no linkage and incomplete type"
}

union A6 {
    int x;
};
void f4(void)
{
    struct A6 x; // @ error "use of `A6' with tag type that does not match previous declaration"
}

struct A7;
void f5(void)
{
    struct A7 *x; // no new type, references the previous
}


void f6(struct A8);
// A8 goes out of scope...
void f6(struct A8); // @ error "conflicting types"


struct A9;
void f7(struct A9);
void f7(struct A9);
