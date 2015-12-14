int x0;
typedef int x0; // @ error "`x0' redeclared as different kind of symbol"

typedef int x1;
int x1; // @ error "`x1' redeclared as different kind of symbol"

typedef int x2;
typedef int x2; // @ error "redefinition of typedef `x2'"

int x3;
enum { x3 }; // @ error "`x3' redeclared as different kind of symbol"

enum { x4 };
int x4; // @ error "`x4' redeclared as different kind of symbol"

enum { x5 };
enum { x5 }; // @ error "redeclaration of enumerator `x5'"

void foo(void)
{
    int x6;
    int x6; // @ error "redeclaration of `x6' with no linkage"

    static int x7;
    static int x7; // @ error "redeclaration of `x7' with no linkage"

    extern int x8;
    extern int x8;

    int x9;
    extern int x9; // @ error "extern declaration of `x9' follows declaration with no linkage"

    extern int x10;
    int x10; // @ error "declaration of `x10' with no linkage follows extern declaration"
}
