// typedefs can be a tricky feature to implement.

typedef int A1;
void f1(void)
{
    int A1;
    int B1 = A1*2;
}


typedef int A2;
void f2(void)
{
    A2 a2;
    long A2;
    A2 = 0;
}


typedef int A3;
void f3(void)
{
    A3 A3;
    int B3 = A3*2;
}


typedef char A4;
void f4(void)
{
    int B4 = sizeof(A4), A4, C4 = sizeof A4;
}


typedef int A5;
void f5(A5 A5, A5 a); // @ error "expecting declaration-specifier"
void f6(A5 A5); /* A5 goes out of scope here */
A5 w; /* again visible as a typedef name */


/*
 * Note related to direct_declarator():
 * how `t (t)' is parsed depend on the context where the declaration appears:
 * 1) It is found in a place where an abstract declarator is allowed,
 * e.g. a parameter declaration.
 * 2) It is found in a place where an abstract declarator is not allowed,
 * e.g. a variable declaration.
 */
typedef signed int t;
t f(t (t)); /* function f is declared with type ‘‘function returning signed int with one
               unnamed parameter with type pointer to function returning signed int with
               one unnamed parameter with type signed int’’ */
void foo1(void) {
    t (t);
    ++t;
}
void foo2(void) {
    long t; /* `long' forces t to be an identifier */
}


/* 6.7.5.3#11 says:
 * "If, in a parameter declaration, an identifier can be treated either as a typedef name
 * or as a parameter name, it shall be taken as a typedef name."
 */
typedef long T;
typedef long U;
int bar(T (U (int a, char b)));
/*
   1) U could be a redundantly parenthesized name of a function which takes (int, char) and
   returns type T, or
   2) U could be the type returned by a function which takes (int, char), which in turn
   is the single parameter of a function returning type T.
6.7.5.3#11 states that the option 2) is the correct interpretation.
   See too http://www.open-std.org/jtc1/sc22/wg14/www/docs/dr_009.html
*/

void ff(void)
{
    sizeof(int (T)); // @ error "invalid application of `sizeof' to a function type"
    {
        int (T);
        sizeof(int (T)); // @ error "identifier not allowed in abstract-declarator"
    }
}
