extern int x0;
int x0;

int x1;
extern int x1;

static int x2;
extern int x2;

int x3;
static int x3; // @ error "static declaration of `x3' follows non-static declaration"

static int x4;
int x4; // @ error "non-static declaration of `x4' follows static declaration"

extern int x5;
static int x5; // @ error "static declaration of `x5' follows non-static declaration"

/*
 * Same as above but at block and file scope.
 */
int x6;
void f0(void)
{
    extern int x6;
}

static int x7;
void f1(void)
{
    extern int x7;
}

void f2(void)
{
    extern int x8;
}
int x8;

void f3(void)
{
    extern int x9;
}
static int x9; // @ error "static declaration of `x9' follows non-static declaration"

void f4_1(void)
{
    extern int x10;
}
void f4_2(void)
{
    extern char x10; // @ error "conflicting types"
}

int x11;         // status: tentatively defined
extern int x11;  // status: tentatively defined
int x11 = 0;     // status: defined

extern int x12;  // status: referenced
int x12;         // status: tentatively defined
int x12 = 0;     // status: defined

int f5(char f(unsigned f2(long w),
              short w),
       unsigned char w)
{
    int w; // @ error "redeclaration of `w' with no linkage"
}

void f6(const int);
void f6(int);

void f7(int *const);
void f7(int *);

void f8(const int *);
void f8(int *); // @ error "conflicting types for `f8'"
