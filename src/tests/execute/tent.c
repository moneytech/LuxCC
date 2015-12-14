#include <stdio.h>

int x1[];

struct foo x2;
struct foo { char i[3]; };

static struct bar x3;
struct bar { int i[3]; };

static int x4[]; /* rejected by gcc when invoked with -pedantic */

int main(void)
{
    return 0;
}
