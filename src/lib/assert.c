#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#define MSG "Assertion failed in file %s, function %s(), line %d\n"

void __assert(const char *file, const char *func, int line)
{
    fprintf(stderr, MSG, file, func, line);
    abort();
}
