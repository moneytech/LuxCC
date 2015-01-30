#include "util.h"

unsigned hash(char *s)
{
    unsigned hash_val;

    for (hash_val = 0; *s != '\0'; s++)
        hash_val = (unsigned)*s + 31*hash_val;
    return hash_val;
}

unsigned long hash2(long k)
{
    return (unsigned long)(k*(k+3));
}
