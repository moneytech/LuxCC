#include "util.h"

unsigned hash(char *s)
{
    unsigned hash_val;

    for (hash_val = 0; *s != '\0'; s++)
        hash_val = (unsigned)*s + 31*hash_val;
    return hash_val;
}

unsigned long hash2(unsigned long k)
{
    return k*(k+3);
}

/*
 * Round up `num' to the nearest multiple of `multiple'.
 * Negative numbers are rounded away from zero.
 */
int round_up(int num, int multiple)
{
    int remainder;

    if (multiple == 0)
        return num;
    remainder = num%multiple;
    if (remainder == 0)
        return num;
    if (num < 0)
        return num-multiple-remainder;
    else
        return num+multiple-remainder;
}
