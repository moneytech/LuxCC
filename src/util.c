#include "util.h"
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>

char *replace_extension(char *fname, char *newext)
{
    char *p;

    if ((p=strrchr(fname, '.')) == NULL) {
        p = malloc(strlen(fname)+strlen(newext)+1);
        strcpy(p, fname);
        strcat(p, newext);
    } else {
        int n;

        n = p-fname;
        p = malloc(n+strlen(newext)+1);
        strncpy(p, fname, n);
        p[n] = '\0';
        strcat(p, newext);
    }
    return p;
}

int file_exist(char *file_path)
{
    struct stat st;

    return (stat(file_path, &st) == 0);
}

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
 * Round up `num' to the nearest multiple of `mul'.
 * Negative numbers are rounded toward negative infinity.
 */
int round_up(int num, int mul)
{
    int rem;

    if (mul==0 || (rem=num%mul)==0)
        return num;
    return (num < 0) ? num-mul-rem : num+mul-rem;
}

int ilog2(unsigned val)
{
    int x = -1;

    while (val != 0) {
        val >>= 1;
        ++x;
    }
    return x;
}
