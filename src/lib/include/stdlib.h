#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

typedef struct {
    int quot;
    int rem;
} div_t;

typedef struct {
    long int quot;
    long int rem;
} ldiv_t;

typedef struct {
    long long int quot;
    long long int rem;
} lldiv_t;

#define EXIT_FAILURE    1
#define EXIT_SUCCESS    0

#define RAND_MAX        32767
/*#define MB_CUR_MAX*/

// ===========
// Standard C
// ===========

/* Numeric conversion functions */
int atoi(const char *nptr);
long int atol(const char *nptr);
long long int atoll(const char *nptr);
long int strtol(const char *nptr, char **endptr, int base);
long long int strtoll(const char *nptr, char **endptr, int base);
unsigned long int strtoul(const char *nptr, char **endptr, int base);
unsigned long long int strtoull(const char *nptr, char **endptr, int base);

/* Pseudo-random sequence generation functions */
int rand(void);
void srand(unsigned int seed);

/* Communication with the environment */
void abort(void);
int atexit(void (*func)(void));
void exit(int status);
void _Exit(int status);
char *getenv(const char *name);
int system(const char *string);

/* Memory management functions */
void *malloc(size_t size);
void free(void *ptr);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);

/* Searching and sorting utilities */
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
void qsort(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));

/* Integer arithmetich functions */
int abs(int j);
long int labs(long int j);
long long int llabs(long long int j);
div_t div(int numer, int denom);
ldiv_t ldiv(long int numer, long int denom);
lldiv_t lldiv(long long int numer, long long int denom);

// ===========
// POSIX
// ===========
long random(void);
void srandom(unsigned int seed);

#endif
