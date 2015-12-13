#ifndef _STDLIB_H
#define _STDLIB_H

/*
 * Note: commented out functions are not implemented.
 */

#include <stddef.h> /* for size_t, wchar_t, NULL */

#define	EXIT_FAILURE	1	/* Failing exit status.  */
#define	EXIT_SUCCESS	0	/* Successful exit status.  */
// #define RAND_MAX
// #define MB_CUR_MAX

/*
  Missing types:
    div_t,
    ldiv_t,
    lldiv_t
*/

/* Numeric conversion functions */
// double atof(const char *nptr);
int atoi(const char *nptr);
// long int atol(const char *nptr);
// long long int atoll(const char *nptr);
// double strtod(const char *nptr, char **endptr);
// float strtof(const char *nptr, char **endptr);
// long double strtold(const char *nptr, char **endptr);
long int strtol(const char *nptr, char **endptr, int base);
long long int strtoll(const char *nptr, char **endptr, int base);
unsigned long int strtoul(const char *nptr, char **endptr, int base);
unsigned long long int strtoull(const char *nptr, char **endptr, int base);

/* Pseudo-random sequence generation functions */
// int rand(void);
// void srand(unsigned int seed);

/* Memory management functions */
void *calloc(size_t nmemb, size_t size);
void free(void *ptr);
void *malloc(size_t size);
void *realloc(void *ptr, size_t size);

/* Communication with the environment */
// void abort(void);
// int atexit(void (*func)(void));
void exit(int status);
// void _Exit(int status);
char *getenv(const char *name);
// int system(const char *string);

/* Searching and sorting utilities */
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
void qsort(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));

/* Integer arithmetic functions */
// int abs(int j);
// long int labs(long int j);
// long long int llabs(long long int j);
// div_t div(int numer, int denom);
// ldiv_t ldiv(long int numer, long int denom);
// lldiv_t lldiv(long long int numer, long long int denom);

/* Multibyte/wide character conversion functions */
// int mblen(const char *s, size_t n);
// int mbtowc(wchar_t *pwc, const char *s, size_t n);
// int wctomb(char *s, wchar_t wchar);

/* Multibyte/wide string conversion functions */
// size_t mbstowcs(wchar_t *pwcs, const char *s, size_t n);
// size_t wcstombs(char *s, const wchar_t *pwcs, size_t n);

/* POSIX */
long int random(void);
void srandom(unsigned int seed);

#endif
