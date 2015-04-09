#ifndef UTIL_H_
#define UTIL_H_

#define TRUE  1
#define FALSE 0

#define TERMINATE(...) fprintf(stderr, __VA_ARGS__), fprintf(stderr, "\n"), exit(EXIT_FAILURE)

#if DEBUG
#define DEBUG_PRINTF(...) fprintf(stderr, __VA_ARGS__)
#else
#define DEBUG_PRINTF(...)
#endif

/*
 * Currently stringification is not implemented, and there is no predefined
 * macro names __LINE__, __FILE__, etc, so this is a cheap replacement for
 * assert() until those features are available. The caller is expected to provide
 * the function name.
 */
#define my_assert(expr, msg) do { if (!(expr)) fprintf(stderr, "Assertion failed at function `%s'\n", msg), exit(1); } while (0)

#define equal(s, t)     (strcmp((s), (t)) == 0)
#define not_equal(s, t) (strcmp((s), (t)) != 0)

#define NELEMS(a) (sizeof(a)/sizeof(a[0]))

unsigned long hash2(unsigned long k);
unsigned hash(char *s);
int round_up(int num, int multiple);

#endif
