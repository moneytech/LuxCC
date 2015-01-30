#ifndef UTIL_H_
#define UTIL_H_

#define TRUE  1
#define FALSE 0

#define ERROR_COLOR        "\x1b[1;31m"
#define WARNING_COLOR      "\x1b[1;33m"
#define INFO_COLOR         "\x1b[1;37m"
#define RESET_ATTR         "\x1b[0m"

// #if FATAL_ERROR
#define ERROR(...)fprintf(stderr, "%s:%d:%d: error: ", SRC_FILE, SRC_LINE, SRC_COLUMN),fprintf(stderr, __VA_ARGS__),fprintf(stderr, "\n"),exit(EXIT_FAILURE)
// #else
// #define ERROR(...)fprintf(stderr, "%s:%d:%d: error: ", SRC_FILE, SRC_LINE, SRC_COLUMN),fprintf(stderr, __VA_ARGS__),fprintf(stderr, "\n")
// #endif

#if DEBUG
#define DEBUG_PRINTF(...) fprintf(stderr, __VA_ARGS__)
#else
#define DEBUG_PRINTF(...)
#endif

unsigned long hash2(long k);
unsigned hash(char *s);

#endif
