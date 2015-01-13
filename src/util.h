#ifndef UTIL_H_
#define UTIL_H_

#define TRUE  1
#define FALSE 0

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

unsigned hash(char *s);

#endif
