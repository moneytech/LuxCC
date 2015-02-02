#ifndef UTIL_H_
#define UTIL_H_

#define TRUE  1
#define FALSE 0

#define ERROR_COLOR        "\x1b[1;31m"
#define WARNING_COLOR      "\x1b[1;33m"
#define INFO_COLOR         "\x1b[1;37m"
#define RESET_ATTR         "\x1b[0m"

#define PRINT_ERROR(file, line, column, ...)\
    fprintf(stderr, INFO_COLOR "%s:%d:%d: " ERROR_COLOR "error: " RESET_ATTR, file, line, column),\
    fprintf(stderr, __VA_ARGS__),\
    fprintf(stderr, "\n")

#define PRINT_WARNING(file, line, column, ...)\
    fprintf(stderr, INFO_COLOR "%s:%d:%d: " WARNING_COLOR "warning: " RESET_ATTR, file, line, column),\
    fprintf(stderr, __VA_ARGS__),\
    fprintf(stderr, "\n")

#if DEBUG
#define DEBUG_PRINTF(...) fprintf(stderr, __VA_ARGS__)
#else
#define DEBUG_PRINTF(...)
#endif

unsigned long hash2(long k);
unsigned hash(char *s);

#endif
