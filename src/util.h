#ifndef UTIL_H_
#define UTIL_H_

unsigned hash(char *s);

#if DEBUG
#define DEBUG_PRINTF(...) printf(__VA_ARGS__)
#else
#define DEBUG_PRINTF(...)
#endif

#endif
