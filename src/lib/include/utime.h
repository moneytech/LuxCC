#ifndef _UTIME_H
#define _UTIME_H

#include <time.h>

struct utimbuf {
    time_t actime;  /* access time */
    time_t modtime; /* modification time */
};

int utime(const char *filename, const struct utimbuf *times);

#endif
