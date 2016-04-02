#ifndef _SYS_TIME_H
#define _SYS_TIME_H

struct timeval {
    long tv_sec;    /* seconds */
    long tv_usec;   /* microseconds */
};

int utimes(const char *filename, const struct timeval times[2]);

#endif
