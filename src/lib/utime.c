#include <utime.h>
#include <sys/syscall.h>

int utime(const char *filename, const struct utimbuf *times)
{
#ifdef __arm__
#include <sys/time.h>
    struct timeval tvp[2];

    tvp[0].tv_sec = times->actime;
    tvp[0].tv_usec = 0;
    tvp[1].tv_sec = times->modtime;
    tvp[1].tv_usec = 0;

    return utimes(filename, tvp);
#else
    return syscall(SYS_utime, filename, times);
#endif
}
