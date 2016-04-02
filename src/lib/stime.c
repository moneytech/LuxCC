#include <sys/time.h>
#include <sys/syscall.h>

int utimes(const char *filename, const struct timeval times[2])
{
    return syscall(SYS_utimes, filename, times);
}
