#include <sys/times.h>
#include <sys/syscall.h>

clock_t times(struct tms *buf)
{
    return syscall(SYS_times, buf);
}
