#include <fcntl.h>
#include <sys/syscall.h>

int open(const char *pathname, int flags, mode_t mode)
{
    return syscall(SYS_open, pathname, flags, mode);
}
