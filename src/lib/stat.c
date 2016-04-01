#include <sys/stat.h>
#include <sys/syscall.h>

int stat(const char *path, struct stat *buf)
{
    return syscall(SYS_stat, path, buf);
}

int chmod(const char *path, mode_t mode)
{
    return syscall(SYS_chmod, path, mode);
}
