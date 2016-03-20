#include <unistd.h>
#include <termios.h>
#include <sys/syscall.h>
#include <sys/ioctl.h>

int close(int fd)
{
    return syscall(SYS_close, fd);
}

ssize_t read(int fd, void *buf, size_t count)
{
    return syscall(SYS_read, fd, buf, count);
}

ssize_t write(int fd, const void *buf, size_t count)
{
    return syscall(SYS_write, fd, buf, count);
}

off_t lseek(int fd, off_t offset, int whence)
{
    return syscall(SYS_lseek, fd, offset, whence);
}

int unlink(const char *pathname)
{
    return syscall(SYS_unlink, pathname);
}

int isatty(int fd)
{
    struct termios s;

    return ioctl(fd, TCGETS, &s) == 0;
}

int brk(void *addr)
{
    void *new_brk;

    new_brk = (void *)syscall(SYS_brk, addr);
    if (new_brk != addr)
        return -1;
    return 0;
}

void *sbrk(intptr_t increment)
{
    void *pb;

    pb = (void *)syscall(SYS_brk, (void *)0);
    if (brk((char *)pb+increment) == -1)
        return (void *)-1;
    return pb;
}

pid_t getpid(void)
{
    return syscall(SYS_getpid);
}

pid_t fork(void)
{
    return syscall(SYS_fork);
}

int execve(const char *filename, char *const argv[], char *const envp[])
{
    return syscall(SYS_execve, filename, argv, envp);
}

void _exit(int status)
{
    syscall(SYS_exit, status);
}
