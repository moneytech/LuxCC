#include <sys/wait.h>
#include <sys/syscall.h>

pid_t waitpid(pid_t pid, int *status, int options)
{
    return syscall(SYS_waitpid, pid, status, options);
}
