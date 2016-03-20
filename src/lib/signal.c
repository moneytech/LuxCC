#include <signal.h>
#include <unistd.h>
#include <sys/syscall.h>

int raise(int sig)
{
    return kill(getpid(), sig);
}

int kill(pid_t pid, int sig)
{
    return syscall(SYS_kill, pid, sig);
}
