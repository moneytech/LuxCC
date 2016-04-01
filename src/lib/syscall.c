#include <sys/syscall.h>
#include <stdarg.h>
#include <errno.h>
#include <assert.h>

/* functions defined in raw_syscall_<platform>.asm */
long __raw_syscall_0(long);
long __raw_syscall_1(long, long);
long __raw_syscall_2(long, long, long);
long __raw_syscall_3(long, long, long, long);
long __raw_syscall_4(long, long, long, long, long);

#if defined __x86_64__ || defined __arm__
/* Structure which says how much of each resource has been used.  */
struct rusage {
    /* Total amount of user time used.  */
    long ru_utime[2];
    /* Total amount of system time used.  */
    long ru_stime[2];
    /* Maximum resident set size (in kilobytes).  */
    long int ru_maxrss;
    /* Amount of sharing of text segment memory
       with other processes (kilobyte-seconds).  */
    long int ru_ixrss;
    /* Amount of data segment memory used (kilobyte-seconds).  */
    long int ru_idrss;
    /* Amount of stack memory used (kilobyte-seconds).  */
    long int ru_isrss;
    /* Number of soft page faults (i.e. those serviced by reclaiming
       a page from the list of pages awaiting reallocation.  */
    long int ru_minflt;
    /* Number of hard page faults (i.e. those that required I/O).  */
    long int ru_majflt;
    /* Number of times a process was swapped out of physical memory.  */
    long int ru_nswap;
    /* Number of input operations via the file system.  Note: This
       and `ru_oublock' do not include operations with the cache.  */
    long int ru_inblock;
    /* Number of output operations via the file system.  */
    long int ru_oublock;
    /* Number of IPC messages sent.  */
    long int ru_msgsnd;
    /* Number of IPC messages received.  */
    long int ru_msgrcv;
    /* Number of signals delivered.  */
    long int ru_nsignals;
    /* Number of voluntary context switches, i.e. because the process
       gave up the process before it had to (usually to wait for some
       resource to be available).  */
    long int ru_nvcsw;
    /* Number of involuntary context switches, i.e. a higher priority process
       became runnable or the current process used up its time slice.  */
    long int ru_nivcsw;
};
#endif

long syscall(long number, ...)
{
    va_list ap;
    long a1, a2, a3;
    long res;
    int neg_on_err;

    neg_on_err = 1; /* the syscall returns -errno on error */
    va_start(ap, number);

    switch (number) {
    /*
     * syscalls with no args.
     */
    case SYS_getpid:    /* pid_t getpid(void); */
    case SYS_fork:      /* pid_t fork(void); */
        res = __raw_syscall_0(number);
        break;

    /*
     * syscalls with 1 arg.
     */
    case SYS_brk:       /* void *brk(void *addr); */
        a1 = va_arg(ap, long);
        res = __raw_syscall_1(number, a1);
        neg_on_err = 0; /* brk() returns the old break on error */
        break;
    case SYS_exit:      /* void exit(int status); */
    case SYS_close:     /* int close(int fd); */
    case SYS_unlink:    /* int unlink(const char *pathname); */
    case SYS_time:      /* time_t time(time_t *tloc); */
    case SYS_times:     /* clock_t times(struct tms *buf); */
        a1 = va_arg(ap, long);
        res = __raw_syscall_1(number, a1);
        break;

    /*
     * syscalls with 2 args.
     */
    case SYS_rename:    /* int rename(const char *oldpath, const char *newpath); */
    case SYS_stat:      /* int stat(const char *path, struct stat *buf); */
    case SYS_kill:      /* int kill(pid_t pid, int sig); */
    case SYS_chmod:     /* int chmod(const char *path, mode_t mode); */
    case SYS_utimes:    /* int utimes(const char *filename, const struct timeval times[2]); */
#ifndef __arm__
    case SYS_utime:     /* int utime(const char *filename, const struct utimbuf *times); */
#endif
        a1 = va_arg(ap, long);
        a2 = va_arg(ap, long);
        res = __raw_syscall_2(number, a1, a2);
        break;

    /*
     * syscalls with 3 args.
     */
    case SYS_read:      /* ssize_t read(int fd, void *buf, size_t count); */
    case SYS_write:     /* ssize_t write(int fd, const void *buf, size_t count); */
    case SYS_open:      /* int open(const char *pathname, int flags, mode_t mode); */
    case SYS_lseek:     /* off_t lseek(int fd, off_t offset, int whence); */
    case SYS_ioctl:     /* int ioctl(int fd, unsigned long request, void *arg); */
#if defined __i386__ || defined __mips__
    case SYS_waitpid:   /* pid_t waitpid(pid_t pid, int *status, int options); */
#endif
    case SYS_execve:    /* int execve(const char *filename, char *const argv[], char *const envp[]); */
        a1 = va_arg(ap, long);
        a2 = va_arg(ap, long);
        a3 = va_arg(ap, long);
        res = __raw_syscall_3(number, a1, a2, a3);
        break;

    /*
     * syscalls with 4 args.
     */
#if defined __x86_64__ || defined __arm__
    case SYS_wait4: {    /* pid_t wait4(pid_t pid, int *status, int options, struct rusage *rusage); */
        struct rusage s;

        a1 = va_arg(ap, long);
        a2 = va_arg(ap, long);
        a3 = va_arg(ap, long);
        res = __raw_syscall_4(number, a1, a2, a3, &s);
    }
        break;
#endif

    default:
        /* FIXME */
        assert(0);
        break;
    }
    if (neg_on_err) {
        if (res < 0) {
            errno = -res;
            return -1;
        } else {
            return res;
        }
    } else {
        return res;
    }
}
