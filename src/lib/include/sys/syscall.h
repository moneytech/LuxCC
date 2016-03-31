#ifndef _SYSCALL_H
#define _SYSCALL_H

#if defined __i386__
#define SYS_exit    1
#define SYS_fork    2
#define SYS_read    3
#define SYS_write   4
#define SYS_open    5
#define SYS_close   6
#define SYS_waitpid 7
#define SYS_unlink  10
#define SYS_execve  11
#define SYS_time    13
#define SYS_lseek   19
#define SYS_getpid  20
#define SYS_kill    37
#define SYS_rename  38
#define SYS_times   43
#define SYS_brk     45
#define SYS_ioctl   54
#define SYS_stat64  195
#define SYS_stat    SYS_stat64
#elif defined __x86_64__
#define SYS_exit    60
#define SYS_fork    57
#define SYS_read    0
#define SYS_write   1
#define SYS_open    2
#define SYS_close   3
#define SYS_wait4   61
#define SYS_waitpid SYS_wait4
#define SYS_unlink  87
#define SYS_execve  59
#define SYS_time    201
#define SYS_lseek   8
#define SYS_getpid  39
#define SYS_kill    62
#define SYS_rename  82
#define SYS_times   100
#define SYS_brk     12
#define SYS_ioctl   16
#define SYS_stat    4
#elif defined __mips__
/* o32 style syscalls (range [4000, 4999]) */
#define SYS_exit    4001
#define SYS_fork    4002
#define SYS_read    4003
#define SYS_write   4004
#define SYS_open    4005
#define SYS_close   4006
#define SYS_waitpid 4007
#define SYS_unlink  4010
#define SYS_execve  4011
#define SYS_time    4013
#define SYS_lseek   4019
#define SYS_getpid  4020
#define SYS_kill    4037
#define SYS_rename  4038
#define SYS_times   4043
#define SYS_brk     4045
#define SYS_ioctl   4054
#define SYS_stat64  4213
#define SYS_stat    SYS_stat64
#elif defined __arm__
/* ARM EABI style syscalls (syscall base == 0) */
#define SYS_exit    1
#define SYS_fork    2
#define SYS_read    3
#define SYS_write   4
#define SYS_open    5
#define SYS_close   6
#define SYS_wait4   114
#define SYS_waitpid SYS_wait4
#define SYS_unlink  10
#define SYS_execve  11
#define SYS_time    13
#define SYS_lseek   19
#define SYS_getpid  20
#define SYS_kill    37
#define SYS_rename  38
#define SYS_times   43
#define SYS_brk     45
#define SYS_ioctl   54
#define SYS_stat64  195
#define SYS_stat    SYS_stat64
#endif

long syscall(long number, ...);

#endif
