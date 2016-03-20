#ifndef _UNISTD_H
#define _UNISTD_H

#include <stddef.h>
#include <sys/types.h>
#include <stdint.h>

#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

int close(int fd);
off_t lseek(int fd, off_t offset, int whence);
ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, const void *buf, size_t count);
void _exit(int status);
int brk(void *addr);
void *sbrk(intptr_t increment);
int isatty(int fd);
int unlink(const char *pathname);
pid_t getpid(void);
pid_t fork(void);
int execve(const char *filename, char *const argv[], char *const envp[]);

#endif
