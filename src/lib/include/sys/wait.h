#ifndef _WAIT_H
#define _WAIT_H

#include <sys/types.h>

#define WEXITSTATUS(s)  (((s)>>8) & 0xFF)

pid_t waitpid(pid_t pid, int *status, int options);

#endif
