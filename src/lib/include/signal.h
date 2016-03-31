#ifndef _SIGNAL_H
#define _SIGNAL_H

#include <sys/types.h>

#define SIGABRT 6

// ===========
// Standard C
// ===========

int raise(int sig);

// ===========
// POSIX
// ===========

int kill(pid_t pid, int sig);

#endif
