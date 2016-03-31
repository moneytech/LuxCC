#ifndef _FCNTL_H
#define _FCNTL_H

#include <sys/types.h>

/* access modes */
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR   2
/* file creation flags */
#define O_CREAT  0x040
#define O_EXCL   0x080
#define O_TRUNC  0x200
#define O_NOCTTY 0x100
/* file status flags */
#define O_APPEND 0x400

int open(const char *pathname, int flags, mode_t mode);

#endif
