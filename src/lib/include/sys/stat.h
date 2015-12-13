#ifndef _STAT_H
#define _STAT_H

#ifdef _MUSL_LIBC
#define _STAT_STRUCT_SIZE 96
#elif defined __LP64__
#define _STAT_STRUCT_SIZE 144	/* in 64-bit glibc */
#else
#define _STAT_STRUCT_SIZE 88 	/* in 32-bit glibc */
#endif

struct stat {
    char a[_STAT_STRUCT_SIZE];
};
typedef unsigned int mode_t;

int stat(const char *path, struct stat *buf);
int chmod(const char *path, mode_t mode);

#endif
