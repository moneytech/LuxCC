#ifndef _ERRNO_H
#define _ERRNO_H

#define EDOM    33
#define EILSEQ  84
#define ERANGE  34

#if defined __i386__ || defined __x86_64__

/* Function to get address of global `errno' variable.  */
extern int *__errno_location (void);
/* When using threads, errno is a per-thread value.  */
#define errno (*__errno_location ())

#endif

#ifdef __LuxVM__

extern int *errno;
#define errno (*errno)

#endif

#endif
