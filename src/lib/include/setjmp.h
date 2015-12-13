#ifndef _SETJMP_H
#define _SETJMP_H

#if defined __i386__ || defined __x86_64__

/* From GNU libc headers */

# define _SIGSET_NWORDS	(1024 / (8 * sizeof (unsigned long int)))
typedef struct {
    unsigned long int __val[_SIGSET_NWORDS];
} __sigset_t;

#ifdef __i386__
typedef int __jmp_buf[6];
#else
typedef long int __jmp_buf[8];
#endif

/* Calling environment, plus possibly a saved signal mask.  */
struct __jmp_buf_tag {
    /* NOTE: The machine-dependent definitions of `__sigsetjmp'
       assume that a `jmp_buf' begins with a `__jmp_buf' and that
       `__mask_was_saved' follows it.  Do not move these members
       or add others before it.  */
    __jmp_buf __jmpbuf;		/* Calling environment.  */
    int __mask_was_saved;	/* Saved the signal mask?  */
    __sigset_t __saved_mask;	/* Saved signal mask.  */
};

typedef struct __jmp_buf_tag jmp_buf[1];

/* Store the calling environment in ENV, also saving the signal mask.
   Return 0.  */
int setjmp(jmp_buf env);

/* Jump to the environment saved in ENV, making the
   `setjmp' call there return VAL, or 1 if VAL is 0.  */
void longjmp(jmp_buf env, int val);

#endif

#endif
