#ifndef _SETJMP_H
#define _SETJMP_H

#if defined _GLIBC

# define _SIGSET_NWORDS	(1024 / (8 * sizeof (unsigned long int)))
typedef struct {
    unsigned long int __val[_SIGSET_NWORDS];
} __sigset_t;

#if defined __i386__
typedef int __jmp_buf[6];
#elif defined __x86_64__
typedef long int __jmp_buf[8];
#elif defined __mips__
typedef struct {
    int __pc;               /* Program counter.  */
    int __sp;               /* Stack pointer.  */
    int __regs[8];          /* Callee-saved registers s0 through s7.  */
    int __fp;               /* The frame pointer.  */
    int __gp;               /* The global pointer.  */
    int __fpc_csr;          /* Floating point status register.  */
    long long __fpregs[6];  /* Callee-saved floating point registers.  */
} __jmp_buf[1];
#elif defined __arm__
/* The exact set of registers saved may depend on the particular core
   in use, as some coprocessor registers may need to be saved.  The C
   Library ABI requires that the buffer be 8-byte aligned, and
   recommends that the buffer contain 64 words.  The first 28 words
   are occupied by v1-v6, sl, fp, sp, pc, d8-d15, and fpscr.  (Note
   that d8-15 require 17 words, due to the use of fstmx.)  */
/*typedef int __jmp_buf[64] __attribute__((__aligned__ (8)));*/
typedef long long __jmp_buf[32];
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

#elif defined _MUSL

#if defined __i386__
typedef unsigned long __jmp_buf[6];
#elif defined __x86_64__
typedef unsigned long __jmp_buf[8];
#elif defined __mips__
typedef unsigned long long __jmp_buf[13];
#elif defined __arm__
typedef unsigned long long __jmp_buf[32];
#endif

typedef struct __jmp_buf_tag {
	__jmp_buf __jb;
	unsigned long __fl;
	unsigned long __ss[128/sizeof(long)];
} jmp_buf[1];

#else

typedef struct {
#if defined __i386__
    int _ebx;
    int _ebp;
    int _esp;
    int _edi;
    int _esi;
    int _eip;
#elif defined __x86_64__
    long _rbx;
    long _rbp;
    long _rsp;
    long _r12;
    long _r13;
    long _r14;
    long _r15;
    long _rip;
#elif defined __mips__
    int _sp;
    int _fp;
    int _s0;
    int _s1;
    int _s2;
    int _s3;
    int _s4;
    int _s5;
    int _s6;
    int _s7;
    int _pc;
#elif defined __arm__
    long long _align;
    int _sp;
    int _v1;
    int _v2;
    int _v3;
    int _v4;
    int _v5;
    int _v6;
    int _v7;
    int _v8; /* aka fp */
    int _lr;
#endif
} jmp_buf[1];

#endif

int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int val);


#endif
