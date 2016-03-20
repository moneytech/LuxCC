#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#include <sys/wait.h>

#define ATEXIT_MAX 32   /* maximum # of function that can be registered with atexit() */

typedef struct Header Header; /* this is the basic heap unit */

#define IHEAP   8192    /* initial heap size (in units) */
#define NALLOC  4096    /* minimum # of units the heap is incremented by */

static struct Header {
    Header *next;
    size_t used_size;
    size_t free_size;
    /*size_t _pad;*/
} *heap, *hptr;

void __set_up_heap(void)
{
    heap = sbrk(IHEAP*sizeof(Header));
    hptr = heap;
    hptr->next = heap;
    hptr->used_size = 1;
    hptr->free_size = IHEAP-1;
}

void __heap_fini(void)
{
    brk(heap);
}

static Header *_expand_heap(size_t nunits)
{
    Header *p;

    /*
     * Expand the last block's free space
     * by max(nunits, NALLOC) units.
     */
    for (p = heap; p->next != heap; p = p->next)
        ;
    if (nunits < NALLOC)
        nunits = NALLOC;
    if (sbrk(nunits*sizeof(Header)) == (void *)-1)
        return NULL;
    p->free_size += nunits;
    return p;
}

void *malloc(size_t size)
{
    Header *p, *newp;
    size_t nunits;

    if (size == 0)
        return NULL;
    nunits = (size+sizeof(Header)-1)/sizeof(Header)+1;

    p = hptr;
    while (p->next!=hptr && p->free_size<nunits)
        p = p->next;
    if (p->free_size<nunits && (p=_expand_heap(nunits))==NULL)
        return NULL;
    newp = p+p->used_size;
    newp->used_size = nunits;
    newp->free_size = p->free_size-nunits;
    newp->next = p->next;
    p->free_size = 0;
    p->next = newp;
    hptr = newp;
    return (void *)(newp+1);
}

void free(void *ptr)
{
    Header *bp, *p, *prev;

    if (ptr == NULL)
        return;

    bp = (Header *)ptr-1;
    prev = hptr;
    p = hptr->next;
    while (p!=bp && p!=hptr) {
        prev = p;
        p = p->next;
    }
    if (p != bp)
        return; /* corrupted list, do nothing */
    prev->free_size += p->used_size+p->free_size;
    prev->next = p->next;
    hptr = prev;
}

void *realloc(void *ptr, size_t size)
{
    Header *p;
    size_t nunits, avail;

    if (ptr == NULL) {
        return malloc(size);
    } else if (size == 0) {
        free(ptr);
        return NULL;
    }

    nunits = (size+sizeof(Header)-1)/sizeof(Header)+1;
    p = (Header *)ptr-1;
    avail = p->used_size+p->free_size;
    if (nunits <= avail) {
        p->used_size = nunits;
        p->free_size = avail-nunits;
        return ptr;
    } else {
        void *newptr;

        if ((newptr=malloc(size)) != NULL) {
            size_t oldsiz;

            oldsiz = p->used_size*sizeof(Header);
            memcpy(newptr, ptr, (size<oldsiz)?size:oldsiz);
            free(ptr);
            return newptr;
        }
    }
    return NULL;
}

void *calloc(size_t nmemb, size_t size)
{
    void *p;
    size_t nb;

    if ((nb=nmemb*size) == 0)
        return NULL;
    if ((p=malloc(nb)) != NULL) {
        size_t i, lim;

        /* the chunk allocated by malloc() is always a multiple of 4 */
        lim = ((nb+3)&~3)/4;
        for (i = 0; i < lim; i++)
            ((int *)p)[i] = 0;
    }
    return p;
}

/*
 * The strtoX() functions divide the input string as follows:
 *      [white-space][subject-sequence][final-string]
 *
 * This implementation tries to be compatible with the glibc's behavior.
 * This means that, for example, the input string "0xz" will be divided as follows:
 *  while-space: - (nothing)
 *  subject-sequence: "0"
 *  final-string: "xz" (an so *endptr will end pointing to "xz").
 * Note that this is different to scanf()'s behavior.
 */
static unsigned long _strtoxl(const char *nptr, char **endptr, int base, int _signed)
{
    unsigned long res, val, cutoff, cutlim;
    const char *p, *sseq;
    unsigned flags;
    enum {
        F_OVERFLOW  = 0x1,
        F_ERROR     = 0x2,
        F_NEG       = 0x4,
    };

    res = 0;
    flags = 0;
    p = nptr;
    while (isspace(p[0]))
        ++p;

    if (p[0] == '+') {
        ++p;
    } else if (p[0] == '-') {
        flags |= F_NEG;
        ++p;
    }

    if (base == 0) {
        /* infer the base */
        if (p[0] != '0')
            base = 10;
        else if ((p[1]=='x' || p[1]=='X') && isxdigit(p[2]))
            base = 16;
        else
            base = 8;
    } else if (base<2 || base>36) {
        flags |= F_ERROR;
        goto done;
    }

    /* skip any "0x"/"0X" prefix */
    if (base==16 && p[0]=='0' && (p[1]=='x' || p[1]=='X') && isxdigit(p[2]))
        p += 2;

    cutoff = ULONG_MAX/base;
    cutlim = ULONG_MAX%base;
    sseq = p;

    for (;;) {
        int c;

        c = toupper(*p);
        if (isdigit(c))
            val = c-'0';
        else if (c>='A' && c<='Z')
            val = c-'A'+10;
        else
            break;
        if (val >= base)
            break;
        if (res<cutoff || res==cutoff && val<=cutlim)
            res = res*base+val;
        else
            /*
             * We don't "break" here because, even
             * after overflow, we need to consume
             * the remaining characters that are of
             * the "expected form".
             */
            flags |= F_OVERFLOW;
        p++;
    }
    if (p == sseq) { /* empty subject sequence */
        flags |= F_ERROR;
        goto done;
    }

    if (flags&F_OVERFLOW                                    /* > ULONG_MAX */
    || _signed                                              /* or signed and */
    && (flags&F_NEG && res>(unsigned long)LONG_MAX+1U       /* < LONG_MIN */
    || !(flags&F_NEG) && res>(unsigned long)LONG_MAX)) {    /* > LONG_MAX */
        if (!_signed)
            res = ULONG_MAX;
        else if (flags & F_NEG)
            res = LONG_MIN;
        else
            res = LONG_MAX;
        errno = ERANGE;
    } else if (flags & F_NEG) {
        res = -res;
    }
done:
    if (endptr != NULL) {
        if (flags & F_ERROR)
            *endptr = (char *)nptr;
        else
            *endptr = (char *)p;
    }

    return res;
}

long int strtol(const char *nptr, char **endptr, int base)
{
    return _strtoxl(nptr, endptr, base, 1);
}

unsigned long int strtoul(const char *nptr, char **endptr, int base)
{
    return _strtoxl(nptr, endptr, base, 0);
}

#ifndef __LP64__
static unsigned long long _strtoxll(const char *nptr, char **endptr, int base, int _signed)
{
    unsigned long long res, val, cutoff, cutlim;
    const char *p, *sseq;
    unsigned flags;
    enum {
        F_OVERFLOW  = 0x1,
        F_ERROR     = 0x2,
        F_NEG       = 0x4,
    };

    res = 0;
    flags = 0;
    p = nptr;
    while (isspace(p[0]))
        ++p;

    if (p[0] == '+') {
        ++p;
    } else if (p[0] == '-') {
        flags |= F_NEG;
        ++p;
    }

    if (base == 0) {
        /* infer the base */
        if (p[0] != '0')
            base = 10;
        else if ((p[1]=='x' || p[1]=='X') && isxdigit(p[2]))
            base = 16;
        else
            base = 8;
    } else if (base<2 || base>36) {
        flags |= F_ERROR;
        goto done;
    }

    /* skip any "0x"/"0X" prefix */
    if (base==16 && p[0]=='0' && (p[1]=='x' || p[1]=='X') && isxdigit(p[2]))
        p += 2;

    cutoff = ULLONG_MAX/base;
    cutlim = ULLONG_MAX%base;
    sseq = p;

    for (;;) {
        int c;

        c = toupper(*p);
        if (isdigit(c))
            val = c-'0';
        else if (c>='A' && c<='Z')
            val = c-'A'+10;
        else
            break;
        if (val >= base)
            break;
        if (res<cutoff || res==cutoff && val<=cutlim)
            res = res*base+val;
        else
            flags |= F_OVERFLOW;
        p++;
    }
    if (p == sseq) { /* empty subject sequence */
        flags |= F_ERROR;
        goto done;
    }

    if (flags&F_OVERFLOW                                        /* > ULLONG_MAX */
    || _signed                                                  /* or signed and */
    && (flags&F_NEG && res>(unsigned long long)LLONG_MAX+1U     /* < LLONG_MIN */
    || !(flags&F_NEG) && res>(unsigned long long)LLONG_MAX)) {  /* > LLONG_MAX */
        if (!_signed)
            res = ULLONG_MAX;
        else if (flags & F_NEG)
            res = LLONG_MIN;
        else
            res = LLONG_MAX;
        errno = ERANGE;
    } else if (flags & F_NEG) {
        res = -res;
    }
done:
    if (endptr != NULL) {
        if (flags & F_ERROR)
            *endptr = (char *)nptr;
        else
            *endptr = (char *)p;
    }

    return res;
}
#endif

long long int strtoll(const char *nptr, char **endptr, int base)
{
#ifndef __LP64__
    return _strtoxll(nptr, endptr, base, 1);
#else
    return _strtoxl(nptr, endptr, base, 1);
#endif
}

unsigned long long int strtoull(const char *nptr, char **endptr, int base)
{
#ifndef __LP64__
    return _strtoxll(nptr, endptr, base, 0);
#else
    return _strtoxl(nptr, endptr, base, 0);
#endif
}

int atoi(const char *nptr)
{
    int res, tmp;

    tmp = errno;
    res = (int)strtol(nptr, NULL, 10);
    errno = tmp;
    return res;
}

long int atol(const char *nptr)
{
    int tmp;
    long int res;

    tmp = errno;
    res = strtol(nptr, NULL, 10);
    errno = tmp;
    return res;
}

long long int atoll(const char *nptr)
{
    int tmp;
    long long int res;

    tmp = errno;
    res = strtoll(nptr, NULL, 10);
    errno = tmp;
    return res;
}

int abs(int j)
{
    return (j < 0) ? -j : j;
}

long int labs(long int j)
{
    return (j < 0L) ? -j : j;
}

long long int llabs(long long int j)
{
    return (j < 0LL) ? -j : j;
}

div_t div(int numer, int denom)
{
    div_t res;

    res.quot = numer/denom;
    res.rem = numer%denom;
    return res;
}

ldiv_t ldiv(long int numer, long int denom)
{
    ldiv_t res;

    res.quot = numer/denom;
    res.rem = numer%denom;
    return res;
}

lldiv_t lldiv(long long int numer, long long int denom)
{
    lldiv_t res;

    res.quot = numer/denom;
    res.rem = numer%denom;
    return res;
}

static unsigned long int next = 1;

/* C99 7.20.2.2#5 */
int rand(void)
{
    next = next*1103515245+12345;
    return (unsigned int)(next/65536)%32768;
}

void srand(unsigned int seed)
{
    next = seed;
}

long random(void) /* FIXME */
{
    return rand();
}

void srandom(unsigned int seed)
{
    next = seed;
}

void abort(void)
{
    raise(SIGABRT);
}

extern char *(*__env)[];

char *getenv(const char *name)
{
    int i;
    size_t n;

    n = strlen(name);
    for (i = 0; ; i++) {
        char *v;

        if ((v=(*__env)[i]) == NULL)
            break;
        else if (strncmp(v, name, n) == 0)
            return strchr(v, '=')+1;
    }
    return NULL; /* not found */
}

int system(const char *string)
{
    pid_t pid;
    int status;
    char *argv[4];

    if (string == NULL)
        return 1;

    argv[0] = "sh";
    argv[1] = "-c";
    argv[2] = (char *)string;
    argv[3] = NULL;

    if ((pid=fork()) == 0) {
        execve("/bin/sh", argv, *__env);
        _exit(127);
    } else if (pid == -1) {
        return -1;
    } else {
        if (waitpid(-1, &status, 0) != pid)
            return -1;
        return status; /* WEXITSTATUS() applied to this value is the exit status */
    }
}

void _Exit(int status)
{
    _exit(status);
}

static void (*atexit_functions[ATEXIT_MAX])(void);
static int atexit_fncnt;

int atexit(void (*func)(void))
{
    if (atexit_fncnt >= ATEXIT_MAX)
        return -1;
    atexit_functions[atexit_fncnt++] = func;
    return 0;
}

void exit(int status)
{
    extern void __close_all_open_streams(void); /* stdio.c */
    extern void __heap_fini(void);

    while (atexit_fncnt)
        atexit_functions[--atexit_fncnt]();
    __close_all_open_streams();
    __heap_fini();
    _exit(status);
}

void *bsearch(const void *key, const void *base,
              size_t nmemb, size_t size,
              int (*compar)(const void *, const void *))
{
    int low, high, mid;

    if (nmemb==0 || size==0)
        return NULL;

    low = 0;
    high = nmemb-1;
    while (low <= high) {
        int cmp;

        mid = (low+high)/2;
        if ((cmp=(*compar)(key, (char *)base+mid*size)) < 0)
            high = mid-1;
        else if (cmp > 0)
            low = mid+1;
        else
            return (char *)base+mid*size;
    }
    return NULL;
}

/* See "sanos_LICENSE.txt" */
#define CUTOFF 8

static void _swap(char *a, char *b, unsigned width) {
  char tmp;

  if (a != b) {
    while (width--) {
      tmp = *a;
      *a++ = *b;
      *b++ = tmp;
    }
  }
}

static void _shortsort(char *lo, char *hi, unsigned width, int (*comp)(const void *, const void *)) {
  char *p, *max;

  while (hi > lo) {
    max = lo;
    for (p = lo + width; p <= hi; p += width) if (comp(p, max) > 0) max = p;
    _swap(max, hi, width);
    hi -= width;
  }
}

void qsort(void *base, size_t num, size_t width, int (*comp)(const void *, const void *))
{
  char *lo, *hi;
  char *mid;
  char *l, *h;
  unsigned size;
  char *lostk[30], *histk[30];
  int stkptr;

  if (num < 2 || width == 0) return;
  stkptr = 0;

  lo = base;
  hi = (char *) base + width * (num - 1);

recurse:
  size = (hi - lo) / width + 1;

  if (size <= CUTOFF) {
    _shortsort(lo, hi, width, comp);
  } else {
    mid = lo + (size / 2) * width;
    _swap(mid, lo, width);

    l = lo;
    h = hi + width;

    for (;;) {
      do { l += width; } while (l <= hi && comp(l, lo) <= 0);
      do { h -= width; } while (h > lo && comp(h, lo) >= 0);
      if (h < l) break;
      _swap(l, h, width);
    }

    _swap(lo, h, width);

    if (h - 1 - lo >= hi - l) {
      if (lo + width < h) {
        lostk[stkptr] = lo;
        histk[stkptr] = h - width;
        ++stkptr;
      }

      if (l < hi) {
        lo = l;
        goto recurse;
      }
    } else {
      if (l < hi) {
        lostk[stkptr] = l;
        histk[stkptr] = hi;
        ++stkptr;
      }

      if (lo + width < h) {
        hi = h - width;
        goto recurse;
      }
    }
  }

  --stkptr;
  if (stkptr >= 0) {
    lo = lostk[stkptr];
    hi = histk[stkptr];
    goto recurse;
  }
}
