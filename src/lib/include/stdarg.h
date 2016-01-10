#ifndef _STDARG_H
#define _STDARG_H

#define _INTSIZEOF(n)  ((sizeof(n)+sizeof(int)-1) & ~(sizeof(int)-1))

#ifndef __x86_64__
typedef char *va_list;
#endif

#if defined __i386__ || defined __mips__
#define va_start(ap, last)  (ap = (va_list)&last + _INTSIZEOF(last))
#define va_arg(ap, type)    (*(type *)((ap += _INTSIZEOF(type)) - _INTSIZEOF(type)))
#define va_copy(dest, src)  (dest) = (src)
#define va_end(ap)
#endif

#ifdef __LuxVM__
#define va_start(ap, last)  (ap = (char *)&(last))
#define va_arg(ap, type)    (ap-=_INTSIZEOF(type), *(type *)(ap))
#define va_copy(dest, src)  (dest) = (src)
#define va_end(ap)
#endif

#ifdef __x86_64__
/*
 * Reference: AMD64-ABI, section 3.5.7.
 */

#define _LONGSIZE(n) ((n+sizeof(long)-1) & ~(sizeof(long)-1))

typedef struct {
    unsigned int gp_offset;
    unsigned int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
} va_list[1];

static void *__va_arg(va_list ap, unsigned long siz)
{
    void *p;
    int num_gp;

    num_gp = _LONGSIZE(siz)/8;

    if (siz>16 || ap->gp_offset>48-num_gp*8) {
        p = ap->overflow_arg_area;
        ap->overflow_arg_area = (char *)ap->overflow_arg_area+_LONGSIZE(siz);
    } else {
        p = (char *)ap->reg_save_area+ap->gp_offset;
        ap->gp_offset += num_gp*8;
    }
    return p;
}

#define va_start(ap, last)  __builtin_va_start(ap)
#define va_arg(ap, type)    (*(type *)__va_arg(ap, sizeof(type)))
#define va_copy(dest, src)  (*(dest)) = (*(src))
#define va_end(ap)

#endif

#endif
