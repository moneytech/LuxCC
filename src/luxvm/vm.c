#include "vm.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <getopt.h>
#include <sys/stat.h>
#include <unistd.h>
#include <assert.h>
#include "as.h"
#include "operations.h"
#include "../util.h"

char *prog_name;

typedef unsigned char uchar;
typedef unsigned short ushort;

#define DEFAULT_STACK_SIZE  32768
long *stack, *data, *bss;
uchar *text;
int text_size, data_size, bss_size;

int vm_argc;
char **vm_argv;

/* search function used with `switch' */
int cmp_int(const void *p1, const void *p2)
{
    long v1 = *(long *)p1;
    long v2 = *(long *)p2;

    if (v1 < v2)
        return -1;
    else if (v1 == v2)
        return 0;
    else
        return 1;
}

#ifndef __LP64__
/* search function used with `switch2' */
int cmp_int2(const void *p1, const void *p2)
{
    long long v1 = *(long long *)p1;
    long long v2 = *(long long *)p2;

    if (v1 < v2)
        return -1;
    else if (v1 == v2)
        return 0;
    else
        return 1;
}
#endif

void do_libcall(long *sp, long *bp, long c)
{
    long a;
    long *p;

    switch (c) {
    case 0: /* getvars */
        p = (void *)bp[-3];
        p[0] = (long)stdin;
        p[1] = (long)stdout;
        p[2] = (long)stderr;
        p[3] = vm_argc;
        p[4] = (long)vm_argv;
        p[5] = (long)&optarg;
        p[6] = (long)&optind;
        sp[0] = 0;
        break;
    case 1: /* malloc */
        sp[0] = (long)malloc(bp[-3]);
        break;
    case 2: /* free */
        free((void *)bp[-3]);
        sp[0] = 0;
        break;
    case 3: /* exit */
        exit(bp[-3]);
        break;
    case 4: /* realloc */
        p = (void *)bp[-3];
        a = bp[-4];
        sp[0] = (long)realloc(p, a);
        break;
    case 5: /* fputc */
        sp[0] = fputc(bp[-3], (FILE *)bp[-4]);
        break;
    case 6: /* fgetc */
        sp[0] = fgetc((FILE *)bp[-3]);
        break;
    case 7: /* fread */
        sp[0] = fread((void *)bp[-3], bp[-4], bp[-5], (FILE *)bp[-6]);
        break;
    case 8: /* fwrite */
        sp[0] = fwrite((void *)bp[-3], bp[-4], bp[-5], (FILE *)bp[-6]);
        break;
    case 9: /* ferror */
        sp[0] = ferror((FILE *)bp[-3]);
        break;
    case 10: /* fopen */
        sp[0] = (long)fopen((char *)bp[-3], (char *)bp[-4]);
        break;
    case 11: /* fclose */
        sp[0] = fclose((FILE *)bp[-3]);
        break;
    case 12: /* fseek */
        sp[0] = fseek((FILE *)bp[-3], bp[-4], bp[-5]);
        break;
    case 13: /* ftell */
        sp[0] = (int)ftell((FILE *)bp[-3]);
        break;
    case 14: /* rewind */
        rewind((FILE *)bp[-3]);
        sp[0] = 0;
        break;
    case 15: /* getopt_long */
        sp[0] = getopt_long(bp[-3], (char *const *)bp[-4], (const char *)bp[-5],
        (const struct option *)bp[-6], (int *)bp[-7]);
        break;
    case 16: /* fgets */
        sp[0] = (long)fgets((char *)bp[-3], bp[-4], (FILE *)bp[-5]);
        break;
    case 17:
        sp[0] = stat((char *)bp[-3], (struct stat *)bp[-4]);
        break;
    case 18:
        sp[0] = fileno((FILE *)bp[-3]);
        break;
    case 19:
        sp[0] = isatty(bp[-3]);
        break;
    default:
        fprintf(stderr, "libcall %ld not implemented\n", c);
        break;
    }
}

long *exec(void)
{
    uchar *ip, *ip1;
    long *sp, *bp;
    long a, b;
    int opcode;

    ip = text;
    sp = stack;
    bp = stack;

    while (1) {
        opcode = *ip++;
        switch (opcode) {
                /* memory read */
            case OpLdB:
                sp[0] = *(char *)sp[0];
                break;
            case OpLdUB:
                sp[0] = *(uchar *)sp[0];
                break;
            case OpLdW:
                sp[0] = *(short *)sp[0];
                break;
            case OpLdUW:
                sp[0] = *(ushort *)sp[0];
                break;
            case OpLdDW:
                sp[0] = *(int *)sp[0];
                break;
            case OpLdQW:
                *(long long *)sp = *(long long *)sp[0];
                break;
            case OpLdN: {
                long n;
                uchar *src, *dest;

                n = *(long *)ip;
                ip += sizeof(long);
                src = (uchar *)sp[0];
                dest = (uchar *)sp;
                while (n-- > 0)
                    *dest++ = *src++;
                break;
            }

                /* memory write */
            case OpStB:
                *(char *)sp[0] = (char)sp[-1];
                --sp;
                break;
            case OpStW:
                *(short *)sp[0] = (short)sp[-1];
                --sp;
                break;
            case OpStDW:
                *(int *)sp[0] = sp[-1];
                --sp;
                break;
            case OpStQW:
                *(long long *)sp[0] = *(long long *)((char *)sp-8);
                sp = (long *)((char *)sp-8);
                break;
            case OpMemCpy:
                memmove((void *)sp[-1], (const void *)sp[0], *(long *)ip);
                ip += sizeof(long);
                --sp;
                break;

            case OpFill:
                memset((void *)sp[-1], sp[0], *(long *)ip);
                ip += sizeof(long);
                --sp;
                break;

                /* load immediate pointers */
            case OpLdBP:
                ++sp;
                sp[0] = (long)bp + *(long *)ip;
                ip += sizeof(long);
                break;

                /* load immediate data */
            case OpLdI:
                ++sp;
                sp[0] = *(long *)ip;
                ip += sizeof(long);
                break;

                /* arithmetic */
            case OpAdd:
                sp[-1] += sp[0];
                --sp;
                break;
            case OpSub:
                sp[-1] -= sp[0];
                --sp;
                break;
            case OpMul:
                sp[-1] *= sp[0];
                --sp;
                break;
            case OpSDiv:
                sp[-1] /= sp[0];
                --sp;
                break;
            case OpUDiv:
                sp[-1] = (unsigned long)sp[-1]/(unsigned long)sp[0];
                --sp;
                break;
            case OpSMod:
                sp[-1] %= sp[0];
                --sp;
                break;
            case OpUMod:
                sp[-1] = (unsigned long)sp[-1]%(unsigned long)sp[0];
                --sp;
                break;
            case OpNeg:
                sp[0] = -sp[0];
                break;
            case OpNot:
                sp[0] = !sp[0];
                break;

                /* comparisons */
            case OpSLT:
                sp[-1] = sp[-1]<sp[0];
                --sp;
                break;
            case OpULT:
                sp[-1] = (unsigned long)sp[-1]<(unsigned long)sp[0];
                --sp;
                break;
            case OpSLET:
                sp[-1] = sp[-1]<=sp[0];
                --sp;
                break;
            case OpULET:
                sp[-1] = (unsigned long)sp[-1]<=(unsigned long)sp[0];
                --sp;
                break;
            case OpSGT:
                sp[-1] = sp[-1]>sp[0];
                --sp;
                break;
            case OpUGT:
                sp[-1] = (unsigned long)sp[-1]>(unsigned long)sp[0];
                --sp;
                break;
            case OpSGET:
                sp[-1] = sp[-1]>=sp[0];
                --sp;
                break;
            case OpUGET:
                sp[-1] = (unsigned long)sp[-1]>=(unsigned long)sp[0];
                --sp;
                break;
            case OpEQ:
                sp[-1] = sp[-1]==sp[0];
                --sp;
                break;
            case OpNEQ:
                sp[-1] = sp[-1]!=sp[0];
                --sp;
                break;

                /* bitwise */
            case OpAnd:
                sp[-1] &= sp[0];
                --sp;
                break;
            case OpOr:
                sp[-1] |= sp[0];
                --sp;
                break;
            case OpXor:
                sp[-1] ^= sp[0];
                --sp;
                break;
            case OpCmpl:
                sp[0] = ~sp[0];
                break;
            case OpSLL:
                sp[-1] <<= sp[0];
                --sp;
                break;
            case OpSRL:
                sp[-1] = (unsigned long)sp[-1] >> sp[0];
                --sp;
                break;
            case OpSRA:
                sp[-1] >>= sp[0];
                --sp;
                break;

                /* conversions */
#ifndef __LP64__
            case OpDW2B:
                sp[0] = (char)sp[0];
                break;
            case OpDW2UB:
                sp[0] = (uchar)sp[0];
                break;
            case OpDW2W:
                sp[0] = (short)sp[0];
                break;
            case OpDW2UW:
                sp[0] = (ushort)sp[0];
                break;
#else
            case OpQW2B:
                sp[0] = (char)sp[0];
                break;
            case OpQW2UB:
                sp[0] = (uchar)sp[0];
                break;
            case OpQW2W:
                sp[0] = (short)sp[0];
                break;
            case OpQW2UW:
                sp[0] = (ushort)sp[0];
                break;
            case OpQW2DW:
                sp[0] = (int)sp[0];
                break;
            case OpQW2UDW:
                sp[0] = (unsigned)sp[0];
                break;
#endif

                /* subroutines */
            case OpCall:
                a = *(long *)ip; /* size of param area */
                ip += sizeof(long);
                ip1 = (uchar *)sp[0];
                sp[0] = (long)ip;
                sp[1] = (long)bp;
                sp[2] = a;
                sp += 2;
                ip = ip1;
                bp = sp;
                break;
            case OpRet:
                a = sp[0]; /* return value */
                sp = bp;
                ip = (uchar *)sp[-2];
                bp = (long *)sp[-1];
                b = sp[0]; /* size of param area */
                sp = (long *)((long)sp-sizeof(long)*2-b); /* sizeof(long)*2: old bp + ret addr */
                sp[0] = a;
                break;

                /* jumps */
            case OpJmp:
                ip1 = (uchar *)(*(long *)ip);
                ip = ip1;
                break;
            case OpJmpF:
                ip1 = (uchar *)(*(long *)ip);
                ip += sizeof(long);
                if (!sp[0])
                    ip = ip1;
                --sp;
                break;
            case OpJmpT:
                ip1 = (uchar *)(*(long *)ip);
                ip += sizeof(long);
                if (sp[0])
                    ip = ip1;
                --sp;
                break;

            case OpSwitch: {
                long val, count;
                long *tab, *p, *p_end, *res;

                val = sp[-1];
                tab = (long *)sp[0];
                sp -= 2;

                p = tab;
                count = *p++;
                p_end = tab+count;

                if ((res=bsearch(&val, p, count-1, sizeof(*p), cmp_int)) == NULL)
                    ip = (uchar *)*p_end; /* default */
                else
                    ip = (uchar *)*(p_end+(res-tab));
                break;
            }
#ifndef __LP64__
            case OpSwitch2: {
                long long val, count;
                long long *tab, *p, *res;
                long *p_end;

                val = *(long long *)&sp[-2];
                tab = (long long *)sp[0];
                sp -= 3;

                p = tab;
                count = *p++;
                p_end = (long *)(tab+count);

                if ((res=bsearch(&val, p, count-1, sizeof(*p), cmp_int2)) == NULL)
                    ip = (uchar *)*p_end; /* default */
                else
                    ip = (uchar *)*(p_end+(res-tab));
            }
                break;
#endif

                /* system library calls */
            case OpLibCall:
                a = *(long *)ip;
                ip += sizeof(long);
                ++sp;
                do_libcall(sp, bp, a);
                break;

                /* stack management */
            case OpAddSP:
                a = *(long *)ip;
                ip += sizeof(long);
                sp = (long *)((long)sp+a);
                break;
            case OpDup:
                ++sp;
                sp[0] = sp[-1];
                break;
            case OpPop:
                --sp;
                break;
            case OpSwap:
                sp[0]  ^= sp[-1];
                sp[-1] ^= sp[0];
                sp[0]  ^= sp[-1];
                break;
            case OpPushSP:
                ++sp;
                sp[0] = (long)(sp-1);
                break;

            /* misc */
            case OpNop:
                break;
            case OpHalt:    /* OK */
            default:        /* error, unknown opcode */
                return sp;
        } /* switch (opcode) */
    } /* while (1) */
}

void load_code(char *file_path)
{
    int i;
    FILE *fp;
    int ndreloc, ntreloc;

    if ((fp=fopen(file_path, "rb")) == NULL)
        TERMINATE("%s: error reading file `%s'", prog_name, file_path);

    /* header */
    fread(&bss_size, sizeof(int), 1, fp);
    bss = calloc(1, bss_size);
    fread(&data_size, sizeof(int), 1, fp);
    fread(&text_size, sizeof(int), 1, fp);
    fread(&ndreloc, sizeof(int), 1, fp);
    fread(&ntreloc, sizeof(int), 1, fp);

    /* data&text */
    data = malloc(data_size);
    fread(data, data_size, 1, fp);
    text = malloc(text_size);
    fread(text, text_size, 1, fp);

    /* data relocation table */
    for (i = 0; i < ndreloc; i++) {
        long base;
        int segment, offset;

        fread(&segment, sizeof(int), 1, fp);
        fread(&offset, sizeof(int), 1, fp);
        base = (segment==TEXT_SEG)?(long)text:(segment==DATA_SEG)?(long)data:(long)bss;
        *(long *)((char *)data+offset) += base;
    }

    /* text relocation table */
    for (i = 0; i < ntreloc; i++) {
        long base;
        int segment, offset;

        fread(&segment, sizeof(int), 1, fp);
        fread(&offset, sizeof(int), 1, fp);
        base = (segment==TEXT_SEG)?(long)text:(segment==DATA_SEG)?(long)data:(long)bss;
        *(long *)&text[offset] += base;
    }

    fclose(fp);
}

void disassemble_text(uchar *text, long text_size)
{
    uchar *p, *lim;

    for (p = text, lim = text+text_size; p < lim;) {
        printf("(%p) ", p);
        switch (*p++) {
        case OpHalt:    printf("halt\n");   break;
        case OpLdB:     printf("ldb\n");    break;
        case OpLdUB:    printf("ldub\n");   break;
        case OpLdW:     printf("ldw\n");    break;
        case OpLdUW:    printf("lduw\n");   break;
        case OpLdDW:    printf("lddw\n");   break;
        case OpLdQW:    printf("ldqw\n");   break;
        case OpLdN:     printf("ldn ");     printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpStB:     printf("stb\n");    break;
        case OpStW:     printf("stw\n");    break;
        case OpStDW:    printf("stdw\n");   break;
        case OpStQW:    printf("stqw\n");   break;
        case OpStN:     printf("stn ");     printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpMemCpy:  printf("memcpy ");  printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpAdd:     printf("add\n");    break;
        case OpSub:     printf("sub\n");    break;
        case OpMul:     printf("mul\n");    break;
        case OpSDiv:    printf("sdiv\n");   break;
        case OpUDiv:    printf("udiv\n");   break;
        case OpSMod:    printf("smod\n");   break;
        case OpUMod:    printf("umod\n");   break;
        case OpNeg:     printf("neg\n");    break;
        case OpCmpl:    printf("cmpl\n");   break;
        case OpNot:     printf("not\n");    break;
        case OpSLT:     printf("slt\n");    break;
        case OpULT:     printf("ult\n");    break;
        case OpSLET:    printf("slet\n");   break;
        case OpULET:    printf("ulet\n");   break;
        case OpSGT:     printf("sgt\n");    break;
        case OpUGT:     printf("ugt\n");    break;
        case OpSGET:    printf("sget\n");   break;
        case OpUGET:    printf("uget\n");   break;
        case OpEQ:      printf("eq\n");     break;
        case OpNEQ:     printf("neq\n");    break;
        case OpAnd:     printf("and\n");    break;
        case OpOr:      printf("or\n");     break;
        case OpXor:     printf("xor\n");    break;
        case OpSLL:     printf("sll\n");    break;
        case OpSRL:     printf("srl\n");    break;
        case OpSRA:     printf("sra\n");    break;
        case OpDW2B:    printf("dw2b\n");   break;
        case OpDW2UB:   printf("dw2ub\n");  break;
        case OpDW2W:    printf("dw2w\n");   break;
        case OpDW2UW:   printf("dw2uw\n");  break;
        case OpLdI:     printf("ldi ");     printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpLdBP:    printf("ldbp ");    printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpJmpF:    printf("jmpf ");    printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpJmpT:    printf("jmpt ");    printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpJmp:     printf("jmp ");     printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpCall:    printf("call ");    printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpFill:    printf("fill ");    printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpRet:     printf("ret\n");    break;
        case OpDup:     printf("dup\n");    break;
        case OpPop:     printf("pop\n");    break;
        case OpAddSP:   printf("addsp ");   printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpNop:     printf("nop\n");    break;
        case OpSwap:    printf("swap\n");   break;
        case OpSwitch:  printf("switch\n"); break;
        case OpSwitch2: printf("switch2\n");break;
        case OpLibCall: printf("libcall "); printf("%lx\n", *(long *)p); p+=sizeof(long); break;
        case OpPushSP:  printf("pushsp\n"); break;
        }
    }
}

void disassemble_data(long *data, long data_size)
{
    uchar *p, *lim;

    for (p = (uchar *)data, lim = (uchar *)data+data_size; p < lim;) {
        printf("(%p) ", p);
        printf("%lx\n", *(long *)p);
        p += sizeof(long);
    }
}

void vm_usage(void)
{
    printf("usage: %s [vm-options] <program> [program-options]\n", prog_name);
    exit(0);
}

int main(int argc,char *argv[])
{
    /*
                [ Program image ]
        Code
    +-------------------------------------------------+ <-text
    | Text                                            |
    +-------------------------------------------------+

    +-------------------------------------------------+ <-bss
    | Bss                                             |
    +-------------------------------------------------+

    +-------------------------------------------------+ <-data
    | Data                                            |
    +-------------------------------------------------+

    +-------------------------------------------------+ <-stack
    | Stack                                           |
    +-------------------------------------------------+
    */
    int i;
    long *sp;
    int disas;
    char *infile;
    long stack_size;

    prog_name = argv[0];
    if (argc == 1)
        vm_usage();
    infile = NULL;
    disas = FALSE;
    stack_size = DEFAULT_STACK_SIZE;
    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '-') {
            infile = argv[i];
            break;
        }
        switch (argv[i][1]) {
        case 's':
            if (argv[i][2] != '\0') {
                stack_size = atol(argv[i]+2);
            } else if (argv[i+1] == NULL) {
                fprintf(stderr, "%s: option `s' requires an argument\n", prog_name);
                exit(1);
            } else {
                stack_size = atol(argv[++i]);
            }
            break;
        case 'd':
            disas = TRUE;
            break;
        case 'h':
            printf("usage: %s [ options ] <program>\n"
                   "  The available options are:\n"
                   "    -s<size>    specify stack size\n"
                   "    -d          disassemble code and data after loading\n"
                   "    -h          print this help\n", prog_name);
            exit(0);
            break;
        case '\0':
            break;
        default:
            fprintf(stderr, "%s: unknown option `%s'\n", prog_name, argv[i]);
            exit(1);
        }
    }
    if (infile == NULL)
        vm_usage();

    load_code(argv[1]);
    if (disas) {
        printf("Bss:\n");
        disassemble_data(bss, bss_size);
        printf("Data:\n");
        disassemble_data(data, data_size);
        printf("Code:\n");
        disassemble_text(text, text_size);
    }
    stack = malloc(stack_size*sizeof(long));
    vm_argc = argc-i;
    vm_argv = argv+i;
    sp = exec();

    return (int)*sp;
}
