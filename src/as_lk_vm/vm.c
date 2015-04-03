#include "vm.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "as.h"
#include "operations.h"
#include "../util.h"

char *prog_name;

typedef unsigned char uchar;
typedef unsigned short ushort;

#define MAX_STACK_SIZE  1024
#define MAX_TEXT_SIZE   1024
int *stack, *data, *bss;
uchar text[MAX_TEXT_SIZE];
int text_size, data_size, bss_size;

int cmp_int(const void *p1, const void *p2)
{
    int v1 = *(int *)p1;
    int v2 = *(int *)p2;

    if (v1 < v2)
        return -1;
    else if (v1 == v2)
        return 0;
    else
        return 1;
}

#if 0
void VMLibCall(int *sp,int *bp,int c)
{
	 int a;
	 int *p;

	 switch(c) {
		case 0:
			/* getvars */
			p=(void *)bp[-3];
			p[0]=(int)stdin;
			p[1]=(int)stdout;
			p[2]=(int)stderr;
			p[3]=vm_argc;
			p[4]=(int)vm_argv;
			sp[0]=0;
			break;
		case 1:
			/* malloc */
			sp[0]=(int)malloc(bp[-3]);
			break;
		case 2:
			/* free */
			free((void *)bp[-3]);
			sp[0]=0;
			break;
		case 3:
			/* exit */
			exit(bp[-3]);
			break;
		case 4:
			/* realloc */
			p=(void *)bp[-3];
			a=bp[-4];
			sp[0]=(int) realloc(p,a);
			break;
		case 5:
			/* fputc */
			sp[0]=fputc(bp[-3],(FILE *)bp[-4]);
			break;
		case 6:
			/* fgetc */
			sp[0]=fgetc((FILE *)bp[-3]);
			break;
		case 7:
			/* fread */
			sp[0]=fread((void *)bp[-3],bp[-4],bp[-5],(FILE *)bp[-6]);
			break;
		case 8:
			/* fwrite */
			sp[0]=fwrite((void *)bp[-3],bp[-4],bp[-5],(FILE *)bp[-6]);
			break;
		case 9:
			/* ferror */
			sp[0]=ferror((FILE *)bp[-3]);
			break;
		case 10:
			/* fopen */
			sp[0]=(int)fopen((char *)bp[-3],(char *)bp[-4]);
			break;
		case 11:
			/* fclose */
			sp[0]=fclose((FILE *)bp[-3]);
			break;
		default:
			fprintf(stderr,"libcall %d non implémenté\n",c);
			break;
	 }
}
#endif

void do_libcall(int *sp, int *bp, int c)
{
    switch (c) {
    case 0: break;
    case 1: break;
    case 2: break;
    case 3: break;
    case 4: break;
    case 5: break;
    case 6: break;
    case 7: /* fread */
        sp[0] = fread((void *)bp[-3], bp[-4], bp[-5], (FILE *)bp[-6]);
        break;
    case 8: /* fwrite */
        sp[0] = fwrite((void *)bp[-3], bp[-4], bp[-5], (FILE *)bp[-6]);
        break;
    }
}

int *exec(void)
{
    uchar *ip, *ip1;
    int *sp, *bp;
    int opcode;
    int a, b;

    ip = text;
    sp = stack;
    bp = stack;

    while (1) {
        opcode = *ip++;
        switch (opcode) {
                /* memory read */
            case OpLdB:
                sp[0] = *((char *)sp[0]);
                break;
            case OpLdUB:
                sp[0] = *((uchar *)sp[0]);
                break;
            case OpLdW:
                sp[0] = *((short *)sp[0]);
                break;
            case OpLdUW:
                sp[0] = *((ushort *)sp[0]);
                break;
            case OpLdDW:
                sp[0] = *((int *)sp[0]);
                break;
            case OpLdN: {
                int n;
                uchar *src, *dest;

                n = *(int *)ip;
                ip += 4;
                src = (uchar *)sp[0];
                dest = (uchar *)sp;
                while (n-- > 0)
                    *dest++ = *src++;
                break;
            }

                /* memory write */
            case OpStB:
                *((char *)sp[0]) = sp[-1];
                --sp;
                break;
            case OpStW:
                *((short *)sp[0]) = sp[-1];
                --sp;
                break;
            case OpStDW:
                *((int *)sp[0]) = sp[-1];
                --sp;
                break;
            /*case OpStN: {
                int n;
                uchar *src, *dest;

                n = *(int *)ip;
                ip += 4;
                src = (uchar *)sp-n;
                dest = (uchar *)sp[0];
                sp = (int *)src;
                while (n-- > 0)
                    *dest++ = *src++;
                break;
            }*/
            case OpMemCpy:
                memmove((void *)sp[-1], (const void *)sp[0], *(int *)ip);
                ip += 4;
                --sp;
                break;

            case OpFill:
                memset((void *)sp[-1], sp[0], *(int *)ip);
                ip += 4;
                --sp;
                break;

                /* load immediate pointers */
            case OpLdBP:
                ++sp;
                sp[0] = (int)bp + *(int *)ip;
                ip += 4;
                break;
            case OpLdI:
                ++sp;
                sp[0] = *(int *)ip;
                ip += 4;
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
                sp[-1] = (unsigned)sp[-1]/(unsigned)sp[0];
                --sp;
                break;
            case OpSMod:
                sp[-1] %= sp[0];
                --sp;
                break;
            case OpUMod:
                sp[-1] = (unsigned)sp[-1]%(unsigned)sp[0];
                --sp;
                break;
            case OpNeg:
                sp[0] = -sp[0];
                break;
            case OpNot:
                sp[0] = !sp[0];
                break;

            case OpSLT:
                sp[-1] = sp[-1]<sp[0];
                --sp;
                break;
            case OpULT:
                sp[-1] = (unsigned)sp[-1]<(unsigned)sp[0];
                --sp;
                break;
            case OpSLET:
                sp[-1] = sp[-1]<=sp[0];
                --sp;
                break;
            case OpULET:
                sp[-1] = (unsigned)sp[-1]<=(unsigned)sp[0];
                --sp;
                break;
            case OpSGT:
                sp[-1] = sp[-1]>sp[0];
                --sp;
                break;
            case OpUGT:
                sp[-1] = (unsigned)sp[-1]>(unsigned)sp[0];
                --sp;
                break;
            case OpSGET:
                sp[-1] = sp[-1]>=sp[0];
                --sp;
                break;
            case OpUGET:
                sp[-1] = (unsigned)sp[-1]>=(unsigned)sp[0];
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
                sp[-1] >>= sp[0];
                --sp;
                break;
            case OpSRA:
                sp[-1] = (unsigned)sp[-1] >> sp[0];
                --sp;
                break;

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

                /* subroutines */
            case OpCall:
                a = *(int *)ip; /* size of param area */
                ip += 4;
                ip1 = (uchar *)sp[0];
                sp[0] = (int)ip;
                sp[1] = (int)bp;
                sp[2] = a;
                sp += 2;
                ip = ip1;
                bp = sp;
                break;
            case OpRet:
                a = sp[0]; /* return value */
                sp = bp;
                ip = (uchar *)sp[-2];
                bp = (int *)sp[-1];
                b = sp[0]; /* size of param area */
                sp = (int *)((long)sp-8-b); /* old bp + ret addr == 8 */
                sp[0] = a;
                break;

                /* jumps */
            case OpJmp:
                ip1 = (uchar *)(*(int *)ip);
                ip = ip1;
                break;
            case OpJmpF:
                ip1 = (uchar *)(*(int *)ip);
                ip += 4;
                if (!sp[0])
                    ip = ip1;
                --sp;
                break;
            case OpJmpT:
                ip1 = (uchar *)(*(int *)ip);
                ip += 4;
                if (sp[0])
                    ip = ip1;
                --sp;
                break;

            case OpSwitch: {
                int val, count;
                int *tab, *p, *p_end, *res;

                val = sp[-1];
                tab = (int *)sp[0];
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

            case OpLibCall:
                a = *(int *)ip;
                ip += 4;
                ++sp;
                do_libcall(sp, bp, a);
                break;

                /* stack management */
            case OpAddSP:
                a = *(int *)ip;
                ip += 4;
                sp = (int *)((int)sp+a);
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

    /*
     * Bss
     */
    fread(&bss_size, sizeof(int), 1, fp);
    bss = calloc(bss_size, 1);

    /*
     * Data
     */
    fread(&data_size, sizeof(int), 1, fp);
    data = malloc(data_size);
    fread(data, data_size, 1, fp);
    fread(&ndreloc, sizeof(int), 1, fp);
    for (i = 0; i < ndreloc; i++) {
        int base;
        int segment, offset;

        fread(&segment, sizeof(int), 1, fp);
        fread(&offset, sizeof(int), 1, fp);
        base = (segment==TEXT_SEG)?(int)text:(segment==DATA_SEG)?(int)data:(int)bss;
        *(int *)((char *)data+offset) += base;
    }

    /*
     * Text
     */
    fread(&text_size, sizeof(int), 1, fp);
    fread(text, text_size, 1, fp);
    fread(&ntreloc, sizeof(int), 1, fp);
    for (i = 0; i < ntreloc; i++) {
        int base;
        int segment, offset;

        fread(&segment, sizeof(int), 1, fp);
        fread(&offset, sizeof(int), 1, fp);
        base = (segment==TEXT_SEG)?(int)text:(segment==DATA_SEG)?(int)data:(int)bss;
        *(int *)&text[offset] += base;
    }

    fclose(fp);
}

void disassemble_data(uchar *text, int text_size);
void disassemble_text(uchar *text, int text_size);

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
    int *sp;

    prog_name = argv[0];
    if (argc != 2) {
        printf("usage: %s <program>\n", prog_name);
        exit(0);
    }

    load_code(argv[1]);
    printf("Bss:\n");
    disassemble_data(bss, bss_size);
    printf("Data:\n");
    disassemble_data(data, data_size);
    printf("Code:\n");
    disassemble_text(text, text_size);

    stack = malloc(MAX_STACK_SIZE*sizeof(int));
    sp = exec();
    printf("result ==>%d\n", *sp);
    // printf("result ==>%s\n", *sp);
    // printf("stack[7](%p)=%d (%x)\n", &stack[7], stack[7], stack[7]);
    // printf("stack[6](%p)=%d (%x)\n", &stack[6], stack[6], stack[6]);
    // printf("stack[5](%p)=%d (%x)\n", &stack[5], stack[5], stack[5]);
    // printf("stack[4](%p)=%d (%x)\n", &stack[4], stack[4], stack[4]);
    // printf("stack[3](%p)=%d (%x)\n", &stack[3], stack[3], stack[3]);
    printf("stack[2](%p)=%d (%x)\n", &stack[2], stack[2], stack[2]);
    printf("stack[1](%p)=%d (%x)\n", &stack[1], stack[1], stack[1]);
    printf("stack[0](%p)=%d (%x)\n", &stack[0], stack[0], stack[0]);

    return 0;
}


void disassemble_text(uchar *text, int text_size)
{
    uchar *p;

    for (p = text; p < text+text_size;) {
        printf("(%p) ", p);
        switch (*p++) {
        case OpHalt:    printf("halt\n");   break;
        case OpLdB:     printf("ldb\n");    break;
        case OpLdUB:    printf("ldub\n");   break;
        case OpLdW:     printf("ldw\n");    break;
        case OpLdUW:    printf("lduw\n");   break;
        case OpLdDW:    printf("lddw\n");   break;
        case OpLdN:     printf("ldn ");     printf("%x\n", *(int *)p); p+=4; break;
        case OpStB:     printf("stb\n");    break;
        case OpStW:     printf("stw\n");    break;
        case OpStDW:    printf("stdw\n");   break;
        case OpStN:     printf("stn ");     printf("%x\n", *(int *)p); p+=4; break;
        case OpMemCpy:  printf("memcpy ");  printf("%x\n", *(int *)p); p+=4; break;
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
        case OpLdI:     printf("ldi ");     printf("%x\n", *(int *)p); p+=4; break;
        case OpLdBP:    printf("ldbp ");    printf("%x\n", *(int *)p); p+=4; break;
        case OpJmpF:    printf("jmpf ");    printf("%x\n", *(int *)p); p+=4; break;
        case OpJmpT:    printf("jmpt ");    printf("%x\n", *(int *)p); p+=4; break;
        case OpJmp:     printf("jmp ");     printf("%x\n", *(int *)p); p+=4; break;
        case OpCall:    printf("call ");    printf("%x\n", *(int *)p); p+=4; break;
        case OpFill:    printf("fill ");    printf("%x\n", *(int *)p); p+=4; break;
        case OpRet:     printf("ret\n");    break;
        case OpDup:     printf("dup\n");    break;
        case OpPop:     printf("pop\n");    break;
        case OpAddSP:   printf("addsp ");   printf("%x\n", *(int *)p); p+=4; break;
        case OpNop:     printf("nop\n");    break;
        case OpSwap:    printf("swap\n");   break;
        case OpSwitch:  printf("switch\n"); break;
        case OpLibCall: printf("libcall "); printf("%x\n", *(int *)p); p+=4; break;
        }
    }
}

void disassemble_data(uchar *data, int data_size)
{
    uchar *p;

    for (p = data; p < data+data_size;) {
        printf("(%p) ", p);
        printf("%x\n", *(int *)p);
        p += 4;
    }
}
