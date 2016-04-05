/*
    LUXAS: small nasm-like x86/x64 assembler.

                        ASM grammar
    program = line { line } EOF
    line = [ label ] [ ( source_line | directive ) ] EOL
    source_line = [ prefix ] ( [ instruction [ operand [ "," operand ] ] ] | pseudo_instruction )
    label = ID ":"
    operand = [ size_specifier ] ( REG | "[" eff_addr "]" | OR_expr )
    size_specifier = "byte" | "word" | "dword" | "qword"
    eff_addr = REG "+" REG [ "*" NUM ] "+" OR_expr
    OR_expr = XOR_expr { "|" XOR_expr }
    XOR_expr = AND_expr { "^" AND_expr }
    AND_expr = SHIFT_expr { "&" SHIFT_expr }
    SHIFT_expr = ADD_expr { ( "<<" | ">>" ) ADD_expr }
    ADD_expr = MUL_expr { ( "+" | "-" ) MUL_expr }
    MUL_expr = UNARY_expr { ( "*" | "/" | "//" | "%" | "%%" ) UNARY_expr }
    UNARY_expr = ( "+" | "-" | "~" | "!" ) UNARY_expr | PRIMARY_expr
    PRIMARY_expr = "(" OR_expr ")" | ID | NUM | "$"
    prefix = "rep"   |
             "repe"  |
             "repz"  |
             "repne" |
             "repnz" |
             "times" crit_expr
    directive = ( "segment" | "section" ) id |
                "extern" id { "," id } |
                "global" id [ ":" id ] { "," id [ ":" id ] } |
                "align"  NUM |
                "alignb" NUM
    pseudo_instruction = "resb" crit_expr |
                         "resw" crit_expr |
                         "resd" crit_expr |
                         "resq" crit_expr |
                         "db" OR_expr { "," OR_expr } |
                         "dw" OR_expr { "," OR_expr } |
                         "dd" OR_expr { "," OR_expr } |
                         "dq" OR_expr { "," OR_expr }
    crit_expr = same as OR_expr but cannot reference future symbols

        Tokens
    ID = ( '_' | '.' | '$' | [A-Za-z] ) ( '_' | '.' | '$' | '@' | [0-9A-Za-z] )*
    NUM = [0-9]+ | "0x" [A-F0-9]+
    Comments start with ";" and extend until the end of the line.

        Reserved words
    The following identifiers are reserved:
        - all size specifier names
        - all register names
        - all directive names
        - all prefix names
    A reserved word can be used as an identifier prefixing it with a '$'.
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdint.h>
#include <elf.h>
#include "../util/ELF_util.h"
#include "../util/util.h"
#include "../util/arena.h"

#define bool int

typedef enum {
    TOK_ID,
    TOK_NUM,
    TOK_COMMA,
    TOK_DOT,
    TOK_COLON,
    TOK_DOLLAR,
    TOK_LBRACKET,
    TOK_RBRACKET,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_BYTE,
    TOK_WORD,
    TOK_DWORD,
    TOK_QWORD,
    TOK_SECTION,
    TOK_EXTERN,
    TOK_GLOBAL,
    TOK_ALIGN,
    TOK_ALIGNB,
    TOK_RESB,
    TOK_RESW,
    TOK_RESD,
    TOK_RESQ,
    TOK_DB,
    TOK_DW,
    TOK_DD,
    TOK_DQ,
    TOK_REP,
    TOK_REPE,
    TOK_REPZ,
    TOK_REPNE,
    TOK_REPNZ,
    TOK_TIMES,
    TOK_PLUS,
    TOK_MINUS,
    TOK_MUL,
    TOK_DIV,  /* // */
    TOK_UDIV, /* / */
    TOK_MOD,  /* %% */
    TOK_UMOD, /* % */
    TOK_LSHIFT,
    TOK_RSHIFT,
    TOK_OR,
    TOK_XOR,
    TOK_AND,
    TOK_CMPL, /* ~ */
    TOK_LNEG, /* ! */
    TOK_EOL,
    TOK_EOF,
    TOK_DEREF, /* "[]" */
    TOK_UNARY_PLUS,
    TOK_UNARY_MINUS,
    TOK_AL,
    TOK_R8B,
    TOK_AX,
    TOK_R8W,
    TOK_EAX,
    TOK_R8D,
    TOK_RAX,
    TOK_R8,
    TOK_CL,
    TOK_R9B,
    TOK_CX,
    TOK_R9W,
    TOK_ECX,
    TOK_R9D,
    TOK_RCX,
    TOK_R9,
    TOK_DL,
    TOK_R10B,
    TOK_DX,
    TOK_R10W,
    TOK_EDX,
    TOK_R10D,
    TOK_RDX,
    TOK_R10,
    TOK_BL,
    TOK_R11B,
    TOK_BX,
    TOK_R11W,
    TOK_EBX,
    TOK_R11D,
    TOK_RBX,
    TOK_R11,
    TOK_AH,
    TOK_SPL,
    TOK_R12B,
    TOK_SP,
    TOK_R12W,
    TOK_ESP,
    TOK_R12D,
    TOK_RSP,
    TOK_R12,
    TOK_CH,
    TOK_BPL,
    TOK_R13B,
    TOK_BP,
    TOK_R13W,
    TOK_EBP,
    TOK_R13D,
    TOK_RBP,
    TOK_R13,
    TOK_DH,
    TOK_SIL,
    TOK_R14B,
    TOK_SI,
    TOK_R14W,
    TOK_ESI,
    TOK_R14D,
    TOK_RSI,
    TOK_R14,
    TOK_BH,
    TOK_DIL,
    TOK_R15B,
    TOK_DI,
    TOK_R15W,
    TOK_EDI,
    TOK_R15D,
    TOK_RDI,
    TOK_R15,
} Token;

int regenctab[] = {
    0, 0, 0, 0,
    0, 0, 0, 0,
    1, 1, 1, 1,
    1, 1, 1, 1,
    2, 2, 2, 2,
    2, 2, 2, 2,
    3, 3, 3, 3,
    3, 3, 3, 3,
    4, 4, 4, 4,
    4, 4, 4, 4,
    4,
    5, 5, 5, 5,
    5, 5, 5, 5,
    5,
    6, 6, 6, 6,
    6, 6, 6, 6,
    6,
    7, 7, 7, 7,
    7, 7, 7, 7,
    7,
};

/*
 * Meanings:
 *  0: doesn't need REX
 *  1: needs REX.(R|X|B)
 *  2: needs REX to be present
 *  -1: cannot be used with REX
 */
int regrextab[] = {
    0, 1, 0, 1,
    0, 1, 0, 1,
    0, 1, 0, 1,
    0, 1, 0, 1,
    0, 1, 0, 1,
    0, 1, 0, 1,
    0, 1, 0, 1,
    0, 1, 0, 1,
   -1, 2, 1, 0,
    1, 0, 1, 0,
    1,
   -1, 2, 1, 0,
    1, 0, 1, 0,
    1,
   -1, 2, 1, 0,
    1, 0, 1, 0,
    1,
   -1, 2, 1, 0,
    1, 0, 1, 0,
    1,
};

#define ISSIZ(tok)  (tok>=TOK_BYTE    && tok<=TOK_QWORD)
#define ISDIR(tok)  (tok>=TOK_SECTION && tok<=TOK_ALIGNB)
#define ISPSE(tok)  (tok>=TOK_RESB    && tok<=TOK_DQ)
#define ISPRE(tok)  (tok>=TOK_REP     && tok<=TOK_TIMES)
#define ISREG(tok)  (tok>=TOK_AL      && tok<=TOK_R15)
#define REGENC(r)   regenctab[(r)-TOK_AL]
#define REGREX(r)   regrextab[(r)-TOK_AL]
#define REGSIZ(r)   regsiztab[(r)-TOK_AL]

typedef struct SBlock SBlock;
typedef struct Section Section;
typedef struct Reloc Reloc;
typedef struct Symbol Symbol;
typedef struct Operand Operand;
typedef struct UnrExpr UnrExpr;
typedef struct UnrLab UnrLab;

char *prog_name, *inpath;
int line_number = 1;
char *curr, *buf;
#define MAX_LEXEME      512
char lexeme_buf1[MAX_LEXEME];
char lexeme_buf2[MAX_LEXEME];
char *lexeme;
#define MAX_LINE_BUF    4096
char line_buf[MAX_LINE_BUF];
bool reading_from_stdin;
char *non_local_label;
Arena *opnd_arena;
jmp_buf env;
FILE *output_file;
bool targeting_x64;
void err1(char *fmt, ...);
void err2(Operand *op, char *fmt, ...);

typedef enum {
    SectionKind,
    OtherKind,
} SymKind;
typedef enum {
    LocalBind,
    GlobalBind,
    ExternBind,
} SymBind;
#define HASH_SIZE   1009
#define HASH(s)     (hash(s)%HASH_SIZE)
struct Symbol {
    SymKind kind;
    SymBind bind;
    char *name;
    uint64_t val;
    Section *sec;   /* symbol's associated section (NULL for 'extern' symbols) */
    uint16_t ndx;   /* index into ELF file's symbol table */
    bool is_func;   /* the symbol will have type STT_FUNC in the output's file symtab */
    Symbol *next;
} *symbols[HASH_SIZE];

Symbol *define_symbol(SymKind kind, SymBind bind, char *name, uint64_t val, Section *sec)
{
    unsigned h;
    Symbol *np;

    h = HASH(name);
    for (np = symbols[h]; np != NULL; np = np->next)
        if (np->kind==kind && equal(np->name, name))
            break;
    if (np == NULL) {
        np = malloc(sizeof(Symbol));
        np->kind = kind;
        np->bind = bind;
        np->name = strdup(name);
        np->val = val;
        np->sec = sec;
        np->is_func = FALSE;
        np->next = symbols[h];
        symbols[h] = np;
    } else if (np->bind == ExternBind) {
        if (bind == LocalBind)
            goto redef;
    } else if (np->bind == GlobalBind) {
        if (np->sec == NULL) {
            if (bind == ExternBind) {
                np->bind = ExternBind;
            } else if (bind == LocalBind) {
                np->val = val;
                np->sec = sec;
            }
        } else if (bind != GlobalBind) {
            goto redef;
        }
    } else {
        goto redef;
    }
    return np;
redef:
    err1("symbol `%s' redefined", name);
}

Symbol *lookup_symbol(char *name)
{
    Symbol *np;

    for (np = symbols[HASH(name)]; np != NULL; np = np->next)
        if (equal(np->name, name))
            break;
    return np;
}

struct SBlock {
    char *avail, *limit;
    SBlock *next;
};

struct Section {
    int LC;
    char *name;
    SBlock *first, *last;
    Reloc *relocs;          /* relocations applied to this section */
    Symbol *sym;            /* symbol table entry for this section */
    union {
        Elf32_Shdr hdr32;
        Elf64_Shdr hdr64;
    } h;
    union {
        Elf32_Shdr rhdr32;
        Elf64_Shdr rhdr64;
    } rh;                   /* section header for the associated relocation section (if any) */
    uint16_t shndx;         /* index into section header table */
    Section *next;
} *sections, *curr_section;

#define NOM_BLK_SIZ   512
#define SBLOCK_SIZ(b) ((b->avail)-((char *)(b))-sizeof(SBlock))
#define LC()          (curr_section->LC)
#define GET_POS()     (curr_section->last->avail)
#define DEF_SEC       ".text" /* start to assemble in this section if none is specified */

void set_curr_section(char *name)
{
    Section *s;
    static Elf32_Half shndx = 4; /* [0]=UND, [1]=.shstrtab, [2]=.symtab, [3]=.strtab */

    for (s = sections; s != NULL; s = s->next)
        if (equal(s->name, name))
            break;
    if (s == NULL) {
        unsigned n;

        s = malloc(sizeof(Section));
        s->name = strdup(name);
        s->LC = 0;
        n = sizeof(SBlock)+NOM_BLK_SIZ;
        s->first = malloc(n);
        s->first->limit = (char *)s->first+n;
        s->first->avail = (char *)s->first+sizeof(SBlock);
        s->first->next = NULL;
        s->last = s->first;
        s->relocs = NULL;
        s->sym = define_symbol(SectionKind, LocalBind, name, 0, s);
        memset(&s->h.hdr64, 0, sizeof(Elf64_Shdr));
        memset(&s->rh.rhdr64, 0, sizeof(Elf64_Shdr));
        s->shndx = shndx++;
        s->next = NULL;
        if (sections == NULL) {
            sections = s;
        } else {
            Section *tmp;

            for (tmp = sections; tmp->next != NULL; tmp = tmp->next);
            tmp->next = s;
        }
    }
    curr_section = s;
}

void expand_curr_section(void)
{
    SBlock *s;
    unsigned n;

    n = sizeof(SBlock)+NOM_BLK_SIZ;
    s = malloc(n);
    s->limit = (char *)s+n;
    s->avail = (char *)s+sizeof(SBlock);
    s->next = NULL;
    curr_section->last->next = s;
    curr_section->last = s;
}

void write_byte(int b)
{
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    else if (curr_section->last->avail+1 > curr_section->last->limit)
        expand_curr_section();
    *curr_section->last->avail = (char)b;
    curr_section->last->avail += 1;
    curr_section->LC += 1;
}

void write_word(int w)
{
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    else if (curr_section->last->avail+2 > curr_section->last->limit)
        expand_curr_section();
    *(short *)curr_section->last->avail = (short)w;
    curr_section->last->avail += 2;
    curr_section->LC += 2;
}

void write_dword(int d)
{
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    else if (curr_section->last->avail+4 > curr_section->last->limit)
        expand_curr_section();
    *(int *)curr_section->last->avail = d;
    curr_section->last->avail += 4;
    curr_section->LC += 4;
}

void write_qword(long long q)
{
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    else if (curr_section->last->avail+8 > curr_section->last->limit)
        expand_curr_section();
    *(long long *)curr_section->last->avail = q;
    curr_section->last->avail += 8;
    curr_section->LC += 8;
}

#define Reg_mode            0x000001 /* reg */
#define Imm_mode            0x000002 /* imm */
#define Dir_mode            0x000004 /* [ disp32 ] */
#define RegInd_mode         0x000008 /* [ reg ] */
#define Based_mode          0x000010 /* [ reg+disp8/32 ] */
#define Index_mode          0x000020 /* [ reg*scale+disp32 ] */
#define BasedIndex_mode     0x000040 /* [ reg+reg*scale ] */
#define BasedIndexDisp_mode 0x000080 /* [ reg+reg*scale+disp8/32 ] */
#define Acc_mode            0x000100 /* accumulator (AL, AX, EAX) */
#define None_mode           0x000200
/* size specifiers */
#define Byte                0x000400
#define Word                0x000800
#define Dword               0x001000
#define Qword               0x002000
#define SIZE_MASK           (Byte|Word|Dword|Qword)
/* special */
#define Reg_CL_mode         0x004000
#define Imm_1_mode          0x008000
#define HasSizSpe_mode      0x010000 /* used only with immediates */
#define NeedsREX_mode       0x020000
/* r/m operand */
#define rm  (Reg_mode|Dir_mode|RegInd_mode|Based_mode|Index_mode|BasedIndex_mode|BasedIndexDisp_mode)
/* assembling instructions */
enum {
    I_RM,
    I_MR,
    I_MI,
    I_AI,
    I_RI,    /* mov reg, imm (register encoded in opcode) */
    I_RMI,   /* imul reg, imm16/32 */
    I_M,     /* single r/m operand */
    I_R,     /* register encoded in opcode */
    I_I,     /* single immediate operand */
    I_REL8,
    I_REL32,
    I_MX,    /* shifts with 1 or CL as second operand */
    I_AR,    /* accumulator & register encoded in opcode */
    I_RA,    /* register encoded in opcode & accumulator */
};

/* instruction classes */
typedef enum {
    op_adc,
    op_add,     op_and,     op_call,    op_cdq,
    op_cmp,     op_cqo, 	op_dec,     op_div,
    op_idiv,
    op_imul,    op_inc,     op_ja,      op_jae,
    op_jb,      op_jbe,     op_je,      op_jg,
    op_jge,     op_jl,      op_jle,     op_jmp,
    op_jne,     op_lea,     op_mov,     op_movsb,
    op_movsd,   op_movsq,   op_movsw,   op_movsx,
    op_movzx,
    op_neg,     op_nop,     op_not,     op_or,
    op_pop,     op_push,    op_ret,     op_sal,
    op_sar,     op_sbb,     op_seta,    op_setae,
    op_setb,    op_setbe,   op_sete,    op_setg,
    op_setge,   op_setl,    op_setle,   op_setne,
    op_shl,     op_shr,     op_sub,     op_test,
    op_xchg,    op_xor,     op_int,     op_syscall,
} InstrClass;

/*
 * Note: within each table entry, instruction
 * forms are tested for match from top to bottom.
 */
struct {
    InstrClass iclass;
    bool esc_opc;           /* escape opcode byte (0x0F) */
    unsigned char opcode;
    unsigned char opc_ext;  /* opcode extension (ModRM:reg) */
    unsigned op1, op2;
    int asm_instr;          /* assembling instructions */
} opcode_table[] = { /* sorted for bsearch() */
    /* ADC */
    { op_adc,   0,  0x83,   0x02,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    { op_adc,   0,  0x14,   -1,     Acc_mode|Byte,              Imm_mode|Byte,              I_AI },
    { op_adc,   0,  0x15,   -1,     Acc_mode|Word,              Imm_mode|Word,              I_AI },
    { op_adc,   0,  0x15,   -1,     Acc_mode|Dword|Qword,       Imm_mode|Dword,             I_AI },
    { op_adc,   0,  0x80,   0x02,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_adc,   0,  0x81,   0x02,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_adc,   0,  0x81,   0x02,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    { op_adc,   0,  0x10,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_adc,   0,  0x11,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_adc,   0,  0x12,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_adc,   0,  0x13,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* ADD */
    { op_add,   0,  0x83,   0x00,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    { op_add,   0,  0x04,   -1,     Acc_mode|Byte,              Imm_mode|Byte,              I_AI },
    { op_add,   0,  0x05,   -1,     Acc_mode|Word,              Imm_mode|Word,              I_AI },
    { op_add,   0,  0x05,   -1,     Acc_mode|Dword|Qword,       Imm_mode|Dword,             I_AI },
    { op_add,   0,  0x80,   0x00,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_add,   0,  0x81,   0x00,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_add,   0,  0x81,   0x00,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    { op_add,   0,  0x00,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_add,   0,  0x01,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_add,   0,  0x02,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_add,   0,  0x03,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* AND */
    { op_and,   0,  0x83,   0x04,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    { op_and,   0,  0x24,   -1,     Acc_mode|Byte,              Imm_mode|Byte,              I_AI },
    { op_and,   0,  0x25,   -1,     Acc_mode|Word,              Imm_mode|Word,              I_AI },
    { op_and,   0,  0x25,   -1,     Acc_mode|Dword|Qword,       Imm_mode|Dword,             I_AI },
    { op_and,   0,  0x80,   0x04,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_and,   0,  0x81,   0x04,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_and,   0,  0x81,   0x04,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    { op_and,   0,  0x20,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_and,   0,  0x21,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_and,   0,  0x22,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_and,   0,  0x23,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* CALL */
    { op_call,  0,  0xE8,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_call,  0,  0xFF,   0x02,   rm|Dword|Qword,             None_mode,                  I_M },
    /* CDQ */
    { op_cdq,   0,  0x99,   -1,     None_mode,                  None_mode,                  -1 },
    /* CMP */
    { op_cmp,   0,  0x83,   0x07,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    { op_cmp,   0,  0x3C,   -1,     Acc_mode|Byte,              Imm_mode|Byte,              I_AI },
    { op_cmp,   0,  0x3D,   -1,     Acc_mode|Word,              Imm_mode|Word,              I_AI },
    { op_cmp,   0,  0x3D,   -1,     Acc_mode|Dword|Qword,       Imm_mode|Dword,             I_AI },
    { op_cmp,   0,  0x80,   0x07,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_cmp,   0,  0x81,   0x07,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_cmp,   0,  0x81,   0x07,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    { op_cmp,   0,  0x38,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_cmp,   0,  0x39,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_cmp,   0,  0x3A,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_cmp,   0,  0x3B,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* CQO */
    { op_cqo,	0,	0x99,	-1,		None_mode,					None_mode,					-1 }, /* requires 0x48 prefix */
    /* DEC */
    /*  Not encodable in 64-bit mode                                                                */
    /*{ op_dec,  0,  0x48,   -1,      Reg_mode|Word|Dword,        None_mode,                  I_R },*/
    { op_dec,   0,  0xFE,   0x01,   rm|Byte,                    None_mode,                  I_M },
    { op_dec,   0,  0xFF,   0x01,   rm|Word|Dword|Qword,        None_mode,                  I_M },
    /* DIV */
    { op_div,   0,  0xF6,   0x06,   rm|Byte,                    None_mode,                  I_M },
    { op_div,   0,  0xF7,   0x06,   rm|Word|Dword|Qword,        None_mode,                  I_M },
    /* IDIV */
    { op_idiv,  0,  0xF6,   0x07,   rm|Byte,                    None_mode,                  I_M },
    { op_idiv,  0,  0xF7,   0x07,   rm|Word|Dword|Qword,        None_mode,                  I_M },
    /* IMUL */
    { op_imul,  0,  0xF6,   0x05,   rm|Byte,                    None_mode,                  I_M },
    { op_imul,  0,  0xF7,   0x05,   rm|Word|Dword|Qword,        None_mode,                  I_M },
    { op_imul,  0,  0x6B,   -1,     Reg_mode|Word|Dword|Qword,  Imm_mode|Byte,              I_RMI },
    { op_imul,  0,  0x69,   -1,     Reg_mode|Word,              Imm_mode|Word,              I_RMI },
    { op_imul,  0,  0x69,   -1,     Reg_mode|Dword|Qword,       Imm_mode|Dword,             I_RMI },
    { op_imul,  1,  0xAF,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* INC */
    /*  Not encodable in 64-bit mode                                                                */
    /*{ op_inc,  0,  0x40,   -1,      Reg_mode|Word|Dword,        None_mode,                  I_R },*/
    { op_inc,   0,  0xFE,   0x00,   rm|Byte,                    None_mode,                  I_M },
    { op_inc,   0,  0xFF,   0x00,   rm|Word|Dword|Qword,        None_mode,                  I_M },
    /* INT */
    { op_int,   0,  0xCD,   -1,     Imm_mode|Byte,              None_mode,                  I_I },
    /* Jcc */
    { op_ja,    0,  0x77,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_ja,    1,  0x87,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_jae,   0,  0x73,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_jae,   1,  0x83,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_jb,    0,  0x72,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_jb,    1,  0x82,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_jbe,   0,  0x76,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_jbe,   1,  0x86,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_je,    0,  0x74,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_je,    1,  0x84,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_jg,    0,  0x7F,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_jg,    1,  0x8F,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_jge,   0,  0x7D,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_jge,   1,  0x8D,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_jl,    0,  0x7C,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_jl,    1,  0x8C,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_jle,   0,  0x7E,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_jle,   1,  0x8E,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    /* JMP */
    { op_jmp,   0,  0xEB,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_jmp,   0,  0xE9,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    { op_jmp,   0,  0xFF,   0x04,   rm|Dword|Qword,             None_mode,                  I_M },
    /* Jcc (cont.) */
    { op_jne,   0,  0x75,   -1,     Imm_mode|Byte,              None_mode,                  I_REL8 },
    { op_jne,   1,  0x85,   -1,     Imm_mode|Dword,             None_mode,                  I_REL32 },
    /* LEA */
    { op_lea,   0,  0x8D,   -1,     Reg_mode|Word|Dword|Qword,  rm&~Reg_mode|SIZE_MASK,     I_RM },
    /* MOV */
    { op_mov,   0,  0x88,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_mov,   0,  0x89,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_mov,   0,  0x8A,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_mov,   0,  0x8B,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    { op_mov,   0,  0xB0,   -1,     Reg_mode|Byte,              Imm_mode|Byte,              I_RI },
    { op_mov,   0,  0xB8,   -1,     Reg_mode|Word,              Imm_mode|Word,              I_RI },
    { op_mov,   0,  0xB8,   -1,     Reg_mode|Dword,             Imm_mode|Dword,             I_RI },
    { op_mov,   0,  0xB8,   -1,     Reg_mode|Qword,             Imm_mode|Qword,             I_RI },
    { op_mov,   0,  0xC6,   0x00,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_mov,   0,  0xC7,   0x00,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_mov,   0,  0xC7,   0x00,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    /* MOVSB */
    { op_movsb, 0,  0xA4,   -1,     None_mode,                  None_mode,                  -1 },
    /* MOVSD */
    { op_movsd, 0,  0xA5,   -1,     None_mode,                  None_mode,                  -1 },
    /* MOVSQ */
    { op_movsq, 0,  0xA5,   -1,     None_mode,                  None_mode,                  -1 }, /* requires 0x48 prefix */
    /* MOVSW */
    { op_movsw, 0,  0xA5,   -1,     None_mode,                  None_mode,                  -1 }, /* requires 0x66 prefix */
    /* MOVSX */
    { op_movsx, 1,  0xBE,   -1,     Reg_mode|Word|Dword|Qword,  rm|Byte,                    I_RM },
    { op_movsx, 1,  0xBF,   -1,     Reg_mode|Dword|Qword,       rm|Word,                    I_RM },
    { op_movsx, 0,  0x63,   -1,     Reg_mode|Qword,             rm|Dword,                   I_RM },
    /* MOVZX */
    { op_movzx, 1,  0xB6,   -1,     Reg_mode|Word|Dword|Qword,  rm|Byte,                    I_RM },
    { op_movzx, 1,  0xB7,   -1,     Reg_mode|Dword|Qword,       rm|Word,                    I_RM },
    /* NEG */
    { op_neg,   0,  0xF6,   0x03,   rm|Byte,                    None_mode,                  I_M },
    { op_neg,   0,  0xF7,   0x03,   rm|Word|Dword|Qword,        None_mode,                  I_M },
    /* NOP */
    { op_nop,   0,  0x90,   -1,     None_mode,                  None_mode,                  -1 },
    /* NOT */
    { op_not,   0,  0xF6,   0x02,   rm|Byte,                    None_mode,                  I_M },
    { op_not,   0,  0xF7,   0x02,   rm|Word|Dword|Qword,        None_mode,                  I_M },
    /* OR */
    { op_or,    0,  0x83,   0x01,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    { op_or,    0,  0x0C,   -1,     Acc_mode|Byte,              Imm_mode|Byte,              I_AI },
    { op_or,    0,  0x0D,   -1,     Acc_mode|Word,              Imm_mode|Word,              I_AI },
    { op_or,    0,  0x0D,   -1,     Acc_mode|Dword|Qword,       Imm_mode|Dword,             I_AI },
    { op_or,    0,  0x80,   0x01,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_or,    0,  0x81,   0x01,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_or,    0,  0x81,   0x01,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    { op_or,    0,  0x08,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_or,    0,  0x09,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_or,    0,  0x0A,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_or,    0,  0x0B,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* POP */
    { op_pop,   0,  0x58,   -1,     Reg_mode|Word|Dword|Qword,  None_mode,                  I_R },
    { op_pop,   0,  0x8F,   0x00,   rm|Word|Dword|Qword,        None_mode,                  I_M },
    /* PUSH */
    { op_push,  0,  0x50,   -1,     Reg_mode|Word|Dword|Qword,  None_mode,                  I_R },
    { op_push,  0,  0xFF,   0x06,   rm|Word|Dword|Qword,        None_mode,                  I_M },
    { op_push,  0,  0x6A,   -1,     Imm_mode|Byte,              None_mode,                  I_I },
    /*{ op_push,  0,  0x68,   -1,     Imm_mode|Word,              None_mode,                  I_I },*/
    { op_push,  0,  0x68,   -1,     Imm_mode|Dword,             None_mode,                  I_I },
    /* RET */
    { op_ret,   0,  0xC3,   -1,     None_mode,                  None_mode,                  -1 },
    { op_ret,   0,  0xC2,   -1,     Imm_mode|Word,              None_mode,                  I_I },
    /* SAL */
    { op_sal,   0,  0xD0,   0x04,   rm|Byte,                    Imm_1_mode|Byte,            I_MX },
    { op_sal,   0,  0xD2,   0x04,   rm|Byte,                    Reg_CL_mode|Byte,           I_MX },
    { op_sal,   0,  0xC0,   0x04,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_sal,   0,  0xD1,   0x04,   rm|Word|Dword|Qword,        Imm_1_mode|Byte,            I_MX },
    { op_sal,   0,  0xD3,   0x04,   rm|Word|Dword|Qword,        Reg_CL_mode|Byte,           I_MX },
    { op_sal,   0,  0xC1,   0x04,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    /* SAR */
    { op_sar,   0,  0xD0,   0x07,   rm|Byte,                    Imm_1_mode|Byte,            I_MX },
    { op_sar,   0,  0xD2,   0x07,   rm|Byte,                    Reg_CL_mode|Byte,           I_MX },
    { op_sar,   0,  0xC0,   0x07,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_sar,   0,  0xD1,   0x07,   rm|Word|Dword|Qword,        Imm_1_mode|Byte,            I_MX },
    { op_sar,   0,  0xD3,   0x07,   rm|Word|Dword|Qword,        Reg_CL_mode|Byte,           I_MX },
    { op_sar,   0,  0xC1,   0x07,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    /* SBB */
    { op_sbb,   0,  0x83,   0x03,   rm|Word|Dword,              Imm_mode|Byte,              I_MI },
    { op_sbb,   0,  0x1C,   -1,     Acc_mode|Byte,              Imm_mode|Byte,              I_AI },
    { op_sbb,   0,  0x1D,   -1,     Acc_mode|Word,              Imm_mode|Word,              I_AI },
    { op_sbb,   0,  0x1D,   -1,     Acc_mode|Dword|Qword,       Imm_mode|Dword,             I_AI },
    { op_sbb,   0,  0x80,   0x03,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_sbb,   0,  0x81,   0x03,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_sbb,   0,  0x81,   0x03,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    { op_sbb,   0,  0x18,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_sbb,   0,  0x19,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_sbb,   0,  0x1A,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_sbb,   0,  0x1B,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* SETcc */
    { op_seta,  1,  0x97,   -1,     rm|Byte,                    None_mode,                  I_M },
    { op_setae, 1,  0x93,   -1,     rm|Byte,                    None_mode,                  I_M },
    { op_setb,  1,  0x92,   -1,     rm|Byte,                    None_mode,                  I_M },
    { op_setbe, 1,  0x96,   -1,     rm|Byte,                    None_mode,                  I_M },
    { op_sete,  1,  0x94,   -1,     rm|Byte,                    None_mode,                  I_M },
    { op_setg,  1,  0x9F,   -1,     rm|Byte,                    None_mode,                  I_M },
    { op_setge, 1,  0x9D,   -1,     rm|Byte,                    None_mode,                  I_M },
    { op_setl,  1,  0x9C,   -1,     rm|Byte,                    None_mode,                  I_M },
    { op_setle, 1,  0x9E,   -1,     rm|Byte,                    None_mode,                  I_M },
    { op_setne, 1,  0x95,   -1,     rm|Byte,                    None_mode,                  I_M },
    /* SHL */
    { op_sal,   0,  0xD0,   0x04,   rm|Byte,                    Imm_1_mode|Byte,            I_MX },
    { op_sal,   0,  0xD2,   0x04,   rm|Byte,                    Reg_CL_mode|Byte,           I_MX },
    { op_sal,   0,  0xC0,   0x04,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_sal,   0,  0xD1,   0x04,   rm|Word|Dword|Qword,        Imm_1_mode|Byte,            I_MX },
    { op_sal,   0,  0xD3,   0x04,   rm|Word|Dword|Qword,        Reg_CL_mode|Byte,           I_MX },
    { op_sal,   0,  0xC1,   0x04,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    /* SHR */
    { op_shr,   0,  0xD0,   0x05,   rm|Byte,                    Imm_1_mode|Byte,            I_MX },
    { op_shr,   0,  0xD2,   0x05,   rm|Byte,                    Reg_CL_mode|Byte,           I_MX },
    { op_shr,   0,  0xC0,   0x05,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_shr,   0,  0xD1,   0x05,   rm|Word|Dword|Qword,        Imm_1_mode|Byte,            I_MX },
    { op_shr,   0,  0xD3,   0x05,   rm|Word|Dword|Qword,        Reg_CL_mode|Byte,           I_MX },
    { op_shr,   0,  0xC1,   0x05,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    /* SUB */
    { op_sub,   0,  0x83,   0x05,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    { op_sub,   0,  0x2C,   -1,     Acc_mode|Byte,              Imm_mode|Byte,              I_AI },
    { op_sub,   0,  0x2D,   -1,     Acc_mode|Word,              Imm_mode|Word,              I_AI },
    { op_sub,   0,  0x2D,   -1,     Acc_mode|Dword|Qword,       Imm_mode|Dword,             I_AI },
    { op_sub,   0,  0x80,   0x05,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_sub,   0,  0x81,   0x05,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_sub,   0,  0x81,   0x05,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    { op_sub,   0,  0x28,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_sub,   0,  0x29,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_sub,   0,  0x2A,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_sub,   0,  0x2B,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* SYSCALL */
    { op_syscall,1, 0x05,   -1,     None_mode,                  None_mode,                  -1 },
    /* TEST */
    { op_test,  0,  0xA8,   -1,     Acc_mode|Byte,              Imm_mode|Byte,              I_AI },
    { op_test,  0,  0xA9,   -1,     Acc_mode|Word,              Imm_mode|Word,              I_AI },
    { op_test,  0,  0xA9,   -1,     Acc_mode|Dword|Qword,       Imm_mode|Dword,             I_AI },
    { op_test,  0,  0xF6,   0x00,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_test,  0,  0xF7,   0x00,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_test,  0,  0xF7,   0x00,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    { op_test,  0,  0x84,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_test,  0,  0x85,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    /* XCHG */
    { op_xchg,  0,  0x90,   -1,     Acc_mode|Word|Dword|Qword,  Reg_mode|Word|Dword|Qword,  I_AR },
    { op_xchg,  0,  0x90,   -1,     Reg_mode|Word|Dword|Qword,  Acc_mode|Word|Dword|Qword,  I_RA },
    { op_xchg,  0,  0x86,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_xchg,  0,  0x86,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_xchg,  0,  0x87,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_xchg,  0,  0x87,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* XOR */
    { op_xor,   0,  0x83,   0x06,   rm|Word|Dword|Qword,        Imm_mode|Byte,              I_MI },
    { op_xor,   0,  0x34,   -1,     Acc_mode|Byte,              Imm_mode|Byte,              I_AI },
    { op_xor,   0,  0x35,   -1,     Acc_mode|Word,              Imm_mode|Word,              I_AI },
    { op_xor,   0,  0x35,   -1,     Acc_mode|Dword|Qword,       Imm_mode|Dword,             I_AI },
    { op_xor,   0,  0x80,   0x06,   rm|Byte,                    Imm_mode|Byte,              I_MI },
    { op_xor,   0,  0x81,   0x06,   rm|Word,                    Imm_mode|Word,              I_MI },
    { op_xor,   0,  0x81,   0x06,   rm|Dword|Qword,             Imm_mode|Dword,             I_MI },
    { op_xor,   0,  0x30,   -1,     rm|Byte,                    Reg_mode|Byte,              I_MR },
    { op_xor,   0,  0x31,   -1,     rm|Word|Dword|Qword,        Reg_mode|Word|Dword|Qword,  I_MR },
    { op_xor,   0,  0x32,   -1,     Reg_mode|Byte,              rm|Byte,                    I_RM },
    { op_xor,   0,  0x33,   -1,     Reg_mode|Word|Dword|Qword,  rm|Word|Dword|Qword,        I_RM },
    /* DUMMY ENTRY */
    { -1,       0,  0,      -1,     0,                          0,                          0 }
};

/* map of mnemonics to opcode table entries */
struct {
    char *mne;
    int ote;
} mne2ote[] = { /* sorted for bsearch() */
    { "adc" },
    { "add" },
    { "and" },
    { "call" },
    { "cdq" },
    { "cmp" },
    { "cqo" },
    { "dec" },
    { "div" },
    { "idiv" },
    { "imul" },
    { "inc" },
    { "int" },
    { "ja" },
    { "jae" },
    { "jb" },
    { "jbe" },
    { "je" },
    { "jg" },
    { "jge" },
    { "jl" },
    { "jle" },
    { "jmp" },
    { "jne" },
    { "lea" },
    { "mov" },
    { "movsb" },
    { "movsd" },
    { "movsq" },
    { "movsw" },
    { "movsx" },
    { "movzx" },
    { "neg" },
    { "nop" },
    { "not" },
    { "or" },
    { "pop" },
    { "push" },
    { "ret" },
    { "sal" },
    { "sar" },
    { "sbb" },
    { "seta" },
    { "setae" },
    { "setb" },
    { "setbe" },
    { "sete" },
    { "setg" },
    { "setge" },
    { "setl" },
    { "setle" },
    { "setne" },
    { "shl" },
    { "shr" },
    { "sub" },
    { "syscall" },
    { "test" },
    { "xchg" },
    { "xor" },
};

void init_tables(void)
{
    int i, j, lim;

    j = 0;
    lim = NELEMS(mne2ote);
    for (i = 0; i < lim; i++) {
        InstrClass cl;

        mne2ote[i].ote = j;
        for (cl = opcode_table[j].iclass; cl == opcode_table[j].iclass; j++)
            ;
    }
}

int regsiztab[] = {
    Byte,   Byte,   Word,   Word,
    Dword,  Dword,  Qword,  Qword,
    Byte,   Byte,   Word,   Word,
    Dword,  Dword,  Qword,  Qword,
    Byte,   Byte,   Word,   Word,
    Dword,  Dword,  Qword,  Qword,
    Byte,   Byte,   Word,   Word,
    Dword,  Dword,  Qword,  Qword,
    Byte,   Byte,   Byte,   Word,
    Word,   Dword,  Dword,  Qword,
    Qword,
    Byte,   Byte,   Byte,   Word,
    Word,   Dword,  Dword,  Qword,
    Qword,
    Byte,   Byte,   Byte,   Word,
    Word,   Dword,  Dword,  Qword,
    Qword,
    Byte,   Byte,   Byte,   Word,
    Word,   Dword,  Dword,  Qword,
    Qword,
};

#define ABS_EXPR 0
#define REL_EXPR 1
struct Operand {
    short op;
    short kind;             /* ABS/REL */
    unsigned addr_mode;
    union {
        long long val;
        Operand *child[2];
        struct {
            char *name;
            Symbol *sym;
        } lab;
        struct {
            int val;
            Symbol *sec;    /* symbol table entry for the associated section */
        } LC;
    } attr;
    int lineno;
};

#define LCHILD(n)   ((n)->attr.child[0])
#define RCHILD(n)   ((n)->attr.child[1])
#define EXPRVAL(n)  ((n)->attr.val)
#define EXPRKIND(n) ((n)->kind)

Operand *new_opnd(Token op)
{
    Operand *n;

    if ((n=arena_alloc(opnd_arena, sizeof(Operand))) == NULL)
        TERMINATE("%s: out of memory", prog_name);
    n->op = op;
    n->attr.lab.sym = NULL;
    n->lineno = line_number;
    return n;
}

#define RELOC_ABS    0x01 /* S+A */
#define RELOC_REL    0x02 /* S+A-P */
#define RELOC_SIZ8   0x04
#define RELOC_SIZ16  0x08
#define RELOC_SIZ32  0x10
#define RELOC_SIZ64  0x20
/*
 * For x86-64 the following causes sign-extending relocations (R_X86_64_32S):
 *      -> Effective address calculations that involve symbols.
 *           e.g. mov rax, [sym] ; the displacement is sign-extended to 64 bits (Intel #2.2.1.3)
 *      -> Any instruction that sign-extends the second immediate operand to 64 bits.
 *           e.g. mov rax, dword sym
 *      -> Push of 32 bits immediates.
 *           e.g. push sym
 *
 * For all other cases a zero-extending relocations (R_X86_64_32) is generated:
 *      -> e.g. mov eax, sym
 */
#define RELOC_SIGNED 0x40
struct Reloc {
    unsigned attr;          /* abs/rel, siz8/16/32/64, signed/unsigned */
    int offs;               /* where to do the fix */
    Elf64_Sxword add;
    Symbol *sym;
    Reloc *next;
};

Reloc *new_reloc(unsigned attr, int offs, Symbol *sym)
{
    Reloc *n;

    n = malloc(sizeof(Reloc));
    n->attr = attr;
    n->offs = offs;
    n->sym = sym;
    return n;
}

/* get symbol associated to relocatable expression */
Symbol *get_rel_sym(Operand *e)
{
    if (e->op == TOK_ID)
        return e->attr.lab.sym;
    else if (e->op == TOK_DOLLAR)
        return e->attr.LC.sec;
    else if (EXPRKIND(LCHILD(e)) == REL_EXPR)
        return get_rel_sym(LCHILD(e));
    else
        return get_rel_sym(RCHILD(e));
}

void bad_expr(Operand *e)
{
    err2(e, "invalid operand type");
}

long long eval_expr(Operand *e, bool pass2)
{
    long long res;

    switch (e->op) {
#define CHECK_BINOP()\
        do {\
            if (EXPRKIND(LCHILD(e))==REL_EXPR || EXPRKIND(RCHILD(e))==REL_EXPR)\
                bad_expr(e);\
            EXPRKIND(e) = ABS_EXPR;\
        } while (0)
#define CHECK_UNAOP()\
        do {\
            if (EXPRKIND(LCHILD(e)) == REL_EXPR)\
                bad_expr(e);\
            EXPRKIND(e) = ABS_EXPR;\
        } while (0)
#define L eval_expr(LCHILD(e), pass2)
#define R eval_expr(RCHILD(e), pass2)

    case TOK_OR:
        res = L|R;
        CHECK_BINOP();
        break;
    case TOK_XOR:
        res = L^R;
        CHECK_BINOP();
        break;
    case TOK_AND:
        res = L&R;
        CHECK_BINOP();
        break;
    case TOK_LSHIFT:
        res = L<<R;
        CHECK_BINOP();
        break;
    case TOK_RSHIFT:
        res = (unsigned long long)L>>R;
        CHECK_BINOP();
        break;
    case TOK_MUL:
        res = L*R;
        CHECK_BINOP();
        break;
    case TOK_DIV:
        res = L/R;
        CHECK_BINOP();
        break;
    case TOK_UDIV:
        res = (unsigned long long)L/(unsigned long long)R;
        CHECK_BINOP();
        break;
    case TOK_MOD:
        res = L%R;
        CHECK_BINOP();
        break;
    case TOK_UMOD:
        res = (unsigned long long)L%(unsigned long long)R;
        CHECK_BINOP();
        break;

    case TOK_PLUS:
        res = L+R;
        if (EXPRKIND(LCHILD(e)) == REL_EXPR) {
            if (EXPRKIND(RCHILD(e)) == REL_EXPR)
                bad_expr(e); /* rel + rel */
            else
                EXPRKIND(e) = REL_EXPR; /* rel + abs */
        } else if (EXPRKIND(RCHILD(e)) == REL_EXPR) {
            EXPRKIND(e) = REL_EXPR; /* abs + rel */
        } else {
            EXPRKIND(e) = ABS_EXPR; /* abs + abs */
        }
        break;
    case TOK_MINUS:
        res = L-R;
        if (EXPRKIND(LCHILD(e)) == REL_EXPR) {
            if (EXPRKIND(RCHILD(e)) == REL_EXPR) {
                Symbol *sl, *sr;

                sl = get_rel_sym(LCHILD(e));
                sr = get_rel_sym(RCHILD(e));
                /*
                 * Make sure the operands reside
                 * in the same section and none
                 * of them is 'extern'.
                 */
                if (sl->bind==ExternBind || sr->bind==ExternBind || !equal(sl->sec->name, sr->sec->name))
                    bad_expr(e);
                EXPRKIND(e) = ABS_EXPR; /* rel - rel */
            } else {
                EXPRKIND(e) = REL_EXPR; /* rel - abs */
            }
        } else if (EXPRKIND(RCHILD(e)) == REL_EXPR) {
            bad_expr(e); /* abs - rel */
        } else {
            EXPRKIND(e) = ABS_EXPR; /* abs - abs */
        }
        break;

    case TOK_UNARY_MINUS:
        res = -L;
        CHECK_UNAOP();
        break;
    case TOK_CMPL:
        res = ~L;
        CHECK_UNAOP();
        break;
    case TOK_LNEG:
        res = !L;
        CHECK_UNAOP();
        break;

    case TOK_ID: {
        Symbol *s;

        EXPRKIND(e) = REL_EXPR;
        s = e->attr.lab.sym;
        if (s!=NULL || (s=lookup_symbol(e->attr.lab.name))!=NULL) {
            e->attr.lab.sym = s;
            return s->val;
        } else if (!pass2) {
            longjmp(env, 1);
        } else {
            err2(e, "symbol `%s' undefined", e->attr.lab.name);
        }
    }
    case TOK_NUM:
        EXPRKIND(e) = ABS_EXPR;
        return EXPRVAL(e);
    case TOK_DOLLAR:
        EXPRKIND(e) = REL_EXPR;
        return e->attr.LC.val;

    default:
        assert(0);

#undef CHECK_BINOP
#undef CHECK_UNAOP
#undef L
#undef R
    }
    return res;
}

struct UnrExpr {
    Operand *expr;
    void *dest;
    int size;
    struct {
        int offs;
        Section *sec;
    } loc;          /* sec+offs is where the expression is located */
    bool reldisp;   /* is the expression a relative displacement? */
    bool signext;   /* sign extend the expression when extending to 64-bits? */
    UnrExpr *next;
} *unresolved_expressions_list;

void new_unr_expr(Operand *expr, void *dest, int size, int offs, Section *sec, bool reldisp, bool signext)
{
    UnrExpr *n;

    n = malloc(sizeof(UnrExpr));
    n->expr = expr;
    n->dest = dest;
    n->size = size;
    n->loc.offs = offs;
    n->loc.sec = sec;
    n->reldisp = reldisp;
    n->signext = signext;
    n->next = unresolved_expressions_list;
    unresolved_expressions_list = n;
}

void resolve_expressions(void)
{
    UnrExpr *n, *tmp;

    n = unresolved_expressions_list;
    while (n != NULL) {
        Reloc *r;
        Symbol *s;
        long long res;

        r = NULL;
        res = eval_expr(n->expr, TRUE);
        if (n->reldisp) {
            if (EXPRKIND(n->expr) == REL_EXPR
            && ((s=get_rel_sym(n->expr))->bind == ExternBind
            || !equal(s->sec->name, n->loc.sec->name))) {
                if (s->bind != ExternBind)
                    s = s->sec->sym; /* relocate with respect to section */
                switch (n->size) {
                case Byte:
                    r = new_reloc(RELOC_REL|RELOC_SIZ8, n->loc.offs, s);
                    if (targeting_x64)
                        r->add = res-1;
                    else
                        *(char *)n->dest = (char)(res-1);
                    break;
                /*case Word:
                    r = new_reloc(RELOC_REL|RELOC_SIZ16, n->loc.offs, s);
                    if (targeting_x64)
                        r->add = res-2;
                    else
                        *(short *)n->dest = (short)(res-2);
                    break;*/
                case Dword:
                    r = new_reloc(RELOC_REL|RELOC_SIZ32, n->loc.offs, s);
                    if (targeting_x64)
                        r->add = res-4;
                    else
                        *(int *)n->dest = (int)(res-4);
                    break;
                default:
                    assert(0);
                }
            } else { /* target is absolute or relocatable with respect to this same section */
                switch (n->size) {
                case Byte:
                    *(char *)n->dest = (char)(res-(n->loc.offs+1));
                    break;
                /*case Word:
                    *(short *)n->dest = (short)(res-(n->loc.offs+2));
                    break;*/
                case Dword:
                    *(int *)n->dest = (int)(res-(n->loc.offs+4));
                    break;
                default:
                    assert(0);
                }
            }
        } else {
            if (!targeting_x64 || EXPRKIND(n->expr)==ABS_EXPR) {
                switch (n->size) {
                case Byte:
                    *(char *)n->dest = (char)res;
                    break;
                case Word:
                    *(short *)n->dest = (short)res;
                    break;
                case Dword:
                    *(int *)n->dest = (int)res;
                    break;
                case Qword:
                    *(long long *)n->dest = res;
                    break;
                }
            }

            if (EXPRKIND(n->expr) == REL_EXPR) {
                if ((s=get_rel_sym(n->expr))->bind != ExternBind)
                    s = s->sec->sym; /* relocate with respect to section */
                switch (n->size) {
                case Byte:
                    r = new_reloc(RELOC_ABS|RELOC_SIZ8, n->loc.offs, s);
                    break;
                case Word:
                    r = new_reloc(RELOC_ABS|RELOC_SIZ16, n->loc.offs, s);
                    break;
                case Dword:
                    r = new_reloc(RELOC_ABS|RELOC_SIZ32|(n->signext?RELOC_SIGNED:0), n->loc.offs, s);
                    break;
                case Qword:
                    r = new_reloc(RELOC_ABS|RELOC_SIZ64, n->loc.offs, s);
                    break;
                }
                if (targeting_x64)
                    r->add = res;
            }
        }
        if (r != NULL) {
            r->next = n->loc.sec->relocs;
            n->loc.sec->relocs = r;
        }

        tmp = n;
        n = n->next;
        free(tmp);
    }
}

void dump_section(Section *s)
{
    Reloc *r;
    SBlock *b;

    printf("Section `%s'\n", s->name);
    printf("=> Contents\n");
    for (b = s->first; b != NULL; b = b->next) {
        int n;
        char *c;

        n = SBLOCK_SIZ(b);
        c = (char *)b+sizeof(SBlock);
        while (n--)
            printf("0x%02hhx ", *c++);
    }
    printf("\n=> Relocations\n");
    for (r = s->relocs; r != NULL; r = r->next)
        printf("attr=0x%x, off=%d, sym=%s\n", r->attr, r->offs, r->sym->name);
}

int read_line(void);
long long str2int(char *s);
void init(char *file_path);
Token curr_tok;
Token get_token(void);
void program(void);
void write_ELF32_file(void);
void write_ELF64_file(void);

void err_no_input(void)
{
    fprintf(stderr, "%s: no input file\n", prog_name);
    exit(1);
}

int main(int argc, char *argv[])
{
    int i;
    char *outpath;

    prog_name = argv[0];
    if (argc == 1)
        err_no_input();
    outpath = inpath = NULL;
    for (i = 1; i < argc; i++) {
        if (argv[i][0]!='-' || argv[i][1]=='\0') {
            inpath = argv[i];
            continue;
        }
        switch (argv[i][1]) {
        case 'o':
            if (argv[i][2] != '\0') {
                outpath = argv[i]+2;
            } else if (argv[i+1] == NULL) {
                fprintf(stderr, "%s: option `o' requires an argument\n", prog_name);
                exit(1);
            } else {
                outpath = argv[++i];
            }
            break;
        case 'm':
            if (equal(argv[i], "-m32"))
                ;
            else if (equal(argv[i], "-m64"))
                targeting_x64 = TRUE;
            else
                goto unk_opt;
            break;
        case 'h':
            printf("usage: %s [ options ] <input-file>\n"
                   "  The available options are:\n"
                   "    -o<file>    write output to <file>\n"
                   "    -m32        target x86-32 (default)\n"
                   "    -m64        target x86-64\n"
                   "    -h          print this help\n"
                   "\nnote: if the input file is - the program is read from the standard input\n", prog_name);
            exit(0);
            break;
        default:
        unk_opt:
            fprintf(stderr, "%s: unknown option `%s'\n", prog_name, argv[i]);
            exit(1);
        }
    }
    if (inpath == NULL)
        err_no_input();
    if (equal(inpath, "-")) {
        init(NULL);
        inpath = "stdin";
    } else {
        init(inpath);
    }

    opnd_arena = arena_new(sizeof(Operand)*256, FALSE);
    arena_set_nom_siz(opnd_arena, sizeof(Operand)*256);
    init_tables();

    lexeme = lexeme_buf1;
    curr_tok = get_token();
    program();
    resolve_expressions();
    // dump_section(curr_section);

    if (outpath == NULL) {
        outpath = replace_extension(inpath, ".o");
        output_file = fopen(outpath, "wb");
        free(outpath);
    } else {
        output_file = fopen(outpath, "wb");
    }
    if (targeting_x64)
        write_ELF64_file();
    else
        write_ELF32_file();
    fclose(output_file);

    arena_destroy(opnd_arena);
    if (buf != NULL)
        free(buf);

    return 0;
}

void line(void);
void directive(void);
void source_line(void);
void label(void);
Operand *operand(bool reldisp);
Operand *OR_expr(void);
Operand *XOR_expr(void);
Operand *AND_expr(void);
Operand *SHIFT_expr(void);
Operand *ADD_expr(void);
Operand *MUL_expr(void);
Operand *UNARY_expr(void);
Operand *PRIMARY_expr(void);
Operand *eff_addr(void);
void match(Token expected);
void encode_rm_opnd(char *mod_rm, char *rex, Operand *op, unsigned addr_mode);
void encode_sib(int scale, Token index, Token base, char *rex);
void encode_imm_opnd(Operand *e, unsigned size, unsigned op1_siz);
void emit_i(int opc, Operand *op1, Operand *op2);

int get_opcode_table_entry(char *s)
{
    int i;

    for (i = 0; i < NELEMS(mne2ote); i++)
        if (equal(mne2ote[i].mne, s))
            return mne2ote[i].ote;
    err1("unknown instruction `%s'", s);
}

/* encode immediate operand */
void encode_imm_opnd(Operand *e, unsigned size, unsigned op1_siz)
{
    switch (size) {
    case Byte:
        write_byte(0);
        new_unr_expr(e, GET_POS()-1, Byte, LC()-1, curr_section, FALSE, -1);
        break;
    case Word:
        write_word(0);
        new_unr_expr(e, GET_POS()-2, Word, LC()-2, curr_section, FALSE, -1);
        break;
    case Dword:
        write_dword(0);
        new_unr_expr(e, GET_POS()-4, Dword, LC()-4, curr_section, FALSE, op1_siz==Qword);
        break;
    case Qword:
        write_qword(0);
        new_unr_expr(e, GET_POS()-8, Qword, LC()-8, curr_section, FALSE, -1);
        break;
    }
}

/* encode the sib field of an operand */
void encode_sib(int scale, Token index, Token base, char *rex)
{
    if (index==TOK_ESP || index==TOK_RSP)
        goto invsib;
    switch (scale) {
    case 1:
        write_byte(REGENC(index)<<3|REGENC(base));
        break;
    case 2:
        write_byte(REGENC(index)<<3|REGENC(base)|0x40);
        break;
    case 4:
        write_byte(REGENC(index)<<3|REGENC(base)|0x80);
        break;
    case 8:
        write_byte(REGENC(index)<<3|REGENC(base)|0xC0);
        break;
    default:
        goto invsib;
    }
    if (REGREX(index) == 1)
        *rex |= 0x02;
    if (REGREX(base) == 1)
        *rex |= 0x01;
    return;
invsib:
    err1("invalid effective address");
}

/* encode r/m operand */
void encode_rm_opnd(char *mod_rm, char *rex, Operand *op, unsigned addr_mode)
{
    int renc;
    int scale;
    Token reg, base, index;

    switch (addr_mode) {
    /*
         reg
     */
    case Reg_mode: /* reg */
        reg = op->op;
        *mod_rm |= REGENC(reg)|0xC0;
        if (REGREX(reg) == 1)
            *rex |= 0x01;
        break;

    /*
         deref
           |
         disp
     */
    case Dir_mode: /* [ disp32 ] */
        if (targeting_x64) {
            *mod_rm |= 0x04;  /* mod=00, r/m=100 */
            write_byte(0x25); /* sib */
        } else {
            *mod_rm |= 0x05;  /* mod=00, r/m=101 */
        }
        write_dword(0);
        new_unr_expr(LCHILD(op), GET_POS()-4, Dword, LC()-4, curr_section, FALSE, TRUE);
        break;

    /*
          deref
            |
           reg
     */
    case RegInd_mode: /* [ reg ] */
        reg = LCHILD(op)->op;
        renc = REGENC(reg);
        if (renc == 4) { /* ESP, RSP, or R12 */
            *mod_rm |= 0x04;  /* mod=00, r/m=100 */
            write_byte(0x24); /* sib: scale=00, index=100, base=100 */
        } else if (renc == 5) { /* EBP, RBP, or R13 */
            write_byte(0);   /* disp8=0 */
            *mod_rm |= 0x45; /* mod=01 */
        } else {
            *mod_rm |= renc;
        }
        if (REGREX(reg) == 1)
            *rex |= 0x01;
        break;

    /*
        deref
          |
          +
         / \
      reg   disp
     */
    case Based_mode: /* [ reg+disp8/32 ] */
        reg = LCHILD(LCHILD(op))->op;
        renc = REGENC(reg);
        if (renc == 4) {
            *mod_rm |= 0x04;
            write_byte(0x24); /* sib */
        } else {
            *mod_rm |= renc;
        }
        if (REGREX(reg) == 1)
            *rex |= 0x01;
        if (RCHILD(LCHILD(op))->addr_mode & Byte) {
            *mod_rm |= 0x40; /* disp8 */
            write_byte(0);
            new_unr_expr(RCHILD(LCHILD(op)), GET_POS()-1, Byte, LC()-1, curr_section, FALSE, FALSE);
        } else {
            *mod_rm |= 0x80; /* disp32 */
            write_dword(0);
            new_unr_expr(RCHILD(LCHILD(op)), GET_POS()-4, Dword, LC()-4, curr_section, FALSE, TRUE);
        }
        break;

    /*
        deref           deref
          |               |
          *               +
         / \             / \
      reg   scale       *   disp
                       / \
                    reg   scale
     */
    case Index_mode: /* [ reg*scale+disp32 ] */
        *mod_rm |= 0x04;
        if (LCHILD(op)->op == TOK_MUL) {
            index = LCHILD(LCHILD(op))->op;
            scale = EXPRVAL(RCHILD(LCHILD(op)));
            encode_sib(scale, index, TOK_EBP, rex);
            write_dword(0); /* disp32 */
        } else {
            index = LCHILD(LCHILD(LCHILD(op)))->op;
            scale = EXPRVAL(RCHILD(LCHILD(LCHILD(op))));
            encode_sib(scale, index, TOK_EBP, rex);
            write_dword(0); /* disp32 */
            new_unr_expr(RCHILD(LCHILD(op)), GET_POS()-4, Dword, LC()-4, curr_section, FALSE, TRUE);
        }
        break;

    /*
        deref           deref
          |               |
          +               +
         / \             / \
      reg   *         reg   reg
           / \
        reg   scale
     */
    case BasedIndex_mode: /* [ reg+reg*scale ] */
        base = LCHILD(LCHILD(op))->op;
        if (RCHILD(LCHILD(op))->op == TOK_MUL) {
            index = LCHILD(RCHILD(LCHILD(op)))->op;
            scale = EXPRVAL(RCHILD(RCHILD(LCHILD(op))));
        } else {
            index = RCHILD(LCHILD(op))->op;
            scale = 1;
        }
        encode_sib(scale, index, base, rex);
        if (base==TOK_EBP || base==TOK_RBP || base==TOK_R13) {
            *mod_rm |= 0x44;
            write_byte(0); /* disp8 */
        } else {
            *mod_rm |= 0x04;
        }
        break;

    /*
        deref           deref
          |               |
          +               +
         / \             / \
        +   disp        +   disp
       / \             / \
    reg   *         reg   reg
         / \
      reg   scale
     */
    case BasedIndexDisp_mode: /* [ reg+reg*scale+disp8/32 ] */
        base = LCHILD(LCHILD(LCHILD(op)))->op;
        if (RCHILD(LCHILD(LCHILD(op)))->op == TOK_MUL) {
            index = LCHILD(RCHILD(LCHILD(LCHILD(op))))->op;
            scale = EXPRVAL(RCHILD(RCHILD(LCHILD(LCHILD(op)))));
        } else {
            index = RCHILD(LCHILD(LCHILD(op)))->op;
            scale = 1;
        }
        encode_sib(scale, index, base, rex);
        if (RCHILD(LCHILD(op))->addr_mode & Byte) {
            *mod_rm |= 0x44;
            write_byte(0); /* disp8 */
            new_unr_expr(RCHILD(LCHILD(op)), GET_POS()-1, Byte, LC()-1, curr_section, FALSE, FALSE);
        } else {
            *mod_rm |= 0x84;
            write_dword(0); /* disp32 */
            new_unr_expr(RCHILD(LCHILD(op)), GET_POS()-4, Dword, LC()-4, curr_section, FALSE, TRUE);
        }
        break;
    }
}

void rex_err(void)
{
    err1("cannot use high register in rex instruction");
}

void emit_i(int ote, Operand *op1, Operand *op2)
{
    Token reg;
    InstrClass iclass;
    unsigned _op1, _op2;
    unsigned op1_am, op2_am;
    unsigned op1_siz, op2_siz;
    char *opcode, *rex = NULL;
    unsigned char mod_rm = 0;

    --line_number; /* the lexer has already read the '\n' that follows the instruction */

    if (op1!=NULL && op2!=NULL) {
        bool size_mismatch = FALSE;

        op1_am = op1->addr_mode;
        op2_am = op2->addr_mode;
        op1_siz = op1_am&SIZE_MASK;
        op2_siz = op2_am&SIZE_MASK;
        op1_am &= ~op1_siz;
        op2_am &= ~op2_siz;

        if (op1->op == TOK_DEREF) {
            if (op2->op == TOK_DEREF) { /* mem, mem */
                err1("invalid combination of opcode and operands");
            } else if (ISREG(op2->op)) { /* mem, reg */
                if (op1_siz) {
                    if (op1_siz != op2_siz)
                        err1("mismatch in operand sizes");
                } else {
                    op1_siz = op2_siz;
                }
            } else { /* mem, imm */
                if (op1_siz)
                    op2_siz |= op1_siz;
                else if (op2_am & HasSizSpe_mode)
                    op1_siz = op2_siz;
                else
                    err1("operation size not specified");
            }
        } else if (ISREG(op1->op)) {
            if (op2->op == TOK_DEREF) { /* reg, mem */
                if (op2_siz) {
                    if (op1_siz != op2_siz)
                        size_mismatch = TRUE;
                } else {
                    op2_siz = op1_siz;
                }
            } else if (ISREG(op2->op)) { /* reg, reg */
                if (op1_siz != op2_siz)
                    size_mismatch = TRUE;
            } else { /* reg, imm */
                /*
                 * When the user writes 'mov rax, dword 1',
                 * the form of mov instruction selected
                 * should be
                 *      mov r64, imm32
                 * instead of
                 *      mov r64, imm64
                 */
                if (!(op2_am&HasSizSpe_mode))
                    op2_siz |= op1_siz;
            }
        } else { /* imm, ... */
            err1("invalid combination of opcode and operands");
        }

        for (iclass = opcode_table[ote].iclass; iclass == opcode_table[ote].iclass; ote++) {
            _op1 = opcode_table[ote].op1;
            _op2 = opcode_table[ote].op2;
            if (op1_am&_op1 && op2_am&_op2 && op1_siz&_op1 && op2_siz&_op2)
                break;
        }
        if (iclass != opcode_table[ote].iclass)
            err1("invalid combination of opcode and operands");
        if (size_mismatch
        && opcode_table[ote].asm_instr != I_MX
        && opcode_table[ote].iclass != op_movsx
        && opcode_table[ote].iclass != op_movzx
        && opcode_table[ote].iclass != op_lea)
            err1("mismatch in operand sizes");

        if (op1_siz == Word) {
            write_byte(0x66);
        } else if (op1_siz == Qword) {
            write_byte(0x48);
            rex = GET_POS()-1;
        }
        if (rex==NULL && (op1_am&NeedsREX_mode || op2_am&NeedsREX_mode)) {
            write_byte(0x40);
            rex = GET_POS()-1;
        }
        if (!targeting_x64 && rex!=NULL)
            err1("instruction not supported in 32-bit mode");
        if (opcode_table[ote].esc_opc)
            write_byte(0x0F);
        write_byte(opcode_table[ote].opcode);
        opcode = GET_POS()-1;
        if (opcode_table[ote].opc_ext != (unsigned char)-1)
            mod_rm |= opcode_table[ote].opc_ext<<3;

        switch(opcode_table[ote].asm_instr) {
        case I_RM:  /* reg <- r/m ; ModRM:reg=operand1, ModRM:r/m=operand2 */
            reg = op1->op;
            mod_rm |= REGENC(reg)<<3;
            write_byte(mod_rm);
            switch (REGREX(reg)) {
            case 1:
                if (ISREG(op2->op) && REGREX(op2->op)==-1)
                    rex_err();
                *rex |= 0x04;
                break;
            case 2:
                if (ISREG(op2->op) && REGREX(op2->op)==-1)
                    rex_err();
                break;
            case -1:
                if (op2_am & NeedsREX_mode)
                    rex_err();
                break;
            }
            encode_rm_opnd(GET_POS()-1, rex, op2, op2_am&_op2);
            break;

        case I_MR:  /* r/m <- reg ; ModRM:reg=operand2, ModRM:r/m=operand1 */
            reg = op2->op;
            mod_rm |= REGENC(reg)<<3;
            write_byte(mod_rm);
            switch (REGREX(reg)) {
            case 1:
                if (ISREG(op1->op) && REGREX(op1->op)==-1)
                    rex_err();
                *rex |= 0x04;
                break;
            case 2:
                if (ISREG(op1->op) && REGREX(op1->op)==-1)
                    rex_err();
                break;
            case -1:
                if (op1_am & NeedsREX_mode)
                    rex_err();
                break;
            }
            encode_rm_opnd(GET_POS()-1, rex, op1, op1_am&_op1);
            break;

        case I_MX:  /* ModRM:r/m=operand1, second operand 1 or CL */
            write_byte(mod_rm);
            encode_rm_opnd(GET_POS()-1, rex, op1, op1_am&_op1);
            break;

        case I_MI: /* r/m <- imm ; ModRM:r/m=operand1 */
            write_byte(mod_rm);
            encode_rm_opnd(GET_POS()-1, rex, op1, op1_am&_op1);
            encode_imm_opnd(op2, op2_siz&_op2, op1_siz);
            break;

        case I_AI:  /* acc <- imm ; Acc=operand1 (No ModRM byte) */
            encode_imm_opnd(op2, op2_siz&_op2, op1_siz);
            break;

        case I_RMI:  /* reg <- imm ; ModRM:reg=operand1, ModRM:r/m=operand1 */
            reg = op1->op;
            mod_rm |= REGENC(reg)<<3|REGENC(reg)|0xC0;
            write_byte(mod_rm);
            if (REGREX(reg) == 1)
                *rex |= 0x05;
            encode_imm_opnd(op2, op2_siz&_op2, op1_siz);
            break;

        case I_AR:  /* acc <- reg ; Acc=operand1, Lower3Bits(opcode)=operand2 (No ModRM byte) */
            reg = op2->op;
            *opcode |= REGENC(reg);
            if (REGREX(reg) == 1)
                *rex |= 0x01;
            break;

        case I_RA:  /* reg <- acc ; Acc=operand2, Lower3Bits(opcode)=operand1 (No ModRM byte) */
            reg = op1->op;
            *opcode |= REGENC(reg);
            if (REGREX(reg) == 1)
                *rex |= 0x01;
            break;

        case I_RI:  /* reg <- imm ; Lower3Bits(opcode)=operand1 (No ModRM byte) */
            reg = op1->op;
            *opcode |= REGENC(reg);
            if (REGREX(reg) == 1)
                *rex |= 0x01;
            encode_imm_opnd(op2, op2_siz&_op2, op1_siz);
            break;
        }
    } else if (op1 != NULL) {
        op1_am = op1->addr_mode;
        op1_siz = op1_am&SIZE_MASK;
        op1_am &= ~op1_siz;
        /* assign a default size if none has been given (memory operand) */
        if (!op1_siz)
            op1_siz = targeting_x64?Qword:Dword;

        for (iclass = opcode_table[ote].iclass; iclass == opcode_table[ote].iclass; ote++) {
            _op1 = opcode_table[ote].op1;
            _op2 = opcode_table[ote].op2;
            if (_op2&None_mode && op1_am&_op1 && op1_siz&_op1)
                break;
        }
        if (iclass != opcode_table[ote].iclass)
            err1("invalid combination of opcode and operands");

        if (op1_siz == Word) {
            write_byte(0x66);
        } else if (op1_siz == Qword) {
            write_byte(0x48);
            rex = GET_POS()-1;
        }
        if (rex==NULL && op1_am&NeedsREX_mode) {
            write_byte(0x40);
            rex = GET_POS()-1;
        }
        if (!targeting_x64 && rex!=NULL)
            err1("instruction not supported in 32-bit mode");
        if (opcode_table[ote].esc_opc)
            write_byte(0x0F);
        write_byte(opcode_table[ote].opcode);
        opcode = GET_POS()-1;
        if (opcode_table[ote].opc_ext != (unsigned char)-1)
            mod_rm |= opcode_table[ote].opc_ext<<3;

        switch(opcode_table[ote].asm_instr) {
        case I_M:   /* r/m ; ModRM:r/m=operand1 (generally ModRM:reg is an opcode extension) */
            write_byte(mod_rm);
            encode_rm_opnd(GET_POS()-1, rex, op1, op1_am&_op1);
            break;

        case I_R:   /* reg ; Lower3Bits(opcode)=operand1 (No ModRM byte) */
            reg = op1->op;
            *opcode |= REGENC(reg);
            if (REGREX(reg) == 1)
                *rex |= 0x01;
            break;

        case I_I:   /* imm */
            encode_imm_opnd(op1, op1_siz&_op1, Qword); /* Qword because when doing 'push imm32' */
            break;                                     /* imm32 is sign extended to imm64 */

        case I_REL8: /* 8-bit displacement relative to next instruction */
            write_byte(0);
            new_unr_expr(op1, GET_POS()-1, Byte, LC()-1, curr_section, TRUE, FALSE);
            break;

        case I_REL32: /* 32-bit displacement relative to next instruction */
            write_dword(0);
            new_unr_expr(op1, GET_POS()-4, Dword, LC()-4, curr_section, TRUE, FALSE);
            break;
        }
    } else {
        for (iclass = opcode_table[ote].iclass; iclass == opcode_table[ote].iclass; ote++)
            if (opcode_table[ote].op1&None_mode && opcode_table[ote].op2&None_mode)
                break;
        if (iclass != opcode_table[ote].iclass)
            err1("invalid combination of opcode and operands");
        if (iclass == op_movsw)
            write_byte(0x66);
        else if (iclass==op_movsq || iclass==op_cqo)
            write_byte(0x48);
        else if (opcode_table[ote].esc_opc)
            write_byte(0x0F);
        write_byte(opcode_table[ote].opcode);
    }

    ++line_number;
}

/* program = line { line } EOF */
void program(void)
{
    line();
    while (curr_tok != TOK_EOF)
        line();
    // match(TOK_EOF);
    if (curr_tok != TOK_EOF)
        err1("End-of-File expected");
}

/* line = [ label ] [ ( source_line | directive ) ] EOL */
void line(void)
{
    if (curr_tok == TOK_ID) {
        char *curr_tmp;
        int is_label, line_tmp;

        curr_tmp=curr, lexeme=lexeme_buf2, line_tmp=line_number;
        match(TOK_ID);
        is_label = curr_tok==TOK_COLON;
        curr_tok=TOK_ID, curr=curr_tmp, lexeme=lexeme_buf1, line_number=line_tmp;
        if (is_label) {
            label();
        } else {
            source_line();
            goto done;
        }
    }
    if (ISDIR(curr_tok)) {
        directive();
    } else {
        source_line();
    }
done:
    match(TOK_EOL);
}

/*
 *     directive = ( "segment" | "section" ) id |
 *                  "extern" id { "," id } |
 *                  "global" id [ ":" id ] { "," id [ ":" id ] } |
 *                  "align"  NUM |
 *                  "alignb" NUM
 */
void directive(void)
{
    switch (curr_tok) {
    case TOK_SECTION:
        match(TOK_SECTION);
        if (curr_tok == TOK_ID)
            set_curr_section(lexeme);
        match(TOK_ID);
        break;
    case TOK_EXTERN:
        match(TOK_EXTERN);
        define_symbol(OtherKind, ExternBind, lexeme, 0, NULL);
        match(TOK_ID);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            define_symbol(OtherKind, ExternBind, lexeme, 0, NULL);
            match(TOK_ID);
        }
        break;
    case TOK_GLOBAL: {
        Symbol *sym;

        match(TOK_GLOBAL);
        goto first;
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
        first:
            if (curr_tok == TOK_ID)
                sym = define_symbol(OtherKind, GlobalBind, lexeme, 0, NULL);
            match(TOK_ID);
            if (curr_tok == TOK_COLON) {
                match(TOK_COLON);
                if (curr_tok == TOK_ID) {
                    if (equal(lexeme, "function"))
                        sym->is_func = TRUE;
                    else
                        err1("unknown extension to global directive: `%s'", lexeme);
                }
                match(TOK_ID);
            }
        }
    }
        break;
    case TOK_ALIGN:
        match(TOK_ALIGN);
        if (curr_tok == TOK_NUM) {
            int nb, arg;

            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            arg = str2int(lexeme);
            if (!is_po2(arg))
                err1("section alignment `%d' is not power of two", arg);
            for (nb = round_up(LC(), arg)-LC(); nb; nb--)
                write_byte(0x90);
        }
        match(TOK_NUM);
        break;
    case TOK_ALIGNB:
        match(TOK_ALIGNB);
        if (curr_tok == TOK_NUM) {
            int nb, arg;

            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            arg = str2int(lexeme);
            if (!is_po2(arg))
                err1("section alignment `%d' is not power of two", arg);
            nb = round_up(LC(), arg)-LC();
            curr_section->last->avail += nb;
            LC() += nb;
        }
        match(TOK_NUM);
        break;
    }
}

/* source_line = [ prefix ] ( [ instruction [ operand [ "," operand ] ] ] | pseudo_instruction )
   prefix = "rep"   |
            "repe"  |
            "repz"  |
            "repne" |
            "repnz" |
            "times" crit_expr
   pseudo_instruction = "resb" crit_expr |
                        "resw" crit_expr |
                        "resd" crit_expr |
                        "resq" crit_expr |
                        "db" OR_expr { "," OR_expr } |
                        "dw" OR_expr { "," OR_expr } |
                        "dd" OR_expr { "," OR_expr } |
                        "dq" OR_expr { "," OR_expr }
 */
void source_line(void)
{
    int ntimes = 1;
    int line_tmp;
    Token tok_tmp;
    char *curr_tmp;

    if (ISPRE(curr_tok)) {
        switch (curr_tok) {
        case TOK_REP:
        case TOK_REPE:
        case TOK_REPZ:
            match(curr_tok);
            write_byte(0xF3);
            break;
        case TOK_REPNE:
        case TOK_REPNZ:
            match(curr_tok);
            write_byte(0xF2);
            break;
        case TOK_TIMES:
            match(curr_tok);
            if (!setjmp(env)) {
                Operand *arg;

                arg = OR_expr();
                ntimes = eval_expr(arg, FALSE);
                if (EXPRKIND(arg)!=ABS_EXPR || ntimes<=0) /* note: nasm allows zero as argument */
                    longjmp(env, 1);
                if (ntimes > 1) {
                    curr_tmp = curr;
                    tok_tmp = curr_tok;
                    line_tmp = line_number;
                    strcpy(lexeme_buf2, lexeme_buf1);
                }
            } else {
                err1("invalid argument to times prefix");
            }
            break;
        }
    }
start:
    if (curr_tok == TOK_ID) {
        int ote;
        Operand *op1, *op2;

        ote = get_opcode_table_entry(lexeme);
        op1 = op2 = NULL;

        match(TOK_ID);
        if (curr_tok != TOK_EOL) {
            switch (opcode_table[ote].asm_instr) {
            case I_REL8:
                op1 = operand(8);
                break;
            case I_REL32:
                op1 = operand(32);
                break;
            default:
                op1 = operand(0);
                break;
            }
            if (curr_tok == TOK_COMMA) {
                match(TOK_COMMA);
                op2 = operand(0);
            }
        }
        emit_i(ote, op1, op2);
    } else if (ISPSE(curr_tok)) {
        switch (curr_tok) {
        case TOK_RESB:
        case TOK_RESW:
        case TOK_RESD:
        case TOK_RESQ:
            if (!setjmp(env)) {
                int val, siz;
                Operand *arg;

                switch (curr_tok) {
                case TOK_RESB: siz = 1; break;
                case TOK_RESW: siz = 2; break;
                case TOK_RESD: siz = 4; break;
                case TOK_RESQ: siz = 8; break;
                }
                match(curr_tok);
                arg = OR_expr();
                val = eval_expr(arg, FALSE);
                if (EXPRKIND(arg) != ABS_EXPR)
                    longjmp(env, 1);
                if (curr_section == NULL)
                    set_curr_section(DEF_SEC);
                /*
                 * TOFIX:
                 *  There may be problems if this pseudo-instructions
                 *  is used in a non-bss section (writting/reading off
                 *  limits).
                 */
                curr_section->last->avail += val*siz;
                LC() += val*siz;
            } else {
                err1("invalid argument to resX directive");
            }
            break;
        case TOK_DB:
            match(TOK_DB);
            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            write_byte(0);
            new_unr_expr(OR_expr(), GET_POS()-1, Byte, LC()-1, curr_section, FALSE, FALSE);
            while (curr_tok == TOK_COMMA) {
                match(TOK_COMMA);
                write_byte(0);
                new_unr_expr(OR_expr(), GET_POS()-1, Byte, LC()-1, curr_section, FALSE, FALSE);
            }
            break;
        case TOK_DW:
            match(TOK_DW);
            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            write_word(0);
            new_unr_expr(OR_expr(), GET_POS()-2, Word, LC()-2, curr_section, FALSE, FALSE);
            while (curr_tok == TOK_COMMA) {
                match(TOK_COMMA);
                write_word(0);
                new_unr_expr(OR_expr(), GET_POS()-2, Word, LC()-2, curr_section, FALSE, FALSE);
            }
            break;
        case TOK_DD:
            match(TOK_DD);
            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            write_dword(0);
            new_unr_expr(OR_expr(), GET_POS()-4, Dword, LC()-4, curr_section, FALSE, FALSE);
            while (curr_tok == TOK_COMMA) {
                match(TOK_COMMA);
                write_dword(0);
                new_unr_expr(OR_expr(), GET_POS()-4, Dword, LC()-4, curr_section, FALSE, FALSE);
            }
            break;
        case TOK_DQ:
            match(TOK_DQ);
            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            write_qword(0);
            new_unr_expr(OR_expr(), GET_POS()-8, Qword, LC()-8, curr_section, FALSE, FALSE);
            while (curr_tok == TOK_COMMA) {
                match(TOK_COMMA);
                write_qword(0);
                new_unr_expr(OR_expr(), GET_POS()-8, Qword, LC()-8, curr_section, FALSE, FALSE);
            }
            break;
        }
    }
    if (--ntimes) {
        curr = curr_tmp;
        curr_tok = tok_tmp;
        line_number = line_tmp;
        strcpy(lexeme_buf1, lexeme_buf2);
        goto start;
    }
}

struct UnrLab {
    char **dest;
    UnrLab *next;
} *unresolved_labels_list;

/* label = ID ":" */
void label(void)
{
    char *name;

    if (lexeme[0] == '.') {
        if (non_local_label == NULL)
            err1("attempt to define a local label before any non-local labels");
        name = malloc(strlen(non_local_label)+strlen(lexeme)+1);
        strcpy(name, non_local_label);
        strcat(name, lexeme);
    } else {
        if (non_local_label == NULL) {
            UnrLab *np, *tmp;

            np = unresolved_labels_list;
            while (np != NULL) {
                name = malloc(strlen(lexeme)+strlen(*np->dest)+1);
                strcpy(name, lexeme);
                strcat(name, *np->dest);
                free(*np->dest);
                *np->dest = name;
                tmp = np;
                np = np->next;
                free(tmp);
            }
        } else {
            free(non_local_label);
        }
        non_local_label = strdup(lexeme);
        name = lexeme;
    }
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    define_symbol(OtherKind, LocalBind, name, LC(), curr_section);
    match(TOK_ID);
    match(TOK_COLON);
}

/* reldisp can be 0 (the expression is not a relative displacement), 8, or 32 */
Operand *asm_expr(int reldisp)
{
    Operand *e;

    e = OR_expr();

    if (!setjmp(env)) {
        long long val;

        val = eval_expr(e, FALSE);
        if (!reldisp) {
            if (EXPRKIND(e) == ABS_EXPR) {
                if (val>=-128 && val<=127) {
                    e->addr_mode = Imm_mode|Byte|Word|Dword|Qword;
                    if (val == 1)
                        e->addr_mode |= Imm_1_mode;
                } else if (val>=-32768 && val<=32767) {
                    e->addr_mode = Imm_mode|Word|Dword|Qword;
                } else if (val>=-2147483648LL && val<=2147483647) {
                    e->addr_mode = Imm_mode|Dword|Qword;
                } else {
                    e->addr_mode = Imm_mode|Qword;
                }
            } else {
                e->addr_mode = Imm_mode|Dword|Qword;
            }
        } else if (reldisp == 8) {
            if (EXPRKIND(e) == REL_EXPR) {
                Symbol *s;

                s = get_rel_sym(e);
                if (s->bind==ExternBind || !equal(s->sec->name, curr_section->name)) {
                    e->addr_mode = Imm_mode|Dword; /* disp32 */
                    return e;
                }
            }

            /*
             * Currently, the only supported instructions
             * that use the rel8 mode are Jcc and Jmp.
             * They are 2 bytes long (opcode byte + disp byte).
             */
            val -= LC()+2;
            if (val>=-128 && val<=127)
                e->addr_mode = Imm_mode|Byte;  /* disp8 */
            else
                e->addr_mode = Imm_mode|Dword; /* disp32 */
        } else {
            e->addr_mode = Imm_mode|Dword; /* disp32 */
        }
    } else {
        /*
         * The expression references a not-yet-defined label.
         * Assume worst case, that is, the biggest size.
         */
        e->addr_mode = Imm_mode|Dword|Qword;
    }
    return e;
}

int addr_mode_needs_REX(Operand *op)
{
    Token reg, base, index;

    switch (op->addr_mode) {
    case Reg_mode: /* reg */
        reg = op->op;
        if (REGREX(reg) >= 1)
            return TRUE;
        break;

    case RegInd_mode: /* [ reg ] */
        reg = LCHILD(op)->op;
        if (REGREX(reg) >= 1)
            return TRUE;
        break;

    case Based_mode: /* [ reg+disp8/32 ] */
        reg = LCHILD(LCHILD(op))->op;
        if (REGREX(reg) >= 1)
            return TRUE;
        break;

    case Index_mode: /* [ reg*scale+disp32 ] */
        index = (LCHILD(op)->op == TOK_MUL)
            ? LCHILD(LCHILD(op))->op
            : LCHILD(LCHILD(LCHILD(op)))->op;
        if (REGREX(index) >= 1)
            return TRUE;
        break;

    case BasedIndex_mode: /* [ reg+reg*scale ] */
        base = LCHILD(LCHILD(op))->op;
        index = (RCHILD(LCHILD(op))->op == TOK_MUL)
        ? LCHILD(RCHILD(LCHILD(op)))->op
        : RCHILD(LCHILD(op))->op;
        if (REGREX(base)>=1 || REGREX(index)>=1)
            return TRUE;
        break;

    case BasedIndexDisp_mode: /* [ reg+reg*scale+disp8/32 ] */
        base = LCHILD(LCHILD(LCHILD(op)))->op;
        index = (RCHILD(LCHILD(LCHILD(op)))->op == TOK_MUL)
        ? LCHILD(RCHILD(LCHILD(LCHILD(op))))->op
        : RCHILD(LCHILD(LCHILD(op)))->op;
        if (REGREX(base)>=1 || REGREX(index)>=1)
            return TRUE;
        break;
    }
    return FALSE;
}

/*
 * operand = [ size_specifier ] ( REG | "[" eff_addr "]" | OR_expr )
 * size_specifier = "byte" | "word" | "dword" | "qword"
 */
Operand *operand(bool reldisp)
{
    Operand *op;
    unsigned siz;

    if (ISSIZ(curr_tok)) {
        switch (curr_tok) {
        case TOK_BYTE:  siz = Byte;  break;
        case TOK_WORD:  siz = Word;  break;
        case TOK_DWORD: siz = Dword; break;
        default:        siz = Qword; break;
        }
        match(curr_tok);
    } else {
        siz = 0;
    }
    if (ISREG(curr_tok)) {
        op = new_opnd(curr_tok);
        op->addr_mode = Reg_mode|REGSIZ(curr_tok);
        if (curr_tok==TOK_AL || curr_tok==TOK_AX || curr_tok==TOK_EAX || curr_tok==TOK_RAX)
            op->addr_mode |= Acc_mode;
        else if (curr_tok == TOK_CL)
            op->addr_mode |= Reg_CL_mode;
        if (REGREX(curr_tok) >= 1)
            op->addr_mode |= NeedsREX_mode;
        match(curr_tok);
    } else if (curr_tok == TOK_LBRACKET) {
        Operand *ea;

        match(TOK_LBRACKET);
        ea = eff_addr();
        match(TOK_RBRACKET);
        op = new_opnd(TOK_DEREF);
        LCHILD(op) = ea;
        op->addr_mode = ea->addr_mode;
        if (addr_mode_needs_REX(op))
            op->addr_mode |= NeedsREX_mode;
    } else {
        op = asm_expr(reldisp);
    }
    /* a size specifier applied to a register operand is ignored */
    if (siz && !(op->addr_mode&Reg_mode)) {
        if (op->addr_mode & Imm_mode) {
            op->addr_mode &= ~SIZE_MASK;
            op->addr_mode |= siz+HasSizSpe_mode;
        } else {
            op->addr_mode |= siz;
        }
    }
    return op;
}

/* eff_addr = REG "+" REG [ "*" NUM ] "+" OR_expr */
Operand *eff_addr(void)
{
    /*
     * All but one term can be missing.
     * The order must remain.
     */
    Operand *e, *tmp;

    if (ISREG(curr_tok)) {
        e = new_opnd(curr_tok);
        match(curr_tok);
        if (curr_tok == TOK_PLUS) {
            tmp = e;
            e = new_opnd(TOK_PLUS);
            match(TOK_PLUS);
            LCHILD(e) = tmp;
            if (ISREG(curr_tok)) {
                tmp = new_opnd(curr_tok);
                match(curr_tok);
                RCHILD(e) = tmp;
                if (curr_tok == TOK_MUL) {
                    Operand *tmp2;

                    tmp2 = new_opnd(TOK_MUL);
                    match(TOK_MUL);
                    LCHILD(tmp2) = tmp;
                    tmp = new_opnd(TOK_NUM);
                    EXPRVAL(tmp) = str2int(lexeme);
                    match(TOK_NUM);
                    RCHILD(tmp2) = tmp;
                    RCHILD(e) = tmp2;
                    if (curr_tok == TOK_PLUS) { /* REG "+" REG "*" NUM "+" OR_expr */
                        tmp = e;
                        e = new_opnd(TOK_PLUS);
                        match(TOK_PLUS);
                        LCHILD(e) = tmp;
                        tmp = asm_expr(0);
                        RCHILD(e) = tmp;
                        e->addr_mode = BasedIndexDisp_mode;
                    } else { /* REG "+" REG "*" NUM */
                        e->addr_mode = BasedIndex_mode;
                    }
                } else if (curr_tok == TOK_PLUS) { /* REG "+" REG "+" OR_expr */
                    tmp = e;
                    e = new_opnd(TOK_PLUS);
                    match(TOK_PLUS);
                    LCHILD(e) = tmp;
                    tmp = asm_expr(0);
                    RCHILD(e) = tmp;
                    e->addr_mode = BasedIndexDisp_mode;
                } else { /* REG "+" REG */
                    e->addr_mode = BasedIndex_mode;
                }
            } else { /* REG "+" OR_expr */
                tmp = asm_expr(0);
                RCHILD(e) = tmp;
                e->addr_mode = Based_mode;
            }
        } else if (curr_tok == TOK_MUL) { /* REG "*" NUM */
            tmp = e;
            e = new_opnd(TOK_MUL);
            match(TOK_MUL);
            LCHILD(e) = tmp;
            tmp = new_opnd(TOK_NUM);
            EXPRVAL(tmp) = str2int(lexeme);
            match(TOK_NUM);
            RCHILD(e) = tmp;
            if (curr_tok == TOK_PLUS) { /* REG "*" NUM "+" OR_expr */
                tmp = e;
                e = new_opnd(TOK_PLUS);
                match(TOK_PLUS);
                LCHILD(e) = tmp;
                tmp = asm_expr(0);
                RCHILD(e) = tmp;
            }
            e->addr_mode = Index_mode;
        } else { /* REG */
            e->addr_mode = RegInd_mode;
        }
    } else { /* OR_expr */
        e = asm_expr(0);
        e->addr_mode = Dir_mode;
    }
    return e;
}

/* OR_expr = XOR_expr { "|" XOR_expr } */
Operand *OR_expr(void)
{
    Operand *e, *tmp;

    e = XOR_expr();
    while (curr_tok == TOK_OR) {
        tmp = new_opnd(TOK_OR);
        match(TOK_OR);
        LCHILD(tmp) = e;
        RCHILD(tmp) = XOR_expr();;
        e = tmp;
    }
    return e;
}

/* XOR_expr = AND_expr { "^" AND_expr } */
Operand *XOR_expr(void)
{
    Operand *e, *tmp;

    e = AND_expr();
    while (curr_tok == TOK_XOR) {
        tmp = new_opnd(TOK_XOR);
        match(TOK_XOR);
        LCHILD(tmp) = e;
        RCHILD(tmp) = AND_expr();;
        e = tmp;
    }
    return e;
}

/* AND_expr = SHIFT_expr { "&" SHIFT_expr } */
Operand *AND_expr(void)
{
    Operand *e, *tmp;

    e = SHIFT_expr();
    while (curr_tok == TOK_AND) {
        tmp = new_opnd(TOK_AND);
        match(TOK_AND);
        LCHILD(tmp) = e;
        RCHILD(tmp) = SHIFT_expr();
        e = tmp;
    }
    return e;
}

/* SHIFT_expr = ADD_expr { ( "<<" | ">>" ) ADD_expr } */
Operand *SHIFT_expr(void)
{
    Operand *e, *tmp;

    e = ADD_expr();
    while (curr_tok==TOK_LSHIFT || curr_tok==TOK_RSHIFT) {
        tmp = new_opnd(curr_tok);
        match(curr_tok);
        LCHILD(tmp) = e;
        RCHILD(tmp) = ADD_expr();
        e = tmp;
    }
    return e;
}

/* ADD_expr = MUL_expr { ( "+" | "-" ) MUL_expr } */
Operand *ADD_expr(void)
{
    Operand *e, *tmp;

    e = MUL_expr();
    while (curr_tok==TOK_PLUS || curr_tok==TOK_MINUS) {
        tmp = new_opnd(curr_tok);
        match(curr_tok);
        LCHILD(tmp) = e;
        RCHILD(tmp) = MUL_expr();
        e = tmp;
    }
    return e;
}

/* MUL_expr = UNARY_expr { ( "*" | "/" | "//" | "%" | "%%" ) UNARY_expr } */
Operand *MUL_expr(void)
{
    Operand *e, *tmp;

    e = UNARY_expr();
    while (curr_tok>=TOK_MUL && curr_tok<=TOK_UMOD) {
        tmp = new_opnd(curr_tok);
        match(curr_tok);
        LCHILD(tmp) = e;
        RCHILD(tmp) = UNARY_expr();
        e = tmp;
    }
    return e;
}

/* UNARY_expr = ( "+" | "-" | "~" | "!" ) UNARY_expr | PRIMARY_expr */
Operand *UNARY_expr(void)
{
    Operand *e;

    switch (curr_tok) {
    case TOK_PLUS:
        match(TOK_PLUS);
        e = UNARY_expr();
        break;
    case TOK_MINUS:
        e = new_opnd(TOK_UNARY_MINUS);
        match(TOK_MINUS);
        LCHILD(e) = UNARY_expr();
        break;
    case TOK_CMPL:
    case TOK_LNEG:
        e = new_opnd(curr_tok);
        match(curr_tok);
        LCHILD(e) = UNARY_expr();
        break;
    default:
        e = PRIMARY_expr();
        break;
    }
    return e;
}

/* PRIMARY_expr = "(" OR_expr ")" | ID | NUM | "$" */
Operand *PRIMARY_expr(void)
{
    Operand *e;

    switch (curr_tok) {
    case TOK_LPAREN:
        match(TOK_LPAREN);
        e = OR_expr();
        match(TOK_RPAREN);
        break;
    case TOK_ID:
        e = new_opnd(TOK_ID);
        if (lexeme[0] == '.') {
            if (non_local_label != NULL) {
                char *name;

                name = malloc(strlen(non_local_label)+strlen(lexeme)+1);
                strcpy(name, non_local_label);
                strcat(name, lexeme);
                e->attr.lab.name = name;
            } else {
                /*
                 * Something like:
                 *      <no-previous-non-local-label>
                 *      add eax, .x
                 *      y:
                 *      ...
                 *      .x:
                 * Fix it when the first non-local label (`y') is encountered.
                 */
                UnrLab *n;

                n = malloc(sizeof(UnrLab));
                n->dest = &e->attr.lab.name;
                n->next = unresolved_labels_list;
                unresolved_labels_list = n;
                e->attr.lab.name = strdup(lexeme);
            }
        } else {
            e->attr.lab.name = strdup(lexeme);
        }
        match(TOK_ID);
        break;
    case TOK_NUM:
        e = new_opnd(TOK_NUM);
        EXPRVAL(e) = str2int(lexeme);
        match(TOK_NUM);
        break;
    case TOK_DOLLAR:
        e = new_opnd(TOK_DOLLAR);
        e->attr.LC.val = LC();
        e->attr.LC.sec = lookup_symbol(curr_section->name);
        match(TOK_DOLLAR);
        break;
    default:
        if (isprint(lexeme[0]))
            err1("expression expected (got `%s')", lexeme);
        else
            err1("expression expected (got `0x%02x')", lexeme[0]);
        break;
    }
    return e;
}

void match(Token expected)
{
    if (curr_tok != expected) {
        if (isprint(lexeme[0]))
            err1("syntax error: unexpected `%s'", lexeme);
        else
            err1("syntax error: unexpected `0x%02x'", lexeme[0]);
    }
    curr_tok = get_token();
}

struct RWord {
    char *str;
    Token tok;
} reserved_table[] = { /* sorted for bsearch() */
    { "ah",     TOK_AH      },
    { "al",     TOK_AL      },
    { "align",  TOK_ALIGN   },
    { "alignb", TOK_ALIGNB  },
    { "ax",     TOK_AX      },
    { "bh",     TOK_BH      },
    { "bl",     TOK_BL      },
    { "bp",     TOK_BP      },
    { "bpl",    TOK_BPL     },
    { "bx",     TOK_BX      },
    { "byte",   TOK_BYTE    },
    { "ch",     TOK_CH      },
    { "cl",     TOK_CL      },
    { "cx",     TOK_CX      },
    { "db",     TOK_DB      },
    { "dd",     TOK_DD      },
    { "dh",     TOK_DH      },
    { "di",     TOK_DI      },
    { "dil",    TOK_DIL     },
    { "dl",     TOK_DL      },
    { "dq",     TOK_DQ      },
    { "dw",     TOK_DW      },
    { "dword",  TOK_DWORD   },
    { "dx",     TOK_DX      },
    { "eax",    TOK_EAX     },
    { "ebp",    TOK_EBP     },
    { "ebx",    TOK_EBX     },
    { "ecx",    TOK_ECX     },
    { "edi",    TOK_EDI     },
    { "edx",    TOK_EDX     },
    { "esi",    TOK_ESI     },
    { "esp",    TOK_ESP     },
    { "extern", TOK_EXTERN  },
    { "global", TOK_GLOBAL  },
    { "qword",  TOK_QWORD   },
    { "r10",    TOK_R10     },
    { "r10b",   TOK_R10B    },
    { "r10d",   TOK_R10D    },
    { "r10w",   TOK_R10W    },
    { "r11",    TOK_R11     },
    { "r11b",   TOK_R11B    },
    { "r11d",   TOK_R11D    },
    { "r11w",   TOK_R11W    },
    { "r12",    TOK_R12     },
    { "r12b",   TOK_R12B    },
    { "r12d",   TOK_R12D    },
    { "r12w",   TOK_R12W    },
    { "r13",    TOK_R13     },
    { "r13b",   TOK_R13B    },
    { "r13d",   TOK_R13D    },
    { "r13w",   TOK_R13W    },
    { "r14",    TOK_R14     },
    { "r14b",   TOK_R14B    },
    { "r14d",   TOK_R14D    },
    { "r14w",   TOK_R14W    },
    { "r15",    TOK_R15     },
    { "r15b",   TOK_R15B    },
    { "r15d",   TOK_R15D    },
    { "r15w",   TOK_R15W    },
    { "r8",     TOK_R8      },
    { "r8b",    TOK_R8B     },
    { "r8d",    TOK_R8D     },
    { "r8w",    TOK_R8W     },
    { "r9",     TOK_R9      },
    { "r9b",    TOK_R9B     },
    { "r9d",    TOK_R9D     },
    { "r9w",    TOK_R9W     },
    { "rax",    TOK_RAX     },
    { "rbp",    TOK_RBP     },
    { "rbx",    TOK_RBX     },
    { "rcx",    TOK_RCX     },
    { "rdi",    TOK_RDI     },
    { "rdx",    TOK_RDX     },
    { "rep",    TOK_REP     },
    { "repe",   TOK_REPE    },
    { "repne",  TOK_REPNE   },
    { "repnz",  TOK_REPNZ   },
    { "repz",   TOK_REPZ    },
    { "resb",   TOK_RESB    },
    { "resd",   TOK_RESD    },
    { "resq",   TOK_RESQ    },
    { "resw",   TOK_RESW    },
    { "rsi",    TOK_RSI     },
    { "rsp",    TOK_RSP     },
    { "section",TOK_SECTION },
    { "segment",TOK_SECTION },
    { "si",     TOK_SI      },
    { "sil",    TOK_SIL     },
    { "sp",     TOK_SP      },
    { "spl",    TOK_SPL     },
    { "times",  TOK_TIMES   },
    { "word",   TOK_WORD    }
};

int cmp_rword(const void *p1, const void *p2)
{
    struct RWord *x1 = (struct RWord *)p1;
    struct RWord *x2 = (struct RWord *)p2;

    return strcmp(x1->str, x2->str);
}

Token reserved_lookup(char *s)
{
    struct RWord key, *res;

    key.str = s;
    res = bsearch(&key, reserved_table, NELEMS(reserved_table), sizeof(reserved_table[0]), cmp_rword);
    if (res == NULL)
        return TOK_ID;
    else
        return res->tok;
}

Token get_token(void)
{
    enum {
        START,
        INID,
        INDECNUM,
        INHEXNUM,
        INCOMMENT,
        DONE,
    };
    Token tok;
    int state;
    int save, cindx;

    cindx = 0;
    state = START;
    while (state != DONE) {
        int c;

        c = *curr++;
        save = TRUE;
        switch (state) {
        case START:
            if (c==' ' || c=='\t') {
                save = FALSE;
            } else if (c == '$') {
                if (isalpha(*curr) || *curr=='_' || *curr=='.') {
                    state = INID;
                } else {
                    tok = TOK_DOLLAR;
                    state = DONE;
                }
            } else if (isalpha(c) || c=='_' || c=='.') {
                state = INID;
            } else if (isdigit(c)) {
                if (*curr=='x' || *curr=='X') {
                    lexeme[cindx++] = (char)c;
                    c = *curr++;
                    state = INHEXNUM;
                } else {
                    state = INDECNUM;
                }
            } else if (c == ';') {
                save = FALSE;
                state = INCOMMENT;
            } else {
                state = DONE;
                switch (c) {
                case '\0':
                    save = FALSE;
                    if (reading_from_stdin && read_line())
                        state = START;
                    else
                        tok = TOK_EOF;
                    break;
                case '\n':
                    ++line_number;
                    tok = TOK_EOL;
                    break;
                case ',':
                    tok = TOK_COMMA;
                    break;
                case '.':
                    tok = TOK_DOT;
                    break;
                case ':':
                    tok = TOK_COLON;
                    break;
                case '(':
                    tok = TOK_LPAREN;
                    break;
                case ')':
                    tok = TOK_RPAREN;
                    break;
                case '[':
                    tok = TOK_LBRACKET;
                    break;
                case ']':
                    tok = TOK_RBRACKET;
                    break;
                case '+':
                    tok = TOK_PLUS;
                    break;
                case '-':
                    tok = TOK_MINUS;
                    break;
                case '*':
                    tok = TOK_MUL;
                    break;
                case '~':
                    tok = TOK_CMPL;
                    break;
                case '!':
                    tok = TOK_LNEG;
                    break;
                case '/':
                    if (*curr == '/') {
                        lexeme[cindx++] = (char)c;
                        c = *curr++;
                        tok = TOK_DIV;
                    } else {
                        tok = TOK_UDIV;
                    }
                    break;
                case '%':
                    if (*curr == '%') {
                        lexeme[cindx++] = (char)c;
                        c = *curr++;
                        tok = TOK_MOD;
                    } else {
                        tok = TOK_UMOD;
                    }
                    break;
                case '<':
                    if (*curr == '<') {
                        lexeme[cindx++] = (char)c;
                        c = *curr++;
                        tok = TOK_LSHIFT;
                    } else { /* ignore stray '<' */
                        save = FALSE;
                        state = START;
                    }
                    break;
                case '>':
                    if (*curr == '>') {
                        lexeme[cindx++] = (char)c;
                        c = *curr++;
                        tok = TOK_RSHIFT;
                    } else { /* ignore stray '>' */
                        save = FALSE;
                        state = START;
                    }
                    break;
                }
            }
            break; /* START */

        case INCOMMENT:
            save = FALSE;
            if (c=='\n' || c=='\0') {
                --curr;
                state = START;
            }
            break;

#define FINISH(T)\
    do {\
        save = FALSE;\
        --curr;\
        tok = T;\
        state = DONE;\
    } while (0)

        case INID:
            if (!isalnum(c) && c!='_' && c!='.' && c!='$' && c!='@')
                FINISH(TOK_ID);
            break;

        case INDECNUM:
            if (!isdigit(c))
                FINISH(TOK_NUM);
            break;

        case INHEXNUM:
            if (!isxdigit(c))
                FINISH(TOK_NUM);
            break;

#undef FINISH

        case DONE:
        default:
            assert(0);
            break;
        } /* switch (state) */
        if (save)
            lexeme[cindx++] = (char)c;
        if (state == DONE) {
            lexeme[cindx] = '\0';
            if (tok == TOK_ID) {
                if (lexeme[0] != '$')
                    tok = reserved_lookup(lexeme);
                else
                    memmove(lexeme, lexeme+1, strlen(lexeme));
            }
        }
    }
    return tok;
}

int read_line(void)
{
    curr = line_buf;
    return (fgets(line_buf, MAX_LINE_BUF, stdin) != NULL);
}

void init(char *file_path)
{
    if (file_path != NULL) {
        FILE *fp;
        unsigned len;

        if ((fp=fopen(file_path, "rb")) == NULL)
            TERMINATE("%s: error reading file `%s'", prog_name, file_path);
        fseek(fp, 0, SEEK_END);
        len = ftell(fp);
        rewind(fp);
        curr = buf = malloc(len+1);
        len = fread(buf, 1, len, fp);
        buf[len] = '\0';
    } else {
        reading_from_stdin = TRUE;
        read_line();
    }
}

long long str2int(char *s)
{
    char *ep;

    return (long long)strtoull(s, &ep, 0);
}

/* report pass 1 errors */
void err1(char *fmt, ...)
{
    va_list args;

    fprintf(stderr, "%s: %s:%d: error: ", prog_name, inpath, line_number);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

/* report pass 2 errors */
void err2(Operand *op, char *fmt, ...)
{
    va_list args;

    fprintf(stderr, "%s: %s:%d: error: ", prog_name, inpath, op->lineno);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

void write_ELF64_file(void)
{
    int i;
    unsigned nsym;
    Section *sec;
    Elf64_Ehdr elf_header;
    Elf64_Off curr = 0;
    Elf64_Shdr symtab_header, shstrtab_header, strtab_header;
    StrTab *shstrtab = strtab_new(), *strtab = strtab_new();
    Elf64_Sym sym;

#define ALIGN(n)\
    do {\
        int nb = round_up(curr, n)-curr;\
        for (i = 0; i < nb; i++)\
            fputc(0, output_file), ++curr;\
    } while (0)

    memset(&symtab_header, 0, sizeof(Elf64_Shdr));
    memset(&shstrtab_header, 0, sizeof(Elf64_Shdr));
    memset(&strtab_header, 0, sizeof(Elf64_Shdr));

    /* =============
     * Dummy ELF header
     * ============= */
    memset(&elf_header, 0, sizeof(Elf64_Ehdr));
    fwrite(&elf_header, sizeof(Elf64_Ehdr), 1, output_file);
    curr += sizeof(Elf64_Ehdr);

    /* =============
     * Sections
     * ============= */
    ALIGN(16);
    elf_header.e_shnum = 1; /* SHN_UNDEF */

    /*
     * .shstrtab
     */
    shstrtab_header.sh_name = strtab_append(shstrtab, ".shstrtab");
    symtab_header.sh_name = strtab_append(shstrtab, ".symtab");
    strtab_header.sh_name = strtab_append(shstrtab, ".strtab");
    shstrtab_header.sh_offset = curr;
    for (sec = sections; sec != NULL; sec = sec->next) {
        sec->h.hdr64.sh_name = strtab_append(shstrtab, sec->name);
        if (sec->relocs != NULL) {
            char rsname[64] = ".rela";

            strcat(rsname, sec->name);
            sec->rh.rhdr64.sh_name = strtab_append(shstrtab, strdup(rsname));
        }
    }
    shstrtab_header.sh_size = strtab_write(shstrtab, output_file);
    curr += shstrtab_header.sh_size;
    ++elf_header.e_shnum;

    /*
     * .symtab
     */
    ALIGN(4);
    symtab_header.sh_offset = curr;
    /* first entry (STN_UNDEF) */
    memset(&sym, 0, sizeof(Elf64_Sym));
    fwrite(&sym, sizeof(Elf64_Sym), 1, output_file);
    curr += sizeof(Elf64_Sym);
    symtab_header.sh_size += sizeof(Elf64_Sym);
    ++symtab_header.sh_info;
    /* file symbol table entry */
    memset(&sym, 0, sizeof(Elf64_Sym));
    sym.st_name = strtab_append(strtab, inpath);
    sym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_FILE);
    sym.st_shndx = SHN_ABS;
    fwrite(&sym, sizeof(Elf64_Sym), 1, output_file);
    curr += sizeof(Elf64_Sym);
    symtab_header.sh_size += sizeof(Elf64_Sym);
    ++symtab_header.sh_info;
    /* remaining .symtab entries */
    nsym = 2;
    /* symbols with STB_LOCAL binding */
    for (i = 0; i < HASH_SIZE; i++) {
        if (symbols[i] != NULL) {
            Symbol *np;

            for (np = symbols[i]; np != NULL; np = np->next) {
                if (np->kind == SectionKind) {
                    memset(&sym, 0, sizeof(Elf64_Sym));
                    sym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
                    sym.st_shndx = np->sec->shndx;
                } else if (np->bind == LocalBind) {
                    memset(&sym, 0, sizeof(Elf64_Sym));
                    sym.st_name = strtab_append(strtab, np->name);
                    sym.st_value = np->val;
                    sym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
                    sym.st_shndx = np->sec->shndx;
                } else {
                    continue;
                }
                np->ndx = nsym++;
                ++symtab_header.sh_info;
                fwrite(&sym, sizeof(Elf64_Sym), 1, output_file);
                curr += sizeof(Elf64_Sym);
                symtab_header.sh_size += sizeof(Elf64_Sym);
            }
        }
    }
    /* symbols with STB_GLOBAL binding */
    for (i = 0; i < HASH_SIZE; i++) {
        if (symbols[i] != NULL) {
            Symbol *np;

            for (np = symbols[i]; np != NULL; np = np->next) {
                if (np->bind == LocalBind)
                    continue;
                memset(&sym, 0, sizeof(Elf64_Sym));
                sym.st_name = strtab_append(strtab, np->name);
                sym.st_value = np->val;
                if (np->is_func)
                    sym.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
                else
                    sym.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
                if (np->bind == ExternBind) {
                    sym.st_shndx = SHN_UNDEF;
                } else {
                    assert(np->sec != NULL); /* TBD: undefined 'global' */
                    sym.st_shndx = np->sec->shndx;
                }
                np->ndx = nsym++;
                fwrite(&sym, sizeof(Elf64_Sym), 1, output_file);
                curr += sizeof(Elf64_Sym);
                symtab_header.sh_size += sizeof(Elf64_Sym);
            }
        }
    }
    ++elf_header.e_shnum;

    /*
     * .strtab
     */
    strtab_header.sh_offset = curr;
    strtab_header.sh_size = strtab_write(strtab, output_file);
    curr += strtab_header.sh_size;
    ++elf_header.e_shnum;;

    /*
     * Remaining sections.
     */
    for (sec = sections; sec != NULL; sec = sec->next) {
        SBlock *sb;

        if (sec->name[0] == '.') {
            if (equal(sec->name, ".text")) {
                sec->h.hdr64.sh_type = SHT_PROGBITS;
                sec->h.hdr64.sh_flags = SHF_ALLOC|SHF_EXECINSTR;
                sec->h.hdr64.sh_addralign = 16;
            } else if (equal(sec->name, ".data")) {
                sec->h.hdr64.sh_type = SHT_PROGBITS;
                sec->h.hdr64.sh_flags = SHF_ALLOC|SHF_WRITE;
                sec->h.hdr64.sh_addralign = 4;
            } else if (equal(sec->name, ".rodata")) {
                sec->h.hdr64.sh_type = SHT_PROGBITS;
                sec->h.hdr64.sh_flags = SHF_ALLOC;
                sec->h.hdr64.sh_addralign = 4;
            } else if (equal(sec->name, ".bss")) {
                sec->h.hdr64.sh_type = SHT_NOBITS;
                sec->h.hdr64.sh_flags = SHF_ALLOC|SHF_WRITE;
                sec->h.hdr64.sh_addralign = 4;
                ALIGN(4);
                sec->h.hdr64.sh_offset = curr;
                sec->h.hdr64.sh_size = SBLOCK_SIZ(sec->first);
                ++elf_header.e_shnum;
                continue;
            } else {
                fprintf(stderr, "%s: warning: unknown system section `%s': defaulting attributes\n",
                prog_name, sec->name);
                goto unk_sec;
            }
        } else {
unk_sec:    sec->h.hdr64.sh_type = SHT_PROGBITS;
            sec->h.hdr64.sh_flags = SHF_ALLOC;
            sec->h.hdr64.sh_addralign = 1;
        }
        ALIGN(4);
        sec->h.hdr64.sh_offset = curr;
        for (sb = sec->first; sb != NULL; sb = sb->next) {
            unsigned n;

            n = SBLOCK_SIZ(sb);
            fwrite((char *)sb+sizeof(SBlock), n, 1, output_file);
            sec->h.hdr64.sh_size += n;
            curr += n;
        }
        ++elf_header.e_shnum;
        if (sec->relocs != NULL) {
            Reloc *r;

            ALIGN(4);
            sec->rh.rhdr64.sh_type = SHT_RELA;
            sec->rh.rhdr64.sh_offset = curr;
            sec->rh.rhdr64.sh_link = 2; /* .symtab */
            sec->rh.rhdr64.sh_info = sec->shndx;
            sec->rh.rhdr64.sh_addralign = 4;
            sec->rh.rhdr64.sh_entsize = sizeof(Elf64_Rela);
            for (r = sec->relocs; r != NULL; r = r->next) {
                Elf64_Rela er;

                er.r_offset = r->offs;
                er.r_addend = r->add;
                switch (r->attr & (RELOC_SIZ8|RELOC_SIZ16|RELOC_SIZ32|RELOC_SIZ64)) {
                case RELOC_SIZ8:
                    if (r->attr & RELOC_ABS)
                        er.r_info = ELF64_R_INFO(r->sym->ndx, R_X86_64_8);
                    else
                        er.r_info = ELF64_R_INFO(r->sym->ndx, R_X86_64_PC8);
                    break;
                case RELOC_SIZ16:
                    if (r->attr & RELOC_ABS)
                        er.r_info = ELF64_R_INFO(r->sym->ndx, R_X86_64_16);
                    else
                        er.r_info = ELF64_R_INFO(r->sym->ndx, R_X86_64_PC16);
                    break;
                case RELOC_SIZ32:
                    if (r->attr & RELOC_ABS)
                        er.r_info = ELF64_R_INFO(r->sym->ndx, (r->attr&RELOC_SIGNED)?R_X86_64_32S:R_X86_64_32);
                    else
                        er.r_info = ELF64_R_INFO(r->sym->ndx, R_X86_64_PC32);
                    break;
                default:
                    if (r->attr & RELOC_ABS)
                        er.r_info = ELF64_R_INFO(r->sym->ndx, R_X86_64_64);
                    else
                        er.r_info = ELF64_R_INFO(r->sym->ndx, R_X86_64_PC64);
                    break;
                }
                fwrite(&er, sizeof(Elf64_Rela), 1, output_file);
                sec->rh.rhdr64.sh_size += sizeof(Elf64_Rela);
                curr += sizeof(Elf64_Rela);
            }
            ++elf_header.e_shnum;
        }
    }

    /* =============
     * Section header table
     * ============= */
    ALIGN(4);
    elf_header.e_shoff = curr;
    /* first entry (SHN_UNDEF) */
    for (i = 0; i < sizeof(Elf64_Shdr); i++)
        fputc(0, output_file);
    /* .shstrtab section header */
    shstrtab_header.sh_type = SHT_STRTAB;
    shstrtab_header.sh_addralign = 1;
    fwrite(&shstrtab_header, sizeof(Elf64_Shdr), 1, output_file);
    /* .symtab section header */
    symtab_header.sh_type = SHT_SYMTAB;
    symtab_header.sh_link = 3; /* .strtab */
    symtab_header.sh_addralign = 4;
    symtab_header.sh_entsize = sizeof(Elf64_Sym);
    fwrite(&symtab_header, sizeof(Elf64_Shdr), 1, output_file);
    /* .strtab section header */
    strtab_header.sh_type = SHT_STRTAB;
    strtab_header.sh_addralign = 1;
    fwrite(&strtab_header, sizeof(Elf64_Shdr), 1, output_file);
    /* remaining section headers */
    for (sec = sections; sec != NULL; sec = sec->next)
        fwrite(&sec->h.hdr64, sizeof(Elf64_Shdr), 1, output_file);
    for (sec = sections; sec != NULL; sec = sec->next)
        if (sec->relocs != NULL)
            fwrite(&sec->rh.rhdr64, sizeof(Elf64_Shdr), 1, output_file);

    /*
     * Correct dummy ELF header
     */
    rewind(output_file);
    elf_header.e_ident[EI_MAG0] = ELFMAG0;
    elf_header.e_ident[EI_MAG1] = ELFMAG1;
    elf_header.e_ident[EI_MAG2] = ELFMAG2;
    elf_header.e_ident[EI_MAG3] = ELFMAG3;
    elf_header.e_ident[EI_CLASS] = ELFCLASS64;
    elf_header.e_ident[EI_DATA] = ELFDATA2LSB;
    elf_header.e_ident[EI_VERSION] = EV_CURRENT;
    elf_header.e_type = ET_REL;
    elf_header.e_machine = EM_X86_64;
    elf_header.e_version = EV_CURRENT;
    elf_header.e_ehsize = sizeof(Elf64_Ehdr);
    elf_header.e_shentsize = sizeof(Elf64_Shdr);
    elf_header.e_shstrndx = 1;
    fwrite(&elf_header, sizeof(Elf64_Ehdr), 1, output_file);
    fseek(output_file, 0, SEEK_END);

#undef ALIGN
}

void write_ELF32_file(void)
{
    int i;
    unsigned nsym;
    Section *sec;
    Elf32_Ehdr elf_header;
    Elf32_Off curr = 0;
    Elf32_Shdr symtab_header, shstrtab_header, strtab_header;
    StrTab *shstrtab = strtab_new(), *strtab = strtab_new();
    Elf32_Sym sym;

#define ALIGN(n)\
    do {\
        int nb = round_up(curr, n)-curr;\
        for (i = 0; i < nb; i++)\
            fputc(0, output_file), ++curr;\
    } while (0)

    memset(&symtab_header, 0, sizeof(Elf32_Shdr));
    memset(&shstrtab_header, 0, sizeof(Elf32_Shdr));
    memset(&strtab_header, 0, sizeof(Elf32_Shdr));

    /* =============
     * Dummy ELF header
     * ============= */
    memset(&elf_header, 0, sizeof(Elf32_Ehdr));
    fwrite(&elf_header, sizeof(Elf32_Ehdr), 1, output_file);
    curr += sizeof(Elf32_Ehdr);

    /* =============
     * Sections
     * ============= */
    ALIGN(16);
    elf_header.e_shnum = 1; /* SHN_UNDEF */

    /*
     * .shstrtab
     */
    shstrtab_header.sh_name = strtab_append(shstrtab, ".shstrtab");
    symtab_header.sh_name = strtab_append(shstrtab, ".symtab");
    strtab_header.sh_name = strtab_append(shstrtab, ".strtab");
    shstrtab_header.sh_offset = curr;
    for (sec = sections; sec != NULL; sec = sec->next) {
        sec->h.hdr32.sh_name = strtab_append(shstrtab, sec->name);
        if (sec->relocs != NULL) {
            char rsname[64] = ".rel";

            strcat(rsname, sec->name);
            sec->rh.rhdr32.sh_name = strtab_append(shstrtab, strdup(rsname));
        }
    }
    shstrtab_header.sh_size = strtab_write(shstrtab, output_file);
    curr += shstrtab_header.sh_size;
    ++elf_header.e_shnum;

    /*
     * .symtab
     */
    ALIGN(4);
    symtab_header.sh_offset = curr;
    /* first entry (STN_UNDEF) */
    memset(&sym, 0, sizeof(Elf32_Sym));
    fwrite(&sym, sizeof(Elf32_Sym), 1, output_file);
    curr += sizeof(Elf32_Sym);
    symtab_header.sh_size += sizeof(Elf32_Sym);
    ++symtab_header.sh_info;
    /* file symbol table entry */
    memset(&sym, 0, sizeof(Elf32_Sym));
    sym.st_name = strtab_append(strtab, inpath);
    sym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_FILE);
    sym.st_shndx = SHN_ABS;
    fwrite(&sym, sizeof(Elf32_Sym), 1, output_file);
    curr += sizeof(Elf32_Sym);
    symtab_header.sh_size += sizeof(Elf32_Sym);
    ++symtab_header.sh_info;
    /* remaining .symtab entries */
    nsym = 2;
    /* symbols with STB_LOCAL binding */
    for (i = 0; i < HASH_SIZE; i++) {
        if (symbols[i] != NULL) {
            Symbol *np;

            for (np = symbols[i]; np != NULL; np = np->next) {
                if (np->kind == SectionKind) {
                    memset(&sym, 0, sizeof(Elf32_Sym));
                    sym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
                    sym.st_shndx = np->sec->shndx;
                } else if (np->bind == LocalBind) {
                    memset(&sym, 0, sizeof(Elf32_Sym));
                    sym.st_name = strtab_append(strtab, np->name);
                    sym.st_value = np->val;
                    sym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_NOTYPE);
                    sym.st_shndx = np->sec->shndx;
                } else {
                    continue;
                }
                np->ndx = nsym++;
                ++symtab_header.sh_info;
                fwrite(&sym, sizeof(Elf32_Sym), 1, output_file);
                curr += sizeof(Elf32_Sym);
                symtab_header.sh_size += sizeof(Elf32_Sym);
            }
        }
    }
    /* symbols with STB_GLOBAL binding */
    for (i = 0; i < HASH_SIZE; i++) {
        if (symbols[i] != NULL) {
            Symbol *np;

            for (np = symbols[i]; np != NULL; np = np->next) {
                if (np->bind == LocalBind)
                    continue;
                memset(&sym, 0, sizeof(Elf32_Sym));
                sym.st_name = strtab_append(strtab, np->name);
                sym.st_value = np->val;
                if (np->is_func)
                    sym.st_info = ELF32_ST_INFO(STB_GLOBAL, STT_FUNC);
                else
                    sym.st_info = ELF32_ST_INFO(STB_GLOBAL, STT_NOTYPE);
                if (np->bind == ExternBind) {
                    sym.st_shndx = SHN_UNDEF;
                } else {
                    assert(np->sec != NULL); /* TBD: undefined 'global' */
                    sym.st_shndx = np->sec->shndx;
                }
                np->ndx = nsym++;
                fwrite(&sym, sizeof(Elf32_Sym), 1, output_file);
                curr += sizeof(Elf32_Sym);
                symtab_header.sh_size += sizeof(Elf32_Sym);
            }
        }
    }
    ++elf_header.e_shnum;

    /*
     * .strtab
     */
    strtab_header.sh_offset = curr;
    strtab_header.sh_size = strtab_write(strtab, output_file);
    curr += strtab_header.sh_size;
    ++elf_header.e_shnum;;

    /*
     * Remaining sections.
     */
    for (sec = sections; sec != NULL; sec = sec->next) {
        SBlock *sb;

        if (sec->name[0] == '.') {
            if (equal(sec->name, ".text")) {
                sec->h.hdr32.sh_type = SHT_PROGBITS;
                sec->h.hdr32.sh_flags = SHF_ALLOC|SHF_EXECINSTR;
                sec->h.hdr32.sh_addralign = 16;
            } else if (equal(sec->name, ".data")) {
                sec->h.hdr32.sh_type = SHT_PROGBITS;
                sec->h.hdr32.sh_flags = SHF_ALLOC|SHF_WRITE;
                sec->h.hdr32.sh_addralign = 4;
            } else if (equal(sec->name, ".rodata")) {
                sec->h.hdr32.sh_type = SHT_PROGBITS;
                sec->h.hdr32.sh_flags = SHF_ALLOC;
                sec->h.hdr32.sh_addralign = 4;
            } else if (equal(sec->name, ".bss")) {
                sec->h.hdr32.sh_type = SHT_NOBITS;
                sec->h.hdr32.sh_flags = SHF_ALLOC|SHF_WRITE;
                sec->h.hdr32.sh_addralign = 4;
                ALIGN(4);
                sec->h.hdr32.sh_offset = curr;
                sec->h.hdr32.sh_size = SBLOCK_SIZ(sec->first);
                ++elf_header.e_shnum;
                continue;
            } else {
                fprintf(stderr, "%s: warning: unknown system section `%s': defaulting attributes\n",
                prog_name, sec->name);
                goto unk_sec;
            }
        } else {
unk_sec:    sec->h.hdr32.sh_type = SHT_PROGBITS;
            sec->h.hdr32.sh_flags = SHF_ALLOC;
            sec->h.hdr32.sh_addralign = 1;
        }
        ALIGN(4);
        sec->h.hdr32.sh_offset = curr;
        for (sb = sec->first; sb != NULL; sb = sb->next) {
            unsigned n;

            n = SBLOCK_SIZ(sb);
            fwrite((char *)sb+sizeof(SBlock), n, 1, output_file);
            sec->h.hdr32.sh_size += n;
            curr += n;
        }
        ++elf_header.e_shnum;
        if (sec->relocs != NULL) {
            Reloc *r;

            ALIGN(4);
            sec->rh.rhdr32.sh_type = SHT_REL;
            sec->rh.rhdr32.sh_offset = curr;
            sec->rh.rhdr32.sh_link = 2; /* .symtab */
            sec->rh.rhdr32.sh_info = sec->shndx;
            sec->rh.rhdr32.sh_addralign = 4;
            sec->rh.rhdr32.sh_entsize = sizeof(Elf32_Rel);
            for (r = sec->relocs; r != NULL; r = r->next) {
                Elf32_Rel er;

                er.r_offset = r->offs;
                switch (r->attr&(RELOC_SIZ8|RELOC_SIZ16|RELOC_SIZ32)) {
                case RELOC_SIZ8:
                    if (r->attr & RELOC_ABS)
                        er.r_info = ELF32_R_INFO(r->sym->ndx, R_386_8);
                    else
                        er.r_info = ELF32_R_INFO(r->sym->ndx, R_386_PC8);
                    break;
                case RELOC_SIZ16:
                    if (r->attr & RELOC_ABS)
                        er.r_info = ELF32_R_INFO(r->sym->ndx, R_386_16);
                    else
                        er.r_info = ELF32_R_INFO(r->sym->ndx, R_386_PC16);
                    break;
                default:
                    if (r->attr & RELOC_ABS)
                        er.r_info = ELF32_R_INFO(r->sym->ndx, R_386_32);
                    else
                        er.r_info = ELF32_R_INFO(r->sym->ndx, R_386_PC32);
                    break;
                }
                fwrite(&er, sizeof(Elf32_Rel), 1, output_file);
                sec->rh.rhdr32.sh_size += sizeof(Elf32_Rel);
                curr += sizeof(Elf32_Rel);
            }
            ++elf_header.e_shnum;
        }
    }

    /* =============
     * Section header table
     * ============= */
    ALIGN(4);
    elf_header.e_shoff = curr;
    /* first entry (SHN_UNDEF) */
    for (i = 0; i < sizeof(Elf32_Shdr); i++)
        fputc(0, output_file);
    /* .shstrtab section header */
    shstrtab_header.sh_type = SHT_STRTAB;
    shstrtab_header.sh_addralign = 1;
    fwrite(&shstrtab_header, sizeof(Elf32_Shdr), 1, output_file);
    /* .symtab section header */
    symtab_header.sh_type = SHT_SYMTAB;
    symtab_header.sh_link = 3; /* .strtab */
    symtab_header.sh_addralign = 4;
    symtab_header.sh_entsize = sizeof(Elf32_Sym);
    fwrite(&symtab_header, sizeof(Elf32_Shdr), 1, output_file);
    /* .strtab section header */
    strtab_header.sh_type = SHT_STRTAB;
    strtab_header.sh_addralign = 1;
    fwrite(&strtab_header, sizeof(Elf32_Shdr), 1, output_file);
    /* remaining section headers */
    for (sec = sections; sec != NULL; sec = sec->next)
        fwrite(&sec->h.hdr32, sizeof(Elf32_Shdr), 1, output_file);
    for (sec = sections; sec != NULL; sec = sec->next)
        if (sec->relocs != NULL)
            fwrite(&sec->rh.rhdr32, sizeof(Elf32_Shdr), 1, output_file);

    /*
     * Correct dummy ELF header
     */
    rewind(output_file);
    elf_header.e_ident[EI_MAG0] = ELFMAG0;
    elf_header.e_ident[EI_MAG1] = ELFMAG1;
    elf_header.e_ident[EI_MAG2] = ELFMAG2;
    elf_header.e_ident[EI_MAG3] = ELFMAG3;
    elf_header.e_ident[EI_CLASS] = ELFCLASS32;
    elf_header.e_ident[EI_DATA] = ELFDATA2LSB;
    elf_header.e_ident[EI_VERSION] = EV_CURRENT;
    elf_header.e_type = ET_REL;
    elf_header.e_machine = EM_386;
    elf_header.e_version = EV_CURRENT;
    elf_header.e_ehsize = sizeof(Elf32_Ehdr);
    elf_header.e_shentsize = sizeof(Elf32_Shdr);
    elf_header.e_shstrndx = 1;
    fwrite(&elf_header, sizeof(Elf32_Ehdr), 1, output_file);
    fseek(output_file, 0, SEEK_END);

#undef ALIGN
}
