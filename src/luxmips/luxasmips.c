/*
        Small MIPS32 assembler.

        ASM GRAMMAR
    program = line { line } EOF
    line = label | source_line | directive
    label = ID ":"
    source_line = instruction [ operand [ "," operand [ "," operand ] ] ] EOL
    operand = REG | asm_expr [ "(" REG ")" ]
    asm_expr = "-" NUM | ID [ ( "+" | "-" ) NUM ]
    directive = "%" ( "segment" | "section" ) id |
                "%" "extern" id { "," id } |
                "%" "global" id { "," id } |
                "%" "align"  NUM |
                "%" "alignb" NUM |
                "%" "res" NUM |
                "%" "byte"  asm_expr { "," asm_expr } |
                "%" "word"  asm_expr { "," asm_expr } |
                "%" "dword" asm_expr { "," asm_expr } |
                "%" "zero" NUM

        Tokens
    ID = ( '_' | '@' | [A-Za-z] | '.' ) ( '_' | '@' | [0-9A-Za-z] | '.' )*
    NUM = [0-9]+ | "0x" [A-F0-9]+
    REG = '$' NUM
    Comments start with ";" and extend until the end of the line.

    The MIPS32 Instruction Set: https://imgtec.com/documentation/
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdint.h>
#include <elf.h>
#include "../util.h"
#include "../luxas/ELF_util.h"

#define MAX_LEXEME  512
#define HASH_SIZE   1009
#define HASH(s)     (hash(s)%HASH_SIZE)
#define OPC(x)      (((x)&0x3F) << 26)
#define RS(x)       (((x)&0x1F) << 21)
#define RT(x)       (((x)&0x1F) << 16)
#define RD(x)       (((x)&0x1F) << 11)
#define SHAMT(x)    (((x)&0x1F) << 6)
#define FUNCT(x)    ((x) & 0x3F)
#define IMM(x)      ((x) & 0xFFFF)
#define ADDR(x)     ((x) & 0x3FFFFFF)
#ifndef E_MIPS_ABI_O32
#define E_MIPS_ABI_O32 0x00001000 /* The original o32 abi. */
#endif

typedef struct Instr Instr;
typedef struct Operand Operand;
typedef struct AsmExpr AsmExpr;
typedef struct UnrExpr UnrExpr;
typedef struct Section Section;
typedef struct Symbol Symbol;
typedef struct Reloc Reloc;
typedef int MIPSReg;
typedef int bool;

typedef enum {
    TOK_ID,
    TOK_NUM,
    TOK_REG,
    TOK_COMMA,
    TOK_COLON,
    TOK_PERC,
    TOK_PLUS,
    TOK_MINUS,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_EOL,
    TOK_EOF
} Token;

char *prog_name;
char *inpath;
int line_number = 1;
char *curr, *buf;
char lexeme[MAX_LEXEME];
Token curr_tok;
FILE *output_file;
#define ERR(...)     fprintf(stderr, "%s: %s: line %d: ", prog_name, inpath, line_number), TERMINATE(__VA_ARGS__)
#define ERR2(e, ...) fprintf(stderr, "%s: %s: line %d: ", prog_name, inpath, (e)->lineno), TERMINATE(__VA_ARGS__)

Token get_token(void);
void match(Token expected);
long long str2int(char *s);

typedef enum {
    SectionKind,
    OtherKind,
} SymKind;

typedef enum {
    LocalBind,
    GlobalBind,
    ExternBind,
} SymBind;

struct Symbol {
    SymKind kind;
    SymBind bind;
    char *name;
    uint32_t val;
    Section *sec;   /* symbol's associated section (NULL for 'extern' symbols) */
    uint16_t ndx;   /* index into ELF file's symbol table */
    Symbol *next;
} *symbols[HASH_SIZE];

Symbol *define_symbol(SymKind kind, SymBind bind, char *name, uint32_t val, Section *sec)
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
        } /*else if (bind != GlobalBind) {
            goto redef;
        }*/
    } else {
        goto redef;
    }
    return np;
redef:
    ERR("symbol `%s' redefined", name);
}

Symbol *lookup_symbol(char *name)
{
    Symbol *np;

    for (np = symbols[HASH(name)]; np != NULL; np = np->next)
        if (equal(np->name, name))
            break;
    return np;
}

struct Section {
    char *name;
    char *buf;
    unsigned siz, max;
    Reloc *relocs;      /* relocations applied to this section */
    Symbol *sym;        /* symbol table entry for this section */
    Elf32_Shdr hdr;
    Elf32_Shdr rhdr;    /* section header for the associated relocation section (if any) */
    Elf32_Half shndx;   /* index into section header table */
    Section *next;
} *sections, *curr_section;

#define DEF_SEC     ".text"
#define LC()        (curr_section->siz)

void set_curr_section(char *name)
{
    Section *s;
    static Elf32_Half shndx = 4; /* [0]=UND, [1]=.shstrtab, [2]=.symtab, [3]=.strtab */

    for (s = sections; s != NULL; s = s->next)
        if (equal(s->name, name))
            break;
    if (s == NULL) {
        s = malloc(sizeof(Section));
        s->name = strdup(name);
        s->max = 512;
        s->buf = malloc(s->max);
        s->siz = 0;
        s->relocs = NULL;
        s->sym = define_symbol(SectionKind, LocalBind, name, 0, s);
        memset(&s->hdr, 0, sizeof(Elf32_Shdr));
        memset(&s->rhdr, 0, sizeof(Elf32_Shdr));
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
    curr_section->max *= 2;
    curr_section->buf = realloc(curr_section->buf, curr_section->max); /* XXX */
}

void write_byte(int b)
{
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    else if (curr_section->siz+1 > curr_section->max)
        expand_curr_section();
    *(curr_section->buf+curr_section->siz) = (char)b;
    ++curr_section->siz;
}

void write_word(int w)
{
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    else if (curr_section->siz+2 > curr_section->max)
        expand_curr_section();
    *(short *)(curr_section->buf+curr_section->siz) = (short)w;
    curr_section->siz += 2;
}

void write_dword(int d)
{
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    else if (curr_section->siz+4 > curr_section->max)
        expand_curr_section();
    *(int *)(curr_section->buf+curr_section->siz) = d;
    curr_section->siz += 4;
}

typedef enum {
    RegKind,
    ExprKind,
    RegOffKind
} OpKind;

struct AsmExpr {
    Token op;
    char *id;
    int num;
    Symbol *sym;
    int lineno;
};
#define RELEXPR(e) ((e)->op != TOK_NUM)

AsmExpr *new_asmexpr(void)
{
    AsmExpr *e;

    e = malloc(sizeof(AsmExpr)); /* XXX */
    return e;
}

struct Operand {
    OpKind kind;
    union {
        MIPSReg reg;
        AsmExpr expr;
        struct {
            AsmExpr offs;
            MIPSReg base;
        } regoff;
    } a;
};

int eval_expr(AsmExpr *e)
{
    if (e->op == TOK_NUM) {
        return e->num;
    } else { /* ID or ID+NUM */
        Symbol *s;

        if ((s=lookup_symbol(e->id)) == NULL)
            ERR("symbol `%s' undefined", e->id);
        e->sym = s;
        return s->val+e->num;
    }
}

typedef enum {
    RELOC_HI,    /* [       | hi16   ] */
    RELOC_LO,    /* [       | lo16   ] */
    RELOC_PCREL, /* [       | pc16   ] */
    RELOC_WORD,  /* [       | word16 ] */
    RELOC_ADDR,  /* [   |   targ26   ] */
    RELOC_DWORD, /* [      word32    ] */
} RelocType;

struct Reloc {
    RelocType type;
    int offs;
    Symbol *sym;
    Reloc *next;
};

Reloc *new_reloc(RelocType type, int offs, Symbol *sym)
{
    Reloc *n;

    n = malloc(sizeof(Reloc));
    n->type = type;
    n->offs = offs;
    n->sym = sym;
    return n;
}

enum {
    /* ----- used when assembling instructions */
    DEST_HI16,      /* R_MIPS_HI16 */
    DEST_LO16,      /* R_MIPS_LO16 */
    DEST_ADDR26,    /* R_MIPS_26 */
    /* -----  used when initializing data */
    DEST_BYTE,      /* cannot have relocation */
    DEST_WORD,      /* R_MIPS_16 */
    DEST_DWORD,     /* R_MIPS_32 */
};

struct UnrExpr {
    AsmExpr expr;
    int dfld;
    struct {
        int offs;
        Section *sec;
    } loc;          /* sec+offs is where the expression is located */
    bool reldisp;   /* is the expression a relative displacement? */
    UnrExpr *next;
} *unresolved_expressions_list;

void new_unr_expr(AsmExpr *expr, int dfld, int offs, Section *sec, bool reldisp)
{
    UnrExpr *n;

    n = malloc(sizeof(UnrExpr));
    n->expr = *expr;
    n->dfld = dfld;
    n->loc.offs = offs;
    n->loc.sec = sec;
    n->reldisp = reldisp;
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
        int res;
        void *dest;

        r = NULL;
        res = eval_expr(&n->expr);
        dest = n->loc.sec->buf+n->loc.offs;

        if (n->reldisp) {
            if (RELEXPR(&n->expr)
            && ((s=n->expr.sym)->bind==ExternBind || !equal(s->sec->name, n->loc.sec->name))) {
                assert (n->dfld == DEST_LO16);
                if (s->bind != ExternBind)
                    s = s->sec->sym; /* relocate with respect to section */
                /*
                 * R_MIPS_PC16 is useless without two shift-right's (ABI typo?).
                 * Seems like the GNU assembler just emit 0xFFFF for the offset
                 * field.
                 */
                r = new_reloc(RELOC_PCREL, n->loc.offs, s);
                *(short *)dest = -1;
            } else { /* target is absolute or relocatable with respect to this same section */
                assert(n->dfld == DEST_LO16);
                *(short *)dest = (short)((res-(n->loc.offs+4))>>2);
            }
        } else {
            switch (n->dfld) {
            case DEST_BYTE:
                *(char *)dest = (char)res;
                break;
            case DEST_WORD:
            case DEST_LO16:
                *(short *)dest = (short)res;
                break;
            case DEST_HI16:
                *(short *)dest = (short)(res>>16);
                break;
            case DEST_DWORD:
                *(int *)dest = res;
                break;
            case DEST_ADDR26:
                *(int *)dest |= ADDR(res>>2);
                break;
            }

            if (RELEXPR(&n->expr)) {
                if ((s=n->expr.sym)->bind != ExternBind)
                    s = s->sec->sym; /* relocate with respect to section */
                switch (n->dfld) {
                case DEST_BYTE:
                    ERR2(&n->expr, "cannot represent byte relocation");
                    break;
                case DEST_WORD:
                    r = new_reloc(RELOC_WORD, n->loc.offs, s);
                    break;
                case DEST_LO16:
                    r = new_reloc(RELOC_LO, n->loc.offs, s);
                    break;
                case DEST_HI16:
                    r = new_reloc(RELOC_HI, n->loc.offs, s);
                    break;
                case DEST_DWORD:
                    r = new_reloc(RELOC_DWORD, n->loc.offs, s);
                    break;
                case DEST_ADDR26:
                    r = new_reloc(RELOC_ADDR, n->loc.offs, s);
                    break;
                }
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

enum {
    TY_R1,      /* op rd, rs, rt */
    TY_R2,      /* op rs, rt */
    TY_R3,      /* op rd, rs (rd == $31 if missing) */
    TY_R4,      /* op rd */
    TY_R5,      /* op rs */
    TY_R6,      /* op rd, rt */

    TY_I1,      /* op rt, rs, imm */
    TY_I2,      /* op rt, imm */
    TY_I3,      /* op rs, rt, offset */
    TY_I4,      /* op rs, offset */
    TY_I5,      /* op rt, [ offset(rs) | offset ] */

    TY_J,       /* op addr */

    TY_NOP,     /* nop */
    TY_MOVE,    /* move rX, rY */
    TY_CLEAR,   /* clear rX */
    TY_NOT,     /* not rX, rY */
    TY_NEG,     /* neg rX, rY */
    TY_NEGU,    /* negu rX, rY */
    TY_LA,      /* la rX, label */
    TY_LI,      /* li rX, imm16/32 */
    TY_B,       /* b offset */
    TY_BAL,     /* bal offset */
    TY_BGT,     /* bgt rX, rY, offset */
    TY_BLT,     /* blt rX, rY, offset */
    TY_BGE,     /* bge rX, rY, offset */
    TY_BLE,     /* ble rX, rY, offset */
    TY_BGTU,    /* bgtu rX, rY, offset */
    TY_BLTU,    /* bltu rX, rY, offset */
    TY_BGEU,    /* bgeu rX, rY, offset */
    TY_BLEU,    /* bleu rX, rY, offset */
    TY_DIVD,    /* divd rX, rY, rZ */
    TY_DIVDU,   /* divdu rX, rY, rZ */
    TY_REM,     /* rem rX, rY, rZ */
    TY_REMU,    /* remu rX, rY, rZ */
    TY_SEQ,     /* seq rX, rY, ( rZ | uimm16 ) */
    TY_SNE,     /* sne rX, rY, ( rZ | uimm16 ) */
    TY_SGT,     /* sgt rX, rY, rZ */
    TY_SGE,     /* sge rX, rY, ( rZ | simm16 ) */
    TY_SLE,     /* sle rX, rY, rZ */
    TY_SGTU,    /* sgtu rX, rY, rZ */
    TY_SGEU,    /* sgeu rX, rY, ( rZ | simm16 ) */
    TY_SLEU,    /* sleu rX, rY, rZ */
    TY_ULH,     /* ulh rX, offset(rY) */
    TY_ULHU,    /* ulhu rX, offset(rY) */
    TY_ULW,     /* ulw rX, offset(rY) */
    TY_USH,     /* ush rX, offset(rY) */
    TY_USW,     /* usw rX, offset(rY) */
    TY_ADD,     /* add rX, rY, ( rZ | simm16 ) */
    TY_ADDU,    /* addu rX, rY, ( rZ | simm16 ) */
    TY_AND,     /* and rX, rY, ( rZ | uimm16 ) */
    TY_OR,      /* or rX, rY, ( rZ | uimm16 ) */
    TY_XOR,     /* xor rX, rY, ( rZ | uimm16 ) */
    TY_SLL,     /* sll rX, rY, ( rZ | sa ) */
    TY_SRA,     /* sra rX, rY, ( rZ | sa ) */
    TY_SRL,     /* srl rX, rY, ( rZ | sa ) */
    TY_SLT,     /* slt rX, rY, ( rZ | simm16 ) */
    TY_SLTU,    /* sltu rX, rY, ( rZ | simm16 ) */
    TY_SYSCALL, /* syscall */
};

struct Instr {
    char *mne;
    int type;
    unsigned iword;
} instr_tab[] = { /* sorted for bsearch() */
    { "add",    TY_ADD, 0                       },
    { "addu",   TY_ADDU,0                       },
    { "and",    TY_AND, 0                       },
    { "b",      TY_B,   0                       },
    { "bal",    TY_BAL, 0                       },
    { "beq",    TY_I3,  OPC(0x04)               },
    { "bge",    TY_BGE, 0                       },
    { "bgeu",   TY_BGEU,0                       },
    { "bgez",   TY_I4,  OPC(0x01)|RT(0x01)      },
    { "bgezal", TY_I4,  OPC(0x01)|RT(0x11)      },
    { "bgt",    TY_BGT, 0                       },
    { "bgtu",   TY_BGTU,0                       },
    { "bgtz",   TY_I4,  OPC(0x07)               },
    { "ble",    TY_BLE, 0                       },
    { "bleu",   TY_BLEU,0                       },
    { "blez",   TY_I4,  OPC(0x06)               },
    { "blt",    TY_BLT, 0                       },
    { "bltu",   TY_BLTU,0                       },
    { "bltz",   TY_I4,  OPC(0x01)               },
    { "bltzal", TY_I4,  OPC(0x01)|RT(0x10)      },
    { "bne",    TY_I3,  OPC(0x05)               },
    { "clear",  TY_CLEAR,0                      },
    { "div",    TY_R2,  FUNCT(0x1A)             },
    { "divd",   TY_DIVD,0                       },
    { "divdu",  TY_DIVDU,0                      },
    { "divu",   TY_R2,  FUNCT(0x1B)             },
    { "j",      TY_J,   OPC(0x02)               },
    { "jal",    TY_J,   OPC(0x03)               },
    { "jalr",   TY_R3,  FUNCT(0x09)             },
    { "jr",     TY_R5,  FUNCT(0x08)             },
    { "la",     TY_LA,  0                       },
    { "lb",     TY_I5,  OPC(0x20)               },
    { "lbu",    TY_I5,  OPC(0x24)               },
    { "lh",     TY_I5,  OPC(0x21)               },
    { "lhu",    TY_I5,  OPC(0x25)               },
    { "li",     TY_LI,  0                       },
    { "lui",    TY_I2,  OPC(0x0F)               },
    { "lw",     TY_I5,  OPC(0x23)               },
    { "lwl",    TY_I5,  OPC(0x22)               },
    { "lwr",    TY_I5,  OPC(0x26)               },
    { "mfhi",   TY_R4,  FUNCT(0x10)             },
    { "mflo",   TY_R4,  FUNCT(0x12)             },
    { "move",   TY_MOVE,0                       },
    { "mthi",   TY_R5,  FUNCT(0x11)             },
    { "mtlo",   TY_R5,  FUNCT(0x13)             },
    { "mul",    TY_R1,  OPC(0x1C)|FUNCT(0x02)   },
    { "mult",   TY_R2,  FUNCT(0x18)             },
    { "multu",  TY_R2,  FUNCT(0x19)             },
    { "neg",    TY_NEG, 0                       },
    { "negu",   TY_NEGU,0                       },
    { "nop",    TY_NOP, 0                       },
    { "nor",    TY_R1,  FUNCT(0x27)             },
    { "not",    TY_NOT, 0                       },
    { "or",     TY_OR,  0                       },
    { "rem",    TY_REM, 0                       },
    { "remu",   TY_REMU,0                       },
    { "sb",     TY_I5,  OPC(0x28)               },
    { "seb",    TY_R6,  OPC(0x1F)|SHAMT(0x10)|FUNCT(0x20) },
    { "seh",    TY_R6,  OPC(0x1F)|SHAMT(0x18)|FUNCT(0x20) },
    { "seq",    TY_SEQ, 0                       },
    { "sge",    TY_SGE, 0                       },
    { "sgeu",   TY_SGEU,0                       },
    { "sgt",    TY_SGT, 0                       },
    { "sgtu",   TY_SGTU,0                       },
    { "sh",     TY_I5,  OPC(0x29)               },
    { "sle",    TY_SLE, 0                       },
    { "sleu",   TY_SLEU,0                       },
    { "sll",    TY_SLL, 0                       },
    { "slt",    TY_SLT, 0                       },
    { "sltu",   TY_SLTU,0                       },
    { "sne",    TY_SNE, 0                       },
    { "sra",    TY_SRA, 0                       },
    { "srl",    TY_SRL, 0                       },
    { "sub",    TY_R1,  FUNCT(0x22)             },
    { "subu",   TY_R1,  FUNCT(0x23)             },
    { "sw",     TY_I5,  OPC(0x2B)               },
    { "swl",    TY_I5,  OPC(0x2A)               },
    { "swr",    TY_I5,  OPC(0x2E)               },
    { "syscall",TY_SYSCALL,FUNCT(0x0C)          },
    { "ulh",    TY_ULH, 0                       },
    { "ulhu",   TY_ULHU,0                       },
    { "ulw",    TY_ULW, 0                       },
    { "ush",    TY_USH, 0                       },
    { "usw",    TY_USW, 0                       },
    { "xor",    TY_XOR, 0                       },
};

int cmpi(const void *p1, const void *p2)
{
    Instr *x1 = (Instr *)p1;
    Instr *x2 = (Instr *)p2;

    return strcmp(x1->mne, x2->mne);
}

Instr *lookup_instr(char *mne)
{
    Instr key, *res;

    key.mne = mne;
    res = bsearch(&key, instr_tab, NELEMS(instr_tab), sizeof(instr_tab[0]), cmpi);
    return res;
}

MIPSReg get_reg_val(char *s)
{
    int val;
    unsigned n;

    n = strlen(s+1);
    if (n==0 || n>2 || !isdigit(s[1]) || n==2&&!isdigit(s[2]) || (val=atoi(s+1))>31)
        ERR("invalid register");
    return val;
}

/* asm_expr = [ "-" ] NUM | ID [ ( "+" | "-" ) NUM ] */
AsmExpr asm_expr(void)
{
    AsmExpr e;
    int neg = FALSE;

    e.lineno = line_number;
    if (curr_tok == TOK_ID) {
        e.op = TOK_ID;
        e.id = strdup(lexeme);
        match(TOK_ID);
        if (curr_tok==TOK_PLUS || curr_tok==TOK_MINUS) {
            e.op = TOK_PLUS;
            neg = curr_tok==TOK_MINUS;
            match(curr_tok);
            if (curr_tok == TOK_NUM)
                e.num = str2int(lexeme);
            match(TOK_NUM);
        } else {
            e.num = 0;
        }
    } else if (curr_tok==TOK_MINUS || curr_tok==TOK_NUM) {
        if (curr_tok == TOK_MINUS) {
            neg = TRUE;
            match(TOK_MINUS);
        }
        e.op = TOK_NUM;
        e.num = str2int(lexeme);
        match(TOK_NUM);
    } else {
        ERR("expecting expression");
    }
    if (neg)
        e.num = -e.num;
    return e;
}

/* operand = REG | asm_expr [ "(" REG ")" ] */
Operand operand(void)
{
    Operand op;

    if (curr_tok == TOK_REG) {
        op.kind = RegKind;
        op.a.reg = get_reg_val(lexeme);
        match(TOK_REG);
    } else {
        AsmExpr e;

        e = asm_expr();
        if (curr_tok == TOK_LPAREN) {
            op.kind = RegOffKind;
            op.a.regoff.offs = e;
            match(TOK_LPAREN);
            op.a.regoff.base = get_reg_val(lexeme);
            match(TOK_REG);
            match(TOK_RPAREN);
        } else {
            op.kind = ExprKind;
            op.a.expr = e;
        }
    }
    return op;
}

/*
 * directive = "%" ( "segment" | "section" ) id |
 *             "%" "extern" id { "," id } |
 *             "%" "global" id { "," id } |
 *             "%" "align"  NUM |
 *             "%" "alignb" NUM |
 *             "%" "res" NUM |
 *             "%" "byte"  asm_expr { "," asm_expr } |
 *             "%" "word"  asm_expr { "," asm_expr } |
 *             "%" "dword" asm_expr { "," asm_expr } |
 *             "%" "zero" NUM
 */
void directive(void)
{
    match(TOK_PERC);
    if (equal(lexeme, "segment") || equal(lexeme, "section")) {
        match(TOK_ID);
        if (curr_tok == TOK_ID)
            set_curr_section(lexeme);
        match(TOK_ID);
    } else if (equal(lexeme, "extern")) {
        match(TOK_ID);
        if (curr_tok == TOK_ID)
            define_symbol(OtherKind, ExternBind, lexeme, 0, NULL);
        match(TOK_ID);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            if (curr_tok == TOK_ID);
                define_symbol(OtherKind, ExternBind, lexeme, 0, NULL);
            match(TOK_ID);
        }
    } else if (equal(lexeme, "global")) {
        match(TOK_ID);
        if (curr_tok == TOK_ID)
            define_symbol(OtherKind, GlobalBind, lexeme, 0, NULL);
        match(TOK_ID);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            if (curr_tok == TOK_ID);
                define_symbol(OtherKind, GlobalBind, lexeme, 0, NULL);
            match(TOK_ID);
        }
    } else if (equal(lexeme, "align")) {
        match(TOK_ID);
        if (curr_tok == TOK_NUM) {
            int nb, arg;

            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            arg = str2int(lexeme);
            if (!is_po2(arg))
                ERR("section alignment `%d' is not power of two", arg);
            for (nb = round_up(LC(), arg)-LC(); nb; nb--)
                write_byte(0);
        }
        match(TOK_NUM);
    } else if (equal(lexeme, "alignb")) {
        match(TOK_ID);
        if (curr_tok == TOK_NUM) {
            int nb, arg;

            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            arg = str2int(lexeme);
            if (!is_po2(arg))
                ERR("section alignment `%d' is not power of two", arg);
            nb = round_up(LC(), arg)-LC();
            curr_section->siz += nb;
        }
        match(TOK_NUM);
    } else if (equal(lexeme, "res")) {
        match(TOK_ID);
        if (curr_tok == TOK_NUM) {
            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            curr_section->siz += str2int(lexeme);
        }
        match(TOK_NUM);
    } else if (equal(lexeme, "byte")) {
        AsmExpr *e;

        match(TOK_ID);
        if (curr_section == NULL)
            set_curr_section(DEF_SEC);
        write_byte(0);
        new_unr_expr((*(e=new_asmexpr())=asm_expr(), e), DEST_BYTE, LC()-1, curr_section, FALSE);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            write_byte(0);
            new_unr_expr((*(e=new_asmexpr())=asm_expr(), e), DEST_BYTE, LC()-1, curr_section, FALSE);
        }
    } else if (equal(lexeme, "word")) {
        AsmExpr *e;

        match(TOK_ID);
        if (curr_section == NULL)
            set_curr_section(DEF_SEC);
        write_word(0);
        new_unr_expr((*(e=new_asmexpr())=asm_expr(), e), DEST_WORD, LC()-2, curr_section, FALSE);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            write_word(0);
            new_unr_expr((*(e=new_asmexpr())=asm_expr(), e), DEST_WORD, LC()-2, curr_section, FALSE);
        }
    } else if (equal(lexeme, "dword")) {
        AsmExpr *e;

        match(TOK_ID);
        if (curr_section == NULL)
            set_curr_section(DEF_SEC);
        write_dword(0);
        new_unr_expr((*(e=new_asmexpr())=asm_expr(), e), DEST_DWORD, LC()-4, curr_section, FALSE);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            write_dword(0);
            new_unr_expr((*(e=new_asmexpr())=asm_expr(), e), DEST_DWORD, LC()-4, curr_section, FALSE);
        }
    } else if (equal(lexeme, "zero")) {
        match(TOK_ID);
        if (curr_tok == TOK_NUM) {
            int n;

            for (n = str2int(lexeme); n > 0; n--)
                write_byte(0);
        }
        match(TOK_NUM);
    } else {
        ERR("unknown directive `%s'", lexeme);
    }
}

/* label = ID ":" */
void label(char *id)
{
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    define_symbol(OtherKind, LocalBind, id, LC(), curr_section);
    match(TOK_COLON);
}

void ierr(char *instr)
{
    --line_number;
    ERR("invalid operands to instruction `%s'", instr);
}

/* source_line = instruction [ operand [ "," operand [ "," operand ] ] ] EOL */
void source_line(char *instr)
{
    int no;
    Instr *i;
    Operand op1, op2, op3;
    unsigned iword;

    no = 0;
    if (curr_tok != TOK_EOL) {
        op1 = operand();
        ++no;
        if (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            op2 = operand();
            ++no;
            if (curr_tok == TOK_COMMA) {
                match(TOK_COMMA);
                op3 = operand();
                ++no;
            }
        }
    }
    match(TOK_EOL);

    if ((i=lookup_instr(instr)) == NULL)
        ERR("unknown instruction `%s'", instr);
    iword = i->iword;

#define OPS1R()   ((no!=1 || op1.kind!=RegKind) ? ierr(i->mne) : 0)
#define OPS2R()   ((no!=2 || op1.kind!=RegKind || op2.kind!=RegKind) ? ierr(i->mne) : 0)
#define OPS3R()   ((no!=3 || op1.kind!=RegKind || op2.kind!=RegKind || op3.kind!=RegKind) ? ierr(i->mne) : 0)
#define OPS3RI()  ((no!=3 || op1.kind!=RegKind || op2.kind!=RegKind || op3.kind==RegOffKind) ? ierr(i->mne) : 0)
#define OPS2R1I() ((no!=3 || op1.kind!=RegKind || op2.kind!=RegKind || op3.kind!=ExprKind) ? ierr(i->mne) : 0)

    switch (i->type) {
    /*                     */
    /* R-type instructions */
    /*                     */
    case TY_R1: /* op rd, rs, rt */
        OPS3R();
        write_dword(iword+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        break;

    case TY_R2: /* op rs, rt */
        OPS2R();
        write_dword(iword+RS(op1.a.reg)+RT(op2.a.reg));
        break;

    case TY_R3: /* op rd, rs (rd == $31 if missing) */
        if (no == 1) {
            if (op1.kind == RegKind) { /* op rs */
                write_dword(iword+RS(op1.a.reg)+RD(31));
                break;
            }
        } else if (no == 2) { /* op rd, rs */
            if (op1.kind==RegKind && op2.kind==RegKind) {
                write_dword(iword+RD(op1.a.reg)+RS(op2.a.reg));
                break;
            }
        }
        ierr(i->mne);

    case TY_R4: /* op rd */
        OPS1R();
        write_dword(iword+RD(op1.a.reg));
        break;

    case TY_R5: /* op rs */
        OPS1R();
        write_dword(iword+RS(op1.a.reg));
        break;

    case TY_R6: /* op rd, rt */
        OPS2R();
        write_dword(iword+RD(op1.a.reg)+RT(op2.a.reg));
        break;

    /*                     */
    /* I-type instructions */
    /*                     */
    case TY_I1: /* op rt, rs, imm */
        OPS2R1I();
        write_dword(iword+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
        new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        break;

    case TY_I2: /* op rt, imm */
        if (no!=2 || op1.kind!=RegKind || op2.kind!=ExprKind)
            ierr(i->mne);
        write_dword(iword+RT(op1.a.reg)+IMM(0));
        new_unr_expr(&op2.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        break;

    case TY_I3: /* op rs, rt, offset */
        OPS2R1I();
        write_dword(iword+RS(op1.a.reg)+RT(op2.a.reg)+IMM(0));
        new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, TRUE);
        break;

    case TY_I4: /* op rs, offset */
        if (no!=2 || op1.kind!=RegKind || op2.kind!=ExprKind)
            ierr(i->mne);
        write_dword(iword+RS(op1.a.reg)+IMM(0));
        new_unr_expr(&op2.a.expr, DEST_LO16, LC()-4, curr_section, TRUE);
        break;

    case TY_I5: /* op rt, [ offset(rs) | offset ] */
        if (no==2 && op1.kind==RegKind) {
            if (op2.kind == RegOffKind) { /* op rt, offset(rs) */
                write_dword(iword+RT(op1.a.reg)+RS(op2.a.regoff.base)+IMM(0));
                new_unr_expr(&op2.a.regoff.offs, DEST_LO16, LC()-4, curr_section, FALSE);
                break;
            } else if (op2.kind == ExprKind) { /* op rt, offset => lui $1, offset>>16 ; op rt, (offset0xFFFF)($1) */
                write_dword(OPC(0x0F)+RT(1)+IMM(0));
                new_unr_expr(&op2.a.expr, DEST_HI16, LC()-4, curr_section, FALSE);
                write_dword(iword+RT(op1.a.reg)+RS(1)+IMM(0));
                new_unr_expr(&op2.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
                break;
            }
        }
        ierr(i->mne);

    /*                     */
    /* J-type instructions */
    /*                     */
    case TY_J: /* op addr */
        if (no!=1 || op1.kind!=ExprKind)
            ierr(i->mne);
        write_dword(iword+ADDR(0));
        new_unr_expr(&op1.a.expr, DEST_ADDR26, LC()-4, curr_section, FALSE);
        break;

    /*                     */
    /* Syscall instruction */
    /*                     */
    case TY_SYSCALL: /* syscall */
        write_dword(iword);
        break;

    /*                     */
    /* Pseudo instructions */
    /*                     */
    case TY_NOP:     /* nop => sll $0, $0, 0 */
        if (no != 0)
            ierr(i->mne);
        write_dword(0);
        break;

    case TY_MOVE:    /* move rX, rY => add rX, rY, $0 */
        OPS2R();
        write_dword(FUNCT(0x20)+RD(op1.a.reg)+RS(op2.a.reg)+RT(0));
        break;

    case TY_CLEAR:   /* clear rX    => add rX, $0, $0 */
        OPS1R();
        write_dword(FUNCT(0x20)+RD(op1.a.reg)+RS(0)+RT(0));
        break;

    case TY_NOT:     /* not rX, rY  => nor rX, rY, $0 */
        OPS2R();
        write_dword(FUNCT(0x27)+RD(op1.a.reg)+RS(op2.a.reg)+RT(0));
        break;

    case TY_NEG:     /* neg rX, rY  => sub rX, $0, rY */
        OPS2R();
        write_dword(FUNCT(0x22)+RD(op1.a.reg)+RS(0)+RT(op2.a.reg));
        break;

    case TY_NEGU:    /* negu rX, rY => subu rX, $0, rY */
        OPS2R();
        write_dword(FUNCT(0x23)+RD(op1.a.reg)+RS(0)+RT(op2.a.reg));
        break;

    case TY_LA:     /* la rX, label => lui rX, label>>16 ; addiu rX, rX, label0xFFFF */
        if (no!=2 || op1.kind!=RegKind || op2.kind!=ExprKind || op2.a.expr.op==TOK_NUM)
            ierr(i->mne);
        write_dword(OPC(0x0F)+RT(op1.a.reg)+IMM(0));
        new_unr_expr(&op2.a.expr, DEST_HI16, LC()-4, curr_section, FALSE);
        write_dword(OPC(0x09)+RT(op1.a.reg)+RS(op1.a.reg)+IMM(0));
        new_unr_expr(&op2.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        break;

    case TY_LI: {    /* li rX, imm16/32 */
        int imm;

        if (no!=2 || op1.kind!=RegKind || op2.kind!=ExprKind || op2.a.expr.op!=TOK_NUM)
            ierr(i->mne);
        imm = op2.a.expr.num;
        if (imm >= 0) {
            if (imm <= 65535) { /* => ori rX, $0, imm16 */
                write_dword(OPC(0x0D)+RT(op1.a.reg)+RS(0)+IMM(imm));
                break;
            }
        } else if (imm >= -32768) { /* addiu rX, $0, imm16  */
            write_dword(OPC(0x09)+RT(op1.a.reg)+RS(0)+IMM(imm));
            break;
        }
        /* => lui rX, imm32>>16 ; ori rX, rX, imm32&0xFFFF */
        write_dword(OPC(0x0F)+RT(op1.a.reg)+IMM(imm>>16));
        write_dword(OPC(0x0D)+RT(op1.a.reg)+RS(op1.a.reg)+IMM(imm&0xFFFF));
    }
        break;

    case TY_B:       /* b offset            => beq $0, $0, offset */
        if (no!=1 || op1.kind!=ExprKind)
            ierr(i->mne);
        write_dword(OPC(0x04)+RS(0)+RT(0)+IMM(0));
        new_unr_expr(&op1.a.expr, DEST_LO16, LC()-4, curr_section, TRUE);
        break;

    case TY_BAL:     /* bal offset          => bgezal $0, offset */
        if (no!=1 || op1.kind!=ExprKind)
            ierr(i->mne);
        write_dword(OPC(0x01)|RT(0x11)+RS(0)+IMM(0));
        new_unr_expr(&op1.a.expr, DEST_LO16, LC()-4, curr_section, TRUE);
        break;

    case TY_BGT:     /* bgt rX, rY, offset  => slt $1, rY, rX ; bne $1, $0, offset */
    case TY_BLT:     /* blt rX, rY, offset  => slt $1, rX, rY ; bne $1, $0, offset */
    case TY_BGTU:    /* bgtu rX, rY, offset => sltu $1, rY, rX ; bne $1, $0, offset */
    case TY_BLTU:    /* bltu rX, rY, offset => sltu $1, rX, rY ; bne $1, $0, offset */
        OPS2R1I();
        switch (i->type) {
        case TY_BGT:  write_dword(FUNCT(0x2A)+RD(1)+RS(op2.a.reg)+RT(op1.a.reg)); break;
        case TY_BLT:  write_dword(FUNCT(0x2A)+RD(1)+RS(op1.a.reg)+RT(op2.a.reg)); break;
        case TY_BGTU: write_dword(FUNCT(0x2B)+RD(1)+RS(op2.a.reg)+RT(op1.a.reg)); break;
        case TY_BLTU: write_dword(FUNCT(0x2B)+RD(1)+RS(op1.a.reg)+RT(op2.a.reg)); break;
        }
        write_dword(OPC(0x05)+RS(1)+RT(0)+IMM(0));
        new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, TRUE);
        break;

    case TY_BGE:     /* bge rX, rY, offset  => slt $1, rX, rY ; beq $1, $0, offset */
    case TY_BLE:     /* ble rX, rY, offset  => slt $1, rY, rX ; beq $1, $0, offset */
    case TY_BGEU:    /* bgeu rX, rY, offset => sltu $1, rX, rY ; beq $1, $0, offset */
    case TY_BLEU:    /* bleu rX, rY, offset => sltu $1, rY, rX ; beq $1, $0, offset */
        OPS2R1I();
        switch (i->type) {
        case TY_BGE:  write_dword(FUNCT(0x2A)+RD(1)+RS(op1.a.reg)+RT(op2.a.reg)); break;
        case TY_BLE:  write_dword(FUNCT(0x2A)+RD(1)+RS(op2.a.reg)+RT(op1.a.reg)); break;
        case TY_BGEU: write_dword(FUNCT(0x2B)+RD(1)+RS(op1.a.reg)+RT(op2.a.reg)); break;
        case TY_BLEU: write_dword(FUNCT(0x2B)+RD(1)+RS(op2.a.reg)+RT(op1.a.reg)); break;
        }
        write_dword(OPC(0x04)+RS(1)+RT(0)+IMM(0));
        new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, TRUE);
        break;

    case TY_DIVD:    /* divd rX, rY, rZ => div rY, rZ ; mflo rX */
    case TY_REM:     /* rem rX, rY, rZ => div rY, rZ ; mfhi rX */
        OPS3R();
        write_dword(FUNCT(0x1A)+RS(op2.a.reg)+RT(op3.a.reg));
        write_dword(FUNCT(i->type==TY_DIVD?0x12:0x10)+RD(op1.a.reg));
        break;

    case TY_DIVDU:   /* divdu rX, rY, rZ => divu rY, rZ ; mflo rX */
    case TY_REMU:    /* remu rX, rY, rZ => divu rY, rZ ; mfhi rX */
        OPS3R();
        write_dword(FUNCT(0x1B)+RS(op2.a.reg)+RT(op3.a.reg));
        write_dword(FUNCT(i->type==TY_DIVDU?0x12:0x10)+RD(op1.a.reg));
        break;

    case TY_SEQ:     /* seq rX, rY, ( rZ | uimm16 )  */
        OPS3RI();
        if (op3.kind == RegKind) { /* => xor rX, rY, rZ ; sltiu rX, rX, 1 */
            write_dword(FUNCT(0x26)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* => xori rX, rY, imm16 ; sltiu rX, rX, 1 */
            write_dword(OPC(0x0E)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        write_dword(OPC(0x0B)+RT(op1.a.reg)+RS(op1.a.reg)+IMM(1));
        break;

    case TY_SNE:     /* sne rX, rY, ( rZ | uimm16 )  */
        OPS3RI();
        if (op3.kind == RegKind) { /* => xor rX, rY, rZ ; sltu rX, $0, rX */
            write_dword(FUNCT(0x26)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* => xori rX, rY, imm16 ; sltu rX, $0, rX */
            write_dword(OPC(0x0E)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        write_dword(FUNCT(0x2B)+RD(op1.a.reg)+RS(0)+RT(op1.a.reg));
        break;

    case TY_SGT:     /* sgt rX, rY, rZ => slt rX, rZ, rY */
        OPS3R();
        write_dword(FUNCT(0x2A)+RD(op1.a.reg)+RS(op3.a.reg)+RT(op2.a.reg));
        break;

    case TY_SGE:     /* sge rX, rY, ( rZ | simm16 )  */
        OPS3RI();
        if (op3.kind == RegKind) { /* => slt rX, rY, rZ ; xori rX, rX, 1 */
            write_dword(FUNCT(0x2A)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* => slti rX, rY, imm16 ; xori rX, rX, 1 */
            write_dword(OPC(0x0A)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        write_dword(OPC(0x0E)+RT(op1.a.reg)+RS(op1.a.reg)+IMM(1));
        break;

    case TY_SLE:     /* sle rX, rY, rZ => slt rX, rZ, rY ; xori rX, rX, 1 */
        OPS3R();
        write_dword(FUNCT(0x2A)+RD(op1.a.reg)+RS(op3.a.reg)+RT(op2.a.reg));
        write_dword(OPC(0x0E)+RT(op1.a.reg)+RS(op1.a.reg)+IMM(1));
        break;

    case TY_SGTU:    /* sgtu rX, rY, rZ => sltu rX, rZ, rY */
        OPS3R();
        write_dword(FUNCT(0x2B)+RD(op1.a.reg)+RS(op3.a.reg)+RT(op2.a.reg));
        break;

    case TY_SGEU:    /* sgeu rX, rY, ( rZ | simm16 ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* => sltu rX, rY, rZ ; xori rX, rX, 1 */
            write_dword(FUNCT(0x2B)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* => sltiu rX, rY, imm ; xori rX, rX, 1 */
            write_dword(OPC(0x0B)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        write_dword(OPC(0x0E)+RT(op1.a.reg)+RS(op1.a.reg)+IMM(1));
        break;

    case TY_SLEU:    /* sleu rX, rY, rZ => sltu rX, rZ, rY ; xori rX, rX, 1 */
        OPS3R();
        write_dword(FUNCT(0x2B)+RD(op1.a.reg)+RS(op3.a.reg)+RT(op2.a.reg));
        write_dword(OPC(0x0E)+RT(op1.a.reg)+RS(op1.a.reg)+IMM(1));
        break;

    case TY_ADD:     /* add rX, rY, ( rZ | simm16 ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* add */
            write_dword(FUNCT(0x20)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* addi */
            write_dword(OPC(0x08)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        break;

    case TY_ADDU:    /* addu rX, rY, ( rZ | simm16 ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* addu */
            write_dword(FUNCT(0x21)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* addiu */
            write_dword(OPC(0x09)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        break;

    case TY_AND:     /* and rX, rY, ( rZ | uimm16 ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* and */
            write_dword(FUNCT(0x24)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* andi */
            write_dword(OPC(0x0C)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        break;

    case TY_OR:      /* or rX, rY, ( rZ | uimm16 ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* or */
            write_dword(FUNCT(0x25)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* ori */
            write_dword(OPC(0x0D)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        break;

    case TY_XOR:     /* xor rX, rY, ( rZ | uimm16 ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* xor */
            write_dword(FUNCT(0x26)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* xori */
            write_dword(OPC(0x0E)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        break;

    case TY_SLL:     /* sll rX, rY, ( rZ | sa ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* sllv */
            write_dword(FUNCT(0x04)+RD(op1.a.reg)+RT(op2.a.reg)+RS(op3.a.reg));
        } else { /* sll */
            if (op3.a.expr.op != TOK_NUM)
                ierr(i->mne);
            write_dword(FUNCT(0x00)+RD(op1.a.reg)+RT(op2.a.reg)+SHAMT(op3.a.expr.num));
        }
        break;

    case TY_SRA:     /* sra rX, rY, ( rZ | sa ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* srav */
            write_dword(FUNCT(0x07)+RD(op1.a.reg)+RT(op2.a.reg)+RS(op3.a.reg));
        } else { /* sra */
            if (op3.a.expr.op != TOK_NUM)
                ierr(i->mne);
            write_dword(FUNCT(0x03)+RD(op1.a.reg)+RT(op2.a.reg)+SHAMT(op3.a.expr.num));
        }
        break;

    case TY_SRL:     /* srl rX, rY, ( rZ | sa ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* srlv */
            write_dword(FUNCT(0x06)+RD(op1.a.reg)+RT(op2.a.reg)+RS(op3.a.reg));
        } else { /* srl */
            if (op3.a.expr.op != TOK_NUM)
                ierr(i->mne);
            write_dword(FUNCT(0x02)+RD(op1.a.reg)+RT(op2.a.reg)+SHAMT(op3.a.expr.num));
        }
        break;

    case TY_SLT: /* slt rX, rY, ( rZ | simm16 ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* slt */
            write_dword(FUNCT(0x2A)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* slti */
            write_dword(OPC(0x0A)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        break;

    case TY_SLTU: /* sltu rX, rY, ( rZ | simm16 ) */
        OPS3RI();
        if (op3.kind == RegKind) { /* sltu */
            write_dword(FUNCT(0x2B)+RD(op1.a.reg)+RS(op2.a.reg)+RT(op3.a.reg));
        } else { /* sltiu */
            write_dword(OPC(0x0B)+RT(op1.a.reg)+RS(op2.a.reg)+IMM(0));
            new_unr_expr(&op3.a.expr, DEST_LO16, LC()-4, curr_section, FALSE);
        }
        break;

    /* TODO */
    case TY_ULH:     /* ulh rX, offset(rY) */
    case TY_ULHU:    /* ulhu rX, offset(rY) */
    case TY_ULW:     /* ulw rX, offset(rY) */
    case TY_USH:     /* ush rX, offset(rY) */
    case TY_USW:     /* usw rX, offset(rY) */
    default:
        assert(0);
        break;
    }

#undef OPS3R
#undef OPS2R
#undef OPS1R
#undef OPS2R1I
}

/* line = label | source_line | directive */
void line(void)
{
    if (curr_tok == TOK_PERC) {
        directive();
    } else if (curr_tok == TOK_ID) {
        char tmp[MAX_LEXEME];

        strcpy(tmp, lexeme);
        match(TOK_ID);
        if (curr_tok == TOK_COLON)
            label(tmp);
        else
            source_line(tmp);
    } else if (curr_tok == TOK_EOL) {
        match(TOK_EOL);
    } else {
        ERR("expecting label, instruction, or directive");
    }
}

/* program = line { line } EOF */
void program(void)
{
    line();
    while (curr_tok != TOK_EOF)
        line();
}

void write_ELF_file(void)
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
        sec->hdr.sh_name = strtab_append(shstrtab, sec->name);
        if (sec->relocs != NULL) {
            char rsname[64] = ".rel";

            strcat(rsname, sec->name);
            sec->rhdr.sh_name = strtab_append(shstrtab, strdup(rsname));
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
        if (sec->name[0] == '.') {
            if (equal(sec->name, ".text")) {
                sec->hdr.sh_type = SHT_PROGBITS;
                sec->hdr.sh_flags = SHF_ALLOC|SHF_EXECINSTR;
                sec->hdr.sh_addralign = 16;
            } else if (equal(sec->name, ".data")) {
                sec->hdr.sh_type = SHT_PROGBITS;
                sec->hdr.sh_flags = SHF_ALLOC|SHF_WRITE;
                sec->hdr.sh_addralign = 4;
            } else if (equal(sec->name, ".rodata")) {
                sec->hdr.sh_type = SHT_PROGBITS;
                sec->hdr.sh_flags = SHF_ALLOC;
                sec->hdr.sh_addralign = 4;
            } else if (equal(sec->name, ".bss")) {
                sec->hdr.sh_type = SHT_NOBITS;
                sec->hdr.sh_flags = SHF_ALLOC|SHF_WRITE;
                sec->hdr.sh_addralign = 4;
                ALIGN(4);
                sec->hdr.sh_offset = curr;
                sec->hdr.sh_size = sec->siz;
                ++elf_header.e_shnum;
                continue;
            } else {
                fprintf(stderr, "%s: warning: unknown system section `%s': defaulting attributes\n",
                prog_name, sec->name);
                goto unk_sec;
            }
        } else {
unk_sec:    sec->hdr.sh_type = SHT_PROGBITS;
            sec->hdr.sh_flags = SHF_ALLOC;
            sec->hdr.sh_addralign = 1;
        }
        ALIGN(4);
        sec->hdr.sh_offset = curr;
        sec->hdr.sh_size = sec->siz;
        fwrite(sec->buf, sec->siz, 1, output_file);
        curr += sec->siz;
        ++elf_header.e_shnum;
        if (sec->relocs != NULL) {
            Reloc *r;

            ALIGN(4);
            sec->rhdr.sh_type = SHT_REL;
            sec->rhdr.sh_offset = curr;
            sec->rhdr.sh_link = 2; /* .symtab */
            sec->rhdr.sh_info = sec->shndx;
            sec->rhdr.sh_addralign = 4;
            sec->rhdr.sh_entsize = sizeof(Elf32_Rel);
            for (r = sec->relocs; r != NULL; r = r->next) {
                Elf32_Rel er;

                er.r_offset = r->offs;
                switch (r->type) {
                case RELOC_HI:
                    er.r_info = ELF32_R_INFO(r->sym->ndx, R_MIPS_HI16);
                    break;
                case RELOC_LO:
                    er.r_info = ELF32_R_INFO(r->sym->ndx, R_MIPS_LO16);
                    break;
                case RELOC_PCREL:
                    er.r_info = ELF32_R_INFO(r->sym->ndx, R_MIPS_PC16);
                    break;
                case RELOC_WORD:
                    er.r_info = ELF32_R_INFO(r->sym->ndx, R_MIPS_16);
                    break;
                case RELOC_ADDR:
                    er.r_info = ELF32_R_INFO(r->sym->ndx, R_MIPS_26);
                    break;
                case RELOC_DWORD:
                    er.r_info = ELF32_R_INFO(r->sym->ndx, R_MIPS_32);
                    break;
                }
                fwrite(&er, sizeof(Elf32_Rel), 1, output_file);
                sec->rhdr.sh_size += sizeof(Elf32_Rel);
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
        fwrite(&sec->hdr, sizeof(Elf32_Shdr), 1, output_file);
    for (sec = sections; sec != NULL; sec = sec->next)
        if (sec->relocs != NULL)
            fwrite(&sec->rhdr, sizeof(Elf32_Shdr), 1, output_file);

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
    elf_header.e_machine = EM_MIPS;
    elf_header.e_version = EV_CURRENT;
    elf_header.e_flags = EF_MIPS_NOREORDER|EF_MIPS_CPIC|E_MIPS_ABI_O32;
    elf_header.e_ehsize = sizeof(Elf32_Ehdr);
    elf_header.e_shentsize = sizeof(Elf32_Shdr);
    elf_header.e_shstrndx = 1;
    fwrite(&elf_header, sizeof(Elf32_Ehdr), 1, output_file);
    fseek(output_file, 0, SEEK_END);

#undef ALIGN
}

void dump_section(Section *s)
{
    int i;
    int *p;
    unsigned char *p2;
    Reloc *r;

    printf("Section: %s\n", s->name);
    p = (int *)s->buf;
    for (i = s->siz/4; i; i--) {
        printf("0x%08x\n", *p);
        ++p;
    }
    p2 = (unsigned char *)p;
    switch (s->siz%4) {
    case 1: printf("0x%02x\n", *p2); break;
    case 2: printf("0x%02x%02x\n", *(p2+1), *p2); break;
    case 3: printf("0x%02x%02x%02x\n", *(p2+2), *(p2+1), *p2); break;
    }
    printf("\nRelocations\n");
    for (r = s->relocs; r != NULL; r = r->next)
        printf("type=%d, off=%d, sym=%s\n", r->type, r->offs, r->sym->name);
}

void err_no_input(void)
{
    TERMINATE("%s: no input file", prog_name);
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
            if (argv[i][2] != '\0')
                outpath = argv[i]+2;
            else if (argv[i+1] == NULL)
                TERMINATE("%s: option `o' requires an argument\n", prog_name);
            else
                outpath = argv[++i];
            break;
        case 'h':
            printf("usage: %s [ options ] <input-file>\n"
                   "  The available options are:\n"
                   "    -o<file>    write output to <file>\n"
                   "    -h          print this help\n", prog_name);
            exit(EXIT_SUCCESS);
            break;
        default:
            TERMINATE("%s: unknown option `%s'\n", prog_name, argv[i]);
        }
    }
    if (inpath == NULL)
        err_no_input();
    curr = buf = read_file(inpath);
    curr_tok = get_token();
    program();
    resolve_expressions();
    /*dump_section(curr_section);*/

    if (outpath == NULL) {
        outpath = replace_extension(inpath, ".o");
        output_file = fopen(outpath, "wb");
        free(outpath);
    } else {
        output_file = fopen(outpath, "wb");
    }
    write_ELF_file();
    fclose(output_file);
    if (buf != NULL)
        free(buf);

    return 0;
}

Token get_token(void)
{
    enum {
        START,
        INID,
        INREG,
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
                state = INREG;
            } else if (isalpha(c) || c=='_' || c=='@' || c=='.') {
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
                    tok = TOK_EOF;
                    break;
                case '\n':
                    ++line_number;
                    tok = TOK_EOL;
                    break;
                case ',':
                    tok = TOK_COMMA;
                    break;
                case '%':
                    tok = TOK_PERC;
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
                case '+':
                    tok = TOK_PLUS;
                    break;
                case '-':
                    tok = TOK_MINUS;
                    break;
                default:
                    save = FALSE;
                    state = START;
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
            if (!isalnum(c) && c!='_' && c!='@' && c!='.')
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

        case INREG:
            if (!isdigit(c))
                FINISH(TOK_REG);
            break;

#undef FINISH

        case DONE:
        default:
            assert(0);
            break;
        } /* switch (state) */
        if (save)
            lexeme[cindx++] = (char)c;
        if (state == DONE)
            lexeme[cindx] = '\0';
    }
    return tok;
}

void match(Token expected)
{
    if (curr_tok != expected) {
        if (isprint(lexeme[0]))
            ERR("syntax error: unexpected `%s'", lexeme);
        else
            ERR("syntax error: unexpected `0x%02x'", lexeme[0]);
    }
    curr_tok = get_token();
}

long long str2int(char *s)
{
    char *ep;

    return (long long)strtoull(s, &ep, 0);
}
