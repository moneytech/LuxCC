/*
        Small ARM assembler.

        ASM GRAMMAR
    program = line { line } EOF
    line = label | source_line | directive
    label = ID ":"
    source_line = instruction operand [ "," operand [ "," operand [ "," operand ] ] ] EOL
    operand = NUM |
              ID |
              REG |
              REG "!"
              REG "," shift_spec |
              eff_addr |
              "=" ( NUM | ID ) |
              "{" REG { "," REG } "}"
    shift_spec = "LSL" ( NUM | REG ) |
                 "LSR" ( NUM | REG ) |
                 "ASR" ( NUM | REG ) |
                 "ROR" ( NUM | REG ) |
                 "RRX"
    imm_shift_spec = "LSL" NUM |
                     "LSR" NUM |
                     "ASR" NUM |
                     "ROR" NUM |
                     "RRX"
    eff_addr = "[" REG [ "," [ "-" ] NUM ] "]" [ "!" ] |
               "[" REG "," [ "-" ] REG [ "," imm_shift_spec ] "]" [ "!" ] |
               "[" REG "]" "," [ "-" ] NUM |
               "[" REG "]" "," [ "-" ] REG [ "," imm_shift_spec ]
    directive = "." ( "text" | "data" | "rodata" | "bss" ) |
                "." "extern" id { "," id } |
                "." "global" id { "," id } |
                "." "align"  NUM |
                "." "alignb" NUM |
                "." "res" NUM |
                "." "byte"  NUM { "," NUM } |
                "." "word"  NUM { "," NUM } |
                "." "dword" ( ID [ "+" NUM ] | NUM ) { "," ( ID [ "+" NUM ] | NUM ) } |
                "." "zero" NUM |
                "." "ltorg"

        Tokens
    ID = ( '_' | '@' | [A-Za-z] ) ( '_' | '@' | [0-9A-Za-z] )*
    NUM = '#' ( [0-9]+ | "0x" [A-F0-9]+ )
    REG = 'r' [0-9]+
    Comments start with ";" and extend until the end of the line.
    Prefix a reserved word with a '$' to use it as an identifier.
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
#define BYTE        1
#define WORD        2
#define DWORD       3
#ifndef R_ARM_CALL
#define R_ARM_CALL  28
#endif

typedef struct Operand Operand;
typedef struct EffAddr EffAddr;
typedef struct UnrExpr UnrExpr;
typedef struct Section Section;
typedef struct Symbol Symbol;
typedef struct Reloc Reloc;
typedef struct LitLd LitLd;
typedef int bool;

typedef enum {
    TOK_ID,         TOK_NUM,
    TOK_REG,        TOK_COMMA,
    TOK_COLON,      TOK_EXCL,
    TOK_DOT,        TOK_LSL,
    TOK_LSR,        TOK_ASR,
    TOK_ROR,        TOK_RRX,
    TOK_EQ,         TOK_MINUS,
    TOK_PLUS,
    TOK_LBRACKET,   TOK_RBRACKET,
    TOK_LBRACE,     TOK_RBRACE,
    TOK_EOL,        TOK_EOF
} Token;
#define ISSHIFT(x) ((x)>=TOK_LSL && (x)<=TOK_RRX)

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
int str2int(char *s);

enum {
    AM_Offset,
    AM_PreIndex,
    AM_PostIndex,
};

struct EffAddr {
    char am;           /* addressing mode */
    char ioffs;        /* signals the offset is immediate */
    char down;         /* do base-offset */
    char base;
    union {
        unsigned imm_offs;
        struct {
            short reg;
            short shift;
        } roffs;
    } attr;
};

enum {
    TY_ARI_LOG, /* op Rd, Rn, shifter_operand */
    TY_MOV,     /* op Rd, shifter_operand */
    TY_CMP,     /* op Rn, shifter_operand */
    TY_MUL_1,   /* op Rd, Rm, Rs */
    TY_MUL_2,   /* op Rd, Rm, Rs, Rn */
    TY_MUL_3,   /* op RdLo, RdHi, Rm, Rs */
    TY_BRN,     /* op offset */
    TY_BRNR,    /* op Rm */
    TY_LD_ST_1, /* op[b] Rd, ( mem | =expr ) */
    TY_LD_ST_2, /* op(sb|h|sh) Rd, mem */
    TY_LD_ST_3, /* op Rn[!], reg_list */
    TY_SWP,     /* op Rd, Rm, [Rn] */
    TY_SZX,     /* op Rd, Rm */
};

#define COND(x)     (((x)&0xF) << 28)
#define OPC(x)      (((x)&0xF) << 21)
#define SHIMM(x)    (((x)&0x1F) << 7)
#define OFFS(x)     ((x) & 0xFFF)
#define OFFSH(x)    (((x)&0xF0) << 4)
#define OFFSL(x)    ((x) & 0xF)
#define REGLIST(x)  ((x) & 0xFFFF)
#define Rn(x)       (((x)&0xF) << 16)
#define Rd(x)       (((x)&0xF) << 12)
#define Rs(x)       (((x)&0xF) << 8)
#define Rm(x)       ((x) & 0xF)
#define Mul_Rd(x)   (((x)&0xF) << 16)
#define Mul_Rn(x)   (((x)&0xF) << 12)
#define RdHi(x)     (((x)&0xF) << 16)
#define RdLo(x)     (((x)&0xF) << 12)
/* Data-processing operands */
#define I        (1 << 25)   /* if set shifter_operand is an immediate */
#define S        (1 << 20)   /* if set update flags */
/* Load and Store */
#define P        (1 << 24)   /* if set offset or pre-indexed addressing, else post-indexed addressing */
#define U        (1 << 23)   /* if set base+offset, else base-offset */
#define W        (1 << 21)   /* if set the base register is updated (write-back) */
#define L        (1 << 20)   /* if set Load, else Store */
/* Load and Store Word or Unsigned Byte */
#define B        (1 << 22)   /* if set unsigned byte access, else word access */
/* Miscellaneous Loads and Stores */
#define SIGN     (1 << 6)
#define H        (1 << 5)
/* Data-processing instructions */
#define I_AND   (OPC(0x0))
#define I_EOR   (OPC(0x1))
#define I_SUB   (OPC(0x2))
#define I_RSB   (OPC(0x3))
#define I_ADD   (OPC(0x4))
#define I_ADC   (OPC(0x5))
#define I_SBC   (OPC(0x6))
#define I_RSC   (OPC(0x7))
#define I_TST   (OPC(0x8)|S)
#define I_TEQ   (OPC(0x9)|S)
#define I_CMP   (OPC(0xA)|S)
#define I_CMN   (OPC(0xB)|S)
#define I_ORR   (OPC(0xC))
#define I_MOV   (OPC(0xD))
#define I_BIC   (OPC(0xE))
#define I_MVN   (OPC(0xF))
#define I_SWP   ((1<<24) | (9<<4))

#define CC_EQ   0   /* equal */
#define CC_NE   1   /* not equal */
#define CC_HS   2   /* unsigned higher or same */
#define CC_LO   3   /* unsigned lower */
// #define CC_MI   4   /* negative */
// #define CC_PL   5   /* positive */
// #define CC_VS   6   /* overflow */
// #define CC_VC   7   /* no overflow */
#define CC_HI   8   /* unsigned higher */
#define CC_LS   9   /* unsigned lower or same */
#define CC_GE   10  /* signed greater than or equal */
#define CC_LT   11  /* signed less than */
#define CC_GT   12  /* signed greater than */
#define CC_LE   13  /* signed less than or equal */
#define CC_AL   14  /* always */

typedef enum {
    RegKind,        /* REG */
    NumKind,        /* NUM */
    LabKind,        /* ID */
    WBRegKind,      /* REG! */
    ShRegKind,      /* REG, shift */
    EffAddrKind,
    RegListKind,    /* { reg-list } */
    LitNumKind,     /* =NUM */
    LitLabKind,     /* =ID */
} OpndKind;

struct Operand {
    OpndKind kind;
    union {
        int num;
        char *lab;
        int reg;            /* register/register list */
        struct {
            int reg;
            char kind;      /* LSL, LSR, ASR, ROR, RRX */
            char reg_cnt;   /* -1 when shifted by num */
            char num_cnt;   /* -1 when shifted by reg */
        } shreg;
        EffAddr ea;
    } attr;
};

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
        } else if (bind != GlobalBind) {
            goto redef;
        }
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

struct LitLd {
    int lit_offs;
    int ldr_offs;
    char *lab;
    LitLd *next;
} *literal_loads;

/*
 * TODO/TOIMPROVE:
 *   Check if a literal is already
 *   in the pool before adding it.
 *   Reduce code size / # of relocations.
 */
struct {
    uint32_t *buf;
    unsigned siz, max;
} lit_pool;

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

#define DEF_SEC ".text"
#define LC()    (curr_section->siz)

void set_curr_section(char *name)
{
    Section *s;
    static Elf32_Half shndx = 4; /* [0]=UND, [1]=.shstrtab, [2]=.symtab, [3]=.strtab */

    assert(lit_pool.siz == 0); /* TBD (dump before changing?) */

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
    RELOC_PC,
    RELOC_ABS,
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

struct UnrExpr {
    char *lab;
    struct {
        int offs;
        Section *sec;
    } loc;          /* sec+offs is where the expression is located */
    bool reldisp;   /* is the expression a relative displacement? */
    int lineno;
    UnrExpr *next;
} *unresolved_expressions_list;

void new_unr_expr(char *lab, int offs, Section *sec, bool reldisp)
{
    UnrExpr *n;

    n = malloc(sizeof(UnrExpr));
    n->lab = lab;
    n->loc.offs = offs;
    n->loc.sec = sec;
    n->reldisp = reldisp;
    n->lineno = line_number-1;
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
        void *dest;

        r = NULL;
        if ((s=lookup_symbol(n->lab)) == NULL)
            ERR2(n, "symbol `%s' undefined", n->lab);
        dest = n->loc.sec->buf+n->loc.offs;

        if (n->reldisp) {
            if (s->bind==ExternBind || !equal(s->sec->name, n->loc.sec->name)) {
                if (s->bind != ExternBind)
                    s = s->sec->sym; /* relocate with respect to section */
                *(uint32_t *)dest |= (-8>>2)&0xFFFFFF;
                r = new_reloc(RELOC_PC, n->loc.offs, s);
            } else { /* target is absolute or relocatable with respect to this same section */
                *(uint32_t *)dest |= ((s->val-(n->loc.offs+8))>>2)&0xFFFFFF;
            }
        } else {
            *(uint32_t *)dest += s->val;
            if (s->bind != ExternBind)
                s = s->sec->sym; /* relocate with respect to section */
            r = new_reloc(RELOC_ABS, n->loc.offs, s);
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

int lit_pool_write(uint32_t v)
{
    if (lit_pool.siz >= lit_pool.max) {
        lit_pool.max *= 2;
        lit_pool.buf = realloc(lit_pool.buf, lit_pool.max*sizeof(uint32_t));
        assert(lit_pool.buf != NULL);
    }
    lit_pool.buf[lit_pool.siz] = v;
    return lit_pool.siz++*4;
}

void new_litld(int lit_offs, int ldr_offs, char *lab)
{
    LitLd *p;

    p = malloc(sizeof(LitLd));
    p->lit_offs = lit_offs;
    p->ldr_offs = ldr_offs;
    p->lab = lab;
    p->next = NULL;
    if (literal_loads == NULL) {
        literal_loads = p;
    } else {
        LitLd *t;

        for (t = literal_loads; t->next != NULL; t = t->next)
            ;
        t->next = p;
    }
}

void lit_pool_dump(void)
{
    int i;
    LitLd *p, *t;
    int lp_start, offs;
    uint32_t *ip;

    if (curr_section == NULL)
        return;

    i = 0;
    lp_start = LC();
    for (p = literal_loads; p != NULL; ) {
        if ((offs=(lp_start+p->lit_offs)-(p->ldr_offs+8))<=-4096 || offs>=4096) {
            --line_number;
            ERR("literal pool too away from `ldr Rd,=' pseudo-instruction");
        }
        ip = (uint32_t *)(curr_section->buf+p->ldr_offs);
        if (offs > 0)
            *ip |= OFFS(offs)|U;
        else
            *ip |= OFFS(-offs);
        if (p->lab != NULL)
            new_unr_expr(p->lab, LC(), curr_section, FALSE);
        write_dword(lit_pool.buf[i++]);

        t = p;
        p = p->next;
        free(t);
    }
    literal_loads = NULL;
    assert(i == lit_pool.siz);
    lit_pool.siz = 0;
}

/*
 * directive = "." ( "text" | "data" | "rodata" | "bss" ) |
 *             "." "extern" id { "," id } |
 *             "." "global" id { "," id } |
 *             "." "align"  NUM |
 *             "." "alignb" NUM |
 *             "." "res" NUM |
 *             "." "byte"  NUM { "," NUM } |
 *             "." "word"  NUM { "," NUM } |
 *             "." "dword" ( ID [ "+" NUM ] | NUM ) { "," ( ID [ "+" NUM ] | NUM ) } |
 *             "." "zero" NUM |
 *             "." "ltorg"
 */
void directive(void)
{
    match(TOK_DOT);
    if (equal(lexeme, "text")) {
        match(TOK_ID);
        set_curr_section(".text");
    } else if (equal(lexeme, "data")) {
        match(TOK_ID);
        set_curr_section(".data");
    } else if (equal(lexeme, "rodata")) {
        match(TOK_ID);
        set_curr_section(".rodata");
    } else if (equal(lexeme, "bss")) {
        match(TOK_ID);
        set_curr_section(".bss");
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
    } else if (equal(lexeme, "ltorg")) {
        lit_pool_dump();
        match(TOK_ID);
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
        match(TOK_ID);
        if (curr_section == NULL)
            set_curr_section(DEF_SEC);
        if (curr_tok == TOK_NUM)
            write_byte(str2int(lexeme));
        match(TOK_NUM);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            if (curr_tok == TOK_NUM)
                write_byte(str2int(lexeme));
            match(TOK_NUM);
        }
    } else if (equal(lexeme, "word")) {
        match(TOK_ID);
        if (curr_section == NULL)
            set_curr_section(DEF_SEC);
        if (curr_tok == TOK_NUM)
            write_word(str2int(lexeme));
        match(TOK_NUM);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            if (curr_tok == TOK_NUM)
                write_word(str2int(lexeme));
            match(TOK_NUM);
        }
    } else if (equal(lexeme, "dword")) {
        match(TOK_ID);
        if (curr_section == NULL)
            set_curr_section(DEF_SEC);
        goto first;
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
        first:
            if (curr_tok == TOK_NUM)  {
                write_dword(str2int(lexeme));
                match(TOK_NUM);
            } else {
                if (curr_tok == TOK_ID)
                    new_unr_expr(strdup(lexeme), LC(), curr_section, FALSE);
                match(TOK_ID);
                if (curr_tok == TOK_PLUS) {
                    match(TOK_PLUS);
                    if (curr_tok == TOK_NUM)
                        write_dword(str2int(lexeme));
                    match(TOK_NUM);
                } else {
                    write_dword(0);
                }
            }
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

int get_reg(char *s)
{
    int r;

    if ((r=str2int(s+1))<0 || r>15)
        ERR("invalid register `%s'", s);
    return r;
}

/*
 * shift_spec = "LSL" ( NUM | REG ) |
 *              "LSR" ( NUM | REG ) |
 *              "ASR" ( NUM | REG ) |
 *              "ROR" ( NUM | REG ) |
 *              "RRX"
 */
int shift_spec(void)
{
    int sh;

    /*
        The return value is encoded as follows:

            +----------------+--------------------+----------------------+
            | shifter_is_imm | shift_type (token) | shifter (imm or reg) |
            +----------------+--------------------+----------------------+
           24               16                    8                      0
    */

    switch (curr_tok) {
    case TOK_LSL:
    case TOK_LSR:
    case TOK_ASR:
    case TOK_ROR:
        sh = curr_tok<<8;
        match(curr_tok);
        if (curr_tok == TOK_NUM) {
            sh |= str2int(lexeme)&0xFF;
            sh |= 1<<16;
            match(TOK_NUM);
        } else {
            sh |= get_reg(lexeme);
            match(TOK_REG);
        }
        break;
    case TOK_RRX:
        sh = curr_tok<<8;
        match(TOK_RRX);
        break;
    default:
        assert(0);
    }
    return sh;
}

/*
 * imm_shift_spec = "LSL" NUM |
 *                  "LSR" NUM |
 *                  "ASR" NUM |
 *                  "ROR" NUM |
 *                  "RRX"
 */
int imm_shift_spec(void)
{
    int sh;

    switch (curr_tok) {
    case TOK_LSL:
    case TOK_LSR:
    case TOK_ASR:
    case TOK_ROR:
        sh = curr_tok<<8;
        match(curr_tok);
        if (curr_tok == TOK_NUM)
            sh |= str2int(lexeme)&0xFF;
        match(TOK_NUM);
        break;
    case TOK_RRX:
        sh = curr_tok<<8;
        match(TOK_RRX);
        break;
    default:
        ERR("expecting shift specifier");
    }
    return sh;
}

/*
 * eff_addr = "[" REG [ "," [ "-" ] NUM ] "]" [ "!" ] |
 *            "[" REG "," [ "-" ] REG [ "," imm_shift_spec ] "]" [ "!" ] |
 *            "[" REG "]" "," [ "-" ] NUM |
 *            "[" REG "]" "," [ "-" ] REG [ "," imm_shift_spec ]
 */
EffAddr eff_addr(void)
{
    EffAddr ea;

    memset(&ea, 0, sizeof(EffAddr));
    /* Note: [Rn] is an abbreviation for [Rn, #0] */
    ea.ioffs = TRUE;

    match(TOK_LBRACKET);
    ea.base = get_reg(lexeme);
    match(TOK_REG);
    if (curr_tok == TOK_COMMA) {
        match(TOK_COMMA);
        if (curr_tok == TOK_MINUS) {
            ea.down = TRUE;
            match(TOK_MINUS);
        }
        if (curr_tok == TOK_NUM) {
            ea.attr.imm_offs = str2int(lexeme);
            match(TOK_NUM);
        } else {
            if (curr_tok == TOK_REG) {
                ea.ioffs = FALSE;
                ea.attr.roffs.reg = get_reg(lexeme);
            }
            match(TOK_REG);
            if (curr_tok == TOK_COMMA) {
                match(TOK_COMMA);
                ea.attr.roffs.shift = imm_shift_spec();
            }
        }
        match(TOK_RBRACKET);
        if (curr_tok == TOK_EXCL) {
            ea.am = AM_PreIndex;
            match(TOK_EXCL);
        } else {
            ea.am = AM_Offset;
        }
    } else {
        match(TOK_RBRACKET);
        if (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            if (curr_tok == TOK_MINUS) {
                ea.down = TRUE;
                match(TOK_MINUS);
            }
            if (curr_tok == TOK_NUM) {
                ea.attr.imm_offs = str2int(lexeme);
                match(TOK_NUM);
            } else {
                if (curr_tok == TOK_REG) {
                    ea.ioffs = FALSE;
                    ea.attr.roffs.reg = get_reg(lexeme);
                }
                match(TOK_REG);
                if (curr_tok == TOK_COMMA) {
                    match(TOK_COMMA);
                    ea.attr.roffs.shift = imm_shift_spec();
                }
            }
            ea.am = AM_PostIndex;
        } else if (curr_tok == TOK_EXCL) {
            match(TOK_EXCL);
            ea.am = AM_PreIndex;
        } else {
            ea.am = AM_Offset;
        }
    }
    return ea;
}

int encode_mnemonic(char *mne, int *iword)
{
    int ty;
    bool got_am, got_cond;
    char *tmp;

    got_am = FALSE;
    got_cond = FALSE;
    tmp = mne;

    switch (mne[0]) {
    case 'a':
        switch (mne[1]) {
        case 'd':
            switch (mne[2]) {
            case 'c':
                *iword = I_ADC;
                break;
            case 'd':
                *iword = I_ADD;
                break;
            default:
                return -1;
            }
            break;
        case 'n':
            if (mne[2] != 'd')
                return -1;
            *iword = I_AND;
            break;
        default:
            return -1;
        }
        ty = TY_ARI_LOG;
        mne += 3;
        break;

    case 'b':
        switch (mne[1]) {
        case 'i':
            if (mne[2] != 'c')
                return -1;
            *iword = I_BIC;
            ty = TY_ARI_LOG;
            mne += 3;
            break;
        case 'l':
            if (mne[2] == 'x') {
                *iword = 0x12fff30;
                ty = TY_BRNR;
                mne += 3;
            } else {
                *iword = 5<<25;
                ty = TY_BRN;
                switch (mne[2]) {
                case 't':
                case 'o':
                case 's':
                    ++mne;
                    break;
                case 'e':
                    if (mne[3] != 'q') { /* ble */
                        ++mne;
                        break;
                    }
                    /* bleq */
                default:
                    *iword |= 1<<24;
                    mne += 2;
                    break;
                }
            }
            break;
        default:
            *iword = 5<<25;
            ty = TY_BRN;
            ++mne;
            break;
        }
        break;

    case 'c':
        if (mne[1] != 'm')
            return -1;
        switch (mne[2]) {
        case 'n':
            *iword = I_CMN;
            break;
        case 'p':
            *iword = I_CMP;
            break;
        default:
            return -1;
        }
        ty = TY_CMP;
        mne += 3;
        break;

    case 'e':
        if (mne[1]!='o' || mne[2]!='r')
            return -1;
        *iword = I_EOR;
        ty = TY_ARI_LOG;
        mne += 3;
        break;

    case 'l':
        if (mne[1] != 'd')
            return -1;
        switch (mne[2]) {
        case 'm':
            *iword |= L;
            ty = TY_LD_ST_3;
            break;
        case 'r':
            *iword |= L;
            ty = TY_LD_ST_1;
            break;
        default:
            return -1;
        }
        mne += 3;
        break;

    case 'm':
        switch (mne[1]) {
        case 'l':
            if (mne[2] != 'a')
                return -1;
            *iword = (1<<21)|(9<<4);
            ty = TY_MUL_2;
            break;
        case 'o':
            if (mne[2] != 'v')
                return -1;
            *iword = I_MOV;
            ty = TY_MOV;
            break;
        case 'u':
            if (mne[2] != 'l')
                return -1;
            *iword = 9<<4;
            ty = TY_MUL_1;
            break;
        case 'v':
            if (mne[2] != 'n')
                return -1;
            *iword = I_MVN;
            ty = TY_MOV;
            break;
        default:
            return -1;
        }
        mne += 3;
        break;

    case 'o':
        if (mne[1]!='r' || mne[2]!='r')
            return -1;
        *iword = I_ORR;
        ty = TY_ARI_LOG;
        mne += 3;
        break;

    case 'r':
        if (mne[1] != 's')
            return -1;
        switch (mne[2]) {
        case 'b':
            *iword = I_RSB;
            break;
        case 'c':
            *iword = I_RSC;
            break;
        default:
            return -1;
        }
        ty = TY_ARI_LOG;
        mne += 3;
        break;

    case 's':
        switch (mne[1]) {
        case 'b':
            if (mne[2] != 'c')
                return -1;
            *iword = I_SBC;
            ty = TY_ARI_LOG;
            mne += 3;
            break;
        case 'm':
            if (strncmp(mne, "smlal", 5) == 0)
                *iword = (7<<21)|(9<<4);
            else if (strncmp(mne, "smull", 5) == 0)
                *iword = (6<<21)|(9<<4);
            else
                return -1;
            ty = TY_MUL_3;
            mne += 5;
            break;
        case 't':
            switch (mne[2]) {
            case 'm':
                ty = TY_LD_ST_3;
                break;
            case 'r':
                ty = TY_LD_ST_1;
                break;
            default:
                return -1;
            }
            mne += 3;
            break;
        case 'u':
            if (mne[2] != 'b')
                return -1;
            *iword = I_SUB;
            ty = TY_ARI_LOG;
            mne += 3;
            break;
        case 'w':
            if (mne[2] != 'p')
                return -1;
            *iword = I_SWP;
            ty = TY_SWP;
            mne += 3;
            break;
        case 'x':
            if (mne[2] != 't')
                return -1;
            switch (mne[3]) {
            case 'b':
                *iword = 0x6af0070;
                break;
            case 'h':
                *iword = 0x6bf0070;
                break;
            default:
                return -1;
            }
            ty = TY_SZX;
            mne += 4;
            break;
        default:
            return -1;
        }
        break;

    case 't':
        switch (mne[1]) {
        case 'e':
            if (mne[2] != 'q')
                return -1;
            *iword = I_TEQ;
            break;
        case 's':
            if (mne[2] != 't')
                return -1;
            *iword = I_TST;
            break;
        default:
            return -1;
        }
        ty = TY_CMP;
        mne += 3;
        break;

    case 'u':
        switch (mne[1]) {
        case 'm':
            if (strncmp(mne, "umlal", 5) == 0)
                *iword = (5<<21)|(9<<4);
            else if (strncmp(mne, "umull", 5) == 0)
                *iword = (4<<21)|(9<<4);
            else
                return -1;
            ty = TY_MUL_3;
            mne += 5;
            break;
        case 'x':
            if (mne[2] != 't')
                return -1;
            switch (mne[3]) {
            case 'b':
                *iword = 0x6ef0070;
                break;
            case 'h':
                *iword = 0x6ff0070;
                break;
            default:
                return -1;
            }
            ty = TY_SZX;
            mne += 4;
            break;
        default:
            return -1;
        }
        break;
    }

    if (mne[0] == '\0')
        goto done;

    /*
        Check for suffixes
            <cond> ( 's' | <size> | <addr_mode> )
        <cond>:
            AL | EQ | NE |
            HI | HS | GE |
            GT | LT | LE |
            LO | LS
        <size>:
            B | H |
            SB | SH
        <addr_mode>:
            IA | IB |
            DA | DB
    */

    /* <cond> */
    switch (mne[0]) {
    case 'a':
        if (mne[1] != 'l')
            return -1;
        *iword |= COND(CC_AL);
        got_cond = TRUE;
        mne += 2;
        break;
    case 'e':
        if (mne[1] != 'q')
            return -1;
        *iword |= COND(CC_EQ);
        got_cond = TRUE;
        mne += 2;
        break;
    case 'n':
        if (mne[1] != 'e')
            return -1;
        *iword |= COND(CC_NE);
        got_cond = TRUE;
        mne += 2;
        break;
    case 'h':
        switch (mne[1]) {
        case 'i':
            *iword |= COND(CC_HI);
            got_cond = TRUE;
            mne += 2;
            break;
        case 's':
            *iword |= COND(CC_HS);
            got_cond = TRUE;
            mne += 2;
            break;
        }
        break;
    case 'g':
        switch (mne[1]) {
        case 'e':
            *iword |= COND(CC_GE);
            break;
        case 't':
            *iword |= COND(CC_GT);
            break;
        default:
            return -1;
        }
        got_cond = TRUE;
        mne += 2;
        break;
    case 'l':
        switch (mne[1]) {
        case 't':
            *iword |= COND(CC_LT);
            break;
        case 'e':
            *iword |= COND(CC_LE);
            break;
        case 'o':
            *iword |= COND(CC_LO);
            break;
        case 's':
            *iword |= COND(CC_LS);
            break;
        default:
            return -1;
        }
        got_cond = TRUE;
        mne += 2;
        break;
    }

    /* 's' | <size> | <addr_mode> */
    switch (mne[0]) {
    case 'b':
        if (ty!=TY_LD_ST_1 && ty!=TY_SWP)
            return -1;
        *iword |= B;
        mne += 1;
        break;
    case 'h':
        if (ty != TY_LD_ST_1)
            return -1;
        ty = TY_LD_ST_2;
        if (tmp[0] == 's')
            *iword |= H; /* store halfword */
        else
            *iword |= L|H; /* load unsigned halfword */
        mne += 1;
        break;
    case 'd':
        switch (mne[1]) {
        case 'a':
            if (ty != TY_LD_ST_3)
                return -1;
            got_am = TRUE;
            mne += 2;
            break;
        case 'b':
            if (ty != TY_LD_ST_3)
                return -1;
            *iword |= P;
            got_am = TRUE;
            mne += 2;
            break;
        }
        break;
    case 'i':
        switch (mne[1]) {
        case 'a':
            if (ty != TY_LD_ST_3)
                return -1;
            *iword |= U;
            got_am = TRUE;
            mne += 2;
            break;
        case 'b':
            if (ty != TY_LD_ST_3)
                return -1;
            *iword |= U|P;
            got_am = TRUE;
            mne += 2;
            break;
        }
        break;
    case 's':
        switch (mne[1]) {
        case '\0':
            /* TODO: check if the instruction can have an S bit */
            *iword |= S;
            mne += 1;
            break;
        case 'b':
            if (ty != TY_LD_ST_1)
                return -1;
            ty = TY_LD_ST_2;
            if (tmp[0] == 's') /* store signed byte */
                return -1;
            *iword |= L|SIGN; /* load signed byte */
            mne += 2;
            break;
        case 'h':
            if (ty != TY_LD_ST_1)
                return -1;
            ty = TY_LD_ST_2;
            if (tmp[0] == 's') /* store signed halfword */
                return -1;
            *iword |= L|SIGN|H; /* load signed halfword */
            mne += 2;
            break;
        }
        break;
    }

    if (mne[0] != '\0')
        return -1;
done:
    if (ty==TY_LD_ST_3 && !got_am) /* ldm/stm without addressing mode */
        return -1;
    if (!got_cond)
        *iword |= COND(CC_AL);
    return ty;
}

/*
 * operand = NUM |
 *           ID |
 *           REG |
 *           REG "!"
 *           REG "," shift_spec |
 *           eff_addr |
 *           "=" ( NUM | ID ) |
 *           "{" REG { "," REG } "}"
*/
Operand operand(void)
{
    Operand op;

    switch (curr_tok) {
    case TOK_NUM:
        op.kind = NumKind;
        op.attr.num = str2int(lexeme);
        match(TOK_NUM);
        break;
    case TOK_ID:
        op.kind = LabKind;
        op.attr.lab = strdup(lexeme);
        match(TOK_ID);
        break;
    case TOK_EQ:
        match(TOK_EQ);
        if (curr_tok == TOK_NUM) {
            op.kind = LitNumKind;
            op.attr.num = str2int(lexeme);
            match(TOK_NUM);
        } else {
            op.kind = LitLabKind;
            op.attr.lab = strdup(lexeme);
            match(TOK_ID);
        }
        break;
    case TOK_LBRACKET:
        op.kind = EffAddrKind;
        op.attr.ea = eff_addr();
        break;
    case TOK_LBRACE: {
        int r, hi;

        /* though not strictly necessary, enforce the
           requirement of registers in ascending order
           (this should help to catch some compiler errors) */
        op.kind = RegListKind;
        match(TOK_LBRACE);
        if (curr_tok == TOK_REG) {
            r = hi = get_reg(lexeme);
            op.attr.reg = 1<<r;
        }
        match(TOK_REG);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            if (curr_tok == TOK_REG) {
                if ((r=get_reg(lexeme)) < hi)
                    ERR("register list not in ascending order");
                op.attr.reg |= 1<<r;
            }
            match(TOK_REG);
        }
        match(TOK_RBRACE);
    }
        break;
    case TOK_REG:
        op.attr.reg = get_reg(lexeme);
        match(TOK_REG);
        if (curr_tok == TOK_EXCL) {
            op.kind = WBRegKind;
            match(TOK_EXCL);
        } else if (curr_tok == TOK_COMMA) {
            int ln_tmp;
            Token ct_tmp;
            char *curr_tmp, lexeme_tmp[MAX_LEXEME];

            ln_tmp = line_number;
            curr_tmp = curr;
            strcpy(lexeme_tmp, lexeme);
            ct_tmp = curr_tok;

            match(TOK_COMMA);
            if (ISSHIFT(curr_tok)) {
                int sh;

                op.kind = ShRegKind;
                sh = shift_spec();
                if ((op.attr.shreg.kind=(sh>>8)&0xFF) != TOK_RRX) {
                    if (sh >> 16) {
                        op.attr.shreg.num_cnt = sh&0xFF;
                        op.attr.shreg.reg_cnt = -1;
                    } else {
                        op.attr.shreg.reg_cnt = sh&0xFF;
                        op.attr.shreg.num_cnt = -1;
                    }
                }
            } else { /* next operand */
                line_number = ln_tmp;
                curr = curr_tmp;
                strcpy(lexeme, lexeme_tmp);
                curr_tok = ct_tmp;
                op.kind = RegKind;
            }
        } else {
            op.kind = RegKind;
        }
        break;
    }
    return op;
}

int encode_immediate(unsigned n, int *iword, int ldr)
{
    static unsigned a[16] = { /* rot 0..15 */
        0x000000FF, 0xC000003F,
        0xF000000F, 0xFC000003,
        0xFF000000, 0x3FC00000,
        0x0FF00000, 0x03FC0000,
        0x00FF0000, 0x003FC000,
        0x000FF000, 0x0003FC00,
        0x0000FF00, 0x00003FC0,
        0x00000FF0, 0x000003F0,
    };
    int rot;
    unsigned imm;

    imm = n;
    for (rot = 0; rot < 16; rot++) {
        if (!(n & ~a[rot])) {
            if (ldr)
                *iword |= I_MOV;
            goto done;
        }
        imm = (imm<<2) | (imm>>30);
    }
    if (ldr) {
        rot = 0;
        if (n == (unsigned)-1) {
            *iword |= I_MVN;
            imm = 0;
            goto done;
        }
        for (imm = a[rot]; rot < 16; rot++) {
            if (n == ~a[rot]) {
                *iword |= I_MVN;
                goto done;
            }
            imm = (imm<<2) | (imm>>30);
        }
        return 1;
    }
    --line_number;
    ERR("bad immediate value `#0x%x'", n);
done:
    assert(!(imm&~0xFF));
    *iword |= I|(rot<<8)|imm;
    return 0;
}

int encode_shifter_operand(Operand op, int *iword)
{
    switch (op.kind) {
    case NumKind:
        encode_immediate(op.attr.num, iword, FALSE);
        break;
    case RegKind:
        *iword |= Rm(op.attr.reg);
        break;
    case ShRegKind:
        *iword |= Rm(op.attr.shreg.reg);
        if (op.attr.shreg.kind == TOK_RRX) {
            *iword |= 6<<4;
            break;
        }
        if (op.attr.shreg.reg_cnt != -1) { /* shift by register */
            *iword |= Rs(op.attr.shreg.reg_cnt);
            switch (op.attr.shreg.kind) {
            case TOK_LSL: *iword |= 1<<4; break;
            case TOK_LSR: *iword |= 3<<4; break;
            case TOK_ASR: *iword |= 5<<4; break;
            case TOK_ROR: *iword |= 7<<4; break;
            default: assert(0);
            }
        } else { /* shift by immediate */
            *iword |= SHIMM(op.attr.shreg.num_cnt);
            switch (op.attr.shreg.kind) {
            case TOK_LSL: break;
            case TOK_LSR: *iword |= 2<<4; break;
            case TOK_ASR: *iword |= 4<<4; break;
            case TOK_ROR: *iword |= 6<<4; break;
            default: assert(0);
            }
        }
        break;
    default:
        return 1;
    }
    return 0;
}

void ierr(char *instr)
{
    --line_number;
    ERR("invalid operands to instruction `%s'", instr);
}

/* source_line = instruction operand [ "," operand [ "," operand [ "," operand ] ] ] EOL */
void source_line(char *instr)
{
    int no, ty, iword;
    EffAddr ea;
    Operand op1, op2, op3, op4;

    iword = 0;
    if ((ty=encode_mnemonic(instr, &iword)) == -1)
        ERR("invalid mnemonic `%s'", instr);

    op1 = operand();
    no = 1;
    if (curr_tok == TOK_COMMA) {
        match(TOK_COMMA);
        op2 = operand();
        ++no;
        if (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            op3 = operand();
            ++no;
            if (curr_tok == TOK_COMMA) {
                match(TOK_COMMA);
                op4 = operand();
                ++no;
            }
        }
    }
    match(TOK_EOL);

    switch (ty) {
    case TY_ARI_LOG: /* op Rd, Rn, shifter_operand */
        if (no!=3 || op1.kind!=RegKind || op2.kind!=RegKind)
            ierr(instr);
        iword |= Rd(op1.attr.reg)|Rn(op2.attr.reg);
        if (encode_shifter_operand(op3, &iword))
            ierr(instr);
        break;

    case TY_MOV:     /* op Rd, shifter_operand */
        if (no!=2 || op1.kind!=RegKind)
            ierr(instr);
        iword |= Rd(op1.attr.reg);
        if (encode_shifter_operand(op2, &iword))
            ierr(instr);
        break;

    case TY_CMP:     /* op Rn, shifter_operand */
        if (no!=2 || op1.kind!=RegKind)
            ierr(instr);
        iword |= Rn(op1.attr.reg);
        if (encode_shifter_operand(op2, &iword))
            ierr(instr);
        break;

    case TY_MUL_1:   /* op Rd, Rm, Rs */
        if (no!=3 || op1.kind!=RegKind || op2.kind!=RegKind || op3.kind!=RegKind)
            ierr(instr);
        if (op1.attr.reg==15 || op2.attr.reg==15 || op3.attr.reg==15) {
            --line_number;
            ERR("r15 cannot be used here");
        }
        iword |= Mul_Rd(op1.attr.reg)|Rm(op2.attr.reg)|Rs(op3.attr.reg);
        break;

    case TY_MUL_2:   /* op Rd, Rm, Rs, Rn */
        if (no!=4 || op1.kind!=RegKind || op2.kind!=RegKind || op3.kind!=RegKind || op4.kind!=RegKind)
            ierr(instr);
        if (op1.attr.reg==15 || op2.attr.reg==15 || op3.attr.reg==15 || op4.attr.reg==15) {
            --line_number;
            ERR("r15 cannot be used here");
        }
        iword |= Mul_Rd(op1.attr.reg)|Rm(op2.attr.reg)|Rs(op3.attr.reg)|Mul_Rn(op4.attr.reg);
        break;

    case TY_MUL_3:   /* op RdLo, RdHi, Rm, Rs */
        if (no!=4 || op1.kind!=RegKind || op2.kind!=RegKind || op3.kind!=RegKind || op4.kind!=RegKind)
            ierr(instr);
        if (op1.attr.reg==15 || op2.attr.reg==15 || op3.attr.reg==15 || op4.attr.reg==15) {
            --line_number;
            ERR("r15 cannot be used here");
        }
        if (op1.attr.reg == op2.attr.reg) {
            --line_number;
            ERR("RdHi and RdLo must be different");
        }
        iword |= RdLo(op1.attr.reg)|RdHi(op2.attr.reg)|Rm(op3.attr.reg)|Rs(op4.attr.reg);
        break;

    case TY_BRN:     /* op offset */
        if (no!=1 || op1.kind!=LabKind)
            ierr(instr);
        new_unr_expr(op1.attr.lab, LC(), curr_section, TRUE);
        break;

    case TY_BRNR:   /* op Rm */
        if (no!=1 || op1.kind!=RegKind)
            ierr(instr);
        iword |= Rm(op1.attr.reg);
        break;

    case TY_LD_ST_1: /* op[b] Rd, ( mem | =expr ) */
        if (no!=2 || op1.kind!=RegKind)
            ierr(instr);
        iword |= (1<<26)|Rd(op1.attr.reg);
        switch (op2.kind) {
        case EffAddrKind:
            ea = op2.attr.ea;
            iword |= Rn(ea.base);
            if (!ea.down)
                iword |= U;
            if (ea.ioffs) { /* base(+/-)offset_12 */
                if (ea.attr.imm_offs > 4095) {
                    --line_number;
                    ERR("offset out of range (max: +/-4095)");
                }
                iword |= OFFS(ea.attr.imm_offs);
            } else { /* base(+/-)reg[<shift> imm] */
                if (ea.attr.roffs.reg == 15) {
                    --line_number;
                    ERR("r15 cannot be used as the index register");
                }
                iword |= (1<<25)|Rm(ea.attr.roffs.reg);
                if (ea.attr.roffs.shift != 0) {
                    if ((ea.attr.roffs.shift>>8) == TOK_RRX) {
                        iword |= 6<<4;
                    } else {
                        switch (ea.attr.roffs.shift >> 8) {
                        case TOK_LSL: break;
                        case TOK_LSR: iword |= 2<<4; goto chcksh;
                        case TOK_ASR: iword |= 4<<4; goto chcksh;
                        case TOK_ROR: iword |= 6<<4;
                        chcksh:
                            if ((ea.attr.roffs.shift&0x1F) == 0) {
                                --line_number;
                                ERR("invalid shift amount");
                            }
                            break;
                        }
                        iword |= SHIMM(ea.attr.roffs.shift);
                    }
                }
            }
            switch (ea.am) {
            case AM_Offset:
                iword |= P;
                break;
            case AM_PreIndex:
                iword |= P|W;
                break;
            case AM_PostIndex:
                break;
            }
            break;
        case LitNumKind: {  /* =NUM */
            int iword2;

            if (!(iword&L) || (iword&B))
                ierr(instr);
            iword2 = iword&((15<<28)|Rd(15)); /* get cond & Rd */
            if (!encode_immediate(op2.attr.num, &iword2, TRUE)) {
                iword = iword2;
                break;
            } else {
                iword |= Rn(15)|P; /* pc-relative load */
                if (curr_section == NULL)
                    set_curr_section(DEF_SEC);
                new_litld(lit_pool_write(op2.attr.num), LC(), NULL);
            }
        }
            break;
        case LitLabKind: {  /* =ID */
            if (!(iword&L) || (iword&B))
                ierr(instr);

            iword |= Rn(15)|P; /* pc-relative load */
            if (curr_section == NULL)
                set_curr_section(DEF_SEC);
            new_litld(lit_pool_write(0), LC(), op2.attr.lab);
        }
            break;
        default:
            ierr(instr);
        }
        break;

    case TY_LD_ST_2: /* op(sb|h|sh) Rd, mem */
        if (no!=2 || op1.kind!=RegKind || op2.kind!=EffAddrKind)
            ierr(instr);
        iword |= Rd(op1.attr.reg);
        ea = op2.attr.ea;
        iword |= Rn(ea.base)|(1<<7)|(1<<4);
        if (!ea.down)
            iword |= U;
        if (ea.ioffs) { /* base(+/-)offset_8 */
            if (ea.attr.imm_offs > 255) {
                --line_number;
                ERR("offset out of range (max: +/-255)");
            }
            iword |= (1<<22)|OFFSH(ea.attr.imm_offs)|OFFSL(ea.attr.imm_offs);
        } else { /* base(+/-)reg */
            if (ea.attr.roffs.reg == 15) {
                --line_number;
                ERR("r15 cannot be used as the index register");
            }
            iword |= Rm(ea.attr.roffs.reg);
            if (ea.attr.roffs.shift != 0) {
                --line_number;
                ERR("the index register cannot be scaled here");
            }
        }
        switch (ea.am) {
        case AM_Offset:
            iword |= P;
            break;
        case AM_PreIndex:
            iword |= P|W;
            break;
        case AM_PostIndex:
            break;
        }
        break;

    case TY_LD_ST_3: /* op Rn[!], reg_list */
        if (no!=2 || op2.kind!=RegListKind)
            ierr(instr);
        if (op1.attr.reg == 15) {
            --line_number;
            ERR("r15 cannot be used as the base register");
        }
        switch (op1.kind) {
        case WBRegKind:
            iword |= W;
        case RegKind:
            iword |= (1<<27)|Rn(op1.attr.reg)|REGLIST(op2.attr.reg);
            break;
        default:
            ierr(instr);
        }
        break;

    case TY_SWP:     /* op Rd, Rm, [Rn] */
        if (no!=3 || op1.kind!=RegKind || op2.kind!=RegKind || op3.kind!=EffAddrKind
        || (ea=op3.attr.ea).am!=AM_Offset || !ea.ioffs || ea.attr.imm_offs!=0)
            ierr(instr);
        if (op1.attr.reg==15 || op2.attr.reg==15 || ea.base==15) {
            --line_number;
            ERR("r15 cannot be used here");
        }
        if (op1.attr.reg==ea.base || op2.attr.reg==ea.base) {
            --line_number;
            ERR("Rn cannot be the same as Rd or Rm");
        }
        iword |= Rd(op1.attr.reg)|Rm(op2.attr.reg)|Rn(ea.base);
        break;

    case TY_SZX:     /* op Rd, Rm */
        /* actually the second operand can be rotated, but we don't need that */
        if (no!=2 || op1.kind!=RegKind || op2.kind!=RegKind)
            ierr(instr);
        iword |= Rd(op1.attr.reg)|Rm(op2.attr.reg);
        break;
    }
    /*printf("iword = %x\n", iword);*/
    write_dword(iword);
}

/* label = ID ":" */
void label(char *id)
{
    if (curr_section == NULL)
        set_curr_section(DEF_SEC);
    define_symbol(OtherKind, LocalBind, id, LC(), curr_section);
    match(TOK_COLON);
}

/* line = label | source_line | directive */
void line(void)
{
    if (curr_tok == TOK_DOT) {
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
    match(TOK_EOF);
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
                sec->hdr.sh_addralign = 4;
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
                case RELOC_ABS:
                    er.r_info = ELF32_R_INFO(r->sym->ndx, R_ARM_ABS32);
                    break;
                case RELOC_PC:
                    er.r_info = ELF32_R_INFO(r->sym->ndx, R_ARM_CALL);
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
    elf_header.e_machine = EM_ARM;
    elf_header.e_version = EV_CURRENT;
    elf_header.e_flags = EF_ARM_EABI_VER5;
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
    lit_pool.max = 32;
    lit_pool.buf = malloc(sizeof(uint32_t)*lit_pool.max);
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

Token reserved_lookup(char *s)
{
    int i;
    static struct {
        char *lex;
        Token tok;
    } shifts[] = {
        { "LSL", TOK_LSL },
        { "LSR", TOK_LSR },
        { "ASR", TOK_ASR },
        { "ROR", TOK_ROR },
        { "RRX", TOK_RRX },
    };

    if (s[0]=='r' || s[0]=='R') {
        if (s[1]>='0' && s[1]<='9') {
            if (s[2] == '\0')
                return TOK_REG;
            else if (s[2]>='0' && s[2]<='9' && s[3]=='\0')
                return TOK_REG;
        }
    }
    for (i = 0; i < NELEMS(shifts); i++)
        if (equal(s, shifts[i].lex))
            return shifts[i].tok;
    return TOK_ID;
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
    static int EOF_reached = FALSE;

    if (EOF_reached)
        return TOK_EOF;

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
            } else if (isalpha(c) || c=='_' || c=='@' || c=='$') {
                state = INID;
            } else if (c == '#') {
                if (!isdigit(*curr))
                    ERR("expecting integer constant after `#'");
                if (*curr=='0' && (*(curr+1)=='x' || *(curr+1)=='X')) {
                    if (!isxdigit(*(curr+2)))
                        ERR("expecting hexadecimal digits after `#0x'");
                    lexeme[cindx++] = *curr++;
                    lexeme[cindx++] = *curr++;
                    c = *curr++;
                    state = INHEXNUM;
                } else {
                    save = FALSE;
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
                    EOF_reached = TRUE;
                    break;
                case '\n':
                    ++line_number;
                    tok = TOK_EOL;
                    break;
                case ',':
                    tok = TOK_COMMA;
                    break;
                case ':':
                    tok = TOK_COLON;
                    break;
                case '!':
                    tok = TOK_EXCL;
                    break;
                case '.':
                    tok = TOK_DOT;
                    break;
                case '+':
                    tok = TOK_PLUS;
                    break;
                case '-':
                    tok = TOK_MINUS;
                    break;
                case '=':
                    tok = TOK_EQ;
                    break;
                case '[':
                    tok = TOK_LBRACKET;
                    break;
                case ']':
                    tok = TOK_RBRACKET;
                    break;
                case '{':
                    tok = TOK_LBRACE;
                    break;
                case '}':
                    tok = TOK_RBRACE;
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
            if (!isalnum(c) && c!='_' && c!='@' && c!='$')
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

int str2int(char *s)
{
    char *ep;

    return (int)strtoul(s, &ep, 0);
}
