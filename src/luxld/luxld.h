#ifndef LUXLD_H_
#define LUXLD_H_

#include <elf.h>
#include <stdint.h>
#include "../util/ELF_util.h"

#define MAX_SEC_PER_SEG 32
#define HASH_SIZE       1009
#ifndef E_MIPS_ABI_O32
#define E_MIPS_ABI_O32 0x00001000 /* The original o32 abi. */
#endif
#ifndef R_ARM_CALL
#define R_ARM_CALL  28
#endif

typedef unsigned char bool;
typedef uint16_t Elf_Half;
typedef struct ObjFile ObjFile;
typedef struct ShrdObjFile ShrdObjFile;
typedef struct PLTEnt PLTEnt;
typedef struct SmplSec SmplSec;
typedef struct CmpndSec CmpndSec;
typedef struct Segment Segment;
typedef struct Symbol Symbol;

/*
 * Simple relocatable object file.
 */
struct ObjFile {
    char *buf;
    union {
        struct {
            Elf32_Ehdr *_ehdr;      /* ELF header */
            Elf32_Shdr *_shtab;     /* Section header table */
            Elf32_Sym *_symtab;     /* Symbol table */
        } _a32;
        struct {
            Elf64_Ehdr *_ehdr;
            Elf64_Shdr *_shtab;
            Elf64_Sym *_symtab;
        } _a64;
    } _a;
    int nsym;           /* # of symbol table entries */
    char *shstrtab;     /* Section name string table */
    char *strtab;       /* String table */
    ObjFile *next;
};

#define OF32_EHDR(x)    ((x)->_a._a32._ehdr)
#define OF32_SHTAB(x)   ((x)->_a._a32._shtab)
#define OF32_SYMTAB(x)  ((x)->_a._a32._symtab)
#define OF64_EHDR(x)    ((x)->_a._a64._ehdr)
#define OF64_SHTAB(x)   ((x)->_a._a64._shtab)
#define OF64_SYMTAB(x)  ((x)->_a._a64._symtab)

/*
 * Shared library.
 */
struct ShrdObjFile {
    char *buf;
    char *name;             /* Library's path passed to the linker in the command line or DT_SONAME */
    union {
        struct {
            Elf32_Ehdr *_ehdr;
            Elf32_Shdr *_shtab;
            Elf32_Sym *_dynsym;     /* Dynamic symbol table */
            Elf32_Dyn *_dynamic;    /* Dynamic section */
        } _a32;
        struct {
            Elf64_Ehdr *_ehdr;
            Elf64_Shdr *_shtab;
            Elf64_Sym *_dynsym;
            Elf64_Dyn *_dynamic;
        } _a64;
    } _a;
    Elf32_Word *hash;       /* Hash table (points to first bucket) */
    Elf32_Word nbucket;     /* Hash table's # of buckets */
    Elf32_Word *chain;      /* Hash chain (length == # of dynsym entries) */
    int nsym;               /* dynsym's # of entries */
    char *dynstr;           /* Dynamic string table */
    ShrdObjFile *next;
};

#define SO32_EHDR(x)    ((x)->_a._a32._ehdr)
#define SO32_SHTAB(x)   ((x)->_a._a32._shtab)
#define SO32_DYNSYM(x)  ((x)->_a._a32._dynsym)
#define SO32_DYN(x)     ((x)->_a._a32._dynamic)
#define SO64_EHDR(x)    ((x)->_a._a64._ehdr)
#define SO64_SHTAB(x)   ((x)->_a._a64._shtab)
#define SO64_DYNSYM(x)  ((x)->_a._a64._dynsym)
#define SO64_DYN(x)     ((x)->_a._a64._dynamic)

struct PLTEnt {
    char *fname;
    uint64_t addr;
    PLTEnt *next;
};

/*
 * Simple section with the contribution of a single object file.
 */
struct SmplSec {
    ObjFile *obj;        /* object file that contributed this section */
    union {
        Elf32_Shdr *_shdr32;
        Elf64_Shdr *_shdr64;
    } _s;
    char *data;
    SmplSec *next;
};

#define SS32_SHDR(x)    ((x)->_s._shdr32)
#define SS64_SHDR(x)    ((x)->_s._shdr64)

/*
 * Compound section with the contributions of all object files.
 */
struct CmpndSec {
    char *name;
    Elf_Half shndx;     /* index into output file's section header table */
    union {
        Elf32_Shdr _shdr32;
        Elf64_Shdr _shdr64;
    } _s;
    SmplSec *sslist;
    CmpndSec *next;
};

#define CS32_SHDR(x)    ((x)->_s._shdr32)
#define CS64_SHDR(x)    ((x)->_s._shdr64)

/*
 * Read-only & writable loadable segments.
 */
struct Segment {
    int nsec;
    CmpndSec *secs[MAX_SEC_PER_SEG];
    union {
        Elf32_Phdr _phdr32;
        Elf64_Phdr _phdr64;
    } _p;
};

#define SEG32_PHDR(x)   ((x)->_p._phdr32)
#define SEG64_PHDR(x)   ((x)->_p._phdr64)

struct Symbol {
    char *name;
    Elf_Half shndx;     /* index into input file's section header table */
    uint32_t size;
    uint64_t value;
    unsigned char info;
    unsigned char other;
    bool in_dynsym;
    char *shname;
    Symbol *next;
};

enum {
    EMU_X86,    /* x86 32-bit */
    EMU_X64,    /* x86 64-bit */
    EMU_MIPS,   /* MIPS32 */
    EMU_ARM,    /* ARMv6 */
};
extern int emu_mode;
extern size_t PLT_ENTRY_NB;
extern ObjFile *object_files;
extern ShrdObjFile *shared_object_files;
extern CmpndSec *interp_sec;
extern Elf_Half shndx;
extern CmpndSec *sections;
extern CmpndSec *dynsym_sec;
extern CmpndSec *reldyn_sec;
extern int nreldyn;
extern CmpndSec *bss_sec;
extern Segment ROSeg, WRSeg;
extern PLTEnt *plt_entries;
extern CmpndSec *plt_sec, *relplt_sec, *gotplt_sec;
extern CmpndSec *dynamic_sec;
extern int nplt, nrelplt, ngotplt;
extern CmpndSec *dynstr_sec;
extern CmpndSec *hash_sec;
extern StrTab *dynstr;
extern Symbol *global_symbols[];
extern Symbol *local_symbols;
extern char *entry_symbol;
extern CmpndSec *relaplt_sec;
extern int nrelaplt;
extern CmpndSec *reladyn_sec;
extern int nreladyn;
extern CmpndSec *got_sec;
extern Elf32_Word runpath_val;

void err(char *fmt, ...);
void err_undef(char *sym);
Elf_Half get_shndx(Symbol *sym);
Symbol *lookup_global_symbol(char *name);
Elf32_Half get_dynsym_ndx_32(char *sym);
Elf64_Half get_dynsym_ndx_64(char *sym);
void *lookup_in_shared_object_32(char *symname);
void *lookup_in_shared_object_64(char *symname);
Elf32_Addr get_symval_32(char *name, Elf32_Sym *st_ent, bool *found);
Elf64_Addr get_symval_64(char *name, Elf64_Sym *st_ent, bool *found);
void define_local_symbol(char *name, uint64_t value, unsigned char info, Elf_Half shndx, char *shname);

#endif
