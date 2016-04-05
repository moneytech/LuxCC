#include "luxld.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ar.h>
#include <assert.h>
#include <stdarg.h>
#include "../util/util.h"
#include "../util/ELF_util.h"
#include "x86.h"
#include "x64.h"
#include "mips.h"
#include "arm.h"
#include "out.h"

#define MAX_INPUT_FILES     64
#define PAGE_SIZE           0x1000
#define PAGE_MASK           (PAGE_SIZE-1)
#define HASH(s)             (hash(s)%HASH_SIZE)
#define DEF_EMU_MOD         EMU_X86
#define MAX_RUNPATH_LEN     2048

size_t PLT_ENTRY_NB;
static char *prog_name;
static char *fbuf[MAX_INPUT_FILES];
static int nfbuf;
int emu_mode;
#define EMU32() (emu_mode != EMU_X64)

static int nundef; /* # of undefined ('extern') symbols currently in the global symtab */
static int nglobal;
static int nshaobj;
static int nreloc;
Elf_Half shndx = 4; /* [0]=UND, [1]=.shstrtab, [2]=.symtab, [3]=.strtab */

static char *interp;
char *entry_symbol;
CmpndSec *interp_sec;
static int nreserved; /* # of reserved entries in .got.plt (or .got for ARM) */
static char *runpaths[64];
static int nrunpath;
Elf32_Word runpath_val = (Elf32_Word)-1;

ObjFile *object_files;
ShrdObjFile *shared_object_files;
CmpndSec *sections;
CmpndSec *dynstr_sec;
CmpndSec *dynsym_sec;
CmpndSec *hash_sec;
CmpndSec *dynamic_sec;
CmpndSec *reldyn_sec, *bss_sec;
int nreldyn;
CmpndSec *got_sec;
StrTab *dynstr;
Segment ROSeg, WRSeg;

/* Symbols that will go into the output file's symtabs. */
Symbol *global_symbols[HASH_SIZE];
Symbol *local_symbols; /* Local symbols with type other than SECTION or FILE */

PLTEnt *plt_entries;
CmpndSec *plt_sec, *relplt_sec, *gotplt_sec;
int nplt, nrelplt, ngotplt;

CmpndSec *relaplt_sec;
int nrelaplt;
CmpndSec *reladyn_sec;
int nreladyn;

void err(char *fmt, ...)
{
    va_list args;

    fprintf(stderr, "%s: error: ", prog_name);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

void err_undef(char *sym)
{
    err("undefined reference to `%s'", sym);
}

Elf32_Half get_dynsym_ndx_32(char *sym)
{
    Elf32_Sym *sp;
    Elf32_Half st_ndx, st_siz;

    st_ndx = 1;
    sp = (Elf32_Sym *)dynsym_sec->sslist->data+1;
    st_siz = CS32_SHDR(dynsym_sec).sh_size/sizeof(Elf32_Sym);
    while (st_ndx < st_siz) {
        if (equal(strtab_get_string(dynstr, sp->st_name), sym))
            return st_ndx;
        ++st_ndx, ++sp;
    }
    assert(!"symbol not found");
}

Elf64_Half get_dynsym_ndx_64(char *sym)
{
    Elf64_Sym *sp;
    Elf64_Half st_ndx, st_siz;

    st_ndx = 1;
    sp = (Elf64_Sym *)dynsym_sec->sslist->data+1;
    st_siz = CS64_SHDR(dynsym_sec).sh_size/sizeof(Elf64_Sym);
    while (st_ndx < st_siz) {
        if (equal(strtab_get_string(dynstr, sp->st_name), sym))
            return st_ndx;
        ++st_ndx, ++sp;
    }
    assert(!"symbol not found");
}

void *lookup_in_shared_object_32(char *symname)
{
    ShrdObjFile *so;

    for (so = shared_object_files; so != NULL; so = so->next) {
        char *sn;
        Elf32_Word ci;

        ci = so->hash[elf_hash((unsigned char *)symname)%so->nbucket];
        while (ci != STN_UNDEF) {
            sn = &so->dynstr[SO32_DYNSYM(so)[ci].st_name];
            if (equal(symname, sn)) {
                if (SO32_DYNSYM(so)[ci].st_shndx != SHN_UNDEF)
                    return &SO32_DYNSYM(so)[ci];
                else
                    break;
            } else {
                ci = so->chain[ci];
            }
        }
    }
    return NULL;
}

void *lookup_in_shared_object_64(char *symname)
{
    ShrdObjFile *so;

    for (so = shared_object_files; so != NULL; so = so->next) {
        char *sn;
        Elf64_Word ci;

        ci = so->hash[elf_hash((unsigned char *)symname)%so->nbucket];
        while (ci != STN_UNDEF) {
            sn = &so->dynstr[SO64_DYNSYM(so)[ci].st_name];
            if (equal(symname, sn)) {
                if (SO64_DYNSYM(so)[ci].st_shndx != SHN_UNDEF)
                    return &SO64_DYNSYM(so)[ci];
                else
                    break;
            } else {
                ci = so->chain[ci];
            }
        }
    }
    return NULL;
}

static void define_global_symbol(char *name, uint64_t value, uint8_t info, Elf_Half shndx, char *shname)
{
    unsigned h;
    Symbol *np;

    h = HASH(name);
    for (np = global_symbols[h]; np != NULL; np = np->next)
        if (equal(np->name, name))
            break;
    if (np == NULL) {
        np = calloc(1, sizeof(Symbol));
        np->name = name;
        strtab_append(dynstr, name);
        np->value = value;
        np->info = info;
        np->in_dynsym = FALSE;
        if ((np->shndx=shndx) == SHN_UNDEF)
            ++nundef;
        ++nglobal;
        np->shname = shname;
        np->next = global_symbols[h];
        global_symbols[h] = np;
    } else if (np->shndx == SHN_UNDEF) {
        if (shndx != SHN_UNDEF) {
            np->value = value;
            np->info = info;
            np->shndx = shndx;
            np->shname = shname;
            --nundef;
            assert(nundef >= 0);
        }
    } else if (shndx != SHN_UNDEF) {
        err("multiple definition of `%s'", name);
    }
}

Symbol *lookup_global_symbol(char *name)
{
    Symbol *np;

    for (np = global_symbols[HASH(name)]; np != NULL; np = np->next)
        if (equal(np->name, name))
            break;
    return np;
}

static SmplSec *new_smpl_sec(ObjFile *obj, void *shdr, char *data, SmplSec *next)
{
    SmplSec *n;

    n = malloc(sizeof(SmplSec));
    n->obj = obj;
    if (EMU32())
        SS32_SHDR(n) = shdr;
    else
        SS64_SHDR(n) = shdr;
    n->data = data;
    n->next = next;
    return n;
}

/*
 * Add various sections required for dynamic linking.
 * Estimate sizes based on the number of globals symbols,
 * the number of undefined symbols, and the number of
 * relocations. If required, these values will be adjusted
 * before writing the final executable file.
 */
static void init_dynlink_sections_32(void)
{
    char *buf;
    CmpndSec *sec;
    Elf32_Shdr *shdr;
    uint32_t size, nbucket;

    if (nreloc > 0) {
        /* .plt */
        plt_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".plt";
        CS32_SHDR(sec).sh_type = SHT_PROGBITS;
        CS32_SHDR(sec).sh_flags = SHF_ALLOC|SHF_EXECINSTR;
        size = PLT_ENTRY_NB*(1+nreloc); /* +1 for PLT0 */
        CS32_SHDR(sec).sh_size = size;
        CS32_SHDR(sec).sh_addralign = (emu_mode==EMU_MIPS)?32:16;
        /*sec->shdr.sh_entsize = 4;*/
        buf = calloc(1, size);
        shdr = calloc(1, sizeof(Elf32_Shdr));
        *shdr = CS32_SHDR(sec);
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;

        /* .rel.plt */
        relplt_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".rel.plt";
        CS32_SHDR(sec).sh_type = SHT_REL;
        CS32_SHDR(sec).sh_flags = SHF_ALLOC;
        size = sizeof(Elf32_Rel)*nreloc;
        CS32_SHDR(sec).sh_size = size;
        CS32_SHDR(sec).sh_addralign = 4;
        CS32_SHDR(sec).sh_entsize = sizeof(Elf32_Rel);
        buf = calloc(1, size);
        shdr = calloc(1, sizeof(Elf32_Shdr));
        *shdr = CS32_SHDR(sec);
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;

        /* .got.plt */
        gotplt_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".got.plt";
        CS32_SHDR(sec).sh_type = SHT_PROGBITS;
        CS32_SHDR(sec).sh_flags = SHF_ALLOC|SHF_WRITE;
        size = sizeof(Elf32_Word)*(nreserved+nreloc);
        CS32_SHDR(sec).sh_size = size;
        CS32_SHDR(sec).sh_addralign = 4;
        CS32_SHDR(sec).sh_entsize = 4;
        buf = calloc(1, size);
        shdr = calloc(1, sizeof(Elf32_Shdr));
        *shdr = CS32_SHDR(sec);
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;

        /* .rel.dyn */
        reldyn_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".rel.dyn";
        CS32_SHDR(sec).sh_type = SHT_REL;
        CS32_SHDR(sec).sh_flags = SHF_ALLOC;
        size = sizeof(Elf32_Rel)*(nreloc);
        CS32_SHDR(sec).sh_size = size;
        CS32_SHDR(sec).sh_addralign = 4;
        CS32_SHDR(sec).sh_entsize = sizeof(Elf32_Rel);
        buf = calloc(1, size);
        shdr = calloc(1, sizeof(Elf32_Shdr));
        *shdr = CS32_SHDR(sec);
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;
    }

    /* .dynstr */
    dynstr_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".dynstr";
    CS32_SHDR(sec).sh_type = SHT_STRTAB;
    CS32_SHDR(sec).sh_flags = SHF_ALLOC;
    if (nrunpath) {
        int i;
        char rbuf[MAX_RUNPATH_LEN], *rp;

        rp = rbuf;
        rp[0] = '\0';
        for (i = 0; i < nrunpath; i++) {
            strcat(rp, runpaths[i]);
            rp += strlen(runpaths[i]);
            if (i+1 < nrunpath) {
                strcat(rp, ":");
                ++rp;
            }
        }
        runpath_val = strtab_append(dynstr, rbuf);
    }
    size = round_up(strtab_get_size(dynstr), 4);
    CS32_SHDR(sec).sh_size = size;
    CS32_SHDR(sec).sh_addralign = 1;
    buf = calloc(1, size);
    strtab_copy(dynstr, buf);
    shdr = calloc(1, sizeof(Elf32_Shdr));
    *shdr = CS32_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .dynsym */
    dynsym_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".dynsym";
    CS32_SHDR(sec).sh_type = SHT_DYNSYM;
    CS32_SHDR(sec).sh_flags = SHF_ALLOC;
    size = sizeof(Elf32_Sym)*(nglobal+1); /* +1 for STN_UNDEF */
    CS32_SHDR(sec).sh_size = size;
    CS32_SHDR(sec).sh_addralign = 4;
    CS32_SHDR(sec).sh_entsize = sizeof(Elf32_Sym);
    buf = calloc(1, size);
    shdr = calloc(1, sizeof(Elf32_Shdr));
    *shdr = CS32_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .hash */
    hash_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".hash";
    CS32_SHDR(sec).sh_type = SHT_HASH;
    CS32_SHDR(sec).sh_flags = SHF_ALLOC;
    /* size: nbucket + nchain + buckets + chain */
    nbucket = elf_get_nbucket(nglobal+1);
    size = sizeof(Elf32_Word)*(2+nbucket+(nglobal+1));
    CS32_SHDR(sec).sh_size = size;
    CS32_SHDR(sec).sh_addralign = 4;
    CS32_SHDR(sec).sh_entsize = 4;
    buf = calloc(1, size);
    *(Elf32_Word *)buf = nbucket;
    *((Elf32_Word *)buf+1) = nglobal+1;
    shdr = calloc(1, sizeof(Elf32_Shdr));
    *shdr = CS32_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .dynamic */
    dynamic_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".dynamic";
    CS32_SHDR(sec).sh_type = SHT_DYNAMIC;
    CS32_SHDR(sec).sh_flags = SHF_ALLOC|SHF_WRITE;
    size = sizeof(Elf32_Dyn)*(nshaobj  /* DT_NEEDED */
                            + nrunpath /* DT_RUNPATH */
                            + 1        /* DT_HASH */
                            + 1        /* DT_STRTAB */
                            + 1        /* DT_SYMTAB */
                            + 1        /* DT_STRSZ */
                            + 1        /* DT_SYMENT */
                            + 1        /* DT_NULL */
                             );
    if (nreloc > 0)
        size += sizeof(Elf32_Dyn)*(1      /* DT_PLTGOT */
                                 + 1      /* DT_PLTRELSZ */
                                 + 1      /* DT_PLTREL */
                                 + 1      /* DT_JMPREL */
                                 + 1      /* DT_REL */
                                 + 1      /* DT_RELSZ */
                                 + 1      /* DT_RELENT */
                                  );
    if (emu_mode == EMU_MIPS)
        size += sizeof(Elf32_Dyn)*(1    /* DT_MIPS_RLD_VERSION */
                                 + 1    /* DT_MIPS_FLAGS */
                                 + 1    /* DT_MIPS_BASE_ADDRESS */
                                 + 1    /* DT_MIPS_PLTGOT */
                                 + 1    /* DT_MIPS_LOCAL_GOTNO */
                                 + 1    /* DT_MIPS_SYMTABNO */
                                 + 1    /* DT_MIPS_GOTSYM */
                                  );
    CS32_SHDR(sec).sh_size = size;
    CS32_SHDR(sec).sh_addralign = 4;
    CS32_SHDR(sec).sh_entsize = sizeof(Elf32_Dyn);
    buf = calloc(1, size);
    shdr = calloc(1, sizeof(Elf32_Shdr));
    *shdr = CS32_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    if (emu_mode == EMU_MIPS) {
        /*
         * For some reason the GNU MIPS dynamic linker
         * requires a .got section with (at least?) two
         * entries. I assume they are reserved for something.
         */

        /* .got */
        got_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".got";
        CS32_SHDR(sec).sh_type = SHT_PROGBITS;
        CS32_SHDR(sec).sh_flags = SHF_ALLOC|SHF_WRITE;
        size = 8;
        CS32_SHDR(sec).sh_size = size;
        CS32_SHDR(sec).sh_addralign = 16;
        CS32_SHDR(sec).sh_entsize = 4;
        buf = calloc(1, size);
        shdr = calloc(1, sizeof(Elf32_Shdr));
        *shdr = CS32_SHDR(sec);
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;
    }

    /* .interp */
    interp_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".interp";
    CS32_SHDR(sec).sh_type = SHT_PROGBITS;
    CS32_SHDR(sec).sh_flags = SHF_ALLOC; /* XXX: "If the file has a loadable segment
                                             that includes relocation..." */
    size = round_up(strlen(interp)+1, 4);
    CS32_SHDR(sec).sh_size = size;
    CS32_SHDR(sec).sh_addralign = 1;
    buf = calloc(1, size);
    strcpy(buf, interp);
    shdr = calloc(1, sizeof(Elf32_Shdr));
    *shdr = CS32_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;
}

static void init_dynlink_sections_64(void)
{
    char *buf;
    CmpndSec *sec;
    Elf64_Shdr *shdr;
    uint32_t size, nbucket;

    if (nreloc > 0) {
        /* .plt */
        plt_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".plt";
        CS64_SHDR(sec).sh_type = SHT_PROGBITS;
        CS64_SHDR(sec).sh_flags = SHF_ALLOC|SHF_EXECINSTR;
        size = PLT_ENTRY_NB*(1+nreloc); /* +1 for PLT0 */
        CS64_SHDR(sec).sh_size = size;
        CS64_SHDR(sec).sh_addralign = 16;
        /*sec->shdr.sh_entsize = 4;*/
        buf = calloc(1, size);
        shdr = calloc(1, sizeof(Elf64_Shdr));
        *shdr = CS64_SHDR(sec);
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;

        /* .rela.plt */
        relaplt_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".rela.plt";
        CS64_SHDR(sec).sh_type = SHT_RELA;
        CS64_SHDR(sec).sh_flags = SHF_ALLOC;
        size = sizeof(Elf64_Rela)*nreloc;
        CS64_SHDR(sec).sh_size = size;
        CS64_SHDR(sec).sh_addralign = 8;
        CS64_SHDR(sec).sh_entsize = sizeof(Elf64_Rela);
        buf = calloc(1, size);
        shdr = calloc(1, sizeof(Elf64_Shdr));
        *shdr = CS64_SHDR(sec);
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;

        /* .got.plt */
        gotplt_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".got.plt";
        CS64_SHDR(sec).sh_type = SHT_PROGBITS;
        CS64_SHDR(sec).sh_flags = SHF_ALLOC|SHF_WRITE;
        size = sizeof(Elf64_Xword)*(nreserved+nreloc);
        CS64_SHDR(sec).sh_size = size;
        CS64_SHDR(sec).sh_addralign = 8;
        CS64_SHDR(sec).sh_entsize = 8;
        buf = calloc(1, size);
        shdr = calloc(1, sizeof(Elf64_Shdr));
        *shdr = CS64_SHDR(sec);
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;

        /* .rela.dyn */
        reladyn_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".rela.dyn";
        CS64_SHDR(sec).sh_type = SHT_RELA;
        CS64_SHDR(sec).sh_flags = SHF_ALLOC;
        size = sizeof(Elf64_Rela)*(nreloc);
        CS64_SHDR(sec).sh_size = size;
        CS64_SHDR(sec).sh_addralign = 8;
        CS64_SHDR(sec).sh_entsize = sizeof(Elf64_Rela);
        buf = calloc(1, size);
        shdr = calloc(1, sizeof(Elf64_Shdr));
        *shdr = CS64_SHDR(sec);
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;
    }

    /* .dynstr */
    dynstr_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".dynstr";
    CS64_SHDR(sec).sh_type = SHT_STRTAB;
    CS64_SHDR(sec).sh_flags = SHF_ALLOC;
    if (nrunpath) {
        int i;
        char rbuf[MAX_RUNPATH_LEN], *rp;

        rp = rbuf;
        rp[0] = '\0';
        for (i = 0; i < nrunpath; i++) {
            strcat(rp, runpaths[i]);
            rp += strlen(runpaths[i]);
            if (i+1 < nrunpath) {
                strcat(rp, ":");
                ++rp;
            }
        }
        runpath_val = strtab_append(dynstr, rbuf);
    }
    size = round_up(strtab_get_size(dynstr), 4);
    CS64_SHDR(sec).sh_size = size;
    CS64_SHDR(sec).sh_addralign = 1;
    buf = calloc(1, size);
    strtab_copy(dynstr, buf);
    shdr = calloc(1, sizeof(Elf64_Shdr));
    *shdr = CS64_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .dynsym */
    dynsym_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".dynsym";
    CS64_SHDR(sec).sh_type = SHT_DYNSYM;
    CS64_SHDR(sec).sh_flags = SHF_ALLOC;
    size = sizeof(Elf64_Sym)*(nglobal+1); /* +1 for STN_UNDEF */
    CS64_SHDR(sec).sh_size = size;
    CS64_SHDR(sec).sh_addralign = 8;
    CS64_SHDR(sec).sh_entsize = sizeof(Elf64_Sym);
    buf = calloc(1, size);
    shdr = calloc(1, sizeof(Elf64_Shdr));
    *shdr = CS64_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .hash */
    hash_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".hash";
    CS64_SHDR(sec).sh_type = SHT_HASH;
    CS64_SHDR(sec).sh_flags = SHF_ALLOC;
    /* size: nbucket + nchain + buckets + chain */
    nbucket = elf_get_nbucket(nglobal+1);
    size = sizeof(Elf64_Word)*(2+nbucket+(nglobal+1));
    CS64_SHDR(sec).sh_size = size;
    CS64_SHDR(sec).sh_addralign = 8;
    CS64_SHDR(sec).sh_entsize = 4;
    buf = calloc(1, size);
    *(Elf64_Word *)buf = nbucket;
    *((Elf64_Word *)buf+1) = nglobal+1;
    shdr = calloc(1, sizeof(Elf64_Shdr));
    *shdr = CS64_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .dynamic */
    dynamic_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".dynamic";
    CS64_SHDR(sec).sh_type = SHT_DYNAMIC;
    CS64_SHDR(sec).sh_flags = SHF_ALLOC|SHF_WRITE;
    size = sizeof(Elf64_Dyn)*(nshaobj /* DT_NEEDED */
                            + 1       /* DT_HASH */
                            + 1       /* DT_STRTAB */
                            + 1       /* DT_SYMTAB */
                            + 1       /* DT_STRSZ */
                            + 1       /* DT_SYMENT */
                            + 1       /* DT_NULL */
                             );
    if (nreloc > 0)
        size += sizeof(Elf64_Dyn)*(1      /* DT_PLTGOT */
                                 + 1      /* DT_PLTRELSZ */
                                 + 1      /* DT_PLTREL */
                                 + 1      /* DT_JMPREL */
                                 + 1      /* DT_REL */
                                 + 1      /* DT_RELSZ */
                                 + 1      /* DT_RELENT */
                                  );
    CS64_SHDR(sec).sh_size = size;
    CS64_SHDR(sec).sh_addralign = 8;
    CS64_SHDR(sec).sh_entsize = sizeof(Elf64_Dyn);
    buf = calloc(1, size);
    shdr = calloc(1, sizeof(Elf64_Shdr));
    *shdr = CS64_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .interp */
    interp_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".interp";
    CS64_SHDR(sec).sh_type = SHT_PROGBITS;
    CS64_SHDR(sec).sh_flags = SHF_ALLOC; /* XXX: "If the file has a loadable segment
                                             that includes relocation..." */
    size = round_up(strlen(interp)+1, 4);
    CS64_SHDR(sec).sh_size = size;
    CS64_SHDR(sec).sh_addralign = 1;
    buf = calloc(1, size);
    strcpy(buf, interp);
    shdr = calloc(1, sizeof(Elf64_Shdr));
    *shdr = CS64_SHDR(sec);
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;
}

static void add_section_32(ObjFile *obj, char *name, Elf32_Shdr *hdr)
{
    CmpndSec *sec;

    for (sec = sections; sec != NULL; sec = sec->next) {
        if (equal(sec->name, name)) {
            assert(CS32_SHDR(sec).sh_type == hdr->sh_type); /* TBD */
            CS32_SHDR(sec).sh_flags |= hdr->sh_flags;
            CS32_SHDR(sec).sh_size += round_up(hdr->sh_size, 4);
            if (hdr->sh_addralign > CS32_SHDR(sec).sh_addralign)
                CS32_SHDR(sec).sh_addralign = hdr->sh_addralign;
            sec->sslist = new_smpl_sec(obj, hdr, obj->buf+hdr->sh_offset, sec->sslist);
            if (CS32_SHDR(sec).sh_type == SHT_REL)
                nreloc += hdr->sh_size/sizeof(Elf32_Rel); /* x86, mips, and arm use REL */
            break;
        }
    }
    if (sec == NULL) {
        sec = calloc(1, sizeof(CmpndSec));
        sec->name = name;
        CS32_SHDR(sec) = *hdr;
        CS32_SHDR(sec).sh_size = round_up(hdr->sh_size, 4);
        sec->sslist = new_smpl_sec(obj, hdr, obj->buf+hdr->sh_offset, NULL);
        sec->next = sections;
        sections = sec;
        if (CS32_SHDR(sec).sh_type == SHT_REL)
            nreloc += hdr->sh_size/sizeof(Elf32_Rel);
    }
}

static void add_section_64(ObjFile *obj, char *name, Elf64_Shdr *hdr)
{
    CmpndSec *sec;

    for (sec = sections; sec != NULL; sec = sec->next) {
        if (equal(sec->name, name)) {
            assert(CS64_SHDR(sec).sh_type == hdr->sh_type); /* TBD */
            CS64_SHDR(sec).sh_flags |= hdr->sh_flags;
            CS64_SHDR(sec).sh_size += round_up(hdr->sh_size, 4);
            if (hdr->sh_addralign > CS64_SHDR(sec).sh_addralign)
                CS64_SHDR(sec).sh_addralign = hdr->sh_addralign;
            sec->sslist = new_smpl_sec(obj, hdr, obj->buf+hdr->sh_offset, sec->sslist);
            if (CS64_SHDR(sec).sh_type == SHT_RELA) {
                assert(emu_mode == EMU_X64);
                nreloc += hdr->sh_size/sizeof(Elf64_Rela);
            }
            break;
        }
    }
    if (sec == NULL) {
        sec = calloc(1, sizeof(CmpndSec));
        sec->name = name;
        CS64_SHDR(sec) = *hdr;
        CS64_SHDR(sec).sh_size = round_up(hdr->sh_size, 4);
        sec->sslist = new_smpl_sec(obj, hdr, obj->buf+hdr->sh_offset, NULL);
        sec->next = sections;
        sections = sec;
        if (CS64_SHDR(sec).sh_type == SHT_RELA) {
            assert(emu_mode == EMU_X64);
            nreloc += hdr->sh_size/sizeof(Elf32_Rela);
        }
    }
}

static void init_sections_32(void)
{
    ObjFile *obj;

    for (obj = object_files; obj != NULL; obj = obj->next) {
        int i;
        Elf32_Shdr *shdr;

        shdr = OF32_SHTAB(obj)+1; /* skip SHN_UNDEF */
        for (i = 1; i < OF32_EHDR(obj)->e_shnum; i++, shdr++)
            add_section_32(obj, obj->shstrtab+shdr->sh_name, shdr);
    }
    if (shared_object_files != NULL)
        init_dynlink_sections_32();
}

static void init_sections_64(void)
{
    ObjFile *obj;

    for (obj = object_files; obj != NULL; obj = obj->next) {
        int i;
        Elf64_Shdr *shdr;

        shdr = OF64_SHTAB(obj)+1; /* skip SHN_UNDEF */
        for (i = 1; i < OF64_EHDR(obj)->e_shnum; i++, shdr++)
            add_section_64(obj, obj->shstrtab+shdr->sh_name, shdr);
    }
    if (shared_object_files != NULL)
        init_dynlink_sections_64();
}

static void init_segments_32(void)
{
    int i;
    SmplSec *ssec;
    CmpndSec *csec;
    uint32_t size;
    unsigned long vaddr;
    unsigned long offset = 0;

    switch (emu_mode) {
    case EMU_X86:  vaddr = 0x8048000; break;
    case EMU_MIPS: vaddr = 0x400000;  break;
    case EMU_ARM:  vaddr = 0x8000;    break;
    }

    for (csec = sections; csec != NULL; csec = csec->next) {
        if (!(CS32_SHDR(csec).sh_flags&SHF_ALLOC))
            continue;
        if (CS32_SHDR(csec).sh_flags & SHF_WRITE)
            WRSeg.secs[WRSeg.nsec++] = csec;
        else
            ROSeg.secs[ROSeg.nsec++] = csec;
    }
    if (!ROSeg.nsec && !WRSeg.nsec)
        err("Input files without loadable sections!");

    offset += round_up(sizeof(Elf32_Ehdr), 16);
    if (ROSeg.nsec > 0)
        offset += sizeof(Elf32_Phdr);
    if (WRSeg.nsec > 0)
        offset += sizeof(Elf32_Phdr);
    if (shared_object_files != NULL)
        offset += sizeof(Elf32_Phdr)*2; /* PT_INTERP+PT_DYNAMIC */

    if (ROSeg.nsec > 0) {
        SEG32_PHDR(&ROSeg).p_type = PT_LOAD;
        SEG32_PHDR(&ROSeg).p_flags = PF_R|PF_X;
        SEG32_PHDR(&ROSeg).p_align = 0x1000;

        /* the ELF header and program header table are mapped into the read-only segment */
        SEG32_PHDR(&ROSeg).p_offset = 0;
        SEG32_PHDR(&ROSeg).p_vaddr = SEG32_PHDR(&ROSeg).p_paddr = vaddr;
        vaddr += offset;
        for (i = 0; i < ROSeg.nsec; i++) {
            CS32_SHDR(ROSeg.secs[i]).sh_addr = vaddr;
            CS32_SHDR(ROSeg.secs[i]).sh_offset = offset;
            ROSeg.secs[i]->shndx = shndx++;
            for (ssec=ROSeg.secs[i]->sslist, size=0; ssec != NULL; ssec = ssec->next) {
                SS32_SHDR(ssec)->sh_addr = vaddr+size;
                size += round_up(SS32_SHDR(ssec)->sh_size, 4);
            }
            offset += size;
            vaddr += size;
        }
        SEG32_PHDR(&ROSeg).p_filesz = offset;
        SEG32_PHDR(&ROSeg).p_memsz = SEG32_PHDR(&ROSeg).p_filesz;
    }

    if (WRSeg.nsec > 0) {
        CmpndSec *bss = NULL;

        SEG32_PHDR(&WRSeg).p_type = PT_LOAD;
        SEG32_PHDR(&WRSeg).p_flags = PF_R|PF_W;
        SEG32_PHDR(&WRSeg).p_align = 0x1000;

        SEG32_PHDR(&WRSeg).p_offset = offset;
        /*
         * If p_align!=0 and p_align!=1, then the following must be true:
         *      p_offset%p_align == p_vaddr%p_align
         */
        vaddr = round_up(vaddr, PAGE_SIZE) + (PAGE_MASK & offset);
        SEG32_PHDR(&WRSeg).p_vaddr = SEG32_PHDR(&WRSeg).p_paddr = vaddr;
        for (i = 0; i < WRSeg.nsec; i++) {
            if (CS32_SHDR(WRSeg.secs[i]).sh_type == SHT_NOBITS) {
                bss = WRSeg.secs[i];
                continue;
            }
            CS32_SHDR(WRSeg.secs[i]).sh_addr = vaddr;
            CS32_SHDR(WRSeg.secs[i]).sh_offset = offset;
            WRSeg.secs[i]->shndx = shndx++;
            for (ssec=WRSeg.secs[i]->sslist, size=0; ssec != NULL; ssec = ssec->next) {
                SS32_SHDR(ssec)->sh_addr = vaddr+size;
                size += round_up(SS32_SHDR(ssec)->sh_size, 4);
            }
            offset += size;
            vaddr += size;
        }
        SEG32_PHDR(&WRSeg).p_filesz = offset-SEG32_PHDR(&ROSeg).p_filesz;
        SEG32_PHDR(&WRSeg).p_memsz = SEG32_PHDR(&WRSeg).p_filesz;
        if (bss != NULL) {
            bss_sec = bss;
            CS32_SHDR(bss).sh_addr = vaddr;
            CS32_SHDR(bss).sh_offset = offset;
            bss->shndx = shndx++;
            for (ssec=bss->sslist, size=0; ssec != NULL; ssec = ssec->next) {
                SS32_SHDR(ssec)->sh_addr = vaddr+size;
                size += round_up(SS32_SHDR(ssec)->sh_size, 4);
            }
            SEG32_PHDR(&WRSeg).p_memsz += CS32_SHDR(bss).sh_size;
        }
    }
}

static void init_segments_64(void)
{
    int i;
    SmplSec *ssec;
    CmpndSec *csec;
    uint32_t size;
    unsigned long vaddr = 0x400000;
    unsigned long offset = 0;

    for (csec = sections; csec != NULL; csec = csec->next) {
        if (!(CS64_SHDR(csec).sh_flags&SHF_ALLOC))
            continue;
        if (CS64_SHDR(csec).sh_flags & SHF_WRITE)
            WRSeg.secs[WRSeg.nsec++] = csec;
        else
            ROSeg.secs[ROSeg.nsec++] = csec;
    }
    if (!ROSeg.nsec && !WRSeg.nsec)
        err("Input files without loadable sections!");

    offset += round_up(sizeof(Elf64_Ehdr), 16);
    if (ROSeg.nsec > 0)
        offset += sizeof(Elf64_Phdr);
    if (WRSeg.nsec > 0)
        offset += sizeof(Elf64_Phdr);
    if (shared_object_files != NULL)
        offset += sizeof(Elf64_Phdr)*2; /* PT_INTERP+PT_DYNAMIC */

    if (ROSeg.nsec > 0) {
        SEG64_PHDR(&ROSeg).p_type = PT_LOAD;
        SEG64_PHDR(&ROSeg).p_flags = PF_R|PF_X;
        SEG64_PHDR(&ROSeg).p_align = 0x1000;

        /* the ELF header and program header table are mapped into the read-only segment */
        SEG64_PHDR(&ROSeg).p_offset = 0;
        SEG64_PHDR(&ROSeg).p_vaddr = SEG64_PHDR(&ROSeg).p_paddr = vaddr;
        vaddr += offset;
        for (i = 0; i < ROSeg.nsec; i++) {
            CS64_SHDR(ROSeg.secs[i]).sh_addr = vaddr;
            CS64_SHDR(ROSeg.secs[i]).sh_offset = offset;
            ROSeg.secs[i]->shndx = shndx++;
            for (ssec=ROSeg.secs[i]->sslist, size=0; ssec != NULL; ssec = ssec->next) {
                SS64_SHDR(ssec)->sh_addr = vaddr+size;
                size += round_up(SS64_SHDR(ssec)->sh_size, 4);
            }
            offset += size;
            vaddr += size;
        }
        SEG64_PHDR(&ROSeg).p_filesz = offset;
        SEG64_PHDR(&ROSeg).p_memsz = SEG64_PHDR(&ROSeg).p_filesz;
    }

    if (WRSeg.nsec > 0) {
        CmpndSec *bss = NULL;

        SEG64_PHDR(&WRSeg).p_type = PT_LOAD;
        SEG64_PHDR(&WRSeg).p_flags = PF_R|PF_W;
        SEG64_PHDR(&WRSeg).p_align = 0x1000;

        SEG64_PHDR(&WRSeg).p_offset = offset;
        /*
         * If p_align!=0 and p_align!=1, then the following must be true:
         *      p_offset%p_align == p_vaddr%p_align
         */
        vaddr = round_up(vaddr, PAGE_SIZE) + (PAGE_MASK & offset);
        SEG64_PHDR(&WRSeg).p_vaddr = SEG64_PHDR(&WRSeg).p_paddr = vaddr;
        for (i = 0; i < WRSeg.nsec; i++) {
            if (CS64_SHDR(WRSeg.secs[i]).sh_type == SHT_NOBITS) {
                bss = WRSeg.secs[i];
                continue;
            }
            CS64_SHDR(WRSeg.secs[i]).sh_addr = vaddr;
            CS64_SHDR(WRSeg.secs[i]).sh_offset = offset;
            WRSeg.secs[i]->shndx = shndx++;
            for (ssec=WRSeg.secs[i]->sslist, size=0; ssec != NULL; ssec = ssec->next) {
                SS64_SHDR(ssec)->sh_addr = vaddr+size;
                size += round_up(SS64_SHDR(ssec)->sh_size, 4);
            }
            offset += size;
            vaddr += size;
        }
        SEG64_PHDR(&WRSeg).p_filesz = offset-SEG64_PHDR(&ROSeg).p_filesz;
        SEG64_PHDR(&WRSeg).p_memsz = SEG64_PHDR(&WRSeg).p_filesz;
        if (bss != NULL) {
            bss_sec = bss;
            CS64_SHDR(bss).sh_addr = vaddr;
            CS64_SHDR(bss).sh_offset = offset;
            bss->shndx = shndx++;
            for (ssec=bss->sslist, size=0; ssec != NULL; ssec = ssec->next) {
                SS64_SHDR(ssec)->sh_addr = vaddr+size;
                size += round_up(SS64_SHDR(ssec)->sh_size, 4);
            }
            SEG64_PHDR(&WRSeg).p_memsz += CS64_SHDR(bss).sh_size;
        }
    }
}

void define_local_symbol(char *name, uint64_t value, unsigned char info, Elf_Half shndx, char *shname)
{
    Symbol *np;

    np = calloc(1, sizeof(Symbol));
    np->name = name;
    np->value = value;
    np->info = info;
    np->shname = shname;
    np->shndx = shndx;
    np->next = local_symbols;
    local_symbols = np;
}

/* return index into output file's section header table */
Elf_Half get_shndx(Symbol *sym)
{
    CmpndSec *csec;

    if (sym->shndx==SHN_UNDEF || sym->shndx>=SHN_LORESERVE)
        return sym->shndx;
    for (csec = sections; csec != NULL; csec = csec->next)
        if (equal(csec->name, sym->shname))
            return csec->shndx;
    assert(!"section not found");
}

/*
 * Install local symbols and assign final run-time addresses
 * to global symbols (build .dynsym and .hash in the process).
 */
static void init_symtab_32(void)
{
    SmplSec *ssec;
    CmpndSec *csec;
    Elf32_Sym *sp;
    Elf32_Sword n;
    Elf32_Half symndx;
    Elf32_Word nbucket, h;
    Elf32_Word *buckets, *chain;

    for (csec = sections; csec != NULL; csec = csec->next)
        if (CS32_SHDR(csec).sh_type == SHT_SYMTAB)
            break;
    if (csec == NULL)
        return;

    if (shared_object_files != NULL) {
        symndx = 1;
        sp = (Elf32_Sym *)dynsym_sec->sslist->data+1;
        nbucket = *(Elf32_Word *)hash_sec->sslist->data;
        buckets = (Elf32_Word *)(hash_sec->sslist->data+sizeof(Elf32_Word)*2);
        chain = buckets+nbucket;
    }
    for (ssec = csec->sslist; ssec != NULL; ssec = ssec->next) {
        int i, nsym;
        Elf32_Sym *symtab;
        Elf32_Shdr *shtab;
        char *strtab, *shstrtab;

        nsym = SS32_SHDR(ssec)->sh_size/sizeof(Elf32_Sym);
        symtab = OF32_SYMTAB(ssec->obj);
        shtab = OF32_SHTAB(ssec->obj);
        strtab = ssec->obj->strtab;
        shstrtab = ssec->obj->shstrtab;
        for (i = 1; i < nsym; i++) {
            switch (ELF32_ST_BIND(symtab[i].st_info)) {
            case STB_LOCAL:
                switch (ELF32_ST_TYPE(symtab[i].st_info)) {
                case STT_SECTION:
                    symtab[i].st_value = shtab[symtab[i].st_shndx].sh_addr;
                    break;
                case STT_FILE:
                    break;
                default:
                    if (symtab[i].st_shndx >= SHN_LORESERVE) {
                        define_local_symbol(&strtab[symtab[i].st_name], symtab[i].st_value,
                        symtab[i].st_info, symtab[i].st_shndx, NULL);
                    } else {
                        symtab[i].st_value += shtab[symtab[i].st_shndx].sh_addr;
                        define_local_symbol(&strtab[symtab[i].st_name], symtab[i].st_value,
                        symtab[i].st_info, symtab[i].st_shndx, &shstrtab[shtab[symtab[i].st_shndx].sh_name]);
                    }
                    break;
                }
                break;

            case STB_GLOBAL: {
                Symbol *sym;

                sym = lookup_global_symbol(&strtab[symtab[i].st_name]);
                assert(sym != NULL);
                if (symtab[i].st_shndx != SHN_UNDEF)
                    sym->value = shtab[symtab[i].st_shndx].sh_addr+symtab[i].st_value;
                if (shared_object_files!=NULL && !sym->in_dynsym) {
                    /* we build .dynsym and .hash simultaneously */
                    sp->st_value = sym->value;
                    sp->st_name = strtab_get_offset(dynstr, sym->name);
                    sp->st_info = sym->info;
                    sp->st_shndx = get_shndx(sym);
                    h = elf_hash((unsigned char *)sym->name)%nbucket;
                    n = h-nbucket; /* index into buckets[] */
                    while (chain[n] != STN_UNDEF)
                        n = chain[n]; /* index into chain[] */
                    chain[n] = symndx;
                    ++sp, ++symndx;
                    sym->in_dynsym = TRUE;
                }
            }
                break;

            case STB_WEAK:
                /* TODO */
                break;
            }
        }
    }
}

static void init_symtab_64(void)
{
    SmplSec *ssec;
    CmpndSec *csec;
    Elf64_Sym *sp;
    Elf64_Sword n;
    Elf64_Half symndx;
    Elf32_Word nbucket, h;
    Elf32_Word *buckets, *chain;

    for (csec = sections; csec != NULL; csec = csec->next)
        if (CS64_SHDR(csec).sh_type == SHT_SYMTAB)
            break;
    if (csec == NULL)
        return;

    if (shared_object_files != NULL) {
        symndx = 1;
        sp = (Elf64_Sym *)dynsym_sec->sslist->data+1;
        nbucket = *(Elf32_Word *)hash_sec->sslist->data;
        buckets = (Elf32_Word *)(hash_sec->sslist->data+sizeof(Elf32_Word)*2);
        chain = buckets+nbucket;
    }
    for (ssec = csec->sslist; ssec != NULL; ssec = ssec->next) {
        int i, nsym;
        Elf64_Sym *symtab;
        Elf64_Shdr *shtab;
        char *strtab, *shstrtab;

        nsym = SS64_SHDR(ssec)->sh_size/sizeof(Elf64_Sym);
        symtab = OF64_SYMTAB(ssec->obj);
        shtab = OF64_SHTAB(ssec->obj);
        strtab = ssec->obj->strtab;
        shstrtab = ssec->obj->shstrtab;
        for (i = 1; i < nsym; i++) {
            switch (ELF64_ST_BIND(symtab[i].st_info)) {
            case STB_LOCAL:
                switch (ELF64_ST_TYPE(symtab[i].st_info)) {
                case STT_SECTION:
                    symtab[i].st_value = shtab[symtab[i].st_shndx].sh_addr;
                    break;
                case STT_FILE:
                    break;
                default:
                    if (symtab[i].st_shndx >= SHN_LORESERVE) {
                        define_local_symbol(&strtab[symtab[i].st_name], symtab[i].st_value,
                        symtab[i].st_info, symtab[i].st_shndx, NULL);
                    } else {
                        symtab[i].st_value += shtab[symtab[i].st_shndx].sh_addr;
                        define_local_symbol(&strtab[symtab[i].st_name], symtab[i].st_value,
                        symtab[i].st_info, symtab[i].st_shndx, &shstrtab[shtab[symtab[i].st_shndx].sh_name]);
                    }
                    break;
                }
                break;

            case STB_GLOBAL: {
                Symbol *sym;

                sym = lookup_global_symbol(&strtab[symtab[i].st_name]);
                assert(sym != NULL);
                if (symtab[i].st_shndx != SHN_UNDEF)
                    sym->value = shtab[symtab[i].st_shndx].sh_addr+symtab[i].st_value;
                if (shared_object_files!=NULL && !sym->in_dynsym) {
                    sp->st_value = sym->value;
                    sp->st_name = strtab_get_offset(dynstr, sym->name);
                    sp->st_info = sym->info;
                    sp->st_shndx = get_shndx(sym);
                    h = elf_hash((unsigned char *)sym->name)%nbucket;
                    n = h-nbucket;
                    while (chain[n] != STN_UNDEF)
                        n = chain[n];
                    chain[n] = symndx;
                    ++sp, ++symndx;
                    sym->in_dynsym = TRUE;
                }
            }
                break;

            case STB_WEAK:
                /* TODO */
                break;
            }
        }
    }
}

Elf32_Addr get_symval_32(char *name, Elf32_Sym *st_ent, bool *found)
{
    Symbol *sym;

    *found = TRUE;
    if (ELF32_ST_BIND(st_ent->st_info) == STB_LOCAL)
        return st_ent->st_value;
    sym = lookup_global_symbol(name);
    assert(sym != NULL);
    if (sym->shndx == SHN_UNDEF)
        *found = FALSE;
    else
        return (Elf32_Addr)sym->value;
    return 0;
}

Elf64_Addr get_symval_64(char *name, Elf64_Sym *st_ent, bool *found)
{
    Symbol *sym;

    *found = TRUE;
    if (ELF64_ST_BIND(st_ent->st_info) == STB_LOCAL)
        return st_ent->st_value;
    sym = lookup_global_symbol(name);
    assert(sym != NULL);
    if (sym->shndx == SHN_UNDEF)
        *found = FALSE;
    else
        return sym->value;
    return 0;
}

static void process_shared_object_file(char *buf, char *path)
{
    int i;
    ShrdObjFile *so;
    enum {
        REQ_DYNSYM  = 0x01,
        REQ_DYNAMIC = 0x02,
        REQ_HASH    = 0x04,
    };
    unsigned missing;

    missing = REQ_DYNSYM+REQ_DYNAMIC+REQ_HASH;
    so = calloc(1, sizeof(ShrdObjFile));
    so->buf = buf;
    if (EMU32()) {
        Elf32_Dyn *dp;

        SO32_EHDR(so) = (Elf32_Ehdr *)buf;
        SO32_SHTAB(so) = (Elf32_Shdr *)(buf+SO32_EHDR(so)->e_shoff);
        for (i = 1; i<SO32_EHDR(so)->e_shnum && missing; i++) {
            if (SO32_SHTAB(so)[i].sh_type == SHT_DYNSYM) {
                SO32_DYNSYM(so) = (Elf32_Sym *)(buf+SO32_SHTAB(so)[i].sh_offset);
                so->nsym = SO32_SHTAB(so)[i].sh_size/sizeof(Elf32_Sym);
                so->dynstr = buf+SO32_SHTAB(so)[SO32_SHTAB(so)[i].sh_link].sh_offset;
                missing &= ~REQ_DYNSYM;
            } else if (SO32_SHTAB(so)[i].sh_type == SHT_DYNAMIC) {
                SO32_DYN(so) = (Elf32_Dyn *)(buf+SO32_SHTAB(so)[i].sh_offset);
                missing &= ~REQ_DYNAMIC;
            } else if (SO32_SHTAB(so)[i].sh_type == SHT_HASH) {
                so->nbucket = *((Elf32_Word *)(buf+SO32_SHTAB(so)[i].sh_offset));
                so->hash = (Elf32_Word *)(buf+SO32_SHTAB(so)[i].sh_offset)+2;
                so->chain = so->hash+so->nbucket;
                missing &= ~REQ_HASH;
            }
        }
        assert(missing == 0); /* TBD (probably .hash is missing because there is .gnu.hash instead) */
        for (dp = SO32_DYN(so); dp->d_tag != DT_NULL; dp++) {
            if (dp->d_tag == DT_SONAME) {
                so->name = &so->dynstr[dp->d_un.d_val];
                break;
            }
        }
        if (dp->d_tag == DT_NULL)
            so->name = strdup(path);
    } else {
        Elf64_Dyn *dp;

        SO64_EHDR(so) = (Elf64_Ehdr *)buf;
        SO64_SHTAB(so) = (Elf64_Shdr *)(buf+SO64_EHDR(so)->e_shoff);
        for (i = 1; i<SO64_EHDR(so)->e_shnum && missing; i++) {
            if (SO64_SHTAB(so)[i].sh_type == SHT_DYNSYM) {
                SO64_DYNSYM(so) = (Elf64_Sym *)(buf+SO64_SHTAB(so)[i].sh_offset);
                so->nsym = SO64_SHTAB(so)[i].sh_size/sizeof(Elf64_Sym);
                so->dynstr = buf+SO64_SHTAB(so)[SO64_SHTAB(so)[i].sh_link].sh_offset;
                missing &= ~REQ_DYNSYM;
            } else if (SO64_SHTAB(so)[i].sh_type == SHT_DYNAMIC) {
                SO64_DYN(so) = (Elf64_Dyn *)(buf+SO64_SHTAB(so)[i].sh_offset);
                missing &= ~REQ_DYNAMIC;
            } else if (SO64_SHTAB(so)[i].sh_type == SHT_HASH) {
                so->nbucket = *((Elf64_Word *)(buf+SO64_SHTAB(so)[i].sh_offset));
                so->hash = (Elf64_Word *)(buf+SO64_SHTAB(so)[i].sh_offset)+2;
                so->chain = so->hash+so->nbucket;
                missing &= ~REQ_HASH;
            }
        }
        assert(missing == 0);
        for (dp = SO64_DYN(so); dp->d_tag != DT_NULL; dp++) {
            if (dp->d_tag == DT_SONAME) {
                so->name = &so->dynstr[dp->d_un.d_val];
                break;
            }
        }
        if (dp->d_tag == DT_NULL)
            so->name = strdup(path);
    }
    strtab_append(dynstr, so->name);
    so->next = shared_object_files;
    shared_object_files = so;
    ++nshaobj;
}

static void process_object_file(char *buf)
{
    int i;
    ObjFile *obj;
    int first_gsym;

    obj = calloc(1, sizeof(ObjFile));
    obj->buf = buf;
    if (EMU32()) {
        OF32_EHDR(obj) = (Elf32_Ehdr *)buf;
        OF32_SHTAB(obj) = (Elf32_Shdr *)(buf+OF32_EHDR(obj)->e_shoff);
        obj->shstrtab = buf+OF32_SHTAB(obj)[OF32_EHDR(obj)->e_shstrndx].sh_offset;
        for (i = 1; i < OF32_EHDR(obj)->e_shnum; i++) {
            if (OF32_SHTAB(obj)[i].sh_type == SHT_SYMTAB) {
                OF32_SYMTAB(obj) = (Elf32_Sym *)(buf+OF32_SHTAB(obj)[i].sh_offset);
                obj->nsym = OF32_SHTAB(obj)[i].sh_size/sizeof(Elf32_Sym);
                obj->strtab = buf+OF32_SHTAB(obj)[OF32_SHTAB(obj)[i].sh_link].sh_offset;
                first_gsym = OF32_SHTAB(obj)[i].sh_info;
                break;
            }
        }
        obj->next = object_files;
        object_files = obj;

        /* install any global symbol */
        if (first_gsym < obj->nsym) {
            Elf32_Sym *symtab = OF32_SYMTAB(obj);
            Elf32_Shdr *shtab = OF32_SHTAB(obj);
            char *strtab = obj->strtab;
            char *shstrtab = obj->shstrtab;

            for (i = first_gsym; i < obj->nsym; i++) {
                char *shname = (symtab[i].st_shndx==SHN_UNDEF || symtab[i].st_shndx>=SHN_LORESERVE)
                             ? NULL : &shstrtab[shtab[symtab[i].st_shndx].sh_name];
                define_global_symbol(&strtab[symtab[i].st_name], 0, symtab[i].st_info, symtab[i].st_shndx, shname);
            }
        }
    } else {
        OF64_EHDR(obj) = (Elf64_Ehdr *)buf;
        OF64_SHTAB(obj) = (Elf64_Shdr *)(buf+OF64_EHDR(obj)->e_shoff);
        obj->shstrtab = buf + OF64_SHTAB(obj)[OF64_EHDR(obj)->e_shstrndx].sh_offset;
        for (i = 1; i < OF64_EHDR(obj)->e_shnum; i++) {
            if (OF64_SHTAB(obj)[i].sh_type == SHT_SYMTAB) {
                OF64_SYMTAB(obj) = (Elf64_Sym *)(buf+OF64_SHTAB(obj)[i].sh_offset);
                obj->nsym = OF64_SHTAB(obj)[i].sh_size/sizeof(Elf64_Sym);
                obj->strtab = buf+OF64_SHTAB(obj)[OF64_SHTAB(obj)[i].sh_link].sh_offset;
                first_gsym = OF64_SHTAB(obj)[i].sh_info;
                break;
            }
        }
        obj->next = object_files;
        object_files = obj;

        /* install any global symbol */
        if (first_gsym < obj->nsym) {
            Elf64_Sym *symtab = OF64_SYMTAB(obj);
            Elf64_Shdr *shtab = OF64_SHTAB(obj);
            char *strtab = obj->strtab;
            char *shstrtab = obj->shstrtab;

            for (i = first_gsym; i < obj->nsym; i++) {
                char *shname = (symtab[i].st_shndx==SHN_UNDEF || symtab[i].st_shndx>=SHN_LORESERVE)
                             ? NULL : &shstrtab[shtab[symtab[i].st_shndx].sh_name];
                define_global_symbol(&strtab[symtab[i].st_name], 0, symtab[i].st_info, symtab[i].st_shndx, shname);
            }
        }
    }
}

void process_archive(char *buf)
{
    char *strtab, *cp;
    struct ar_hdr *hdr;
    int i, nsym;
    int *offs;
    bool added;

    if (nundef == 0)
        return;

    cp = buf+SARMAG;
    hdr = (struct ar_hdr *)cp;
    if (hdr->ar_name[0]!='/' || hdr->ar_name[1]!=' ')
        return; /* no archive symbol table */
    cp = (char *)(hdr+1);
    nsym = be_atoi(cp);
    cp += 4;
    offs = malloc(sizeof(int)*nsym);
    for (i = 0; i < nsym; i++)
        offs[i]=be_atoi(cp), cp+=4;
    strtab = cp;
repeat:
    added = FALSE;
    for (i = 0; i < nsym; i++) {
        Symbol *sym;

        if ((sym=lookup_global_symbol(cp))==NULL || sym->shndx!=SHN_UNDEF) {
            cp += strlen(cp)+1;
            continue;
        }
        process_object_file(buf+offs[i]+sizeof(struct ar_hdr)), added=TRUE;
        if (nundef == 0)
            goto done; /* that file resolved everything up to this point
                          and didn't include any unresolved symbol */
        cp += strlen(cp)+1;
    }
    if (nundef!=0 && added) {
        /* OK, the added file(s) included undefined symbols.
           See if those symbols can be resolved by another file in the archive. */
        cp = strtab;
        goto repeat;
    }
done:
    free(offs);
}

/*
 * Identify and process the file located at path.
 * When the file is a shared library, needed_path will be used as the path that appears
 * in a DT_NEEDED element of the ouput file (unless the library has a DT_SONAME element).
 */
static void process_file(char *path, char *needed_path)
{
    if ((fbuf[nfbuf]=read_file(path)) == NULL)
        err("cannot read file `%s'", path);
    if (strncmp(fbuf[nfbuf], ARMAG, SARMAG) == 0) {
        process_archive(fbuf[nfbuf]);
    } else if (strncmp(fbuf[nfbuf], "\x7f""ELF", 3) == 0) {
        if (EMU32()) {
            Elf32_Ehdr *eh;

            eh = (Elf32_Ehdr *)fbuf[nfbuf];
            if (eh->e_ident[EI_CLASS]!=ELFCLASS32
            || (eh->e_machine!=EM_386 && eh->e_machine!=EM_MIPS && eh->e_machine!=EM_ARM))
                goto unk_arch;
            if (eh->e_type == ET_REL)
                process_object_file(fbuf[nfbuf]);
            else if (eh->e_type == ET_DYN)
                process_shared_object_file(fbuf[nfbuf], needed_path);
            else
                goto unk_obj_ty;
        } else {
            Elf64_Ehdr *eh;

            eh = (Elf64_Ehdr *)fbuf[nfbuf];
            if (eh->e_ident[EI_CLASS]!=ELFCLASS64 || eh->e_machine!=EM_X86_64)
                goto unk_arch;
            if (eh->e_type == ET_REL)
                process_object_file(fbuf[nfbuf]);
            else if (eh->e_type == ET_DYN)
                process_shared_object_file(fbuf[nfbuf], needed_path);
            else
                goto unk_obj_ty;
        }
        goto done; /* OK */
    unk_obj_ty:
        err("file `%s': unknown object file type", path);
    unk_arch:
        err("file `%s': relocatable/shared object for unknown architecture", path);
    } else {
        err("file `%s': unknown file format", path);
    }
done:
    ++nfbuf;
}

int main(int argc, char *argv[])
{
    int i;
    /*bool verbose = FALSE;*/
    char *out_name = "a.out";
    char *dirs[32];
    char chmod_cmd[256];
    FILE *outfp;
    int ndir = 0;

    prog_name = argv[0];
    if (argc < 2)
        err("no input files");
    dynstr = strtab_new();
    emu_mode = -1;

    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '-') {
            process_file(argv[i], argv[i]);
            continue;
        } else if (argv[i][1] == '\0') {
            continue;
        }
        switch (argv[i][1]) {
        case 'o':
            if (argv[i][2] != '\0')
                out_name = argv[i]+2;
            else if (argv[i+1] != NULL)
                out_name = argv[++i];
            break;
        case 'e':
            if (argv[i][2] != '\0')
                entry_symbol = argv[i]+2;
            else if (argv[i+1] != NULL)
                entry_symbol = argv[++i];
            break;
        case 'l': {
            int k, done;
            char libname[64], *p = argv[i][2] ? argv[i]+2 : argv[++i];

            /*
             * If the argument starts with ':' search that verbatim,
             * otherwise first search for 'lib<...>.so' and later for
             * 'lib<...>.a'.
             */
            if (p == NULL)
                break;
            if (p[0] == ':') {
                strcpy(libname, p+1);
                done = TRUE;
            } else {
                strcpy(libname, "lib");
                strcat(libname, p);
                strcat(libname, ".so");
                done = FALSE;
            }
search:
            for (k = 0; k < ndir; k++) {
                char path[256];

                sprintf(path, "%s/%s", dirs[k], libname);
                if (file_exists(path)) {
                    process_file(path, libname);
                    break;
                }
            }
            if (k == ndir) {
                if (done) {
                    err("cannot find library `lib%s.so' nor `lib%s.a'", p, p);
                } else {
                    char *c;

                    c = strrchr(libname, '.');
                    c[1] = 'a', c[2] = '\0';
                    done = TRUE;
                    goto search;
                }
            }
        }
            break;
        case 'L':
            if (argv[i][2] != '\0')
                dirs[ndir++] = argv[i]+2;
            else if (argv[i+1] != NULL)
                dirs[ndir++] = argv[++i];
            break;
        case 'I':
            if (argv[i][2] != '\0')
                interp = argv[i]+2;
            else if (argv[i+1] != NULL)
                interp = argv[++i];
            break;
        case 'm': {
            char *a;

            if (argv[i][2] != '\0')
                a = argv[i]+2;
            else if (argv[i+1] != NULL)
                a = argv[++i];
            else
                break;
            if (equal(a, "elf_i386"))
                emu_mode = EMU_X86;
            else if (equal(a, "elf_x86_64"))
                emu_mode = EMU_X64;
            else if (equal(a, "elf_mipsel"))
                emu_mode = EMU_MIPS;
            else if (equal(a, "elf_armel"))
                emu_mode = EMU_ARM;
        }
            break;
        case 'r': {
            char *a;

            a = NULL;
            if (argv[i][2] != '\0')
                a = argv[i]+2;
            else if (argv[i+1] != NULL)
                a = argv[++i];
            if (a != NULL)
                runpaths[nrunpath++] = a;
        }
            break;
        case 'h':
            /* we only interpret this option if we haven't read any file yet */
            if (nfbuf == 0) {
                printf("usage: %s [ options ] <objfile> ...\n\n"
                       "  The available options are\n"
                       "    -o<file>    write output to <file>\n"
                       "    -e<sym>     set <sym> as the entry point symbol\n"
                       "    -l<name>    link against object file/library <name>\n"
                       "    -L<dir>     add <dir> to the list of directories searched for the -l options\n"
                       "    -I<interp>  set <interp> as the name of the dynamic linker\n"
                       "    -m<mode>    emulate a linker for <mode>\n"
                       "    -r<path>    add <path> to the DT_RUNPATH dynamic array tag\n"
                       "    -h          print this help\n\n"
                       "  Currently valid arguments for -m:\n"
                       "    elf_i386\n"
                       "    elf_x86_64\n"
                       "    elf_mipsel\n"
                       "    elf_armel\n",
                       prog_name);
                exit(0);
            }
            break;
        case 'v':
            /*verbose = TRUE;*/
            break;
        default:
            err("unknown option `%c'", argv[i][1]);
            break;
        }
    }

    if ((outfp=fopen(out_name, "wb")) == NULL)
        err("cannot write to file `%s'", out_name);

    if (emu_mode == -1)
        emu_mode = DEF_EMU_MOD;
    switch (emu_mode) {
    case EMU_X86:
        if (entry_symbol == NULL)
            entry_symbol = "_start";
        if (interp == NULL)
            interp = "/lib/ld-linux.so.2";
        PLT_ENTRY_NB = X86_PLT_ENTRY_NB;
        nreserved = 3;
        break;
    case EMU_X64:
        if (entry_symbol == NULL)
            entry_symbol = "_start";
        if (interp == NULL)
            interp = "/lib64/ld-linux-x86-64.so.2";
        PLT_ENTRY_NB = X64_PLT_ENTRY_NB;
        nreserved = 3;
        break;
    case EMU_MIPS:
        if (entry_symbol == NULL)
            entry_symbol = "__start";
        if (interp == NULL)
            interp = "/usr/mipsel-linux-gnu/lib/ld.so.1";
        PLT_ENTRY_NB = MIPS_PLT_ENTRY_NB;
        nreserved = 2;
        break;
    case EMU_ARM:
        if (entry_symbol == NULL)
            entry_symbol = "_start";
        if (interp == NULL)
            interp = "/usr/arm-linux-gnueabi/lib/ld-linux.so.3";
        PLT_ENTRY_NB = ARM_PLT_ENTRY_NB;
        nreserved = 3;
        break;
    }
    ngotplt = nreserved;

    if (EMU32()) {
        init_sections_32();
        init_segments_32();
        init_symtab_32();
    } else {
        init_sections_64();
        init_segments_64();
        init_symtab_64();
    }
    switch (emu_mode) {
    case EMU_X86:
        x86_apply_relocs();
        break;
    case EMU_X64:
        x64_apply_relocs();
        break;
    case EMU_MIPS:
        mips_apply_relocs();
        break;
    case EMU_ARM:
        arm_apply_relocs();
        break;
    }
    if (EMU32())
        write_ELF_file_32(outfp);
    else
        write_ELF_file_64(outfp);
    fclose(outfp);
    sprintf(chmod_cmd, "chmod u+x %s", out_name);
    system(chmod_cmd);

    return 0;
}
