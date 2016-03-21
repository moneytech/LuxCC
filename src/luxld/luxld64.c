/*
    LUXLD: minimal ELF64 linker capable of linking against static and shared libraries.

    This completes the x64 toolchain (cc+as+ld).

    Documents:
        - gABI: http://www.sco.com/developers/gabi/latest/contents.html
        - AMD64 ABI: http://www.x86-64.org/documentation/abi.pdf
*/
#include "luxld.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <elf.h>
#include <ar.h>
#include <assert.h>
#include <stdarg.h>
#include "../util.h"
#include "../arena.h"
#include "../ELF/ELF_util.h"

#define MAX_INPUT_FILES     64
#define PLT_ENTRY_NB        16
#define MAX_NEEDED_ELEMS    32
#define MAX_SEC_PER_SEG     32
#define PAGE_SIZE           0x1000
#define PAGE_MASK           (PAGE_SIZE-1)
#define HASH_SIZE           1009
#define HASH(s)             (hash(s)%HASH_SIZE)

typedef unsigned char bool;
typedef struct Symbol Symbol;
typedef struct ObjFile ObjFile;
typedef struct ShrdObjFile ShrdObjFile;
typedef struct SmplSec SmplSec;
typedef struct CmpndSec CmpndSec;
typedef struct Segment Segment;
typedef struct PLTEnt PLTEnt;

struct ObjFile { /* simple relocatable object file */
    char *buf;
    Elf64_Ehdr *ehdr;   /* ELF header */
    Elf64_Shdr *shtab;  /* Section header table */
    Elf64_Sym *symtab;  /* Symbol table */
    int nsym;           /* # of symbol table entries */
    char *shstrtab;     /* Section name string table */
    char *strtab;       /* String table */
    ObjFile *next;
} *object_files;

struct ShrdObjFile { /* shared library */
    char *buf;
    char *name;             /* Library's path passed to the linker in the command line or DT_SONAME */
    Elf64_Ehdr *ehdr;
    Elf64_Shdr *shtab;
    Elf64_Sym *dynsym;      /* Dynamic symbol table */
    int nsym;               /* dynsym's # of entries */
    char *dynstr;           /* Dynamic string table */
    Elf64_Dyn *dynamic;     /* Dynamic section */
    Elf64_Word *hash;       /* Hash table (points to first bucket) */
    Elf64_Word nbucket;     /* Hash table's # of buckets */
    Elf64_Word *chain;      /* Hash chain (length == # of dynsym entries) */
    ShrdObjFile *next;
} *shared_object_files;

struct PLTEnt {
    char *fname;
    Elf64_Addr addr;
    PLTEnt *next;
} *plt_entries;

struct SmplSec { /* simple section with the contribution of a single object file */
    ObjFile *obj;       /* object file that contributed this section */
    Elf64_Shdr *shdr;
    char *data;
    SmplSec *next;
};

struct CmpndSec { /* compound section with the contributions of all object files */
    char *name;
    Elf64_Shdr shdr;
    Elf64_Half shndx;   /* index into output file's section header table */
    SmplSec *sslist;
    CmpndSec *next;
} *sections;

struct Segment { /* read-only & writable loadable segments */
    int nsec;
    CmpndSec *secs[MAX_SEC_PER_SEG];
    Elf64_Phdr phdr;
} ROSeg, WRSeg;

struct Symbol {
    char *name;
    Elf64_Addr value;
    Elf64_Word size;
    unsigned char info;
    bool in_dynsym;
    Elf64_Half shndx;   /* index into input file's section header table */
    char *shname;
    Symbol *next;
};

Symbol *lookup_global_symbol(char *name);
char *entry_symbol = "_start";
char *interp = "/lib64/ld-linux-x86-64.so.2";
char *prog_name;
char *fbuf[MAX_INPUT_FILES];
int nfbuf;
int nundef; /* # of undefined ('extern') symbols currently in the symtab */
int nglobal;
int nreloc;
CmpndSec *plt_sec, *relaplt_sec, *gotplt_sec;
int nplt, nrelaplt, ngotplt = 3;
CmpndSec *dynstr_sec, *dynsym_sec;
StrTab *dynstr;
CmpndSec *hash_sec, *dynamic_sec;
int nshaobj;
CmpndSec *interp_sec;
CmpndSec *reladyn_sec, *bss_sec;
int nreladyn;
Arena *mem_arena;
Elf64_Half shndx = 4; /* [0]=UND, [1]=.shstrtab, [2]=.symtab, [3]=.strtab */

/* Symbols that will go into the output file's symtab. */
Symbol *global_symbols[HASH_SIZE];
Symbol *local_symbols; /* with type other than SECTION or FILE */

const char plt0_asm_template[] =
    "\xff\x35\x00\x00\x00\x00"  /* push qword [REL got+8] */
    "\xff\x25\x00\x00\x00\x00"  /* jmp  qword [REL got+16] */
    "\x90\x90\x90\x90"          /* times 4 nop */
;

const char pltn_asm_template[] =
    "\xff\x25\x00\x00\x00\x00"  /* jmp  qword [REL got+n] */
    "\x68\x00\x00\x00\x00"      /* push dword reloc_index */
    "\xe9\x00\x00\x00\x00"      /* jmp  PLT0 */
;

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

/* lookup symname between the *.so files passed in the command line */
Elf64_Sym *lookup_in_shared_object(char *symname)
{
    ShrdObjFile *so;

    for (so = shared_object_files; so != NULL; so = so->next) {
        char *sn;
        Elf64_Word ci;

        ci = so->hash[elf_hash((unsigned char *)symname)%so->nbucket];
        while (ci != STN_UNDEF) {
            sn = &so->dynstr[so->dynsym[ci].st_name];
            if (equal(symname, sn)) {
                if (so->dynsym[ci].st_shndx != SHN_UNDEF)
                    return &so->dynsym[ci];
                else
                    break;
            } else {
                ci = so->chain[ci];
            }
        }
    }
    return NULL;
}

SmplSec *new_smpl_sec(ObjFile *obj, Elf64_Shdr *hdr, char *data, SmplSec *next)
{
    SmplSec *n;

    n = malloc(sizeof(SmplSec));
    n->obj = obj;
    n->shdr = hdr;
    n->data = data;
    n->next = next;
    return n;
}

void add_section(ObjFile *obj, char *name, Elf64_Shdr *hdr)
{
    CmpndSec *sec;

    for (sec = sections; sec != NULL; sec = sec->next) {
        if (equal(sec->name, name)) {
            assert(sec->shdr.sh_type == hdr->sh_type); /* TBD */
            sec->shdr.sh_flags |= hdr->sh_flags;
            sec->shdr.sh_size += round_up(hdr->sh_size, 4);
            if (hdr->sh_addralign > sec->shdr.sh_addralign)
                sec->shdr.sh_addralign = hdr->sh_addralign;
            sec->sslist = new_smpl_sec(obj, hdr, obj->buf+hdr->sh_offset, sec->sslist);
            if (sec->shdr.sh_type == SHT_RELA)
                nreloc += hdr->sh_size/sizeof(Elf64_Rela);
            break;
        }
    }
    if (sec == NULL) {
        sec = calloc(1, sizeof(CmpndSec));
        sec->name = name;
        sec->shdr = *hdr;
        sec->shdr.sh_size = round_up(hdr->sh_size, 4);
        sec->sslist = new_smpl_sec(obj, hdr, obj->buf+hdr->sh_offset, NULL);
        sec->next = sections;
        sections = sec;
        if (sec->shdr.sh_type == SHT_RELA)
            nreloc += hdr->sh_size/sizeof(Elf64_Rela);
    }
}

/*
 * Add various sections required for dynamic linking.
 * Estimate sizes based on the number of globals symbols,
 * the number of undefined symbols, and the number of
 * relocations. If required, these values will be adjusted
 * before writing the final executable file.
 */
void init_dynlink_sections(void)
{
    char *buf;
    CmpndSec *sec;
    Elf64_Shdr *shdr;
    Elf64_Word size, nbucket;

    if (nreloc > 0) {
        /* .plt */
        plt_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".plt";
        sec->shdr.sh_type = SHT_PROGBITS;
        sec->shdr.sh_flags = SHF_ALLOC|SHF_EXECINSTR;
        size = PLT_ENTRY_NB*(1+nreloc); /* +1 for PLT0 */
        sec->shdr.sh_size = size;
        sec->shdr.sh_addralign = 16;
        // sec->shdr.sh_entsize = 4;
        buf = arena_alloc(mem_arena, size);
        shdr = arena_alloc(mem_arena, sizeof(Elf64_Shdr));
        *shdr = sec->shdr;
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;

        /* .rela.plt */
        relaplt_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".rela.plt";
        sec->shdr.sh_type = SHT_RELA;
        sec->shdr.sh_flags = SHF_ALLOC;
        size = sizeof(Elf64_Rela)*nreloc;
        sec->shdr.sh_size = size;
        sec->shdr.sh_addralign = 8;
        sec->shdr.sh_entsize = sizeof(Elf64_Rela);
        buf = arena_alloc(mem_arena, size);
        shdr = arena_alloc(mem_arena, sizeof(Elf64_Shdr));
        *shdr = sec->shdr;
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;

        /* .got.plt */
        gotplt_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".got.plt";
        sec->shdr.sh_type = SHT_PROGBITS;
        sec->shdr.sh_flags = SHF_ALLOC|SHF_WRITE;
        size = sizeof(Elf64_Xword)*(3+nreloc);
        sec->shdr.sh_size = size;
        sec->shdr.sh_addralign = 8;
        sec->shdr.sh_entsize = 8;
        buf = arena_alloc(mem_arena, size);
        shdr = arena_alloc(mem_arena, sizeof(Elf64_Shdr));
        *shdr = sec->shdr;
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;

        /* .rela.dyn */
        reladyn_sec = sec = calloc(1, sizeof(CmpndSec));
        sec->name = ".rela.dyn";
        sec->shdr.sh_type = SHT_RELA;
        sec->shdr.sh_flags = SHF_ALLOC;
        size = sizeof(Elf64_Rela)*(nreloc);
        sec->shdr.sh_size = size;
        sec->shdr.sh_addralign = 8;
        sec->shdr.sh_entsize = sizeof(Elf64_Rela);
        buf = arena_alloc(mem_arena, size);
        shdr = arena_alloc(mem_arena, sizeof(Elf64_Shdr));
        *shdr = sec->shdr;
        sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
        sec->next = sections;
        sections = sec;
    }

    /* .dynstr */
    dynstr_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".dynstr";
    sec->shdr.sh_type = SHT_STRTAB;
    sec->shdr.sh_flags = SHF_ALLOC;
    size = round_up(strtab_get_size(dynstr), 4);
    sec->shdr.sh_size = size;
    sec->shdr.sh_addralign = 1;
    buf = arena_alloc(mem_arena, size);
    strtab_copy(dynstr, buf);
    shdr = arena_alloc(mem_arena, sizeof(Elf64_Shdr));
    *shdr = sec->shdr;
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .dynsym */
    dynsym_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".dynsym";
    sec->shdr.sh_type = SHT_DYNSYM;
    sec->shdr.sh_flags = SHF_ALLOC;
    size = sizeof(Elf64_Sym)*(nglobal+1); /* +1 for STN_UNDEF */
    sec->shdr.sh_size = size;
    sec->shdr.sh_addralign = 8;
    sec->shdr.sh_entsize = sizeof(Elf64_Sym);
    buf = arena_alloc(mem_arena, size);
    shdr = arena_alloc(mem_arena, sizeof(Elf64_Shdr));
    *shdr = sec->shdr;
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .hash */
    hash_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".hash";
    sec->shdr.sh_type = SHT_HASH;
    sec->shdr.sh_flags = SHF_ALLOC;
    /* size: nbucket + nchain + buckets + chain */
    nbucket = elf_get_nbucket(nglobal+1);
    size = sizeof(Elf64_Word)*(2+nbucket+(nglobal+1));
    sec->shdr.sh_size = size;
    sec->shdr.sh_addralign = 8;
    sec->shdr.sh_entsize = 4;
    buf = arena_alloc(mem_arena, size);
    *(Elf64_Word *)buf = nbucket;
    *((Elf64_Word *)buf+1) = nglobal+1;
    shdr = arena_alloc(mem_arena, sizeof(Elf64_Shdr));
    *shdr = sec->shdr;
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .dynamic */
    dynamic_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".dynamic";
    sec->shdr.sh_type = SHT_DYNAMIC;
    sec->shdr.sh_flags = SHF_ALLOC|SHF_WRITE;
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
    sec->shdr.sh_size = size;
    sec->shdr.sh_addralign = 8;
    sec->shdr.sh_entsize = sizeof(Elf64_Dyn);
    buf = arena_alloc(mem_arena, size);
    shdr = arena_alloc(mem_arena, sizeof(Elf64_Shdr));
    *shdr = sec->shdr;
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;

    /* .interp */
    interp_sec = sec = calloc(1, sizeof(CmpndSec));
    sec->name = ".interp";
    sec->shdr.sh_type = SHT_PROGBITS;
    sec->shdr.sh_flags = SHF_ALLOC; /* XXX: "If the file has a loadable segment
                                             that includes relocation..." */
    size = round_up(strlen(interp)+1, 4);
    sec->shdr.sh_size = size;
    sec->shdr.sh_addralign = 1;
    buf = arena_alloc(mem_arena, size);
    strcpy(buf, interp);
    shdr = arena_alloc(mem_arena, sizeof(Elf64_Shdr));
    *shdr = sec->shdr;
    sec->sslist = new_smpl_sec(NULL, shdr, buf, NULL);
    sec->next = sections;
    sections = sec;
}

void init_sections(void)
{
    ObjFile *obj;

    for (obj = object_files; obj != NULL; obj = obj->next) {
        int i;
        Elf64_Shdr *shdr;

        shdr = obj->shtab+1; /* skip SHN_UNDEF */
        for (i = 1; i < obj->ehdr->e_shnum; i++, shdr++)
            add_section(obj, obj->shstrtab+shdr->sh_name, shdr);
    }
    if (shared_object_files != NULL)
        init_dynlink_sections();
}

Elf64_Half get_dynsym_ndx(char *sym)
{
    Elf64_Sym *sp;
    Elf64_Half st_ndx, st_siz;

    st_ndx = 1;
    sp = (Elf64_Sym *)dynsym_sec->sslist->data+1;
    st_siz = dynsym_sec->shdr.sh_size/sizeof(Elf64_Sym);
    while (st_ndx < st_siz) {
        if (equal(strtab_get_string(dynstr, sp->st_name), sym))
            return st_ndx;
        ++st_ndx, ++sp;
    }
    assert(0);
}

/*
 * Install a new entry into .got.plt and a new reloc into .rela.plt.
 * Parameters:
 *  - pa is the address of a push instruction (see pltn_asm_template).
 *  - ri points to the push instruction's argument.
 *  - fname is the name of the function this entry is helping to link.
 * Return the address of the new .got.plt entry.
 */
Elf64_Addr new_gotplt_entry(Elf64_Addr pa, Elf64_Word *ri, char *fname)
{
    Elf64_Addr ea;
    Elf64_Rela *rp;

    ((Elf64_Addr *)(gotplt_sec->sslist->data))[ngotplt] = pa;
    ea = gotplt_sec->shdr.sh_addr+sizeof(Elf64_Addr)*ngotplt;
    ++ngotplt;

    rp = (Elf64_Rela *)relaplt_sec->sslist->data;
    rp[nrelaplt].r_offset = ea;
    rp[nrelaplt].r_info = ELF64_R_INFO(get_dynsym_ndx(fname), R_X86_64_JUMP_SLOT);
    rp[nrelaplt].r_addend = 0;
    *ri = nrelaplt;
    ++nrelaplt;

    return ea;
}

/*
 * Get the PLT entry corresponding to the function fname (defined in a shared object).
 * Create a new entry if fname was not seen before.
 * Return a PLT entry address.
 */
Elf64_Addr get_plt_entry(char *fname)
{
    PLTEnt *np;

    for (np = plt_entries; np != NULL; np = np->next)
        if (equal(np->fname, fname))
            break;

    if (np == NULL) {
        char *p;
        Elf64_Word ri;
        Elf64_Addr addr;

        p = plt_sec->sslist->data+nplt*PLT_ENTRY_NB;
        if (plt_entries == NULL) {
            /* set PLT0 */
            memcpy(p, plt0_asm_template, PLT_ENTRY_NB);
            *(Elf64_Word *)(p+2) = gotplt_sec->shdr.sh_addr-(plt_sec->shdr.sh_addr+6)+8;
            *(Elf64_Word *)(p+8) = gotplt_sec->shdr.sh_addr-(plt_sec->shdr.sh_addr+12)+16;
            p += PLT_ENTRY_NB;
            ++nplt;
        }
        memcpy(p, pltn_asm_template, PLT_ENTRY_NB);
        addr = plt_sec->shdr.sh_addr+nplt*PLT_ENTRY_NB;
        *(Elf64_Word *)(p+2) = new_gotplt_entry(addr+6, &ri, fname)-(addr+6);
        *(Elf64_Word *)(p+7) = ri;
        *(Elf64_Word *)(p+12) = plt_sec->shdr.sh_addr-(addr+PLT_ENTRY_NB);
        ++nplt;

        np = malloc(sizeof(PLTEnt));
        np->fname = fname;
        np->addr = addr;
        np->next = plt_entries;
        plt_entries = np;
        return addr;
    } else {
        return np->addr;
    }
}

/*
 * Add a new reloc into .rela.dyn.
 * Return a pointer to space allocated in .bss.
 */
Elf64_Addr new_copy_reloc(char *symname, Elf64_Sym *syment)
{
    unsigned n;
    Symbol *sym;
    Elf64_Sym *sp;
    Elf64_Rela *rp;
    Elf64_Addr addr;
    Elf64_Half symndx;

    if (bss_sec == NULL) { /* need to create a .bss section */
        bss_sec = calloc(1, sizeof(CmpndSec));
        bss_sec->name = ".bss";
        bss_sec->shndx = shndx++;
        bss_sec->shdr.sh_type = SHT_NOBITS;
        bss_sec->shdr.sh_flags = SHF_ALLOC|SHF_WRITE;
        bss_sec->shdr.sh_addralign = 4;
        if (WRSeg.nsec == 0) /* need to create a writable segment */
            assert(0); /* we should have at least .dynamic by now */
        bss_sec->shdr.sh_offset = WRSeg.phdr.p_offset+WRSeg.phdr.p_filesz;
        bss_sec->shdr.sh_addr = WRSeg.phdr.p_vaddr+WRSeg.phdr.p_memsz;
        bss_sec->next = sections;
        sections = bss_sec;
        WRSeg.secs[WRSeg.nsec++] = bss_sec;
    }

    addr = bss_sec->shdr.sh_addr+bss_sec->shdr.sh_size;
    symndx = get_dynsym_ndx(symname);
    sp = &((Elf64_Sym *)dynsym_sec->sslist->data)[symndx];

    sym = lookup_global_symbol(symname);
    assert(sym != NULL);
    sym->size = sp->st_size = syment->st_size;
    sym->value = sp->st_value = addr;
    sym->shndx = sp->st_shndx = bss_sec->shndx;
    sym->info = sp->st_info = syment->st_info;
    sym->shname = bss_sec->name;

    rp = (Elf64_Rela *)reladyn_sec->sslist->data;
    rp[nreladyn].r_offset = addr;
    rp[nreladyn].r_info = ELF64_R_INFO(symndx, R_X86_64_COPY);
    rp[nreladyn].r_addend = 0;
    ++nreladyn;

    n = round_up(syment->st_size, 4);
    bss_sec->shdr.sh_size += n;
    WRSeg.phdr.p_memsz += n;

    return addr;
}

void init_segments(void)
{
    int i;
    SmplSec *ssec;
    CmpndSec *csec;
    Elf64_Word size;
    unsigned long vaddr = 0x400000;
    unsigned long offset = 0;

    for (csec = sections; csec != NULL; csec = csec->next) {
        if (!(csec->shdr.sh_flags&SHF_ALLOC))
            continue;
        if (csec->shdr.sh_flags & SHF_WRITE)
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
        ROSeg.phdr.p_type = PT_LOAD;
        ROSeg.phdr.p_flags = PF_R|PF_X;
        ROSeg.phdr.p_align = 0x1000; /* TBD: why GNU ld sets this to 2MB? */

        /*
         * Due to disk-space-saving reasons, the ELF header
         * and program header table are mapped into the
         * read-only segment (this is not something the
         * link-editor decides).
         */
        ROSeg.phdr.p_offset = 0;
        ROSeg.phdr.p_vaddr = ROSeg.phdr.p_paddr = vaddr;
        vaddr += offset;
        for (i = 0; i < ROSeg.nsec; i++) {
            ROSeg.secs[i]->shdr.sh_addr = vaddr;
            ROSeg.secs[i]->shdr.sh_offset = offset;
            ROSeg.secs[i]->shndx = shndx++;
            for (ssec=ROSeg.secs[i]->sslist, size=0; ssec != NULL; ssec = ssec->next) {
                ssec->shdr->sh_addr = vaddr+size;
                size += round_up(ssec->shdr->sh_size, 4);
            }
            offset += size;
            vaddr += size;
        }
        ROSeg.phdr.p_filesz = offset;
        ROSeg.phdr.p_memsz = ROSeg.phdr.p_filesz;
    }

    if (WRSeg.nsec > 0) {
        CmpndSec *bss = NULL;

        WRSeg.phdr.p_type = PT_LOAD;
        WRSeg.phdr.p_flags = PF_R|PF_W;
        WRSeg.phdr.p_align = 0x1000;

        WRSeg.phdr.p_offset = offset;
        vaddr = round_up(vaddr, PAGE_SIZE) + (PAGE_MASK & offset);
        WRSeg.phdr.p_vaddr = WRSeg.phdr.p_paddr = vaddr;
        for (i = 0; i < WRSeg.nsec; i++) {
            if (WRSeg.secs[i]->shdr.sh_type == SHT_NOBITS) {
                bss = WRSeg.secs[i];
                continue;
            }
            WRSeg.secs[i]->shdr.sh_addr = vaddr;
            WRSeg.secs[i]->shdr.sh_offset = offset;
            WRSeg.secs[i]->shndx = shndx++;
            for (ssec=WRSeg.secs[i]->sslist, size=0; ssec != NULL; ssec = ssec->next) {
                ssec->shdr->sh_addr = vaddr+size;
                size += round_up(ssec->shdr->sh_size, 4);
            }
            offset += size;
            vaddr += size;
        }
        WRSeg.phdr.p_filesz = offset-ROSeg.phdr.p_filesz;
        WRSeg.phdr.p_memsz = WRSeg.phdr.p_filesz;
        if (bss != NULL) {
            bss_sec = bss;
            bss->shdr.sh_addr = vaddr;
            bss->shdr.sh_offset = offset;
            bss->shndx = shndx++;
            for (ssec=bss->sslist, size=0; ssec != NULL; ssec = ssec->next) {
                ssec->shdr->sh_addr = vaddr+size;
                size += round_up(ssec->shdr->sh_size, 4);
            }
            WRSeg.phdr.p_memsz += bss->shdr.sh_size;
        }
    }
}

void define_global_symbol(char *name, Elf64_Addr value, unsigned char info, Elf64_Half shndx, char *shname)
{
    unsigned h;
    Symbol *np;

    h = HASH(name);
    for (np = global_symbols[h]; np != NULL; np = np->next)
        if (equal(np->name, name))
            break;
    if (np == NULL) {
        np = arena_alloc(mem_arena, sizeof(Symbol));
        np->name = name;
        strtab_append(dynstr, name);
        np->value = value;
        np->size = 0;
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

void define_local_symbol(char *name, Elf64_Addr value, unsigned char info, Elf64_Half shndx, char *shname)
{
    Symbol *np;

    np = arena_alloc(mem_arena, sizeof(Symbol));
    np->name = name;
    np->value = value;
    np->size = 0;
    np->info = info;
    np->shndx = shndx;
    np->shname = shname;
    np->next = local_symbols;
    local_symbols = np;
}

/* return index into output file's section header table */
Elf64_Half get_shndx(Symbol *sym)
{
    CmpndSec *csec;

    if (sym->shndx==SHN_UNDEF || sym->shndx>=SHN_LORESERVE)
        return sym->shndx;
    for (csec = sections; csec != NULL; csec = csec->next)
        if (equal(csec->name, sym->shname))
            return csec->shndx;
    assert(0);
}

/*
 * Install local symbols and assign final run-time addresses
 * to global symbols (build .dynsym and .hash in the process).
 */
void init_symtab(void)
{
    SmplSec *ssec;
    CmpndSec *csec;
    Elf64_Sym *sp;
    Elf64_Sword n;
    Elf64_Half symndx;
    Elf64_Word nbucket, h;
    Elf64_Word *buckets, *chain;

    for (csec = sections; csec != NULL; csec = csec->next)
        if (csec->shdr.sh_type == SHT_SYMTAB)
            break;
    if (csec == NULL)
        return;

    if (shared_object_files != NULL) {
        symndx = 1;
        sp = (Elf64_Sym *)dynsym_sec->sslist->data+1;
        nbucket = *(Elf64_Word *)hash_sec->sslist->data;
        buckets = (Elf64_Word *)(hash_sec->sslist->data+sizeof(Elf64_Word)*2);
        chain = buckets+nbucket;
    }
    for (ssec = csec->sslist; ssec != NULL; ssec = ssec->next) {
        int i, nsym;
        Elf64_Sym *symtab;
        Elf64_Shdr *shtab;
        char *strtab, *shstrtab;

        nsym = ssec->shdr->sh_size/sizeof(Elf64_Sym);
        symtab = ssec->obj->symtab;
        shtab = ssec->obj->shtab;
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
                                            symtab[i].st_info, symtab[i].st_shndx,
                                            NULL);
                    } else {
                        symtab[i].st_value += shtab[symtab[i].st_shndx].sh_addr;
                        define_local_symbol(&strtab[symtab[i].st_name], symtab[i].st_value,
                                            symtab[i].st_info, symtab[i].st_shndx,
                                            &shstrtab[shtab[symtab[i].st_shndx].sh_name]);
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

Elf64_Addr get_symval(char *name, Elf64_Sym *st_ent, bool *found)
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

void apply_relocs(void)
{
    SmplSec *ssec;
    CmpndSec *csec;

    for (csec = sections; csec != NULL; csec = csec->next) {
        if (csec->shdr.sh_type!=SHT_RELA || equal(csec->name, ".rela.plt") || equal(csec->name, ".rela.dyn"))
            continue;
        for (ssec = csec->sslist; ssec != NULL; ssec = ssec->next) {
            int i, nrel;
            Elf64_Sym *symtab;
            Elf64_Shdr *shtab;
            Elf64_Rela *rel;
            char *strtab;
            char *buf;

            rel = (Elf64_Rela *)ssec->data;
            nrel = ssec->shdr->sh_size/sizeof(Elf64_Rela);
            symtab = ssec->obj->symtab;
            shtab = ssec->obj->shtab;
            strtab = ssec->obj->strtab;
            buf = ssec->obj->buf;

            for (i = 0; i < nrel; i++, rel++) {
                bool found;
                void *dest;
                char *symname;
                Elf64_Sym *syment;
                Elf64_Xword A, S, P;

                dest = &buf[shtab[ssec->shdr->sh_info].sh_offset+rel->r_offset];
                symname = &strtab[symtab[ELF64_R_SYM(rel->r_info)].st_name];
                S = get_symval(symname, &symtab[ELF64_R_SYM(rel->r_info)], &found);
                A = rel->r_addend;

                switch (ELF64_R_TYPE(rel->r_info)) {
                /* X86_64_X */
                case R_X86_64_8:
                case R_X86_64_16:
                case R_X86_64_32:
                case R_X86_64_32S:
                case R_X86_64_64:
                    if (found) {
                        ;
                    } else if ((syment=lookup_in_shared_object(symname)) != NULL) {
                        if (ELF64_ST_TYPE(syment->st_info) == STT_FUNC) {
                            Symbol *sym;

                            /* See 'Function Addresses' in the AMD64 ABI. */
                            sym = lookup_global_symbol(symname);
                            assert(sym != NULL);
                            if (sym->value == 0) {
                                syment = &((Elf64_Sym *)dynsym_sec->sslist->data)[get_dynsym_ndx(symname)];
                                syment->st_value = sym->value = get_plt_entry(symname);
                                syment->st_info = sym->info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
                                syment->st_shndx = sym->shndx = SHN_UNDEF;
                            }
                            S = sym->value;
                        } else {
                            S = new_copy_reloc(symname, syment);
                        }
                    } else {
                        err_undef(symname);
                    }
                    switch (ELF64_R_TYPE(rel->r_info)) {
                    case R_X86_64_8:
                        *(char *)dest = S+A;
                        break;
                    case R_X86_64_16:
                        *(Elf64_Half *)dest = S+A;
                        break;
                    /*
                     * XXX: the ABI requires one to check that the
                     * value is the same before/after truncation.
                     */
                    case R_X86_64_32:
                    case R_X86_64_32S:
                        *(Elf64_Word *)dest = S+A;
                        break;
                    case R_X86_64_64:
                        *(Elf64_Xword *)dest = S+A;
                        break;
                    }
                    break;

                /* X86_64_PCX */
                case R_X86_64_PC8:
                case R_X86_64_PC16:
                case R_X86_64_PC32:
                case R_X86_64_PC64:
                    P = shtab[ssec->shdr->sh_info].sh_addr+rel->r_offset;
                    if (found) {
                        ;
                    } else if ((syment=lookup_in_shared_object(symname)) != NULL) {
                        if (ELF64_ST_TYPE(syment->st_info) == STT_FUNC)
                            S = get_plt_entry(symname);
                        else
                            S = new_copy_reloc(symname, syment);
                    } else {
                        err_undef(symname);
                    }
                    switch (ELF64_R_TYPE(rel->r_info)) {
                    case R_X86_64_PC8:
                        *(char *)dest = S+A-P;
                        break;
                    case R_X86_64_PC16:
                        *(Elf64_Half *)dest = S+A-P;
                        break;
                    case R_X86_64_PC32:
                        *(Elf64_Word *)dest = S+A-P;
                        break;
                    case R_X86_64_PC64:
                        *(Elf64_Xword *)dest = S+A-P;
                        break;
                    }
                    break;

                /* other */
                default:
                    err("relocation type `0x%02x' not supported", ELF64_R_TYPE(rel->r_info));
                    break;
                }
            }
        }
    }
}

void write_ELF_file(FILE *outf)
{
    int i;
    Symbol *sym;
    StrTab *strtab = strtab_new(), *shstrtab = strtab_new();
    ObjFile *obj;
    ShrdObjFile *so;
    Elf64_Sym esym;
    Elf64_Off curr = 0;
    Elf64_Ehdr ehdr;
    Elf64_Phdr phdr;
    Elf64_Shdr symtab_header, shstrtab_header, strtab_header, undef_header;
    Elf64_Dyn *dp;

    /*
     * Sections that turned out to be not necessary are written in the file
     * so that previously computed offsets remain correct, but those sections
     * will have size 0 and will not have entries related to them in the dynamic
     * array.
     */

    /* Before start, set values that were left undefined (or only were estimated) */
    if (plt_sec != NULL) {
        plt_sec->shdr.sh_size = nplt*PLT_ENTRY_NB;
        relaplt_sec->shdr.sh_link = dynsym_sec->shndx;
        relaplt_sec->shdr.sh_info = plt_sec->shndx;
        relaplt_sec->shdr.sh_size = nrelaplt*sizeof(Elf64_Rela);
        gotplt_sec->shdr.sh_size = ngotplt*sizeof(Elf64_Xword);
        *(Elf64_Addr *)gotplt_sec->sslist->data = dynamic_sec->shdr.sh_addr;
        reladyn_sec->shdr.sh_link = dynsym_sec->shndx;
        reladyn_sec->shdr.sh_size = nreladyn*sizeof(Elf64_Rela);
    }
    if (shared_object_files != NULL) {
        dynsym_sec->shdr.sh_link = dynstr_sec->shndx;
        dynsym_sec->shdr.sh_info = 1;
        hash_sec->shdr.sh_link = dynsym_sec->shndx;
        dynamic_sec->shdr.sh_link = dynstr_sec->shndx;
        define_local_symbol("_DYNAMIC", dynamic_sec->shdr.sh_addr, ELF64_ST_INFO(STB_LOCAL, STT_OBJECT),
        dynamic_sec->shndx, ".dynamic");
    }

#define ALIGN(n)\
    do {\
        int nb = round_up(curr, n)-curr, i;\
        for (i = 0; i < nb; i++)\
            fputc(0, outf), ++curr;\
    } while (0)

    memset(&symtab_header, 0, sizeof(Elf64_Shdr));
    memset(&shstrtab_header, 0, sizeof(Elf64_Shdr));
    memset(&strtab_header, 0, sizeof(Elf64_Shdr));

    /*
     * ================
     * Dummy ELF header
     * ================
     */
    memset(&ehdr, 0, sizeof(Elf64_Ehdr));
    fwrite(&ehdr, sizeof(Elf64_Ehdr), 1, outf);
    curr += sizeof(Elf64_Ehdr);

    /*
     * ====================
     * Program header table
     * ====================
     */
    ALIGN(16);
    ehdr.e_phoff = curr;
    if (shared_object_files != NULL) {
        phdr.p_type = PT_INTERP;
        phdr.p_offset = interp_sec->shdr.sh_offset;
        phdr.p_vaddr = phdr.p_paddr = interp_sec->shdr.sh_addr;
        phdr.p_filesz = phdr.p_memsz = interp_sec->shdr.sh_size;
        phdr.p_flags = PF_R;
        phdr.p_align = 1;
        fwrite(&phdr, sizeof(Elf64_Phdr), 1, outf);
        curr += sizeof(Elf64_Phdr);
        ++ehdr.e_phnum;
    }
    if (ROSeg.nsec) {
        fwrite(&ROSeg.phdr, sizeof(Elf64_Phdr), 1, outf);
        curr += sizeof(Elf64_Phdr);
        ++ehdr.e_phnum;
    }
    if (WRSeg.nsec) {
        fwrite(&WRSeg.phdr, sizeof(Elf64_Phdr), 1, outf);
        curr += sizeof(Elf64_Phdr);
        ++ehdr.e_phnum;
    }
    if (shared_object_files != NULL) {
        /* Set .dynamic's elements */
        dp = (Elf64_Dyn *)dynamic_sec->sslist->data;
        for (so = shared_object_files; so != NULL; so = so->next, dp++) {
            dp->d_tag = DT_NEEDED;
            dp->d_un.d_val = strtab_get_offset(dynstr, so->name);
        }
        dp->d_tag = DT_HASH;
        dp->d_un.d_ptr = hash_sec->shdr.sh_addr;
        ++dp;
        dp->d_tag = DT_STRTAB;
        dp->d_un.d_ptr = dynstr_sec->shdr.sh_addr;
        ++dp;
        dp->d_tag = DT_SYMTAB;
        dp->d_un.d_ptr = dynsym_sec->shdr.sh_addr;
        ++dp;
        dp->d_tag = DT_STRSZ;
        dp->d_un.d_val = dynstr_sec->shdr.sh_size;
        ++dp;
        dp->d_tag = DT_SYMENT;
        dp->d_un.d_val = sizeof(Elf64_Sym);
        ++dp;
        if (plt_sec != NULL) {
            if (nplt > 0) {
                dp->d_tag = DT_PLTGOT;
                dp->d_un.d_ptr = gotplt_sec->shdr.sh_addr;
                ++dp;
                dp->d_tag = DT_PLTRELSZ;
                dp->d_un.d_val = relaplt_sec->shdr.sh_size;
                ++dp;
                dp->d_tag = DT_PLTREL;
                dp->d_un.d_val = DT_RELA;
                ++dp;
                dp->d_tag = DT_JMPREL;
                dp->d_un.d_ptr = relaplt_sec->shdr.sh_addr;
                ++dp;
            } else {
                dynamic_sec->shdr.sh_size -= sizeof(Elf64_Dyn)*4;
            }
            if (nreladyn > 0) {
                dp->d_tag = DT_RELA;
                dp->d_un.d_ptr = reladyn_sec->shdr.sh_addr;
                ++dp;
                dp->d_tag = DT_RELASZ;
                dp->d_un.d_val = reladyn_sec->shdr.sh_size;
                ++dp;
                dp->d_tag = DT_RELAENT;
                dp->d_un.d_val = sizeof(Elf64_Rela);
                ++dp;
            } else {
                dynamic_sec->shdr.sh_size -= sizeof(Elf64_Dyn)*3;
            }
        }
        dp->d_tag = DT_NULL;
        phdr.p_type = PT_DYNAMIC;
        phdr.p_offset = dynamic_sec->shdr.sh_offset;
        phdr.p_vaddr = phdr.p_paddr = dynamic_sec->shdr.sh_addr;
        phdr.p_filesz = phdr.p_memsz = dynamic_sec->shdr.sh_size;
        phdr.p_flags = PF_R|PF_W;
        phdr.p_align = 8;
        fwrite(&phdr, sizeof(Elf64_Phdr), 1, outf);
        curr += sizeof(Elf64_Phdr);
        ++ehdr.e_phnum;
    }

    /*
     * =================
     * Loadable sections
     * =================
     */
    ehdr.e_shnum = 1; /* SHN_UNDEF */
    if (ROSeg.nsec > 0)
        assert(curr == ROSeg.secs[0]->shdr.sh_offset);
    for (i = 0; i < ROSeg.nsec; i++) {
        SmplSec *ssec;

        for (ssec = ROSeg.secs[i]->sslist; ssec != NULL; ssec = ssec->next) {
            fwrite(ssec->data, ssec->shdr->sh_size, 1, outf);
            curr += ssec->shdr->sh_size;
            ALIGN(4);
        }
        ROSeg.secs[i]->shdr.sh_name = strtab_append(shstrtab, ROSeg.secs[i]->name);
        ++ehdr.e_shnum;
    }
    for (i = 0; i < WRSeg.nsec; i++) {
        SmplSec *ssec;

        if (WRSeg.secs[i]->shdr.sh_type != SHT_NOBITS) {
            for (ssec = WRSeg.secs[i]->sslist; ssec != NULL; ssec = ssec->next) {
                fwrite(ssec->data, ssec->shdr->sh_size, 1, outf);
                curr += ssec->shdr->sh_size;
                ALIGN(4);
            }
        }
        WRSeg.secs[i]->shdr.sh_name = strtab_append(shstrtab, WRSeg.secs[i]->name);
        ++ehdr.e_shnum;
    }

    /*
     * ===============================
     * .shstrtab, .symtab, and .strtab
     * ===============================
     */

    /*
     * .shstrtab
     */
    shstrtab_header.sh_name = strtab_append(shstrtab, ".shstrtab");
    symtab_header.sh_name = strtab_append(shstrtab, ".symtab");
    strtab_header.sh_name = strtab_append(shstrtab, ".strtab");
    shstrtab_header.sh_offset = curr;
    shstrtab_header.sh_size = strtab_write(shstrtab, outf);
    curr += shstrtab_header.sh_size;
    ++ehdr.e_shnum;

    /*
     * .symtab
     */
    ALIGN(4);
    symtab_header.sh_offset = curr;

#define WRITE_ST_ENT()\
    do {\
        fwrite(&esym, sizeof(Elf64_Sym), 1, outf);\
        curr += sizeof(Elf64_Sym);\
        symtab_header.sh_size += sizeof(Elf64_Sym);\
        ++symtab_header.sh_info;\
    } while (0)

    /* first entry (STN_UNDEF) */
    memset(&esym, 0, sizeof(Elf64_Sym));
    WRITE_ST_ENT();

    /* FILE symbol table entry/ies */
    memset(&esym, 0, sizeof(Elf64_Sym));
    esym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_FILE);
    esym.st_shndx = SHN_ABS;
    for (obj = object_files; obj != NULL; obj = obj->next) {
        if (obj->symtab == NULL) /* [!] crtn.o doesn't have .symtab */
            continue;
        if (ELF64_ST_TYPE(obj->symtab[1].st_info) == STT_FILE)
            esym.st_name = strtab_append(strtab, &obj->strtab[obj->symtab[1].st_name]);
        else
            continue;
        WRITE_ST_ENT();
    }

    /* loadable sections */
    for (i = 0; i < ROSeg.nsec; i++) {
        memset(&esym, 0, sizeof(Elf64_Sym));
        esym.st_value = ROSeg.secs[i]->shdr.sh_addr;
        esym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
        esym.st_shndx = ROSeg.secs[i]->shndx;
        WRITE_ST_ENT();
    }
    for (i = 0; i < WRSeg.nsec; i++) {
        memset(&esym, 0, sizeof(Elf64_Sym));
        esym.st_value = WRSeg.secs[i]->shdr.sh_addr;
        esym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
        esym.st_shndx = WRSeg.secs[i]->shndx;
        WRITE_ST_ENT();
    }

    /* local symbols (with type other than FILE or SECTION) */
    for (sym = local_symbols; sym != NULL; sym = sym->next) {
        memset(&esym, 0, sizeof(Elf64_Sym));
        esym.st_name = strtab_append(strtab, sym->name);
        esym.st_value = sym->value;
        esym.st_size = sym->size;
        esym.st_info = sym->info;
        esym.st_shndx = get_shndx(sym);
        WRITE_ST_ENT();
    }
#undef WRITE_ST_ENT

    /* global symbols */
    for (i = 0; i < HASH_SIZE; i++) {
        Symbol *np;

        if (global_symbols[i] == NULL)
            continue;
        for (np = global_symbols[i]; np != NULL; np = np->next) {
            memset(&esym, 0, sizeof(Elf64_Sym));
            esym.st_name = strtab_append(strtab, np->name);
            esym.st_value = np->value;
            esym.st_size = np->size;
            esym.st_info = np->info;
            esym.st_shndx = get_shndx(np);
            fwrite(&esym, sizeof(Elf64_Sym), 1, outf);
            curr += sizeof(Elf64_Sym);
            symtab_header.sh_size += sizeof(Elf64_Sym);
        }
    }
    ++ehdr.e_shnum;

    /*
     * .strtab
     */
    strtab_header.sh_offset = curr;
    strtab_header.sh_size = strtab_write(strtab, outf);
    curr += strtab_header.sh_size;
    ++ehdr.e_shnum;;

    /*
     * ====================
     * Section header table
     * ====================
     */
    ALIGN(4);
    ehdr.e_shoff = curr;

    /* first entry (SHN_UNDEF) */
    memset(&undef_header, 0, sizeof(Elf64_Shdr));
    fwrite(&undef_header, sizeof(Elf64_Shdr), 1, outf);

    /* .shstrtab section header */
    shstrtab_header.sh_type = SHT_STRTAB;
    shstrtab_header.sh_addralign = 1;
    fwrite(&shstrtab_header, sizeof(Elf64_Shdr), 1, outf);

    /* .symtab section header */
    symtab_header.sh_type = SHT_SYMTAB;
    symtab_header.sh_link = 3; /* .strtab */
    symtab_header.sh_addralign = 4;
    symtab_header.sh_entsize = sizeof(Elf64_Sym);
    fwrite(&symtab_header, sizeof(Elf64_Shdr), 1, outf);

    /* .strtab section header */
    strtab_header.sh_type = SHT_STRTAB;
    strtab_header.sh_addralign = 1;
    fwrite(&strtab_header, sizeof(Elf64_Shdr), 1, outf);

    /* remaining section headers */
    for (i = 0; i < ROSeg.nsec; i++)
        fwrite(&ROSeg.secs[i]->shdr, sizeof(Elf64_Shdr), 1, outf);
    for (i = 0; i < WRSeg.nsec; i++)
        fwrite(&WRSeg.secs[i]->shdr, sizeof(Elf64_Shdr), 1, outf);

    /*
     * Correct dummy ELF header
     */
    rewind(outf);
    ehdr.e_ident[EI_MAG0] = ELFMAG0;
    ehdr.e_ident[EI_MAG1] = ELFMAG1;
    ehdr.e_ident[EI_MAG2] = ELFMAG2;
    ehdr.e_ident[EI_MAG3] = ELFMAG3;
    ehdr.e_ident[EI_CLASS] = ELFCLASS64;
    ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
    ehdr.e_ident[EI_VERSION] = EV_CURRENT;
    ehdr.e_type = ET_EXEC;
    ehdr.e_machine = EM_X86_64;
    ehdr.e_version = EV_CURRENT;
    if ((sym=lookup_global_symbol(entry_symbol))==NULL || sym->shndx==SHN_UNDEF)
        /* TBD: lookup entry symbol between the *.so files too? */
        err("cannot find entry symbol `%s'", entry_symbol);
    ehdr.e_entry = sym->value;
    ehdr.e_ehsize = sizeof(Elf64_Ehdr);
    ehdr.e_phentsize = sizeof(Elf64_Phdr);
    ehdr.e_shentsize = sizeof(Elf64_Shdr);
    ehdr.e_shstrndx = 1;
    fwrite(&ehdr, sizeof(Elf64_Ehdr), 1, outf);
    fseek(outf, 0, SEEK_END);

    fclose(outf);
    strtab_destroy(strtab), strtab_destroy(shstrtab);

#undef ALIGN
}

void process_object_file(char *buf)
{
    int i;
    ObjFile *obj;
    int first_gsym;

    obj = calloc(1, sizeof(ObjFile));
    obj->buf = buf;
    obj->ehdr = (Elf64_Ehdr *)buf;
    obj->shtab = (Elf64_Shdr *)(buf+obj->ehdr->e_shoff);
    obj->shstrtab = buf + obj->shtab[obj->ehdr->e_shstrndx].sh_offset;
    for (i = 1; i < obj->ehdr->e_shnum; i++) {
        if (obj->shtab[i].sh_type == SHT_SYMTAB) {
            obj->symtab = (Elf64_Sym *)(buf+obj->shtab[i].sh_offset);
            obj->nsym = obj->shtab[i].sh_size/sizeof(Elf64_Sym);
            obj->strtab = buf+obj->shtab[obj->shtab[i].sh_link].sh_offset;
            first_gsym = obj->shtab[i].sh_info;
            break;
        }
    }
    obj->next = object_files;
    object_files = obj;

    /* install any global symbol */
    if (first_gsym < obj->nsym) {
        Elf64_Sym *symtab = obj->symtab;
        Elf64_Shdr *shtab = obj->shtab;
        char *strtab = obj->strtab;
        char *shstrtab = obj->shstrtab;

        for (i = first_gsym; i < obj->nsym; i++) {
            char *shname = (symtab[i].st_shndx==SHN_UNDEF || symtab[i].st_shndx>=SHN_LORESERVE)
                         ? NULL : &shstrtab[shtab[symtab[i].st_shndx].sh_name];
            define_global_symbol(&strtab[symtab[i].st_name], 0, symtab[i].st_info, symtab[i].st_shndx, shname);
        }
    }
}

void process_shared_object_file(char *buf, char *path)
{
    int i;
    Elf64_Dyn *dp;
    ShrdObjFile *so;
    unsigned missing;

    so = calloc(1, sizeof(ShrdObjFile));
    so->buf = buf;
    so->ehdr = (Elf64_Ehdr *)buf;
    so->shtab = (Elf64_Shdr *)(buf+so->ehdr->e_shoff);
    for (i=1, missing=7; i<so->ehdr->e_shnum && missing; i++) {
        if (so->shtab[i].sh_type == SHT_DYNSYM) {
            so->dynsym = (Elf64_Sym *)(buf+so->shtab[i].sh_offset);
            so->nsym = so->shtab[i].sh_size/sizeof(Elf64_Sym);
            so->dynstr = buf+so->shtab[so->shtab[i].sh_link].sh_offset;
            missing &= ~1;
        } else if (so->shtab[i].sh_type == SHT_DYNAMIC) {
            so->dynamic = (Elf64_Dyn *)(buf+so->shtab[i].sh_offset);
            missing &= ~2;
        } else if (so->shtab[i].sh_type == SHT_HASH) {
            so->nbucket = *((Elf64_Word *)(buf+so->shtab[i].sh_offset));
            so->hash = (Elf64_Word *)(buf+so->shtab[i].sh_offset)+2;
            so->chain = so->hash+so->nbucket;
            missing &= ~4;
        }
    }
    assert(missing == 0); /* TBD (probably .hash is missing because there is .gnu.hash instead) */
    for (dp = so->dynamic; dp->d_tag != DT_NULL; dp++) {
        if (dp->d_tag == DT_SONAME) {
            so->name = &so->dynstr[dp->d_un.d_val];
            break;
        }
    }
    if (dp->d_tag == DT_NULL) {
        so->name = arena_alloc(mem_arena, strlen(path)+1);
        strcpy(so->name, path);
    }
    strtab_append(dynstr, so->name);
    so->next = shared_object_files;
    shared_object_files = so;
    ++nshaobj;
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
void process_file(char *path, char *needed_path)
{
    if ((fbuf[nfbuf]=read_file(path)) == NULL)
        err("cannot read file `%s'", path);
    if (strncmp(fbuf[nfbuf], ARMAG, SARMAG) == 0) {
        process_archive(fbuf[nfbuf]);
    } else if (strncmp(fbuf[nfbuf], "\x7f""ELF", 3) == 0) {
        Elf64_Ehdr *eh;

        eh = (Elf64_Ehdr *)fbuf[nfbuf];
        if (eh->e_ident[EI_CLASS]!=ELFCLASS64 || eh->e_machine!=EM_X86_64)
            err("file `%s': relocatable/shared object for unknown architecture", path);
        if (eh->e_type == ET_REL)
            process_object_file(fbuf[nfbuf]);
        else if (eh->e_type == ET_DYN)
            process_shared_object_file(fbuf[nfbuf], needed_path);
        else
            err("file `%s': unknown object file type", path);
    } else {
        err("file `%s': unknown file format", path);
    }
    ++nfbuf;
}

int main(int argc, char *argv[])
{
    int i;
    bool verbose = FALSE;
    char *out_name = "a.out";
    char *dirs[32];
    char chmod_cmd[256];
    int ndir = 0;
    ObjFile *obj;
    ShrdObjFile *so;
    CmpndSec *csec;
    PLTEnt *pe;

    prog_name = argv[0];
    if (argc < 2)
        err("no input files");
    mem_arena = arena_new(4096, TRUE);
    dynstr = strtab_new();
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
        case 'h':
            if (nfbuf == 0) {
                printf("usage: %s [ options ] <objfile> ...\n\n"
                       "  The available options are\n"
                       "    -o<file>    write output to <file>\n"
                       "    -e<sym>     set <sym> as the entry point symbol\n"
                       "    -l<name>    link against object file/library <name>\n"
                       "    -L<dir>     add <dir> to the list of directories searched for the -l options\n"
                       "    -I<interp>  set <interp> as the name of the dynamic linker\n"
                       "    -h          print this help\n", prog_name);
                if (verbose)
                    printf("\ndefault output name: %s\n"
                           "default entry symbol: %s\n"
                           "default dynamic linker: %s\n", out_name, entry_symbol, interp);
                else
                    printf("\ntype `%s -v -h' to see some default values used for linking\n", prog_name);
                exit(0);
            }
            break;
        case 'v':
            verbose = TRUE;
            break;
        default:
            err("unknown option `%c'", argv[i][1]);
            break;
        }
    }
    init_sections();
    init_segments();
    init_symtab();
    apply_relocs();
    write_ELF_file(fopen(out_name, "wb"));
    sprintf(chmod_cmd, "chmod u+x %s", out_name);
    system(chmod_cmd);

    /* free all */
    arena_destroy(mem_arena);
    strtab_destroy(dynstr);
    for (i = 0; i < nfbuf; i++)
        free(fbuf[i]);
    obj = object_files;
    while (obj != NULL) {
        ObjFile *tmp = obj;
        obj = obj->next;
        free(tmp);
    }
    so = shared_object_files;
    while (so != NULL) {
        ShrdObjFile *tmp = so;
        so = so->next;
        free(tmp);
    }
    csec = sections;
    while (csec != NULL) {
        CmpndSec *tmp1 = csec;
        SmplSec *ssec = csec->sslist;
        while (ssec != NULL) {
            SmplSec *tmp2 = ssec;
            ssec = ssec->next;
            free(tmp2);
        }
        csec = csec->next;
        free(tmp1);
    }
    pe = plt_entries;
    while (pe != NULL) {
        PLTEnt *tmp = pe;
        pe = pe->next;
        free(tmp);
    }

    return 0;
}
