/*
    LUXLD: minimal ELF static linker for MIPS32 (little endian).

    This completes the MIPS toolchain (cc+as+ld).

    Documents:
        - gABI: http://www.sco.com/developers/gabi/latest/contents.html
        - MIPS o32 ABI: http://math-atlas.sourceforge.net/devel/assembly/mipsabi32.pdf
        - MIPS NUBI: http://www.linux-mips.org/pub/linux/mips/doc/NUBI/MD00438-2C-NUBIDESC-SPC-00.16.pdf
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <elf.h>
#include <ar.h>
#include <assert.h>
#include <stdarg.h>
#include "../util.h"
#include "../luxas/ELF_util.h"

#define MAX_INPUT_FILES     64
#define MAX_SEC_PER_SEG     32
#define PAGE_SIZE           0x1000
#define PAGE_MASK           (PAGE_SIZE-1)
#define HASH_SIZE           1009
#define HASH(s)             (hash(s)%HASH_SIZE)

#ifndef E_MIPS_ABI_O32
#define E_MIPS_ABI_O32 0x00001000 /* The original o32 abi. */
#endif

typedef unsigned char bool;
typedef struct Symbol Symbol;
typedef struct ObjFile ObjFile;
typedef struct SmplSec SmplSec;
typedef struct CmpndSec CmpndSec;
typedef struct Segment Segment;

struct ObjFile { /* simple relocatable object file */
    char *buf;
    Elf32_Ehdr *ehdr;   /* ELF header */
    Elf32_Shdr *shtab;  /* Section header table */
    Elf32_Sym *symtab;  /* Symbol table */
    int nsym;           /* # of symbol table entries */
    char *shstrtab;     /* Section name string table */
    char *strtab;       /* String table */
    ObjFile *next;
} *object_files;

struct SmplSec { /* simple section with the contribution of a single object file */
    ObjFile *obj;       /* object file that contributed this section */
    Elf32_Shdr *shdr;
    char *data;
    SmplSec *next;
};

struct CmpndSec { /* compound section with the contributions of all object files */
    char *name;
    Elf32_Shdr shdr;
    Elf32_Half shndx;   /* index into output file's section header table */
    SmplSec *sslist;
    CmpndSec *next;
} *sections;

struct Segment { /* read-only & writable loadable segments */
    int nsec;
    CmpndSec *secs[MAX_SEC_PER_SEG];
    Elf32_Phdr phdr;
} ROSeg, WRSeg;

struct Symbol {
    char *name;
    Elf32_Addr value;
    Elf32_Word size;
    unsigned char info;
    Elf32_Half shndx;   /* index into input file's section header table */
    char *shname;
    Symbol *next;
};

Symbol *lookup_global_symbol(char *name);
char *entry_symbol = "__start";
char *prog_name;
char *fbuf[MAX_INPUT_FILES];
int nfbuf;
int nundef; /* # of undefined ('extern') symbols currently in the symtab */
Elf32_Half shndx = 4; /* [0]=UND, [1]=.shstrtab, [2]=.symtab, [3]=.strtab */

/* Symbols that will go into the output file's symtab. */
Symbol *global_symbols[HASH_SIZE];
Symbol *local_symbols; /* with type other than SECTION or FILE */

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

SmplSec *new_smpl_sec(ObjFile *obj, Elf32_Shdr *hdr, char *data, SmplSec *next)
{
    SmplSec *n;

    n = malloc(sizeof(SmplSec));
    n->obj = obj;
    n->shdr = hdr;
    n->data = data;
    n->next = next;
    return n;
}

void add_section(ObjFile *obj, char *name, Elf32_Shdr *hdr)
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
    }
}

void init_sections(void)
{
    ObjFile *obj;

    for (obj = object_files; obj != NULL; obj = obj->next) {
        int i;
        Elf32_Shdr *shdr;

        shdr = obj->shtab+1; /* skip SHN_UNDEF */
        for (i = 1; i < obj->ehdr->e_shnum; i++, shdr++)
            add_section(obj, obj->shstrtab+shdr->sh_name, shdr);
    }
}

void init_segments(void)
{
    int i;
    SmplSec *ssec;
    CmpndSec *csec;
    Elf32_Word size;
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

    offset += round_up(sizeof(Elf32_Ehdr), 16);
    if (ROSeg.nsec > 0)
        offset += sizeof(Elf32_Phdr);
    if (WRSeg.nsec > 0)
        offset += sizeof(Elf32_Phdr);
    if (ROSeg.nsec > 0) {
        ROSeg.phdr.p_type = PT_LOAD;
        ROSeg.phdr.p_flags = PF_R|PF_X;
        ROSeg.phdr.p_align = 0x1000;

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

void define_global_symbol(char *name, Elf32_Addr value, unsigned char info, Elf32_Half shndx, char *shname)
{
    unsigned h;
    Symbol *np;

    h = HASH(name);
    for (np = global_symbols[h]; np != NULL; np = np->next)
        if (equal(np->name, name))
            break;
    if (np == NULL) {
        np = malloc(sizeof(Symbol));
        np->name = name;
        np->value = value;
        np->size = 0;
        np->info = info;
        if ((np->shndx=shndx) == SHN_UNDEF)
            ++nundef;
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

void define_local_symbol(char *name, Elf32_Addr value, unsigned char info, Elf32_Half shndx, char *shname)
{
    Symbol *np;

    np = malloc(sizeof(Symbol));
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
Elf32_Half get_shndx(Symbol *sym)
{
    CmpndSec *csec;

    if (sym->shndx==SHN_UNDEF || sym->shndx>=SHN_LORESERVE)
        return sym->shndx;
    for (csec = sections; csec != NULL; csec = csec->next)
        if (equal(csec->name, sym->shname))
            return csec->shndx;
    assert(0);
}

void init_symtab(void)
{
    SmplSec *ssec;
    CmpndSec *csec;

    for (csec = sections; csec != NULL; csec = csec->next)
        if (csec->shdr.sh_type == SHT_SYMTAB)
            break;
    if (csec == NULL)
        return;

    for (ssec = csec->sslist; ssec != NULL; ssec = ssec->next) {
        int i, nsym;
        Elf32_Sym *symtab;
        Elf32_Shdr *shtab;
        char *strtab, *shstrtab;

        nsym = ssec->shdr->sh_size/sizeof(Elf32_Sym);
        symtab = ssec->obj->symtab;
        shtab = ssec->obj->shtab;
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
            }
                break;

            case STB_WEAK:
                /* TODO */
                break;
            }
        }
    }
}

Elf32_Addr get_symval(char *name, Elf32_Sym *st_ent, bool *found)
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
        return sym->value;
    return 0;
}

void apply_relocs(void)
{
    SmplSec *ssec;
    CmpndSec *csec;

    for (csec = sections; csec != NULL; csec = csec->next) {
        if (csec->shdr.sh_type != SHT_REL)
            continue;
        for (ssec = csec->sslist; ssec != NULL; ssec = ssec->next) {
            int i, nrel;
            Elf32_Sym *symtab;
            Elf32_Shdr *shtab;
            Elf32_Rel *rel;
            char *strtab;
            char *buf;

            rel = (Elf32_Rel *)ssec->data;
            nrel = ssec->shdr->sh_size/sizeof(Elf32_Rel);
            symtab = ssec->obj->symtab;
            shtab = ssec->obj->shtab;
            strtab = ssec->obj->strtab;
            buf = ssec->obj->buf;

            for (i = 0; i < nrel; i++, rel++) {
                bool found;
                void *dest;
                char *symname;
                Elf32_Word A, S;

                dest = &buf[shtab[ssec->shdr->sh_info].sh_offset+rel->r_offset];
                symname = &strtab[symtab[ELF32_R_SYM(rel->r_info)].st_name];
                S = get_symval(symname, &symtab[ELF32_R_SYM(rel->r_info)], &found);

                switch (ELF32_R_TYPE(rel->r_info)) {
                case R_MIPS_16:
                    A = *(short *)dest;
                    *(short *)dest = (short)(S+A);
                    break;

                case R_MIPS_32:
                    A = *(Elf32_Word *)dest;
                    *(Elf32_Word *)dest = S+A;
                    break;

                case R_MIPS_26:
                    A = (*(Elf32_Word *)dest&0x3FFFFFF)<<2;
                    *(Elf32_Word *)dest = (*(Elf32_Word *)dest&~0x3FFFFFF) | (((S+A)>>2)&0x3FFFFFF);
                    break;

                case R_MIPS_HI16:
                    if (i!=(nrel-1) && ELF32_R_TYPE((rel+1)->r_info)==R_MIPS_LO16) {
                        Elf32_Word V;
                        Elf32_Half *dest2;

                        dest2 = (Elf32_Half *)&buf[shtab[ssec->shdr->sh_info].sh_offset+(rel+1)->r_offset];
                        V = S+(*(Elf32_Half *)dest<<16)+*dest2;
                        *(Elf32_Half *)dest = (Elf32_Half)(V>>16);
                        if (V & 0x8000)
                            ++*(Elf32_Half *)dest;
                        *dest2 = (Elf32_Half)V;
                        ++i, ++rel;
                    } else {
                        err("R_MIPS_HI16 relocation not followed by R_MIPS_LO16 relocation");
                    }
                    break;

                case R_MIPS_LO16:
                    err("orphaned R_MIPS_LO16 relocation");
                    break;

                case R_MIPS_PC16: {
                    assert(0);
                    /*
                    Elf32_Word P;

                    A = *(short *)dest;
                    P = shtab[ssec->shdr->sh_info].sh_addr+rel->r_offset;
                    *(Elf32_Half *)dest = S+A-P;
                    */
                }
                    break;

                /* other */
                default:
                    err("relocation type `0x%02x' not supported", ELF32_R_TYPE(rel->r_info));
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
    Elf32_Sym esym;
    Elf32_Off curr = 0;
    Elf32_Ehdr ehdr;
    Elf32_Shdr symtab_header, shstrtab_header, strtab_header, undef_header;

#define ALIGN(n)\
    do {\
        int nb = round_up(curr, n)-curr, i;\
        for (i = 0; i < nb; i++)\
            fputc(0, outf), ++curr;\
    } while (0)

    memset(&symtab_header, 0, sizeof(Elf32_Shdr));
    memset(&shstrtab_header, 0, sizeof(Elf32_Shdr));
    memset(&strtab_header, 0, sizeof(Elf32_Shdr));

    /*
     * ================
     * Dummy ELF header
     * ================
     */
    memset(&ehdr, 0, sizeof(Elf32_Ehdr));
    fwrite(&ehdr, sizeof(Elf32_Ehdr), 1, outf);
    curr += sizeof(Elf32_Ehdr);

    /*
     * ====================
     * Program header table
     * ====================
     */
    ALIGN(16);
    ehdr.e_phoff = curr;
    if (ROSeg.nsec) {
        fwrite(&ROSeg.phdr, sizeof(Elf32_Phdr), 1, outf);
        curr += sizeof(Elf32_Phdr);
        ++ehdr.e_phnum;
    }
    if (WRSeg.nsec) {
        fwrite(&WRSeg.phdr, sizeof(Elf32_Phdr), 1, outf);
        curr += sizeof(Elf32_Phdr);
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
        fwrite(&esym, sizeof(Elf32_Sym), 1, outf);\
        curr += sizeof(Elf32_Sym);\
        symtab_header.sh_size += sizeof(Elf32_Sym);\
        ++symtab_header.sh_info;\
    } while (0)

    /* first entry (STN_UNDEF) */
    memset(&esym, 0, sizeof(Elf32_Sym));
    WRITE_ST_ENT();

    /* FILE symbol table entry/ies */
    memset(&esym, 0, sizeof(Elf32_Sym));
    esym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_FILE);
    esym.st_shndx = SHN_ABS;
    for (obj = object_files; obj != NULL; obj = obj->next) {
        if (obj->symtab == NULL)
            continue;
        if (ELF32_ST_TYPE(obj->symtab[1].st_info) == STT_FILE)
            esym.st_name = strtab_append(strtab, &obj->strtab[obj->symtab[1].st_name]);
        else
            continue;
        WRITE_ST_ENT();
    }

    /* loadable sections */
    for (i = 0; i < ROSeg.nsec; i++) {
        memset(&esym, 0, sizeof(Elf32_Sym));
        esym.st_value = ROSeg.secs[i]->shdr.sh_addr;
        esym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
        esym.st_shndx = ROSeg.secs[i]->shndx;
        WRITE_ST_ENT();
    }
    for (i = 0; i < WRSeg.nsec; i++) {
        memset(&esym, 0, sizeof(Elf32_Sym));
        esym.st_value = WRSeg.secs[i]->shdr.sh_addr;
        esym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
        esym.st_shndx = WRSeg.secs[i]->shndx;
        WRITE_ST_ENT();
    }

    /* local symbols (with type other than FILE or SECTION) */
    for (sym = local_symbols; sym != NULL; sym = sym->next) {
        memset(&esym, 0, sizeof(Elf32_Sym));
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
            memset(&esym, 0, sizeof(Elf32_Sym));
            esym.st_name = strtab_append(strtab, np->name);
            esym.st_value = np->value;
            esym.st_size = np->size;
            esym.st_info = np->info;
            esym.st_shndx = get_shndx(np);
            fwrite(&esym, sizeof(Elf32_Sym), 1, outf);
            curr += sizeof(Elf32_Sym);
            symtab_header.sh_size += sizeof(Elf32_Sym);
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
    memset(&undef_header, 0, sizeof(Elf32_Shdr));
    fwrite(&undef_header, sizeof(Elf32_Shdr), 1, outf);

    /* .shstrtab section header */
    shstrtab_header.sh_type = SHT_STRTAB;
    shstrtab_header.sh_addralign = 1;
    fwrite(&shstrtab_header, sizeof(Elf32_Shdr), 1, outf);

    /* .symtab section header */
    symtab_header.sh_type = SHT_SYMTAB;
    symtab_header.sh_link = 3; /* .strtab */
    symtab_header.sh_addralign = 4;
    symtab_header.sh_entsize = sizeof(Elf32_Sym);
    fwrite(&symtab_header, sizeof(Elf32_Shdr), 1, outf);

    /* .strtab section header */
    strtab_header.sh_type = SHT_STRTAB;
    strtab_header.sh_addralign = 1;
    fwrite(&strtab_header, sizeof(Elf32_Shdr), 1, outf);

    /* remaining section headers */
    for (i = 0; i < ROSeg.nsec; i++)
        fwrite(&ROSeg.secs[i]->shdr, sizeof(Elf32_Shdr), 1, outf);
    for (i = 0; i < WRSeg.nsec; i++)
        fwrite(&WRSeg.secs[i]->shdr, sizeof(Elf32_Shdr), 1, outf);

    /*
     * Correct dummy ELF header
     */
    rewind(outf);
    ehdr.e_ident[EI_MAG0] = ELFMAG0;
    ehdr.e_ident[EI_MAG1] = ELFMAG1;
    ehdr.e_ident[EI_MAG2] = ELFMAG2;
    ehdr.e_ident[EI_MAG3] = ELFMAG3;
    ehdr.e_ident[EI_CLASS] = ELFCLASS32;
    ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
    ehdr.e_ident[EI_VERSION] = EV_CURRENT;
    ehdr.e_type = ET_EXEC;
    ehdr.e_machine = EM_MIPS;
    ehdr.e_version = EV_CURRENT;
    if ((sym=lookup_global_symbol(entry_symbol))==NULL || sym->shndx==SHN_UNDEF)
        err("cannot find entry symbol `%s'", entry_symbol);
    ehdr.e_entry = sym->value;
    ehdr.e_ehsize = sizeof(Elf32_Ehdr);
    ehdr.e_phentsize = sizeof(Elf32_Phdr);
    ehdr.e_shentsize = sizeof(Elf32_Shdr);
    ehdr.e_shstrndx = 1;
    fwrite(&ehdr, sizeof(Elf32_Ehdr), 1, outf);
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
    obj->ehdr = (Elf32_Ehdr *)buf;
    obj->shtab = (Elf32_Shdr *)(buf+obj->ehdr->e_shoff);
    obj->shstrtab = buf + obj->shtab[obj->ehdr->e_shstrndx].sh_offset;
    for (i = 1; i < obj->ehdr->e_shnum; i++) {
        if (obj->shtab[i].sh_type == SHT_SYMTAB) {
            obj->symtab = (Elf32_Sym *)(buf+obj->shtab[i].sh_offset);
            obj->nsym = obj->shtab[i].sh_size/sizeof(Elf32_Sym);
            obj->strtab = buf+obj->shtab[obj->shtab[i].sh_link].sh_offset;
            first_gsym = obj->shtab[i].sh_info;
            break;
        }
    }
    obj->next = object_files;
    object_files = obj;

    /* install any global symbol */
    if (first_gsym < obj->nsym) {
        Elf32_Sym *symtab = obj->symtab;
        Elf32_Shdr *shtab = obj->shtab;
        char *strtab = obj->strtab;
        char *shstrtab = obj->shstrtab;

        for (i = first_gsym; i < obj->nsym; i++) {
            char *shname = (symtab[i].st_shndx==SHN_UNDEF || symtab[i].st_shndx>=SHN_LORESERVE)
                         ? NULL : &shstrtab[shtab[symtab[i].st_shndx].sh_name];
            define_global_symbol(&strtab[symtab[i].st_name], 0, symtab[i].st_info, symtab[i].st_shndx, shname);
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

void process_file(char *path, char *needed_path)
{
    if ((fbuf[nfbuf]=read_file(path)) == NULL)
        err("cannot read file `%s'", path);
    if (strncmp(fbuf[nfbuf], ARMAG, SARMAG) == 0) {
        process_archive(fbuf[nfbuf]);
    } else if (strncmp(fbuf[nfbuf], "\x7f""ELF", 3) == 0) {
        Elf32_Ehdr *eh;

        eh = (Elf32_Ehdr *)fbuf[nfbuf];
        if (eh->e_type == ET_REL)
            process_object_file(fbuf[nfbuf]);
        else if (eh->e_type == ET_DYN)
            err("file `%s': shared object files are not supported", path);
        else
            err("file `%s': unknown object file type", path);
        if (eh->e_ident[EI_CLASS]!=ELFCLASS32 || eh->e_machine!=EM_MIPS)
            err("file `%s': object file for unknown architecture");
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
    CmpndSec *csec;

    prog_name = argv[0];
    if (argc < 2)
        err("no input files");
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
        case 'h':
            if (nfbuf == 0) {
                printf("usage: %s [ options ] <objfile> ...\n\n"
                       "  The available options are\n"
                       "    -o<file>    write output to <file>\n"
                       "    -e<sym>     set <sym> as the entry point symbol\n"
                       "    -l<name>    link against object file/library <name>\n"
                       "    -L<dir>     add <dir> to the list of directories searched for the -l options\n"
                       "    -h          print this help\n", prog_name);
                if (verbose)
                    printf("\ndefault output name: %s\n"
                           "default entry symbol: %s\n", out_name, entry_symbol);
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
    for (i = 0; i < nfbuf; i++)
        free(fbuf[i]);
    obj = object_files;
    while (obj != NULL) {
        ObjFile *tmp = obj;
        obj = obj->next;
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

    return 0;
}
