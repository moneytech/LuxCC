#include "x86.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../util/util.h"
#include "luxld.h"
#include "copy.h"

static const char plt0_template[] =
    "\xff\x35\x00\x00\x00\x00"  /* push dword [got+4] */
    "\xff\x25\x00\x00\x00\x00"  /* jmp  dword [got+8] */
    "\x90\x90\x90\x90"          /* times 4 nop */
;

static const char pltn_template[] =
    "\xff\x25\x00\x00\x00\x00"  /* jmp  dword [got+n] */
    "\x68\x00\x00\x00\x00"      /* push dword reloc_offset */
    "\xe9\x00\x00\x00\x00"      /* jmp  PLT0 */
;

/*
 * Install a new entry into .got.plt and a new reloc into .rel.plt.
 * Parameters:
 *  - pa is the address of a push instruction (see pltn_template).
 *  - ro points to the push instruction's argument.
 *  - fname is the name of the function this entry is helping to link.
 * Return the address of the new .got.plt entry.
 */
static Elf32_Addr new_gotplt_entry(Elf32_Addr pa, Elf32_Off *ro, char *fname)
{
    Elf32_Addr ea;
    Elf32_Rel *rp;

    ((Elf32_Word *)(gotplt_sec->sslist->data))[ngotplt] = pa;
    ea = CS32_SHDR(gotplt_sec).sh_addr+sizeof(Elf32_Word)*ngotplt;
    ++ngotplt;

    rp = (Elf32_Rel *)relplt_sec->sslist->data;
    rp[nrelplt].r_offset = ea;
    rp[nrelplt].r_info = ELF32_R_INFO(get_dynsym_ndx_32(fname), R_386_JMP_SLOT);
    *ro = nrelplt*sizeof(Elf32_Rel);
    ++nrelplt;

    return ea;
}

/*
 * Get the PLT entry corresponding to the function fname (defined in a shared object).
 * Create a new entry if fname was not seen before.
 * Return a PLT entry address.
 */
static Elf32_Addr get_plt_entry(char *fname)
{
    PLTEnt *np;

    for (np = plt_entries; np != NULL; np = np->next)
        if (equal(np->fname, fname))
            break;

    if (np == NULL) {
        char *p;
        Elf32_Off ro;
        Elf32_Addr addr;

        p = plt_sec->sslist->data+nplt*PLT_ENTRY_NB;
        if (plt_entries == NULL) {
            /* set PLT0 */
            memcpy(p, plt0_template, PLT_ENTRY_NB);
            *(Elf32_Addr *)(p+2) = CS32_SHDR(gotplt_sec).sh_addr+4;
            *(Elf32_Addr *)(p+8) = CS32_SHDR(gotplt_sec).sh_addr+8;
            p += PLT_ENTRY_NB;
            ++nplt;
        }
        memcpy(p, pltn_template, PLT_ENTRY_NB);
        addr = CS32_SHDR(plt_sec).sh_addr+nplt*PLT_ENTRY_NB;
        *(Elf32_Addr *)(p+2) = new_gotplt_entry(addr+6, &ro, fname);
        *(Elf32_Off *)(p+7) = ro;
        *(Elf32_Off *)(p+12) = CS32_SHDR(plt_sec).sh_addr-(addr+PLT_ENTRY_NB);
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

void x86_apply_relocs(void)
{
    SmplSec *ssec;
    CmpndSec *csec;

    for (csec = sections; csec != NULL; csec = csec->next) {
        if (CS32_SHDR(csec).sh_type!=SHT_REL || equal(csec->name, ".rel.plt") || equal(csec->name, ".rel.dyn"))
            continue;
        for (ssec = csec->sslist; ssec != NULL; ssec = ssec->next) {
            int i, nrel;
            Elf32_Sym *symtab;
            Elf32_Shdr *shtab;
            Elf32_Rel *rel;
            char *strtab;
            char *buf;

            rel = (Elf32_Rel *)ssec->data;
            nrel = SS32_SHDR(ssec)->sh_size/sizeof(Elf32_Rel);
            symtab = OF32_SYMTAB(ssec->obj);
            shtab = OF32_SHTAB(ssec->obj);
            strtab = ssec->obj->strtab;
            buf = ssec->obj->buf;
            for (i = 0; i < nrel; i++, rel++) {
                bool found;
                void *dest;
                char *symname;
                Elf32_Sym *syment;
                Elf32_Word A, S, P;

                dest = &buf[shtab[SS32_SHDR(ssec)->sh_info].sh_offset+rel->r_offset];
                symname = &strtab[symtab[ELF32_R_SYM(rel->r_info)].st_name];
                S = get_symval_32(symname, &symtab[ELF32_R_SYM(rel->r_info)], &found);

                switch (ELF32_R_TYPE(rel->r_info)) {
                /* 386_X */
                case R_386_8:
                    A = *(char *)dest;
                    goto r_386;
                case R_386_16:
                    A = *(short *)dest;
                    goto r_386;
                case R_386_32:
                    A = *(Elf32_Sword *)dest;
r_386:              if (found) {
                        ;
                    } else if ((syment=lookup_in_shared_object_32(symname)) != NULL) {
                        if (ELF32_ST_TYPE(syment->st_info) == STT_FUNC) {
                            Symbol *sym;

                            /* See 'Function Addresses' in the i386 psABI. */
                            sym = lookup_global_symbol(symname);
                            assert(sym != NULL);
                            if (sym->value == 0) {
                                syment = &((Elf32_Sym *)dynsym_sec->sslist->data)[get_dynsym_ndx_32(symname)];
                                syment->st_value = sym->value = get_plt_entry(symname);
                                syment->st_info = sym->info = ELF32_ST_INFO(STB_GLOBAL, STT_FUNC);
                                syment->st_shndx = sym->shndx = SHN_UNDEF;
                            }
                            S = sym->value;
                        } else {
                            S = new_copy_reloc_32(symname, syment);
                        }
                    } else {
                        err_undef(symname);
                    }
                    switch (ELF32_R_TYPE(rel->r_info)) {
                    case R_386_8:
                        *(char *)dest = S+A;
                        break;
                    case R_386_16:
                        *(short *)dest = S+A;
                        break;
                    case R_386_32:
                        *(Elf32_Word *)dest = S+A;
                        break;
                    }
                    break;

                /* 386_PCX */
                case R_386_PC8:
                    A = *(char *)dest;
                    goto r_386_pc;
                case R_386_PC16:
                    A = *(short *)dest;
                    goto r_386_pc;
                case R_386_PC32:
                    A = *(Elf32_Sword *)dest;
r_386_pc:           P = shtab[SS32_SHDR(ssec)->sh_info].sh_addr+rel->r_offset;
                    if (found) {
                        ;
                    } else if ((syment=lookup_in_shared_object_32(symname)) != NULL) {
                        if (ELF32_ST_TYPE(syment->st_info) == STT_FUNC)
                            S = get_plt_entry(symname);
                        else
                            S = new_copy_reloc_32(symname, syment);
                    } else {
                        err_undef(symname);
                    }
                    switch (ELF32_R_TYPE(rel->r_info)) {
                    case R_386_PC8:
                        *(char *)dest = S+A-P;
                        break;
                    case R_386_PC16:
                        *(short *)dest = S+A-P;
                        break;
                    case R_386_PC32:
                        *(Elf32_Word *)dest = S+A-P;
                        break;
                    }
                    break;

#if 0
                case R_386_GOT32:
                case R_386_PLT32:
                case R_386_GOTPC:
                    break;
#endif

                /* other */
                default:
                    err("relocation type `0x%02x' not supported", ELF32_R_TYPE(rel->r_info));
                    break;
                }
            }
        }
    }
}
