#include "x64.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../util/util.h"
#include "luxld.h"
#include "copy.h"

static const char plt0_template[] =
    "\xff\x35\x00\x00\x00\x00"  /* push qword [REL got+8] */
    "\xff\x25\x00\x00\x00\x00"  /* jmp  qword [REL got+16] */
    "\x90\x90\x90\x90"          /* times 4 nop */
;

static const char pltn_template[] =
    "\xff\x25\x00\x00\x00\x00"  /* jmp  qword [REL got+n] */
    "\x68\x00\x00\x00\x00"      /* push dword reloc_index */
    "\xe9\x00\x00\x00\x00"      /* jmp  PLT0 */
;

static Elf64_Addr new_gotplt_entry(Elf64_Addr pa, Elf64_Word *ri, char *fname)
{
    Elf64_Addr ea;
    Elf64_Rela *rp;

    ((Elf64_Addr *)(gotplt_sec->sslist->data))[ngotplt] = pa;
    ea = CS64_SHDR(gotplt_sec).sh_addr+sizeof(Elf64_Addr)*ngotplt;
    ++ngotplt;

    rp = (Elf64_Rela *)relaplt_sec->sslist->data;
    rp[nrelaplt].r_offset = ea;
    rp[nrelaplt].r_info = ELF64_R_INFO(get_dynsym_ndx_64(fname), R_X86_64_JUMP_SLOT);
    rp[nrelaplt].r_addend = 0;
    *ri = nrelaplt;
    ++nrelaplt;

    return ea;
}

static Elf64_Addr get_plt_entry(char *fname)
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
            memcpy(p, plt0_template, PLT_ENTRY_NB);
            *(Elf64_Word *)(p+2) = CS64_SHDR(gotplt_sec).sh_addr-(CS64_SHDR(plt_sec).sh_addr+6)+8;
            *(Elf64_Word *)(p+8) = CS64_SHDR(gotplt_sec).sh_addr-(CS64_SHDR(plt_sec).sh_addr+12)+16;
            p += PLT_ENTRY_NB;
            ++nplt;
        }
        memcpy(p, pltn_template, PLT_ENTRY_NB);
        addr = CS64_SHDR(plt_sec).sh_addr+nplt*PLT_ENTRY_NB;
        *(Elf64_Word *)(p+2) = new_gotplt_entry(addr+6, &ri, fname)-(addr+6);
        *(Elf64_Word *)(p+7) = ri;
        *(Elf64_Word *)(p+12) = CS64_SHDR(plt_sec).sh_addr-(addr+PLT_ENTRY_NB);
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

void x64_apply_relocs(void)
{
    SmplSec *ssec;
    CmpndSec *csec;

    for (csec = sections; csec != NULL; csec = csec->next) {
        if (CS64_SHDR(csec).sh_type!=SHT_RELA || equal(csec->name, ".rela.plt") || equal(csec->name, ".rela.dyn"))
            continue;
        for (ssec = csec->sslist; ssec != NULL; ssec = ssec->next) {
            int i, nrel;
            Elf64_Sym *symtab;
            Elf64_Shdr *shtab;
            Elf64_Rela *rel;
            char *strtab;
            char *buf;

            rel = (Elf64_Rela *)ssec->data;
            nrel = SS64_SHDR(ssec)->sh_size/sizeof(Elf64_Rela);
            symtab = OF64_SYMTAB(ssec->obj);
            shtab = OF64_SHTAB(ssec->obj);
            strtab = ssec->obj->strtab;
            buf = ssec->obj->buf;

            for (i = 0; i < nrel; i++, rel++) {
                bool found;
                void *dest;
                char *symname;
                Elf64_Sym *syment;
                Elf64_Xword A, S, P;

                dest = &buf[shtab[SS64_SHDR(ssec)->sh_info].sh_offset+rel->r_offset];
                symname = &strtab[symtab[ELF64_R_SYM(rel->r_info)].st_name];
                S = get_symval_64(symname, &symtab[ELF64_R_SYM(rel->r_info)], &found);
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
                    } else if ((syment=lookup_in_shared_object_64(symname)) != NULL) {
                        if (ELF64_ST_TYPE(syment->st_info) == STT_FUNC) {
                            Symbol *sym;

                            /* See 'Function Addresses' in the AMD64 ABI. */
                            sym = lookup_global_symbol(symname);
                            assert(sym != NULL);
                            if (sym->value == 0) {
                                syment = &((Elf64_Sym *)dynsym_sec->sslist->data)[get_dynsym_ndx_64(symname)];
                                syment->st_value = sym->value = get_plt_entry(symname);
                                syment->st_info = sym->info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
                                syment->st_shndx = sym->shndx = SHN_UNDEF;
                            }
                            S = sym->value;
                        } else {
                            S = new_copy_reloc_64(symname, syment);
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
                    P = shtab[SS64_SHDR(ssec)->sh_info].sh_addr+rel->r_offset;
                    if (found) {
                        ;
                    } else if ((syment=lookup_in_shared_object_64(symname)) != NULL) {
                        if (ELF64_ST_TYPE(syment->st_info) == STT_FUNC)
                            S = get_plt_entry(symname);
                        else
                            S = new_copy_reloc_64(symname, syment);
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
