#include "arm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../util/util.h"
#include "luxld.h"
#include "copy.h"

static const char plt0_template[] =
    "\x04\xe0\x2d\xe5"  /* push   {lr}        */
    "\x00\xe0\x9f\xe5"  /* ldr    lr, [pc]    */
    "\x00\xf0\x9e\xe5"  /* ldr    pc, [lr]    */
    "\x00\x00\x00\x00"  /* .word  &.got[2]    */
;

static const char pltn_template[] =
    "\x00\xc0\x9f\xe5"  /* ldr   ip, [pc]     */
    "\x00\xf0\x9c\xe5"  /* ldr   pc, [ip]     */
    "\x00\x00\x00\x00"  /* .word .got[n]      */
    "\x00\x00\xa0\xe1"  /* nop                */
;

/*
 * Install a new entry into .got.plt and a new reloc into .rel.plt.
 * fname is the name of the function this entry is helping to link.
 * Return the address of the new .got.plt entry.
 *
 * Note:
 * For ARM, the GNU linker's default script places .got.plt into the
 * .got section, and the resulting executable has no .got.plt section
 * at all.
 */
static Elf32_Addr new_gotplt_entry(char *fname)
{
    Elf32_Addr ea;
    Elf32_Rel *rp;

    /*
     * Note that there must be a one-to-one correspondense between
     * .got.plt and .rel.plt entries and it should be possible to convert
     * an index into .got.plt to an index into .rel.plt by subtracting
     * three (because of the three .got.plt reserved entries).
     */

    ((Elf32_Word *)(gotplt_sec->sslist->data))[ngotplt] = CS32_SHDR(plt_sec).sh_addr; /* PLT0 (&.plt) */
    ea = CS32_SHDR(gotplt_sec).sh_addr+sizeof(Elf32_Word)*ngotplt;
    ++ngotplt;

    rp = (Elf32_Rel *)relplt_sec->sslist->data;
    rp[nrelplt].r_offset = ea;
    rp[nrelplt].r_info = ELF32_R_INFO(get_dynsym_ndx_32(fname), R_ARM_JUMP_SLOT);
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
        Elf32_Addr addr;

        p = plt_sec->sslist->data+nplt*ARM_PLT_ENTRY_NB;
        if (plt_entries == NULL) {
            /* set PLT0 */
            memcpy(p, plt0_template, ARM_PLT_ENTRY_NB);
            *(Elf32_Addr *)(p+12) = CS32_SHDR(gotplt_sec).sh_addr+8;
            p += ARM_PLT_ENTRY_NB;
            ++nplt;
        }
        memcpy(p, pltn_template, ARM_PLT_ENTRY_NB);
        addr = CS32_SHDR(plt_sec).sh_addr+nplt*ARM_PLT_ENTRY_NB;
        *(Elf32_Addr *)(p+8) = new_gotplt_entry(fname);
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

void arm_apply_relocs(void)
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
                case R_ARM_ABS32:
                    A = *(Elf32_Word *)dest;
                    if (found) {
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
                    *(Elf32_Word *)dest = S+A;
                    break;

                case R_ARM_CALL:
                    A = ((*(Elf32_Sword *)dest&0xFFFFFF)<<8)>>6; /* A = sign_extend_30(signed_imm_24)<<2 */
                    P = shtab[SS32_SHDR(ssec)->sh_info].sh_addr+rel->r_offset;
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
                    *(Elf32_Word *)dest = (*(Elf32_Word *)dest&~0xFFFFFF) | (((S+A-P)>>2)&0xFFFFFF);
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
