#include "mips.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../util.h"
#include "luxld.h"
#include "copy.h"

static const char plt0_template[] =
    "\x00\x00\x1c\x3c"  /* lui   $gp, high(&.got.plt[0])        */
    "\x00\x00\x99\x8f"  /* lw    $t9, low(&.got.plt[0])($gp)    */
    "\x00\x00\x9c\x27"  /* addiu $gp, $gp, low(&.got.plt[0])    */
    "\x23\xc0\x1c\x03"  /* subu  $t8, $t8, $gp                  */
    "\x21\x78\xe0\x03"  /* move  $t7, $ra                       */
    "\x82\xc0\x18\x00"  /* srl   $t8, $t8, 2                    */
    "\x09\xf8\x20\x03"  /* jalr  $t9                            */
    "\xfe\xff\x18\x27"  /* addiu $t8, $t8, -2                   */
;

/*
 * Note: pltn entries with size != 16 bytes are
 * going to confuse objdump (it will label them
 * wrong).
 */
static const char pltn_template[] =
    "\x00\x00\x0f\x3c"  /* lui   $t7, high(&.got.plt[n])        */
    "\x00\x00\xf9\x8d"  /* lw    $t9, low(&.got.plt[n])($t7)    */
    "\x08\x00\x20\x03"  /* jr    $t9                            */
    "\x00\x00\xf8\x25"  /* addiu $t8, $t7, low(&.got.plt[n])    */
    "\x00\x00\x00\x00"  /* nop                                  */
    "\x00\x00\x00\x00"  /* nop                                  */
    "\x00\x00\x00\x00"  /* nop                                  */
    "\x00\x00\x00\x00"  /* nop                                  */
;

/*
 * Install a new entry into .got.plt and a new reloc into .rel.plt.
 * fname is the name of the function this entry is helping to link.
 * Return the address of the new .got.plt entry.
 */
static Elf32_Addr new_gotplt_entry(char *fname)
{
    Elf32_Addr ea;
    Elf32_Rel *rp;

    /*
     * Note that there must be a one-to-one correspondense between
     * .got.plt and .rel.plt entries and it should be possible to convert
     * an index into .got.plt to an index into .rel.plt by subtracting
     * two (because of the two .got.plt reserved entries).
     */

    ((Elf32_Word *)(gotplt_sec->sslist->data))[ngotplt] = CS32_SHDR(plt_sec).sh_addr; /* PLT0 (&.plt) */
    ea = CS32_SHDR(gotplt_sec).sh_addr+sizeof(Elf32_Word)*ngotplt;
    ++ngotplt;

    rp = (Elf32_Rel *)relplt_sec->sslist->data;
    rp[nrelplt].r_offset = ea;
    rp[nrelplt].r_info = ELF32_R_INFO(get_dynsym_ndx_32(fname), R_MIPS_JUMP_SLOT);
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
        Elf32_Addr addr, gpn_addr;

        p = plt_sec->sslist->data+nplt*MIPS_PLT_ENTRY_NB;
        if (plt_entries == NULL) {
            /* set PLT0 */
            Elf32_Addr gp0_addr;

            memcpy(p, plt0_template, MIPS_PLT_ENTRY_NB);
            gp0_addr = CS32_SHDR(gotplt_sec).sh_addr;
            *(Elf32_Half *)(p+0) = gp0_addr>>16;
            if (gp0_addr & 0x8000)
                ++*(Elf32_Half *)(p+0);
            *(Elf32_Half *)(p+4) = gp0_addr&0xFFFF;
            *(Elf32_Half *)(p+8) = gp0_addr&0xFFFF;
            p += MIPS_PLT_ENTRY_NB;
            ++nplt;
        }
        memcpy(p, pltn_template, MIPS_PLT_ENTRY_NB);
        addr = CS32_SHDR(plt_sec).sh_addr+nplt*MIPS_PLT_ENTRY_NB;
        gpn_addr = new_gotplt_entry(fname);
        *(Elf32_Half *)(p+0) = gpn_addr>>16;
        if (gpn_addr & 0x8000)
            ++*(Elf32_Half *)(p+0);
        *(Elf32_Half *)(p+4) = gpn_addr&0xFFFF;
        *(Elf32_Half *)(p+12) = gpn_addr&0xFFFF;
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

void mips_apply_relocs(void)
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
                Elf32_Word A, S;

                dest = &buf[shtab[SS32_SHDR(ssec)->sh_info].sh_offset+rel->r_offset];
                symname = &strtab[symtab[ELF32_R_SYM(rel->r_info)].st_name];
                S = get_symval_32(symname, &symtab[ELF32_R_SYM(rel->r_info)], &found);

                switch (ELF32_R_TYPE(rel->r_info)) {
                case R_MIPS_16:
                    A = *(short *)dest;
                    goto r_mips;
                case R_MIPS_32:
                    A = *(Elf32_Word *)dest;
                    goto r_mips;
                case R_MIPS_HI16:
                    A = *(Elf32_Half *)dest<<16;
r_mips:             if (found) {
                        ;
                    } else if ((syment=lookup_in_shared_object_32(symname)) != NULL) {
                        if (ELF32_ST_TYPE(syment->st_info) == STT_FUNC) {
                            Symbol *sym;

                            /* See 'Function Addresses' in the `MIPS non-PIC ABI specification' document */
                            sym = lookup_global_symbol(symname);
                            assert(sym != NULL);
                            if (sym->value == 0) {
                                syment = &((Elf32_Sym *)dynsym_sec->sslist->data)[get_dynsym_ndx_32(symname)];
                                syment->st_value = sym->value = get_plt_entry(symname);
                                syment->st_info = sym->info = ELF32_ST_INFO(STB_GLOBAL, STT_FUNC);
                                syment->st_shndx = sym->shndx = SHN_UNDEF;
                                syment->st_other = sym->other = STO_MIPS_PLT;
                            }
                            S = sym->value;
                        } else {
                            S = new_copy_reloc_32(symname, syment);
                        }
                    } else {
                        err_undef(symname);
                    }
                    switch (ELF32_R_TYPE(rel->r_info)) {
                    case R_MIPS_16:
                        *(short *)dest = (short)(S+A);
                        break;
                    case R_MIPS_32:
                        *(Elf32_Word *)dest = S+A;
                        break;
                    case R_MIPS_HI16:
                        if (i!=(nrel-1) && ELF32_R_TYPE((rel+1)->r_info)==R_MIPS_LO16) {
                            Elf32_Word V;
                            Elf32_Half *dest2;

                            dest2 = (Elf32_Half *)&buf[shtab[SS32_SHDR(ssec)->sh_info].sh_offset+(rel+1)->r_offset];
                            V = S+A+*dest2;
                            *(Elf32_Half *)dest = (Elf32_Half)(V>>16);
                            if (V & 0x8000)
                                ++*(Elf32_Half *)dest;
                            *dest2 = (Elf32_Half)V;
                            ++i, ++rel;
                        } else {
                            err("R_MIPS_HI16 relocation not followed by R_MIPS_LO16 relocation");
                        }
                        break;
                    }
                    break;

                case R_MIPS_LO16:
                    err("orphaned R_MIPS_LO16 relocation");
                    break;

                case R_MIPS_26:
                    A = (*(Elf32_Word *)dest&0x3FFFFFF)<<2;
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
                    *(Elf32_Word *)dest = (*(Elf32_Word *)dest&~0x3FFFFFF) | (((S+A)>>2)&0x3FFFFFF);
                    break;

                case R_MIPS_PC16: { /* this reloc is obsolete */
#if 0
                    Elf32_Word P;

                    A = *(short *)dest;
                    P = shtab[SS32_SHDR(ssec)->sh_info].sh_addr+rel->r_offset;
                    *(Elf32_Half *)dest = S+A-P;
#else
                    assert(0);
#endif
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

