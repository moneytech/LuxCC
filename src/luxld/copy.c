#include "copy.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../util.h"

/*
 * Add a new R_*_COPY reloc into .rel.dyn.
 * Return a pointer to space allocated in .bss.
 */
Elf32_Addr new_copy_reloc_32(char *symname, Elf32_Sym *syment)
{
    unsigned n;
    Symbol *sym;
    Elf32_Sym *sp;
    Elf32_Rel *rp;
    Elf32_Addr addr;
    Elf32_Half symndx;

    if (bss_sec == NULL) { /* need to create a .bss section */
        bss_sec = calloc(1, sizeof(CmpndSec));
        bss_sec->name = ".bss";
        bss_sec->shndx = shndx++;
        CS32_SHDR(bss_sec).sh_type = SHT_NOBITS;
        CS32_SHDR(bss_sec).sh_flags = SHF_ALLOC|SHF_WRITE;
        CS32_SHDR(bss_sec).sh_addralign = 4;
        if (WRSeg.nsec == 0) /* need to create a writable segment */
            assert(0); /* we should have at least .dynamic by now */
        CS32_SHDR(bss_sec).sh_offset = SEG32_PHDR(&WRSeg).p_offset+SEG32_PHDR(&WRSeg).p_filesz;
        CS32_SHDR(bss_sec).sh_addr = SEG32_PHDR(&WRSeg).p_vaddr+SEG32_PHDR(&WRSeg).p_memsz;
        bss_sec->next = sections;
        sections = bss_sec;
        WRSeg.secs[WRSeg.nsec++] = bss_sec;
    }

    addr = CS32_SHDR(bss_sec).sh_addr+CS32_SHDR(bss_sec).sh_size;
    symndx = get_dynsym_ndx_32(symname);
    sp = &((Elf32_Sym *)dynsym_sec->sslist->data)[symndx];

    /*
     * From now on this will be the definition of the symbol.
     * References to it from within the executable and associated
     * shared objects will be resolved to this (the dynamic linker
     * always scan the executable's symtab first, so the version
     * defined by the shared object will never be found).
     */
    sym = lookup_global_symbol(symname);
    assert(sym != NULL);
    sym->size = sp->st_size = syment->st_size;
    sym->value = sp->st_value = addr;
    sym->shndx = sp->st_shndx = bss_sec->shndx;
    sym->info = sp->st_info = syment->st_info;
    sym->shname = bss_sec->name;

    rp = (Elf32_Rel *)reldyn_sec->sslist->data;
    rp[nreldyn].r_offset = addr;
    switch (emu_mode) {
    case EMU_X86:  rp[nreldyn].r_info = ELF32_R_INFO(symndx, R_386_COPY);  break;
    case EMU_MIPS: rp[nreldyn].r_info = ELF32_R_INFO(symndx, R_MIPS_COPY); break;
    case EMU_ARM:  rp[nreldyn].r_info = ELF32_R_INFO(symndx, R_ARM_COPY);  break;
    }
    ++nreldyn;

    n = round_up(syment->st_size, 4);
    CS32_SHDR(bss_sec).sh_size += n;
    SEG32_PHDR(&WRSeg).p_memsz += n;

    return addr;
}

Elf64_Addr new_copy_reloc_64(char *symname, Elf64_Sym *syment)
{
    unsigned n;
    Symbol *sym;
    Elf64_Sym *sp;
    Elf64_Rela *rp;
    Elf64_Addr addr;
    Elf64_Half symndx;

    assert(emu_mode == EMU_X64);

    if (bss_sec == NULL) { /* need to create a .bss section */
        bss_sec = calloc(1, sizeof(CmpndSec));
        bss_sec->name = ".bss";
        bss_sec->shndx = shndx++;
        CS64_SHDR(bss_sec).sh_type = SHT_NOBITS;
        CS64_SHDR(bss_sec).sh_flags = SHF_ALLOC|SHF_WRITE;
        CS64_SHDR(bss_sec).sh_addralign = 4;
        if (WRSeg.nsec == 0) /* need to create a writable segment */
            assert(0); /* we should have at least .dynamic by now */
        CS64_SHDR(bss_sec).sh_offset = SEG64_PHDR(&WRSeg).p_offset+SEG64_PHDR(&WRSeg).p_filesz;
        CS64_SHDR(bss_sec).sh_addr = SEG64_PHDR(&WRSeg).p_vaddr+SEG64_PHDR(&WRSeg).p_memsz;
        bss_sec->next = sections;
        sections = bss_sec;
        WRSeg.secs[WRSeg.nsec++] = bss_sec;
    }

    addr = CS64_SHDR(bss_sec).sh_addr+CS64_SHDR(bss_sec).sh_size;
    symndx = get_dynsym_ndx_64(symname);
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
    CS64_SHDR(bss_sec).sh_size += n;
    SEG64_PHDR(&WRSeg).p_memsz += n;

    return addr;
}
