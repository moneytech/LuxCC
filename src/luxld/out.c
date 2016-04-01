#include "out.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../util.h"
#include "luxld.h"

void write_ELF_file_32(FILE *outf)
{
    int i;
    Symbol *sym;
    StrTab *strtab = strtab_new(), *shstrtab = strtab_new();
    ObjFile *obj;
    ShrdObjFile *so;
    Elf32_Sym esym;
    Elf32_Off curr = 0;
    Elf32_Ehdr ehdr;
    Elf32_Phdr phdr;
    Elf32_Shdr symtab_header, shstrtab_header, strtab_header, undef_header;
    Elf32_Dyn *dp;

    /*
     * Sections that turned out to be not necessary are written in the file
     * so that previously computed offsets remain correct, but those sections
     * will have size 0 and will not have entries related to them in the dynamic
     * array.
     */

    /* Before start, set values that were left undefined (or only were estimated) */
    if (plt_sec != NULL) {
        CS32_SHDR(plt_sec).sh_size = nplt*PLT_ENTRY_NB;
        CS32_SHDR(relplt_sec).sh_link = dynsym_sec->shndx;
        CS32_SHDR(relplt_sec).sh_info = plt_sec->shndx; /* or gotplt_sec->shndx instead? */
        CS32_SHDR(relplt_sec).sh_size = nrelplt*sizeof(Elf32_Rel);
        CS32_SHDR(gotplt_sec).sh_size = ngotplt*sizeof(Elf32_Word);
        if (emu_mode==EMU_X86 || emu_mode==EMU_X64 || emu_mode==EMU_ARM)
            *(Elf32_Addr *)gotplt_sec->sslist->data = CS32_SHDR(dynamic_sec).sh_addr;
        CS32_SHDR(reldyn_sec).sh_link = dynsym_sec->shndx;
        CS32_SHDR(reldyn_sec).sh_size = nreldyn*sizeof(Elf32_Rel);
    }
    if (shared_object_files != NULL) {
        CS32_SHDR(dynsym_sec).sh_link = dynstr_sec->shndx;
        CS32_SHDR(dynsym_sec).sh_info = 1;
        CS32_SHDR(hash_sec).sh_link = dynsym_sec->shndx;
        CS32_SHDR(dynamic_sec).sh_link = dynstr_sec->shndx;
        define_local_symbol("_DYNAMIC", CS32_SHDR(dynamic_sec).sh_addr, ELF32_ST_INFO(STB_LOCAL, STT_OBJECT),
        dynamic_sec->shndx, ".dynamic");
    }
    if (emu_mode==EMU_ARM && plt_sec!=NULL)
        /* this is useful for when objdump'ing the .plt */
        define_local_symbol("$a", CS32_SHDR(plt_sec).sh_addr, ELF32_ST_INFO(STB_LOCAL, STT_NOTYPE),
        plt_sec->shndx, ".plt");

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
    if (shared_object_files != NULL) {
        phdr.p_type = PT_INTERP;
        phdr.p_offset = CS32_SHDR(interp_sec).sh_offset;
        phdr.p_vaddr = phdr.p_paddr = CS32_SHDR(interp_sec).sh_addr;
        phdr.p_filesz = phdr.p_memsz = CS32_SHDR(interp_sec).sh_size;
        phdr.p_flags = PF_R;
        phdr.p_align = 1;
        fwrite(&phdr, sizeof(Elf32_Phdr), 1, outf);
        curr += sizeof(Elf32_Phdr);
        ++ehdr.e_phnum;
    }
    if (ROSeg.nsec) {
        fwrite(&SEG32_PHDR(&ROSeg), sizeof(Elf32_Phdr), 1, outf);
        curr += sizeof(Elf32_Phdr);
        ++ehdr.e_phnum;
    }
    if (WRSeg.nsec) {
        fwrite(&SEG32_PHDR(&WRSeg), sizeof(Elf32_Phdr), 1, outf);
        curr += sizeof(Elf32_Phdr);
        ++ehdr.e_phnum;
    }
    if (shared_object_files != NULL) {
        /* Set .dynamic's elements */
        dp = (Elf32_Dyn *)dynamic_sec->sslist->data;
        for (so = shared_object_files; so != NULL; so = so->next, dp++) {
            dp->d_tag = DT_NEEDED;
            dp->d_un.d_val = strtab_get_offset(dynstr, so->name);
        }
        if (runpath_val != (Elf32_Word)-1) {
            dp->d_tag = DT_RUNPATH;
            dp->d_un.d_val = runpath_val;
            ++dp;
        }
        dp->d_tag = DT_HASH;
        dp->d_un.d_ptr = CS32_SHDR(hash_sec).sh_addr;
        ++dp;
        dp->d_tag = DT_STRTAB;
        dp->d_un.d_ptr = CS32_SHDR(dynstr_sec).sh_addr;
        ++dp;
        dp->d_tag = DT_SYMTAB;
        dp->d_un.d_ptr = CS32_SHDR(dynsym_sec).sh_addr;
        ++dp;
        dp->d_tag = DT_STRSZ;
        dp->d_un.d_val = CS32_SHDR(dynstr_sec).sh_size;
        ++dp;
        dp->d_tag = DT_SYMENT;
        dp->d_un.d_val = sizeof(Elf32_Sym);
        ++dp;
        if (plt_sec != NULL) {
            if (nplt > 0) {
                if (emu_mode == EMU_MIPS)
                    dp->d_tag = DT_MIPS_PLTGOT;
                else
                    dp->d_tag = DT_PLTGOT;
                dp->d_un.d_ptr = CS32_SHDR(gotplt_sec).sh_addr;
                ++dp;
                dp->d_tag = DT_PLTRELSZ;
                dp->d_un.d_val = CS32_SHDR(relplt_sec).sh_size;
                ++dp;
                dp->d_tag = DT_PLTREL;
                dp->d_un.d_val = DT_REL;
                ++dp;
                dp->d_tag = DT_JMPREL;
                dp->d_un.d_ptr = CS32_SHDR(relplt_sec).sh_addr;
                ++dp;
            } else {
                CS32_SHDR(dynamic_sec).sh_size -= sizeof(Elf32_Dyn)*4;
            }
            if (nreldyn > 0) {
                dp->d_tag = DT_REL;
                dp->d_un.d_ptr = CS32_SHDR(reldyn_sec).sh_addr;
                ++dp;
                dp->d_tag = DT_RELSZ;
                dp->d_un.d_val = CS32_SHDR(reldyn_sec).sh_size;
                ++dp;
                dp->d_tag = DT_RELENT;
                dp->d_un.d_val = sizeof(Elf32_Rel);
                ++dp;
            } else {
                CS32_SHDR(dynamic_sec).sh_size -= sizeof(Elf32_Dyn)*3;
            }
        }
        if (emu_mode == EMU_MIPS) {
            /* [!] without some of these the GNU MIPS dynamic linker directly crashes at startup */
            dp->d_tag = DT_MIPS_RLD_VERSION;
            dp->d_un.d_val = 1;
            ++dp;
            dp->d_tag = DT_MIPS_FLAGS;
            dp->d_un.d_val = RHF_NOTPOT;
            ++dp;
            dp->d_tag = DT_MIPS_BASE_ADDRESS;
            dp->d_un.d_ptr = 0x400000;
            ++dp;
            dp->d_tag = DT_PLTGOT;
            dp->d_un.d_ptr = CS32_SHDR(got_sec).sh_addr;
            ++dp;
            dp->d_tag = DT_MIPS_LOCAL_GOTNO;
            dp->d_un.d_val = 2;
            ++dp;
            dp->d_tag = DT_MIPS_SYMTABNO;
            dp->d_un.d_val = CS32_SHDR(dynsym_sec).sh_size/sizeof(Elf32_Sym);
            ++dp;
            dp->d_tag = DT_MIPS_GOTSYM;
            dp->d_un.d_val = CS32_SHDR(dynsym_sec).sh_size/sizeof(Elf32_Sym);
            ++dp;
        }
        dp->d_tag = DT_NULL;
        phdr.p_type = PT_DYNAMIC;
        phdr.p_offset = CS32_SHDR(dynamic_sec).sh_offset;
        phdr.p_vaddr = phdr.p_paddr = CS32_SHDR(dynamic_sec).sh_addr;
        phdr.p_filesz = phdr.p_memsz = CS32_SHDR(dynamic_sec).sh_size;
        phdr.p_flags = PF_R|PF_W;
        phdr.p_align = 4;
        fwrite(&phdr, sizeof(Elf32_Phdr), 1, outf);
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
        assert(curr == CS32_SHDR(ROSeg.secs[0]).sh_offset);
    for (i = 0; i < ROSeg.nsec; i++) {
        SmplSec *ssec;

        for (ssec = ROSeg.secs[i]->sslist; ssec != NULL; ssec = ssec->next) {
            fwrite(ssec->data, SS32_SHDR(ssec)->sh_size, 1, outf);
            curr += SS32_SHDR(ssec)->sh_size;
            ALIGN(4);
        }
        CS32_SHDR(ROSeg.secs[i]).sh_name = strtab_append(shstrtab, ROSeg.secs[i]->name);
        ++ehdr.e_shnum;
    }
    for (i = 0; i < WRSeg.nsec; i++) {
        SmplSec *ssec;

        if (CS32_SHDR(WRSeg.secs[i]).sh_type != SHT_NOBITS) {
            for (ssec = WRSeg.secs[i]->sslist; ssec != NULL; ssec = ssec->next) {
                fwrite(ssec->data, SS32_SHDR(ssec)->sh_size, 1, outf);
                curr += SS32_SHDR(ssec)->sh_size;
                ALIGN(4);
            }
        }
        CS32_SHDR(WRSeg.secs[i]).sh_name = strtab_append(shstrtab, WRSeg.secs[i]->name);
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
#if 0
        if (OF32_SYMTAB(obj) == NULL) /* [!] crtn.o doesn't have .symtab */
            continue;
#endif
        if (ELF32_ST_TYPE(OF32_SYMTAB(obj)[1].st_info) == STT_FILE)
            esym.st_name = strtab_append(strtab, &obj->strtab[OF32_SYMTAB(obj)[1].st_name]);
        else
            continue;
        WRITE_ST_ENT();
    }

    /* loadable sections */
    for (i = 0; i < ROSeg.nsec; i++) {
        memset(&esym, 0, sizeof(Elf32_Sym));
        esym.st_value = CS32_SHDR(ROSeg.secs[i]).sh_addr;
        esym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
        esym.st_shndx = ROSeg.secs[i]->shndx;
        WRITE_ST_ENT();
    }
    for (i = 0; i < WRSeg.nsec; i++) {
        memset(&esym, 0, sizeof(Elf32_Sym));
        esym.st_value = CS32_SHDR(WRSeg.secs[i]).sh_addr;
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
            esym.st_other = np->other;
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
        fwrite(&CS32_SHDR(ROSeg.secs[i]), sizeof(Elf32_Shdr), 1, outf);
    for (i = 0; i < WRSeg.nsec; i++)
        fwrite(&CS32_SHDR(WRSeg.secs[i]), sizeof(Elf32_Shdr), 1, outf);

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
    switch (emu_mode) {
    case EMU_X86:
        ehdr.e_machine = EM_386;
        break;
    case EMU_MIPS:
        ehdr.e_ident[EI_ABIVERSION] = 1;
        ehdr.e_machine = EM_MIPS;
        ehdr.e_flags = EF_MIPS_NOREORDER|E_MIPS_ABI_O32|EF_MIPS_CPIC;
        break;
    case EMU_ARM:
        ehdr.e_machine = EM_ARM;
        ehdr.e_flags = EF_ARM_EABI_VER5;
        break;
    }
    ehdr.e_version = EV_CURRENT;
    if ((sym=lookup_global_symbol(entry_symbol))==NULL || sym->shndx==SHN_UNDEF)
        /* TBD: lookup entry symbol between the *.so files too? */
        err("cannot find entry symbol `%s'", entry_symbol);
    ehdr.e_entry = sym->value;
    ehdr.e_ehsize = sizeof(Elf32_Ehdr);
    ehdr.e_phentsize = sizeof(Elf32_Phdr);
    ehdr.e_shentsize = sizeof(Elf32_Shdr);
    ehdr.e_shstrndx = 1;
    fwrite(&ehdr, sizeof(Elf32_Ehdr), 1, outf);
    fseek(outf, 0, SEEK_END);

    /*fclose(outf);*/
    strtab_destroy(strtab), strtab_destroy(shstrtab);

#undef ALIGN
}

void write_ELF_file_64(FILE *outf)
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
        CS64_SHDR(plt_sec).sh_size = nplt*PLT_ENTRY_NB;
        CS64_SHDR(relaplt_sec).sh_link = dynsym_sec->shndx;
        CS64_SHDR(relaplt_sec).sh_info = plt_sec->shndx;
        CS64_SHDR(relaplt_sec).sh_size = nrelaplt*sizeof(Elf64_Rela);
        CS64_SHDR(gotplt_sec).sh_size = ngotplt*sizeof(Elf64_Xword);
        *(Elf64_Addr *)gotplt_sec->sslist->data = CS64_SHDR(dynamic_sec).sh_addr;
        CS64_SHDR(reladyn_sec).sh_link = dynsym_sec->shndx;
        CS64_SHDR(reladyn_sec).sh_size = nreladyn*sizeof(Elf64_Rela);
    }
    if (shared_object_files != NULL) {
        CS64_SHDR(dynsym_sec).sh_link = dynstr_sec->shndx;
        CS64_SHDR(dynsym_sec).sh_info = 1;
        CS64_SHDR(hash_sec).sh_link = dynsym_sec->shndx;
        CS64_SHDR(dynamic_sec).sh_link = dynstr_sec->shndx;
        define_local_symbol("_DYNAMIC", CS64_SHDR(dynamic_sec).sh_addr, ELF64_ST_INFO(STB_LOCAL, STT_OBJECT),
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
        phdr.p_offset = CS64_SHDR(interp_sec).sh_offset;
        phdr.p_vaddr = phdr.p_paddr = CS64_SHDR(interp_sec).sh_addr;
        phdr.p_filesz = phdr.p_memsz = CS64_SHDR(interp_sec).sh_size;
        phdr.p_flags = PF_R;
        phdr.p_align = 1;
        fwrite(&phdr, sizeof(Elf64_Phdr), 1, outf);
        curr += sizeof(Elf64_Phdr);
        ++ehdr.e_phnum;
    }
    if (ROSeg.nsec) {
        fwrite(&SEG64_PHDR(&ROSeg), sizeof(Elf64_Phdr), 1, outf);
        curr += sizeof(Elf64_Phdr);
        ++ehdr.e_phnum;
    }
    if (WRSeg.nsec) {
        fwrite(&SEG64_PHDR(&WRSeg), sizeof(Elf64_Phdr), 1, outf);
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
        if (runpath_val != (Elf32_Word)-1) {
            dp->d_tag = DT_RUNPATH;
            dp->d_un.d_val = runpath_val;
            ++dp;
        }
        dp->d_tag = DT_HASH;
        dp->d_un.d_ptr = CS64_SHDR(hash_sec).sh_addr;
        ++dp;
        dp->d_tag = DT_STRTAB;
        dp->d_un.d_ptr = CS64_SHDR(dynstr_sec).sh_addr;
        ++dp;
        dp->d_tag = DT_SYMTAB;
        dp->d_un.d_ptr = CS64_SHDR(dynsym_sec).sh_addr;
        ++dp;
        dp->d_tag = DT_STRSZ;
        dp->d_un.d_val = CS64_SHDR(dynstr_sec).sh_size;
        ++dp;
        dp->d_tag = DT_SYMENT;
        dp->d_un.d_val = sizeof(Elf64_Sym);
        ++dp;
        if (plt_sec != NULL) {
            if (nplt > 0) {
                dp->d_tag = DT_PLTGOT;
                dp->d_un.d_ptr = CS64_SHDR(gotplt_sec).sh_addr;
                ++dp;
                dp->d_tag = DT_PLTRELSZ;
                dp->d_un.d_val = CS64_SHDR(relaplt_sec).sh_size;
                ++dp;
                dp->d_tag = DT_PLTREL;
                dp->d_un.d_val = DT_RELA;
                ++dp;
                dp->d_tag = DT_JMPREL;
                dp->d_un.d_ptr = CS64_SHDR(relaplt_sec).sh_addr;
                ++dp;
            } else {
                CS64_SHDR(dynamic_sec).sh_size -= sizeof(Elf64_Dyn)*4;
            }
            if (nreladyn > 0) {
                dp->d_tag = DT_RELA;
                dp->d_un.d_ptr = CS64_SHDR(reladyn_sec).sh_addr;
                ++dp;
                dp->d_tag = DT_RELASZ;
                dp->d_un.d_val = CS64_SHDR(reladyn_sec).sh_size;
                ++dp;
                dp->d_tag = DT_RELAENT;
                dp->d_un.d_val = sizeof(Elf64_Rela);
                ++dp;
            } else {
                CS64_SHDR(dynamic_sec).sh_size -= sizeof(Elf64_Dyn)*3;
            }
        }
        dp->d_tag = DT_NULL;
        phdr.p_type = PT_DYNAMIC;
        phdr.p_offset = CS64_SHDR(dynamic_sec).sh_offset;
        phdr.p_vaddr = phdr.p_paddr = CS64_SHDR(dynamic_sec).sh_addr;
        phdr.p_filesz = phdr.p_memsz = CS64_SHDR(dynamic_sec).sh_size;
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
        assert(curr == CS64_SHDR(ROSeg.secs[0]).sh_offset);
    for (i = 0; i < ROSeg.nsec; i++) {
        SmplSec *ssec;

        for (ssec = ROSeg.secs[i]->sslist; ssec != NULL; ssec = ssec->next) {
            fwrite(ssec->data, SS64_SHDR(ssec)->sh_size, 1, outf);
            curr += SS64_SHDR(ssec)->sh_size;
            ALIGN(4);
        }
        CS64_SHDR(ROSeg.secs[i]).sh_name = strtab_append(shstrtab, ROSeg.secs[i]->name);
        ++ehdr.e_shnum;
    }
    for (i = 0; i < WRSeg.nsec; i++) {
        SmplSec *ssec;

        if (CS64_SHDR(WRSeg.secs[i]).sh_type != SHT_NOBITS) {
            for (ssec = WRSeg.secs[i]->sslist; ssec != NULL; ssec = ssec->next) {
                fwrite(ssec->data, SS64_SHDR(ssec)->sh_size, 1, outf);
                curr += SS64_SHDR(ssec)->sh_size;
                ALIGN(4);
            }
        }
        CS64_SHDR(WRSeg.secs[i]).sh_name = strtab_append(shstrtab, WRSeg.secs[i]->name);
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
#if 0
        if (OF64_SYMTAB(obj) == NULL) /* [!] crtn.o doesn't have .symtab */
            continue;
#endif
        if (ELF64_ST_TYPE(OF64_SYMTAB(obj)[1].st_info) == STT_FILE)
            esym.st_name = strtab_append(strtab, &obj->strtab[OF64_SYMTAB(obj)[1].st_name]);
        else
            continue;
        WRITE_ST_ENT();
    }

    /* loadable sections */
    for (i = 0; i < ROSeg.nsec; i++) {
        memset(&esym, 0, sizeof(Elf64_Sym));
        esym.st_value = CS64_SHDR(ROSeg.secs[i]).sh_addr;
        esym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
        esym.st_shndx = ROSeg.secs[i]->shndx;
        WRITE_ST_ENT();
    }
    for (i = 0; i < WRSeg.nsec; i++) {
        memset(&esym, 0, sizeof(Elf64_Sym));
        esym.st_value = CS64_SHDR(WRSeg.secs[i]).sh_addr;
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
        fwrite(&CS64_SHDR(ROSeg.secs[i]), sizeof(Elf64_Shdr), 1, outf);
    for (i = 0; i < WRSeg.nsec; i++)
        fwrite(&CS64_SHDR(WRSeg.secs[i]), sizeof(Elf64_Shdr), 1, outf);

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

    /*fclose(outf);*/
    strtab_destroy(strtab), strtab_destroy(shstrtab);

#undef ALIGN
}
