#ifndef COPY_H_
#define COPY_H_

#include "luxld.h"

Elf32_Addr new_copy_reloc_32(char *symname, Elf32_Sym *syment);
Elf64_Addr new_copy_reloc_64(char *symname, Elf64_Sym *syment);

#endif
