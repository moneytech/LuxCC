/*
    !!!
        NOT USED ANYMORE. QEMU IS USED INSTEAD NOW.
    !!!
*/

/*
    Emulator for a subset of the MIPS32 ISA. It reads ELF static executables.

            Main memory organization

        +-----------------------------------+
        |       argv[]'s strings            |
        +-----------------------------------+
        |       argv[]                      |
        +-----------------------------------+ <- emu_argv
        |       Stack                       |
        |  (grows up to STACK_SIZE bytes)   |
        +-----------------------------------+ <- $29
        |         |                         |
        |         v                         |
        |                                   |
        |                                   |
        |                                   |
        +-----------------------------------+
        |       Heap                        |
        |  (fixed to HEAP_SIZE bytes)       |
        +-----------------------------------+
        |       Program segments            |
        +-----------------------------------+ BASE_ADDR
        |       ...                         |
        +-----------------------------------+ 0

    The program running in the emulator only handles virtual (fake) addresses (with one
    exception, see below). The emulator translates every address used to access memory
    from program's address space to emulator's address space.

    The exception to the above translation are FILE pointers. The program handles the real
    pointers as returned by fopen() and the real addresses of the streams stdout/stderr/stdin.

    Because of the above, a FILE pointer must not be dereferenced by the program or otherwise
    a translation from a real address to a real address will be performed and an incorrect
    memory address will be accessed.

    Instruction that generate an exception on overflow behave the same as the ones
    that don't.

    Almost no memory protection is implemented. If the program wants to, it can easily
    make the emulator crash.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <elf.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

typedef struct Header Header;

#define TRUE            1
#define FALSE           0
#define BASE_ADDR       0x00400000
#define STACK_SIZE      (8192*1024)
#define HEAP_SIZE       (sizeof(Header)*873812) /* ~ 10MiB */
#define NGPR            32
#define ADDR2OFFS(a)    ((a)-BASE_ADDR)
#define OPC(x)          (((x)>>26) & 0x3F)
#define RS(x)           (((x)>>21) & 0x1F)
#define RT(x)           (((x)>>16) & 0x1F)
#define RD(x)           (((x)>>11) & 0x1F)
#define SHAMT(x)        (((x)>>6)  & 0x1F)
#define FUNCT(x)        ((x) & 0x3F)
#define SIMM(x)         ((int16_t)(x))
#define UIMM(x)         ((uint16_t)(x))
#define ADDR(x)         ((x) & 0x3FFFFFF)

uint8_t *main_mem;
uint32_t PC;            /* $pc */
uint32_t R[NGPR];       /* $0..$31 */
union {
    struct {
        uint32_t lo;
        uint32_t hi;
    } h;
    uint64_t hilo;
} HI_LO;                /* $hi/$lo */
uint32_t HP;            /* $hp (heap pointer) */
int emu_argc;
char **emu_argv;
int *program_errno;
#define update_errno() (*program_errno = errno)

char *read_file(char *path)
{
    FILE *fp;
    char *buf;
    unsigned len;

    if ((fp=fopen(path, "rb")) == NULL)
        return NULL;
    fseek(fp, 0, SEEK_END);
    len = ftell(fp);
    rewind(fp);
    buf = malloc(len+1);
    len = fread(buf, 1, len, fp);
    buf[len] = '\0';
    fclose(fp);
    return buf;
}

struct Header {
    Header *next;
    unsigned used_size;
    unsigned free_size;
} *hptr;

void *virt2real(uint32_t addr)
{
    if (addr == 0)
        return NULL;
    return (void *)(main_mem+ADDR2OFFS(addr));
}

uint32_t real2virt(uint8_t *addr)
{
    if (addr == NULL)
        return 0;
    return BASE_ADDR+(addr-main_mem);
}

char *emu_name;

void die(char *msg)
{
    fprintf(stderr, "%s: %s\n", emu_name, msg);
    exit(EXIT_FAILURE);
}

void validate_ELF(Elf32_Ehdr *eh)
{
    if (eh->e_ident[EI_MAG0]!=ELFMAG0
    || eh->e_ident[EI_MAG1]!=ELFMAG1
    || eh->e_ident[EI_MAG2]!=ELFMAG2
    || eh->e_ident[EI_MAG3]!=ELFMAG3)
        die("bad magic number");
    if (eh->e_ident[EI_CLASS] != ELFCLASS32)
        die("bad file class");
    if (eh->e_ident[EI_DATA] != ELFDATA2LSB)
        die("bad encoding");
    if (eh->e_type != ET_EXEC)
        die("non-executable file");
    if (eh->e_machine != EM_MIPS)
        die("executable for unknown architecture");
}

void emu_init(void)
{
    int i;
    char *buf;
    Elf32_Ehdr *ehdr;
    Elf32_Shdr *sht, *sh;
    Elf32_Phdr *ph;
    unsigned segs_siz;
    unsigned argv_siz, argv_str_siz;
    char **p1, *p2;

    if ((buf=read_file(emu_argv[0])) == NULL) {
        fprintf(stderr, "%s: cannot read file `%s'\n", emu_name, emu_argv[0]);
        exit(EXIT_FAILURE);
    }
    ehdr = (Elf32_Ehdr *)buf;
    validate_ELF(ehdr);
    sht = (Elf32_Shdr *)(buf+ehdr->e_shoff);

    for (ph = (Elf32_Phdr *)(buf+ehdr->e_phoff), i = 0; i < ehdr->e_phnum; ph++, i++)
        if (ph->p_type == PT_LOAD)
            break;;

    /* expect 2 PT_LOAD segments */
    assert(i < ehdr->e_phnum-1);
    assert(ph[1].p_type == PT_LOAD);
    assert(ph[0].p_vaddr == BASE_ADDR);
    segs_siz = (ph[1].p_vaddr+ph[1].p_memsz-ph[0].p_vaddr);

    argv_siz = (emu_argc+1)*4;
    argv_str_siz = 0;
    for (i = 0; i < emu_argc; i++)
        argv_str_siz += strlen(emu_argv[i])+1;

    main_mem = malloc(argv_siz+argv_str_siz+STACK_SIZE+HEAP_SIZE+segs_siz);

    /* set $sp */
    R[29] = BASE_ADDR+segs_siz+HEAP_SIZE+STACK_SIZE;

    /* set heap pointer */
    hptr = (Header *)(main_mem+segs_siz);
    /* set first block (it is never freed) */
    hptr->next = hptr;
    hptr->used_size = 1;
    hptr->free_size = (HEAP_SIZE/sizeof(Header))-1;

    /* load sections */
    for (sh = sht+1, i = 1; i < ehdr->e_shnum; sh++, i++) {
        if (!(sh->sh_flags&SHF_ALLOC))
            ;
        else if (sh->sh_type == SHT_NOBITS)
            memset(main_mem+ADDR2OFFS(sh->sh_addr), 0, sh->sh_size);
        else
            memcpy(main_mem+ADDR2OFFS(sh->sh_addr), buf+sh->sh_offset, sh->sh_size);
    }

    /* set $pc */
    PC = ehdr->e_entry;

    free(buf);

    /* build emu's argv[] and strings */
    p1 = (char **)(main_mem+segs_siz+HEAP_SIZE+STACK_SIZE);
    p2 = (char *)p1+argv_siz;
    for (i = 0; i < emu_argc; i++) {
        p1[i] = (char *)real2virt((uint8_t *)p2);
        strcpy(p2, emu_argv[i]);
        p2 += strlen(emu_argv[i])+1;
    }
    p1[i] = NULL;
    emu_argv = (char **)(main_mem+segs_siz+HEAP_SIZE+STACK_SIZE);
}

void *do_malloc(unsigned size)
{
    Header *p, *newp;
    unsigned nunits;

    nunits = (size+sizeof(Header)-1)/sizeof(Header)+1;

    p = hptr;
    while (p->next!=hptr && p->free_size<nunits)
        p = p->next;
    if (p->free_size < nunits)
        return NULL; /* no block big enough */
    newp = p+p->used_size;
    newp->used_size = nunits;
    newp->free_size = p->free_size-nunits;
    newp->next = p->next;
    p->free_size = 0;
    p->next = newp;
    hptr = newp;
    return (void *)(newp+1);
}

void do_free(void *ap)
{
    Header *bp, *p, *prev;

    bp = (Header *)ap-1;
    prev = hptr;
    p = hptr->next;
    while (p!=bp && p!=hptr) {
        prev = p;
        p = p->next;
    }
    if (p != bp)
        return; /* corrupted list, do nothing */
    prev->free_size += p->used_size + p->free_size;
    prev->next = p->next;
    hptr = prev;
}

void *do_realloc(void *ap, unsigned size)
{
    Header *p;
    unsigned nunits, avail;

    if (ap == NULL) {
        return do_malloc(size);
    } else if (size == 0) {
        do_free(ap);
        return NULL;
    }

    nunits = (size+sizeof(Header)-1)/sizeof(Header)+1;
    p = (Header *)ap-1;
    avail = p->used_size+p->free_size;
    if (nunits <= avail) {
        p->used_size = nunits;
        p->free_size = avail-nunits;
        return ap;
    } else {
        void *newap;

        if ((newap=do_malloc(size)) != NULL) {
            unsigned oldsiz;

            oldsiz = p->used_size*sizeof(Header);
            memcpy(newap, ap, (size<oldsiz)?size:oldsiz);
            do_free(ap);
            return newap;
        }
    }
    return NULL;
}

void do_exit(int status)
{
    free(main_mem);
    exit(status);
}

void do_syscall(void)
{
    uint32_t *p;
    uint64_t r;

    switch (R[2]) {
    case 0: /* getvars */
        p = virt2real(R[4]);
        p[0] = (uint32_t)stdin;
        p[1] = (uint32_t)stdout;
        p[2] = (uint32_t)stderr;
        p[3] = emu_argc;
        p[4] = (uint32_t)real2virt((uint8_t *)emu_argv);
        program_errno = (int *)&p[5];
        break;
    case 1: /* malloc */
        R[2] = real2virt(do_malloc(R[4]));
        break;
    case 2: /* free */
        do_free(virt2real(R[4]));
        break;
    case 3: /* exit */
        do_exit(R[4]);
        break;
    case 4: /* realloc */
        R[2] = real2virt(do_realloc(virt2real(R[4]), R[5]));
        break;
    case 5: /* fputc */
        R[2] = fputc(R[4], (FILE *)R[5]);
        break;
    case 6: /* fgetc */
        R[2] = fgetc((FILE *)R[4]);
        break;
    case 7: /* fread */
        R[2] = fread(virt2real(R[4]), R[5], R[6], (FILE *)R[7]);
        break;
    case 8: /* fwrite */
        R[2] = fwrite(virt2real(R[4]), R[5], R[6], (FILE *)R[7]);
        break;
    case 9: /* ferror */
        R[2] = ferror((FILE *)R[4]);
        break;
    case 10: /* fopen */
        R[2] = (uint32_t)fopen(virt2real(R[4]), virt2real(R[5]));
        break;
    case 11: /* fclose */
        R[2] = fclose((FILE *)R[4]);
        break;
    case 12: /* fseek */
        R[2] = fseek((FILE *)R[4], R[5], R[6]);
        break;
    case 13: /* ftell */
        R[2] = ftell((FILE *)R[4]);
        break;
    case 14: /* rewind */
        rewind((FILE *)R[4]);
        break;
    case 15: /* fgets */
        R[2] = (uint32_t)fgets(virt2real(R[4]), R[5], (FILE *)R[6]);
        break;
    case 16: /* stat */
        R[2] = stat(virt2real(R[4]), virt2real(R[5]));
        break;
    case 17: /* fileno */
        R[2] = fileno((FILE *)R[4]);
        break;
    case 18: /* isatty */
        R[2] = isatty(R[4]);
        break;
    case 19: /* strtol */
        R[2] = strtol(virt2real(R[4]), virt2real(R[5]), R[6]);
        update_errno();
        break;
    case 20: /* strtoul */
        R[2] = strtoul(virt2real(R[4]), virt2real(R[5]), R[6]);
        update_errno();
        break;
    case 21: /* strtoll */
        r = strtoll(virt2real(R[4]), virt2real(R[5]), R[6]);
        R[2] = ((uint32_t *)&r)[0];
        R[3] = ((uint32_t *)&r)[1];
        update_errno();
        break;
    case 22: /* strtoull */
        r = strtoll(virt2real(R[4]), virt2real(R[5]), R[6]);
        R[2] = ((uint32_t *)&r)[0];
        R[3] = ((uint32_t *)&r)[1];
        update_errno();
        break;
    default:
        fprintf(stderr, "unknown syscall number `%u'\n", R[2]);
        do_exit(1);
    }
}

void exec_err(void)
{
    fprintf(stderr, "unable to execute instruction word `0x%08x' at 0x%08x\n", *(uint32_t *)virt2real(PC-4), PC-4);
    exit(EXIT_FAILURE);
}

void emu_exec(void)
{
    int in_ds;
    uint32_t instr, target;

    in_ds = FALSE;
nexti:
    R[0] = 0;
    instr = *(uint32_t *)virt2real(PC);
    PC += 4;
    switch (OPC(instr)) {
    case 0x00:
        if (instr == 0) /* nop */
            break;
        switch (FUNCT(instr)) {
        case 0x00: /* sll */
            R[RD(instr)] = R[RT(instr)]<<SHAMT(instr);
            break;
        case 0x02: /* srl */
            R[RD(instr)] = R[RT(instr)]>>SHAMT(instr);
            break;
        case 0x03: /* sra */
            R[RD(instr)] = (int32_t)R[RT(instr)]>>SHAMT(instr);
            break;
        case 0x04: /* sllv */
            R[RD(instr)] = R[RT(instr)]<<R[RS(instr)];
            break;
        case 0x06: /* srlv */
            R[RD(instr)] = R[RT(instr)]>>R[RS(instr)];
            break;
        case 0x07: /* srav */
            R[RD(instr)] = (int32_t)R[RT(instr)]>>R[RS(instr)];
            break;
        case 0x09: /* jalr */
            R[RD(instr)] = PC+4;
        case 0x08: /* jr */
            target = R[RS(instr)];
            in_ds = TRUE;
            goto nexti;
        case 0x0C: /* syscall */
            do_syscall();
            break;
        case 0x10: /* mfhi */
            R[RD(instr)] = HI_LO.h.hi;
            break;
        case 0x12: /* mflo */
            R[RD(instr)] = HI_LO.h.lo;
            break;
        case 0x11: /* mthi */
            HI_LO.h.hi = R[RS(instr)];
            break;
        case 0x13: /* mtlo */
            HI_LO.h.lo = R[RS(instr)];
            break;
        case 0x18: /* mult */
            HI_LO.hilo = (int64_t)(int32_t)R[RS(instr)]*(int64_t)(int32_t)R[RT(instr)];
            break;
        case 0x19: /* multu */
            HI_LO.hilo = (uint64_t)R[RS(instr)]*(uint64_t)R[RT(instr)];
            break;
        case 0x1A: /* div */
            HI_LO.h.lo = (int32_t)R[RS(instr)]/(int32_t)R[RT(instr)];
            HI_LO.h.hi = (int32_t)R[RS(instr)]%(int32_t)R[RT(instr)];
            break;
        case 0x1B: /* divu */
            HI_LO.h.lo = R[RS(instr)]/R[RT(instr)];
            HI_LO.h.hi = R[RS(instr)]%R[RT(instr)];
            break;
        case 0x20: /* add */
        case 0x21: /* addu */
            R[RD(instr)] = R[RS(instr)]+R[RT(instr)];
            break;
        case 0x22: /* sub */
        case 0x23: /* subu */
            R[RD(instr)] = R[RS(instr)]-R[RT(instr)];
            break;
        case 0x24: /* and */
            R[RD(instr)] = R[RS(instr)]&R[RT(instr)];
            break;
        case 0x25: /* or */
            R[RD(instr)] = R[RS(instr)]|R[RT(instr)];
            break;
        case 0x26: /* xor */
            R[RD(instr)] = R[RS(instr)]^R[RT(instr)];
            break;
        case 0x27: /* nor */
            R[RD(instr)] = ~(R[RS(instr)]|R[RT(instr)]);
            break;
        case 0x2A: /* slt */
            R[RD(instr)] = (int32_t)R[RS(instr)]<(int32_t)R[RT(instr)];
            break;
        case 0x2B: /* sltu */
            R[RD(instr)] = R[RS(instr)]<R[RT(instr)];
            break;
        default:
            exec_err();
        }
        break;

    case 0x01:
        switch (RT(instr)) {
        case 0x10: /* bltzal */
            R[31] = PC+4;
        case 0x00: /* bltz */
            if ((int32_t)R[RS(instr)] < 0) {
                target = PC+(SIMM(instr)<<2);
                in_ds = TRUE;
            }
            goto nexti;
        case 0x11: /* bgezal */
            R[31] = PC+4;
        case 0x01: /* bgez */
            if ((int32_t)R[RS(instr)] >= 0) {
                target = PC+(SIMM(instr)<<2);
                in_ds = TRUE;
            }
            goto nexti;
        default:
            exec_err();
        }
        break;

    case 0x03: /* jal */
        R[31] = PC+4;
    case 0x02: /* j */
        target = (PC&0xF0000000) | (ADDR(instr)<<2);
        in_ds = TRUE;
        goto nexti;
    case 0x04: /* beq */
        if (R[RS(instr)] == R[RT(instr)]) {
            target = PC+(SIMM(instr)<<2);
            in_ds = TRUE;
        }
        goto nexti;
    case 0x05: /* bne */
        if (R[RS(instr)] != R[RT(instr)]) {
            target = PC+(SIMM(instr)<<2);
            in_ds = TRUE;
        }
        goto nexti;
    case 0x06: /* blez */
        if ((int32_t)R[RS(instr)] <= 0) {
            target = PC+(SIMM(instr)<<2);
            in_ds = TRUE;
        }
        goto nexti;
    case 0x07: /* bgtz */
        if ((int32_t)R[RS(instr)] > 0) {
            target = PC+(SIMM(instr)<<2);
            in_ds = TRUE;
        }
        goto nexti;
    case 0x08: /* addi */
    case 0x09: /* addiu */
        R[RT(instr)] = R[RS(instr)]+SIMM(instr);
        break;
    case 0x0A: /* slti */
        R[RT(instr)] = (int32_t)R[RS(instr)]<SIMM(instr);
        break;
    case 0x0B: /* sltiu */
        R[RT(instr)] = R[RS(instr)]<SIMM(instr);
        break;
    case 0x0C: /* andi */
        R[RT(instr)] = R[RS(instr)]&UIMM(instr);
        break;
    case 0x0D: /* ori */
        R[RT(instr)] = R[RS(instr)]|UIMM(instr);
        break;
    case 0x0E: /* xori */
        R[RT(instr)] = R[RS(instr)]^UIMM(instr);
        break;
    case 0x0F: /* lui */
        R[RT(instr)] = UIMM(instr)<<16;
        break;
    case 0x20: /* lb */
        R[RT(instr)] = *(int8_t *)virt2real(R[RS(instr)]+SIMM(instr));
        break;
    case 0x21: /* lh */
        R[RT(instr)] = *(int16_t *)virt2real(R[RS(instr)]+SIMM(instr));
        break;
    case 0x23: /* lw */
        R[RT(instr)] = *(uint32_t *)virt2real(R[RS(instr)]+SIMM(instr));
        break;
    case 0x24: /* lbu */
        R[RT(instr)] = *(uint8_t *)virt2real(R[RS(instr)]+SIMM(instr));
        break;
    case 0x25: /* lhu */
        R[RT(instr)] = *(uint16_t *)virt2real(R[RS(instr)]+SIMM(instr));
        break;
    case 0x28: /* sb */
        *(uint8_t *)virt2real(R[RS(instr)]+SIMM(instr)) = (uint8_t)R[RT(instr)];
        break;
    case 0x29: /* sh */
        *(uint16_t *)virt2real(R[RS(instr)]+SIMM(instr)) = (uint16_t)R[RT(instr)];
        break;
    case 0x2B: /* sw */
        *(uint32_t *)virt2real(R[RS(instr)]+SIMM(instr)) = R[RT(instr)];
        break;

    case 0x1C:
        if (FUNCT(instr) == 0x02) /* mul */
            R[RD(instr)] = R[RS(instr)]*R[RT(instr)];
        else
            exec_err();
        break;
    case 0x1F:
        if (FUNCT(instr) == 0x20) {
            switch (SHAMT(instr)) {
            case 0x10: /* seb */
                R[RD(instr)] = (int8_t)R[RT(instr)];
                break;
            case 0x18: /* seh */
                R[RD(instr)] = (int16_t)R[RT(instr)];
                break;
            default:
                exec_err();
            }
        } else {
            exec_err();
        }
        break;

    default:
        exec_err();
    }
    if (in_ds) {
        in_ds = FALSE;
        PC = target;
    }
    goto nexti;
}

int main(int argc, char *argv[])
{
    emu_name = argv[0];
    if (argc == 1) {
        fprintf(stderr, "usage: %s <mips-executable> <args>\n", emu_name);
        exit(EXIT_SUCCESS);
    }
    emu_argc = argc-1;
    emu_argv = argv+1;
    emu_init();
    emu_exec();

    return 0;
}
