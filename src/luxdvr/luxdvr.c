/*
    Lux Driver: invokes the core compiler, the assembler, and the linker.

    This should be the main user interface to the compiler.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <assert.h>
#include <unistd.h>
#include <ctype.h>
#include "../util/util.h"
#include "../util/str.h"

typedef struct File File;

char helpstr[] =
    "\nGeneral options:\n"
    "  -m<mach>         Target machine <mach>\n"
    "  -o<file>         Write output to <file>\n"
    "  -E               Preprocess only\n"
    "  -S               Compile but do not assemble\n"
    "  -c               Compile and assemble but do not link\n"
    "  -use-musl        Link against musl standard library\n"
    "  -use-glibc       Link against glibc standard library\n"
    "  -static          Link against a static version of libc\n"
    "  -v               Show invoked commands\n"
    "  -h               Print this help\n"
    "\nCompiler options:\n"
    "  -q               Disable all warnings\n"
    "  -I<dir>          Add <dir> to the list of directories searched for #include <...>\n"
    "  -i<dir>          Add <dir> to the list of directories searched for #include \"...\"\n"
    "  -analyze         Perform static analysis only\n"
    "  -show-stats      Show compilation stats\n"
    "  -D<name>         Predefine <name> as a macro, with definition 1\n"
    "  -uncolored       Print uncolored diagnostics\n"
    "  -dump-tokens     Dump program tokens\n"
    "  -dump-ast        Dump program AST\n"
    "  -dump-ic<func>   Dump intermediate code for function <func>\n"
    "  -dump-cfg<func>  Dump CFG for function <func>\n"
    "  -dump-cg         Dump program call-graph\n"
    "  -verbose-asm     Comment the generated assembly to make it more readable\n"
    "\nLinker options:\n"
    "  -Xe<sym>         Set <sym> as the entry point symbol\n"
    "  -Xl<name>        Link against object file/library <name>\n"
    "  -XL<dir>         Add <dir> to the list of directories searched for the -l options\n"
    "  -XI<interp>      Set <interp> as the name of the dynamic linker\n"
    "  -Xr<path>        Add <path> to the DT_RUNPATH dynamic array tag\n"
;

int verbose;
char *prog_name;

enum {
    DVR_HELP            = 0x00001,
    DVR_PREP_ONLY       = 0x00002,
    DVR_COMP_ONLY       = 0x00004,
    DVR_NOLINK          = 0x00008,
    DVR_ANALYZE_ONLY    = 0x00010,
    DVR_GLIBC           = 0x00020,
    DVR_MUSL            = 0x00040,
    DVR_STATIC          = 0x00080,
    DVR_VM32_TARGET     = 0x01000,
    DVR_VM64_TARGET     = 0x02000,
    DVR_X86_TARGET      = 0x04000,
    DVR_X64_TARGET      = 0x08000,
    DVR_MIPS_TARGET     = 0x10000,
    DVR_ARM_TARGET      = 0x20000,
};
#define DVR_TARGETS (DVR_VM32_TARGET+DVR_VM64_TARGET+DVR_X86_TARGET+DVR_X64_TARGET+DVR_MIPS_TARGET+DVR_ARM_TARGET)

typedef enum {
    ASM_Kind,
    C_Kind,
    OTHER_Kind,
} FileKind;

struct File {
    char *path;
    FileKind kind;
    File *next;
};

struct {
    char *name;
    File *dirs;
} required_files[64];
int nreq;

int ncfls, nasmfls, notherfls;

File *new_file(char *path, FileKind kind)
{
    File *newf;

    newf = malloc(sizeof(File));
    newf->path = path;
    newf->kind = kind;
    newf->next = NULL;
    switch (kind) {
    case C_Kind:    ++ncfls; break;
    case ASM_Kind:  ++nasmfls; break;
    case OTHER_Kind:++notherfls; break;
    }
    return newf;
}

File *insert_front(File *flist, File *newf)
{
    newf->next = flist;
    return newf;
}

File *insert_end(File *flist, File *newf)
{
    File *p;

    if (flist == NULL)
        return newf;
    for (p = flist; p->next != NULL; p = p->next)
        ;
    p->next = newf;
    return flist;
}

/* insert newf into flist at position n (the first node is at position 1) */
File *insert_at(File *flist, File *newf, int n)
{
    File *p;

    assert(n >= 1);
    if (--n == 0)
        return insert_front(flist, newf);
    for (p = flist; --n>0 && p->next!=NULL; p = p->next)
        ;
    newf->next = p->next;
    p->next = newf;
    return flist;
}

char *strbuf(String *s)
{
    char *buf;
    unsigned tmp;

    tmp = string_get_pos(s);
    string_set_pos(s, 0);
    buf = string_curr(s);
    string_set_pos(s, tmp);
    return buf;
}

int exec_cmd(String *cmd)
{
    int status;
    char *buf;

    buf = strbuf(cmd);
    if (verbose)
        printf("%s\n", buf);

    if ((status=system(buf)) == -1)
        TERMINATE("%s: error: cannot execute `%s'", prog_name, buf);
    if (!WIFEXITED(status))
        exit(1);
    return WEXITSTATUS(status);
}

int is_in_path(char *exe)
{
    char cmd[64];

    sprintf(cmd, "which %s >/dev/null", exe);
    return (WEXITSTATUS(system(cmd)) == 0);
}

void delete_file(char *path)
{
    if (verbose)
        printf("deleting `%s'...\n", path);
    unlink(path);
}

/*
 * Syntax of a .conf file:
 *
 *   conf_file = file { file } eof
 *   file = name ":" dir { "," dir } new_line
 *
 * Empty lines and lines starting with a '#' are ignored.
 */
void parse_conf_file(char *cf)
{
    FILE *fp;
    char buf[BUFSIZ], *cp;

#define SKIPWHITE() while (isblank(*cp)) ++cp

    sprintf(buf, "src/luxdvr/%s", cf);
    if (file_exists(buf)) {
        fp = fopen(buf, "rb");
    } else {
        sprintf(buf, "/usr/local/lib/luxcc/%s", cf);
        if (file_exists(buf))
            fp = fopen(buf, "rb");
        else
            TERMINATE("%s: cannot find `%s'", prog_name, cf);
    }

    cp = buf;
    while (fgets(cp, 0x7FFFFFFF, fp) != NULL) {
        int n;
        char buf2[256], *cp2;
        File *pl;

        if (strlen(cp)<=1 || cp[0]=='#')
            continue;
        cp2 = strchr(cp, ':');
        n = cp2-cp;
        strncpy(buf2, cp, n);
        buf2[n] = '\0';
        required_files[nreq].name = strdup(buf2);
        cp += n+1;

        for (;;) {
            int done = FALSE;

            SKIPWHITE();
            if ((cp2=strchr(cp, ',')) == NULL) {
                cp2 = strchr(cp, '\n');
                assert(cp2 != NULL);
                done = TRUE;
            }
            n = cp2-cp;
            strncpy(buf2, cp, n);
            buf2[n] = '\0';
            pl = malloc(sizeof(File));
            pl->path = strdup(buf2);
            pl->next = NULL;
            if (required_files[nreq].dirs == NULL) {
                required_files[nreq].dirs = pl;
            } else {
                File *t;

                for (t = required_files[nreq].dirs; t->next != NULL; t = t->next)
                    ;
                t->next = pl;
            }
            cp += n+1;
            if (done)
                break;
        }

        ++nreq;
    }
    fclose(fp);
}

char *search_required(char *file, int use_PATH)
{
    int i;
    static char path[256];

    for (i = 0; i < nreq; i++) {
        File *pl;

        if (!equal(required_files[i].name, file))
            continue;
        for (pl = required_files[i].dirs; pl != NULL; pl = pl->next) {
            sprintf(path, "%s/%s", pl->path, file);
            if (file_exists(path))
                return path;
        }
        /*break;*/
    }

    if (use_PATH && is_in_path(file))
        return file;

    TERMINATE("%s: cannot find `%s'", prog_name, file);
}

void usage(int more_inf)
{
    printf("USAGE: %s [ OPTIONS ] <file> ...\n", prog_name);
    if (more_inf)
        printf("type `%s -h' to see command-line options\n", prog_name);
}

void unknown_opt(char *opt)
{
    TERMINATE("%s: unknown option `%s'", prog_name, opt);
}

void missing_arg(char *opt)
{
    TERMINATE("%s: option `%s' requires an argument", prog_name, opt);
}

int main(int argc, char *argv[])
{
    int i, exst;
    unsigned driver_flags;
    char *outpath, *alt_asm_tmp, *chp;
    String *cc_cmd, *as_cmd, *ld_cmd;
    File *infiles;
    char asm_tmp[] = "/tmp/luxXXXXXX.s";
    char obj_tmp[] = "/tmp/luxXXXXXX.o";

    prog_name = argv[0];
    if (argc == 1) {
        usage(TRUE);
        exit(0);
    }

    infiles = NULL;
#ifdef __LP64__
    driver_flags = DVR_X64_TARGET;
#else
    driver_flags = DVR_X86_TARGET;
#endif
    outpath = alt_asm_tmp = NULL;
    cc_cmd = string_new(32); string_printf(cc_cmd, "");
    as_cmd = string_new(32); string_printf(as_cmd, "");
    ld_cmd = string_new(32); string_printf(ld_cmd, "");

    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '-') {
            if ((chp=strrchr(argv[i], '.')) == NULL)
                infiles = insert_end(infiles, new_file(argv[i], OTHER_Kind));
            else if (equal(chp, ".c"))
                infiles = insert_end(infiles, new_file(argv[i], C_Kind));
            else if (equal(chp, ".asm") || equal(chp, ".s"))
                infiles = insert_end(infiles, new_file(argv[i], ASM_Kind));
            else
                infiles = insert_end(infiles, new_file(argv[i], OTHER_Kind));
        } else {
            switch (argv[i][1]) {
            /*
             Driver-to-Compiler options mapping
                E           -> p
                analyze     -> a
                show-stats  -> s
                uncolored   -> u
                dump-tokens -> T
                dump-ast    -> A
                dump-cfg    -> G
                dump-cg     -> C
                dump-ic     -> N
                verbose-asm -> v
             The rest of the options are equal to both.
            */
            case 'a':
                if (equal(argv[i], "-analyze")) {
                    string_printf(cc_cmd, " -a");
                    driver_flags |= DVR_ANALYZE_ONLY;
                } else if (strncmp(argv[i], "-alt-asm-tmp", 12) == 0) {
                    /*
                     * This option is only used when self-compiling in targets
                     * that use ELF as the object file format.
                     * It is necessary because otherwise the temporary asm file
                     * name that will be embedded into the object files will be
                     * different from one invocation to the other and the comparison
                     * of binaries will fail.
                     */
                    if (argv[i][12] == '\0') {
                        if (argv[i+1] == NULL)
                            missing_arg(argv[i]);
                        alt_asm_tmp = argv[++i];
                    } else {
                        alt_asm_tmp = argv[i]+12;
                    }
                } else {
                    unknown_opt(argv[i]);
                }
                break;
            case 'c':
                driver_flags |= DVR_NOLINK;
                break;
            case 'd':
                if (equal(argv[i], "-dump-tokens")) {
                    string_printf(cc_cmd, " -T");
                } else if (equal(argv[i], "-dump-ast")) {
                    string_printf(cc_cmd, " -A");
                } else if (equal(argv[i], "-dump-cg")) {
                    string_printf(cc_cmd, " -C");
                } else if (strncmp(argv[i], "-dump-cfg", 9) == 0) {
                    string_printf(cc_cmd, " -G");
                    if (argv[i][9] == '\0') {
                        if (argv[i+1] == NULL)
                            missing_arg(argv[i]);
                        string_printf(cc_cmd, " %s", argv[++i]);
                    } else {
                        string_printf(cc_cmd, " %s", argv[i]+9);
                    }
                } else if (strncmp(argv[i], "-dump-ic", 8) == 0) {
                    string_printf(cc_cmd, " -N");
                    if (argv[i][8] == '\0') {
                        if (argv[i+1] == NULL)
                            missing_arg(argv[i]);
                        string_printf(cc_cmd, " %s", argv[++i]);
                    } else {
                        string_printf(cc_cmd, " %s", argv[i]+8);
                    }
                } else {
                    unknown_opt(argv[i]);
                }
                break;
            case 'D':
                string_printf(cc_cmd, " %s", argv[i]);
                if (argv[i][2] == '\0') {
                    if (argv[i+1] == NULL)
                        missing_arg(argv[i]);
                    string_printf(cc_cmd, " %s", argv[++i]);
                }
                break;
            case 'E':
                driver_flags |= DVR_PREP_ONLY;
                string_printf(cc_cmd, " -p");
                break;
            case 'X':
                switch (argv[i][2]) {
                case 'e':
                case 'L':
                case 'l':
                case 'I':
                case 'r':
                    string_printf(ld_cmd, " -%s", argv[i]+2);
                    if (argv[i][3] == '\0') {
                        if (argv[i+1] == NULL)
                            missing_arg(argv[i]);
                        string_printf(ld_cmd, " %s", argv[++i]);
                    }
                    break;
                }
                break;
            case 'h':
                driver_flags |= DVR_HELP;
                break;
            case 'I':
            case 'i':
                string_printf(cc_cmd, " %s", argv[i]);
                if (argv[i][2] == '\0') {
                    if (argv[i+1] == NULL)
                        missing_arg(argv[i]);
                    string_printf(cc_cmd, " %s", argv[++i]);
                }
                break;
            case 'm': {
                char *m;

                string_printf(cc_cmd, " %s", argv[i]);
                if (argv[i][2] == '\0') {
                    if (argv[i+1] == NULL)
                        missing_arg(argv[i]);
                    string_printf(cc_cmd, " %s", argv[++i]);
                    m = argv[i-1];
                } else {
                    m = argv[i]+2;
                }
                if (equal(m, "x86")) {
                    driver_flags &= ~DVR_TARGETS;
                    driver_flags |= DVR_X86_TARGET;
                } else if (equal(m, "x64")) {
                    driver_flags &= ~DVR_TARGETS;
                    driver_flags |= DVR_X64_TARGET;
                } else if (equal(m, "vm32")) {
                    driver_flags &= ~DVR_TARGETS;
                    driver_flags |= DVR_VM32_TARGET;
                } else if (equal(m, "vm64")) {
                    driver_flags &= ~DVR_TARGETS;
                    driver_flags |= DVR_VM64_TARGET;
                } else if (equal(m, "mips")) {
                    driver_flags &= ~DVR_TARGETS;
                    driver_flags |= DVR_MIPS_TARGET;
                } else if (equal(m, "arm")) {
                    driver_flags &= ~DVR_TARGETS;
                    driver_flags |= DVR_ARM_TARGET;
                }
            }
                break;
            case 'o':
                outpath = (argv[i][2]!='\0') ? argv[i]+2 : argv[++i];
                if (outpath == NULL)
                    missing_arg("-o");
                break;
            case 'q':
                string_printf(cc_cmd, " %s", argv[i]);
                break;
            case 'S':
                driver_flags |= DVR_COMP_ONLY;
                break;
            case 's':
                if (equal(argv[i], "-show-stats"))
                    string_printf(cc_cmd, " -s");
                else if (equal(argv[i], "-static"))
                    driver_flags |= DVR_STATIC;
                else
                    unknown_opt(argv[i]);
                break;
            case 'u':
                if (equal(argv[i], "-uncolored")) {
                    string_printf(cc_cmd, " -u");
                } else if (equal(argv[i], "-use-musl")) {
                    driver_flags &= ~DVR_GLIBC;
                    driver_flags |= DVR_MUSL;
                } else if (equal(argv[i], "-use-glibc")) {
                    driver_flags &= ~DVR_MUSL;
                    driver_flags |= DVR_GLIBC;
                } else {
                    unknown_opt(argv[i]);
                }
                break;
            case 'v':
                if (equal(argv[i], "-v"))
                    verbose = TRUE;
                else if (equal(argv[i], "-verbose-asm"))
                    string_printf(cc_cmd, " -v");
                else
                    unknown_opt(argv[i]);
                break;
            case '\0': /* stray '-' */
                break;
            default:
                unknown_opt(argv[i]);
                break;
            }
        }
    }
    if (outpath!=NULL && (driver_flags & (DVR_PREP_ONLY|DVR_COMP_ONLY|DVR_NOLINK))) {
        if (nasmfls != 0) {
            if (nasmfls>1 || ncfls!=0)
                goto err_1;
        } else if (ncfls > 1) {
            goto err_1;
        }
        goto ok_1;
err_1:
        fprintf(stderr, "%s: error: -o cannot be used with -E, -S, -c and multiple input files\n", prog_name);
        goto done;
    }
ok_1:
    exst = 0;
    if (driver_flags & DVR_HELP) {
        usage(FALSE);
        printf("%s", helpstr);
        if (verbose)
            printf("\nCurrent valid arguments for -m:\n"
                   "  vm32\n"
                   "  vm64\n"
                   "  x86\n"
                   "  x64\n"
                   "  mips\n"
                   "  arm\n");
        else
            printf("\nFor a list of valid arguments to -m, use -h -v.\n");
        goto done;
    }
    if (ncfls==0 && nasmfls==0 && notherfls==0) {
        fprintf(stderr, "%s: error: no input files\n", prog_name);
        exst = 1;
        goto done;
    }
    if (driver_flags & DVR_VM32_TARGET) {
        /* ==================================================================== */
        /*      VM32 Target                                                     */
        /* ==================================================================== */
        parse_conf_file("vm32.conf");
        string_printf(as_cmd, "%s -vm32", search_required("luxasvm", TRUE));
        string_clear(ld_cmd);
        string_printf(ld_cmd, "%s -vm32", search_required("luxldvm", TRUE));
        infiles = insert_front(infiles, new_file(strdup(search_required("crt0.o", FALSE)), OTHER_Kind));
        infiles = insert_end(infiles, new_file(strdup(search_required("libc.o", FALSE)), OTHER_Kind));
    } else if (driver_flags & DVR_VM64_TARGET) {
        /* ==================================================================== */
        /*      VM64 Target                                                     */
        /* ==================================================================== */
        parse_conf_file("vm64.conf");
        string_printf(as_cmd, "%s -vm64", search_required("luxasvm", TRUE));
        string_clear(ld_cmd);
        string_printf(ld_cmd, "%s -vm64", search_required("luxldvm", TRUE));
        infiles = insert_front(infiles, new_file(strdup(search_required("crt0.o", FALSE)), OTHER_Kind));
        infiles = insert_end(infiles, new_file(strdup(search_required("libc.o", FALSE)), OTHER_Kind));
    } else if (driver_flags & DVR_MIPS_TARGET) {
        /* ==================================================================== */
        /*      MIPS Target                                                     */
        /* ==================================================================== */
        chp = strdup(strbuf(ld_cmd));
        string_clear(ld_cmd);
        if (driver_flags & DVR_GLIBC) {
            parse_conf_file("glibc_mips.conf");
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -static", search_required("mipsel-linux-gnu-gcc", TRUE));
            } else {
                string_printf(ld_cmd, "%s -melf32ltsmip -I/usr/mipsel-linux-gnu/lib/ld.so.1 %s", search_required("mipsel-linux-gnu-ld", TRUE), chp);
                infiles = insert_front(infiles, new_file(strdup(search_required("crti.o", FALSE)), OTHER_Kind));
                infiles = insert_front(infiles, new_file(strdup(search_required("crt1.o", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so.6", FALSE)), OTHER_Kind));
                /*infiles = insert_end(infiles, new_file(strdup(search_required("ld.so.1", FALSE)), OTHER_Kind));*/
                infiles = insert_end(infiles, new_file(strdup(search_required("crtn.o", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libc_nonshared.a", FALSE)), OTHER_Kind));
            }
        } else if (driver_flags & DVR_MUSL) {
            parse_conf_file("musl_mips.conf");
            infiles = insert_front(infiles, new_file(strdup(search_required("crti.o", FALSE)), OTHER_Kind));
            infiles = insert_front(infiles, new_file(strdup(search_required("crt1.o", FALSE)), OTHER_Kind));
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -melf32ltsmip %s", search_required("mipsel-linux-gnu-ld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.a", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libgcc.a", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("crtn.o", FALSE)), OTHER_Kind));
            } else {
                string_printf(ld_cmd, "%s -melf32ltsmip -I/usr/mipsel-linux-gnu/lib/ld.so.1 %s", search_required("mipsel-linux-gnu-ld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so", FALSE)), OTHER_Kind));
            }
        } else {
            parse_conf_file("mips.conf");
            infiles = insert_front(infiles, new_file(strdup(search_required("crt0.o", FALSE)), OTHER_Kind));
            if (driver_flags & DVR_STATIC) {
                /*string_printf(ld_cmd, "%s -melf32ltsmip %s", search_required("mipsel-linux-gnu-ld", TRUE), chp);*/
                string_printf(ld_cmd, "%s -melf_mipsel %s", search_required("luxld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.a", FALSE)), OTHER_Kind));
            } else {
                /*string_printf(ld_cmd, "%s -melf32ltsmip -I/usr/mipsel-linux-gnu/lib/ld.so.1 %s", search_required("mipsel-linux-gnu-ld", TRUE), chp);*/
                string_printf(ld_cmd, "%s -melf_mipsel -I/usr/mipsel-linux-gnu/lib/ld.so.1 %s", search_required("luxld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so", FALSE)), OTHER_Kind));
            }
        }
        infiles = insert_front(infiles, new_file(strdup(search_required("luxmemcpy.o", FALSE)), OTHER_Kind));
        infiles = insert_front(infiles, new_file(strdup(search_required("liblux.o", FALSE)), OTHER_Kind));
        string_printf(as_cmd, "%s", search_required("luxasmips", TRUE));
        free(chp);
    } else if (driver_flags & DVR_ARM_TARGET) {
        /* ==================================================================== */
        /*      ARM Target                                                      */
        /* ==================================================================== */
        chp = strdup(strbuf(ld_cmd));
        string_clear(ld_cmd);
        if (driver_flags & DVR_GLIBC) {
            parse_conf_file("glibc_arm.conf");
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -static", search_required("arm-linux-gnueabi-gcc", TRUE));
            } else {
                string_printf(ld_cmd, "%s -marmelf_linux_eabi -I/usr/arm-linux-gnueabi/lib/ld-linux.so.3 %s",
                search_required("arm-linux-gnueabi-ld", TRUE), chp);
                infiles = insert_front(infiles, new_file(strdup(search_required("crti.o", FALSE)), OTHER_Kind));
                infiles = insert_front(infiles, new_file(strdup(search_required("crt1.o", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so.6", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("crtn.o", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libc_nonshared.a", FALSE)), OTHER_Kind));
            }
        } else if (driver_flags & DVR_MUSL) {
            parse_conf_file("musl_arm.conf");
            infiles = insert_front(infiles, new_file(strdup(search_required("crti.o", FALSE)), OTHER_Kind));
            infiles = insert_front(infiles, new_file(strdup(search_required("crt1.o", FALSE)), OTHER_Kind));
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -marmelf_linux_eabi %s", search_required("arm-linux-gnueabi-ld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.a", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libgcc.a", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libgcc_eh.a", FALSE)), OTHER_Kind));
                /* libgcc uses libc functions */
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.a", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("crtn.o", FALSE)), OTHER_Kind));
            } else {
                string_printf(ld_cmd, "%s -marmelf_linux_eabi -I/usr/arm-linux-gnueabi/lib/ld-linux.so.3 %s", search_required("arm-linux-gnueabi-ld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so", FALSE)), OTHER_Kind));
            }
        } else {
            parse_conf_file("arm.conf");
            infiles = insert_front(infiles, new_file(strdup(search_required("crt0.o", FALSE)), OTHER_Kind));
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -melf_armel %s", search_required("luxld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.a", FALSE)), OTHER_Kind));
            } else {
                /*string_printf(ld_cmd, "%s -marmelf_linux_eabi -I/usr/arm-linux-gnueabi/lib/ld-linux.so.3 %s", search_required("arm-linux-gnueabi-ld", TRUE), chp);*/
                string_printf(ld_cmd, "%s -melf_armel -I/usr/arm-linux-gnueabi/lib/ld-linux.so.3 %s", search_required("luxld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so", FALSE)), OTHER_Kind));
            }
        }
        infiles = insert_front(infiles, new_file(strdup(search_required("luxmemcpy.o", FALSE)), OTHER_Kind));
        infiles = insert_front(infiles, new_file(strdup(search_required("liblux.o", FALSE)), OTHER_Kind));
        string_printf(as_cmd, "%s", search_required("luxasarm", TRUE));
        free(chp);
    } else if (driver_flags & DVR_X64_TARGET) {
        /* ==================================================================== */
        /*      x64 Target                                                      */
        /* ==================================================================== */
        chp = strdup(strbuf(ld_cmd));
        string_clear(ld_cmd);
        if (driver_flags & DVR_GLIBC) {
            parse_conf_file("glibc_x64.conf");
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -m64 -static", search_required("gcc", TRUE));
            } else {
                string_printf(ld_cmd, "%s -melf_x86_64 -I/lib64/ld-linux-x86-64.so.2 %s", search_required("ld", TRUE), chp);
                infiles = insert_front(infiles, new_file(strdup(search_required("crt1.o", FALSE)), OTHER_Kind));
                infiles = insert_front(infiles, new_file(strdup(search_required("crti.o", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("crtn.o", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so.6", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libc_nonshared.a", FALSE)), OTHER_Kind));
            }
        } else if (driver_flags & DVR_MUSL) {
            parse_conf_file("musl_x64.conf");
            infiles = insert_front(infiles, new_file(strdup(search_required("crti.o", FALSE)), OTHER_Kind));
            infiles = insert_front(infiles, new_file(strdup(search_required("crt1.o", FALSE)), OTHER_Kind));
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -melf_x86_64 %s", search_required("ld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.a", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libgcc.a", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("crtn.o", FALSE)), OTHER_Kind));
            } else {
                string_printf(ld_cmd, "%s -melf_x86_64 -I/lib64/ld-linux-x86-64.so.2 %s", search_required("ld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so", FALSE)), OTHER_Kind));
            }
        } else {
            parse_conf_file("x64.conf");
            infiles = insert_front(infiles, new_file(strdup(search_required("crt0.o", FALSE)), OTHER_Kind));
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -melf_x86_64 %s", search_required("luxld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.a", FALSE)), OTHER_Kind));
            } else {
                string_printf(ld_cmd, "%s -melf_x86_64 -I/lib64/ld-linux-x86-64.so.2 %s", search_required("luxld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so", FALSE)), OTHER_Kind));
            }
        }
        string_printf(as_cmd, "%s -m64", search_required("luxasx86", TRUE));
        free(chp);
    } else {
        /* ==================================================================== */
        /*      x86 Target                                                      */
        /* ==================================================================== */
        chp = strdup(strbuf(ld_cmd));
        string_clear(ld_cmd);
        if (driver_flags & DVR_GLIBC) {
            parse_conf_file("glibc_x86.conf");
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -m32 -static", search_required("gcc", TRUE));
            } else {
                string_printf(ld_cmd, "%s -melf_i386 -I/lib/ld-linux.so.2 %s", search_required("ld", TRUE), chp);
                infiles = insert_front(infiles, new_file(strdup(search_required("crt1.o", FALSE)), OTHER_Kind));
                infiles = insert_front(infiles, new_file(strdup(search_required("crti.o", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("crtn.o", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so.6", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libc_nonshared.a", FALSE)), OTHER_Kind));
            }
        } else if (driver_flags & DVR_MUSL) {
            parse_conf_file("musl_x86.conf");
            infiles = insert_front(infiles, new_file(strdup(search_required("crti.o", FALSE)), OTHER_Kind));
            infiles = insert_front(infiles, new_file(strdup(search_required("crt1.o", FALSE)), OTHER_Kind));
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -melf_i386 %s", search_required("ld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.a", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("libgcc.a", FALSE)), OTHER_Kind));
                infiles = insert_end(infiles, new_file(strdup(search_required("crtn.o", FALSE)), OTHER_Kind));
            } else {
                string_printf(ld_cmd, "%s -melf_i386 %s", search_required("luxld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so", FALSE)), OTHER_Kind));
            }
        } else {
            parse_conf_file("x86.conf");
            infiles = insert_front(infiles, new_file(strdup(search_required("crt0.o", FALSE)), OTHER_Kind));
            if (driver_flags & DVR_STATIC) {
                string_printf(ld_cmd, "%s -melf_i386 %s", search_required("luxld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.a", FALSE)), OTHER_Kind));
            } else {
                /*string_printf(ld_cmd, "%s -melf_i386 -I/lib/ld-linux.so.2 %s", search_required("ld", TRUE), chp);*/
                string_printf(ld_cmd, "%s -melf_i386 -I/lib/ld-linux.so.2 %s", search_required("luxld", TRUE), chp);
                infiles = insert_end(infiles, new_file(strdup(search_required("libc.so", FALSE)), OTHER_Kind));
            }
        }
        infiles = insert_front(infiles, new_file(strdup(search_required("liblux.o", FALSE)), OTHER_Kind));
        string_printf(as_cmd, "%s -m32", search_required("luxasx86", TRUE));
        free(chp);
    }
    chp = strdup(strbuf(cc_cmd));
    string_clear(cc_cmd);
    string_printf(cc_cmd, "%s %s", search_required("luxcc", TRUE), chp);
    free(chp);
    if (driver_flags & (DVR_VM32_TARGET|DVR_VM64_TARGET)) {
        string_printf(cc_cmd, " -Isrc/lib/vm_lib/include");
        string_printf(cc_cmd, " -I/usr/local/lib/luxcc/vm_lib/include");
    }
    if (driver_flags & DVR_GLIBC)
        string_printf(cc_cmd, " -D_GLIBC");
    else if (driver_flags & DVR_MUSL)
        string_printf(cc_cmd, " -D_MUSL");
    if (driver_flags & DVR_ANALYZE_ONLY) {
        File *fp;
        unsigned pos;

        if (ncfls == 0)
            goto done;
        pos = string_get_pos(cc_cmd);
        for (fp = infiles; fp != NULL; fp = fp->next) {
            if (fp->kind != C_Kind)
                continue;
            string_printf(cc_cmd, " %s", fp->path);
            if (exec_cmd(cc_cmd))
                exst = 1;
            string_set_pos(cc_cmd, pos);
        }
    } else if (driver_flags & (DVR_PREP_ONLY|DVR_COMP_ONLY)) {
        File *fp;

        if (ncfls == 0)
            goto done;
        if (outpath != NULL) { /* there is a single C input file */
            for (fp = infiles; fp->kind != C_Kind; fp = fp->next)
                ;
            string_printf(cc_cmd, " %s -o %s", fp->path, outpath);
            exst = !!exec_cmd(cc_cmd);
        } else {
            unsigned pos;

            pos = string_get_pos(cc_cmd);
            for (fp = infiles; fp != NULL; fp = fp->next) {
                if (fp->kind != C_Kind)
                    continue;
                string_printf(cc_cmd, " %s", fp->path);
                if (exec_cmd(cc_cmd))
                    exst = 1;
                string_set_pos(cc_cmd, pos);
            }
        }
    } else if (driver_flags & DVR_NOLINK) {
        File *fp;

        if (ncfls==0 && nasmfls==0)
            goto done;
        if (ncfls != 0)
            mkstemps(asm_tmp, 2);
        if (outpath != NULL) { /* there is a single C/ASM input file */
            if (ncfls != 0) {
                for (fp = infiles; fp->kind != C_Kind; fp = fp->next)
                    ;
                string_printf(cc_cmd, " %s -o %s", fp->path, asm_tmp);
                if (exec_cmd(cc_cmd) == 0) {
                    string_printf(as_cmd, " %s -o %s", asm_tmp, outpath);
                    exst = !!exec_cmd(as_cmd);
                } else {
                    exst = 1;
                }
            } else {
                for (fp = infiles; fp->kind != ASM_Kind; fp = fp->next)
                    ;
                string_printf(as_cmd, " %s -o %s", fp->path, outpath);
                exst = !!exec_cmd(as_cmd);
            }
        } else {
            char *s;
            unsigned cpos, apos;

            cpos = string_get_pos(cc_cmd);
            apos = string_get_pos(as_cmd);
            for (fp = infiles; fp != NULL; fp = fp->next) {
                if (fp->kind != C_Kind)
                    continue;
                string_printf(cc_cmd, " %s -o %s", fp->path, asm_tmp);
                if (exec_cmd(cc_cmd) == 0) {
                    s = replace_extension(fp->path, ".o");
                    string_printf(as_cmd, " %s -o %s", asm_tmp, s);
                    free(s);
                    if (exec_cmd(as_cmd))
                        exst = 1;
                    string_set_pos(as_cmd, apos);
                } else {
                    exst = 1;
                }
                string_set_pos(cc_cmd, cpos);
            }
            for (fp = infiles; fp != NULL; fp = fp->next) {
                if (fp->kind != ASM_Kind)
                    continue;
                s = replace_extension(fp->path, ".o");
                string_printf(as_cmd, " %s -o %s", fp->path, s);
                free(s);
                if (exec_cmd(as_cmd))
                    exst = 1;
                string_set_pos(as_cmd, apos);
            }
        }
        if (ncfls != 0)
            delete_file(asm_tmp);
    } else {
        int ntmp;
        char *obj_tmps[64];
        unsigned cpos, apos;
        char *asm_tmp_p;
        File *fp;

        if (ncfls != 0) {
            if (alt_asm_tmp != NULL) {
                asm_tmp_p = alt_asm_tmp;
            } else {
                asm_tmp_p = asm_tmp;
                mkstemps(asm_tmp_p, 2);
            }
        }
        ntmp = 0;
        cpos = string_get_pos(cc_cmd);
        apos = string_get_pos(as_cmd);
        for (fp = infiles; fp != NULL; fp = fp->next) {
            if (fp->kind != C_Kind)
                continue;
            string_printf(cc_cmd, " %s -o %s", fp->path, asm_tmp_p);
            if (exec_cmd(cc_cmd) == 0) {
                sprintf(obj_tmp, "/tmp/luxXXXXXX.o");
                mkstemps(obj_tmp, 2);
                obj_tmps[ntmp++] = strdup(obj_tmp);
                string_printf(as_cmd, " %s -o %s", asm_tmp_p, obj_tmp);
                fp->kind = OTHER_Kind;
                fp->path = obj_tmps[ntmp-1];
                if (exec_cmd(as_cmd))
                    exst = 1;
                string_set_pos(as_cmd, apos);
            } else {
                exst = 1;
            }
            string_set_pos(cc_cmd, cpos);
        }
        for (fp = infiles; fp != NULL; fp = fp->next) {
            if (fp->kind != ASM_Kind)
                continue;
            sprintf(obj_tmp, "/tmp/luxXXXXXX.o");
            mkstemps(obj_tmp, 2);
            obj_tmps[ntmp++] = strdup(obj_tmp);
            string_printf(as_cmd, " %s -o %s", fp->path, obj_tmps);
            fp->kind = OTHER_Kind;
            fp->path = obj_tmps[ntmp-1];
            if (exec_cmd(as_cmd))
                exst = 1;
            string_set_pos(as_cmd, apos);
        }
        if (exst == 0) {
            for (fp = infiles; fp != NULL; fp = fp->next)
                string_printf(ld_cmd, " %s", fp->path);
            if (outpath != NULL)
                string_printf(ld_cmd, " -o %s", outpath);
            exst = !!exec_cmd(ld_cmd);
        }
        if (alt_asm_tmp==NULL && ncfls!=0)
            delete_file(asm_tmp_p);
        for (i = 0; i < ntmp; i++) {
            delete_file(obj_tmps[i]);
            free(obj_tmps[i]);
        }
    }
done:
    string_free(cc_cmd);
    string_free(as_cmd);
    string_free(ld_cmd);

    return exst;
}
