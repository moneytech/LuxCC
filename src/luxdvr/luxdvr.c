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
#include "../util.h"
#include "../str.h"

#define PATH_TO_CC          "src/luxcc"
#define PATH_TO_X86_AS      "src/luxas/luxas"
#define PATH_TO_X86_LD      "src/luxld/luxld"
#define PATH_TO_VM_AS       "src/luxvm/luxvmas"
#define PATH_TO_VM_LD       "src/luxvm/luxvmld"
#define GNU_LD              "ld"

#define PATH_TO_VM_RUNC1    "src/lib/crt.o"
#define PATH_TO_VM_LIBC1    "src/lib/libc.o"
#define PATH_TO_VM_RUNC2    "/usr/local/lib/luxcc/crt.o"
#define PATH_TO_VM_LIBC2    "/usr/local/lib/luxcc/libc.o"

#define PATH_TO_LIBCONF1    "src/luxdvr/library_path.conf"
#define PATH_TO_LIBCONF2    "/usr/local/lib/luxcc/library_path.conf"

/* musl's default installation path */
#define PATH_TO_MUSL_RUNC   "/usr/local/musl/lib/crt1.o "\
                            "/usr/local/musl/lib/crti.o"
#define PATH_TO_MUSL_LIBC   "/usr/local/musl/lib/libc.so"

int musl_libc_is_installed;
int verbose;
char *prog_name;
char helpstr[] =
    "\nGeneral options:\n"
    "  -o<file>         Write output to <file>\n"
    "  -E               Preprocess only\n"
    "  -S               Compile but do not assemble\n"
    "  -c               Compile and assemble but do not link\n"
    "  -v               Show invoked commands\n"
    "  -h               Print this help\n"
    "\nCompiler options:\n"
    "  -q               Disable all warnings\n"
    "  -m<mach>         Generate target code for machine <mach>\n"
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
    "\nLinker options (x86 only):\n"
    "  -e<sym>          Set <sym> as the entry point symbol\n"
    "  -l<name>         Link against object file/library <name>\n"
    "  -L<dir>          Add <dir> to the list of directories searched for the -l options\n"
    "  -I<interp>       Set <interp> as the name of the dynamic linker\n"
;

enum {
    CC,
    X86_AS,
    X86_LD,
    VM_AS,
    VM_LD,
    X86_LIBC,
    VM_LIBC,
};

enum {
    DVR_HELP            = 0x01,
    DVR_PREP_ONLY       = 0x02,
    DVR_COMP_ONLY       = 0x04,
    DVR_NOLINK          = 0x08,
    DVR_ANALYZE_ONLY    = 0x10,
};

typedef struct InFile InFile;
struct InFile {
    char *path;
    InFile *next;
};

InFile *add_file(InFile *flist, InFile *newf)
{
    InFile *p;

    if (flist == NULL)
        return newf;
    for (p = flist; p->next != NULL; p = p->next)
        ;
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

int exec_cmd(char *cmd)
{
    int status;

    if (verbose)
        printf("%s\n", cmd);

    if ((status=system(cmd)) == -1) {
        fprintf(stderr, "%s: error: cannot execute `%s'\n", prog_name, cmd);
        exit(1);
    }
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

char *get_path(int file)
{
    char *p;

    switch (file) {
    case CC:
        if (file_exist(PATH_TO_CC))
            return PATH_TO_CC;
        else if (is_in_path(p = "luxcc"))
            return p;
        break;

    case X86_AS:
        if (file_exist(PATH_TO_X86_AS))
            return PATH_TO_X86_AS;
        else if (is_in_path(p = "luxas"))
            return p;
        break;

    case X86_LD:
        if (musl_libc_is_installed) {
            if (file_exist(PATH_TO_X86_LD))
                return PATH_TO_X86_LD;
            else if (is_in_path(p = "luxld"))
                return p;
            break;
        } else {
            return GNU_LD;
        }

    case VM_AS:
        if (file_exist(PATH_TO_VM_AS))
            return PATH_TO_VM_AS;
        else if (is_in_path(p = "luxvmas"))
            return p;
        break;

    case VM_LD:
        if (file_exist(PATH_TO_VM_LD))
            return PATH_TO_VM_LD;
        else if (is_in_path(p = "luxvmld"))
            return p;
        break;

    case X86_LIBC:
        if (musl_libc_is_installed) {
            return PATH_TO_MUSL_RUNC " " PATH_TO_MUSL_LIBC;
        } else { /* get the paths from the .conf file */
            FILE *fp;
            char *cp;
            static char lib_path[BUFSIZ];

            if (file_exist(PATH_TO_LIBCONF1))
                fp = fopen(PATH_TO_LIBCONF1, "rb");
            else if (file_exist(PATH_TO_LIBCONF2))
                fp = fopen(PATH_TO_LIBCONF2, "rb");
            else
                break;
            cp = lib_path;
            while (fgets(cp, 0x7FFFFFFF, fp) != NULL) {
                cp += strlen(cp)-1;
                *cp++ = ' ';
            }
            return lib_path;
        }
        break;

    case VM_LIBC:
        if (file_exist(PATH_TO_VM_LIBC1))
            return PATH_TO_VM_RUNC1 " " PATH_TO_VM_LIBC1;
        else if (file_exist(PATH_TO_VM_LIBC2))
            return PATH_TO_VM_RUNC2 " " PATH_TO_VM_LIBC2;
        break;
    }

    if (file==X86_LIBC || file==VM_LIBC)
        p = "libc";
    fprintf(stderr, "%s: cannot find `%s'\n", prog_name, p);
    exit(1);
}

void usage(int more_inf)
{
    printf("USAGE: %s [ OPTIONS ] <file> ...\n", prog_name);
    if (more_inf)
        printf("type `%s -h' to see command-line options\n", prog_name);
}

void unknown_opt(char *opt)
{
    fprintf(stderr, "%s: unknown option `%s'\n", prog_name, opt);
    exit(1);
}

void missing_arg(char *opt)
{
    fprintf(stderr, "%s: option `%s' requires an argument\n", prog_name, opt);
    exit(1);
}

int main(int argc, char *argv[])
{
    int i, exst;
    unsigned driver_args;
    char *outpath, *alt_asm_tmp;
    String *cc_cmd, *as_cmd, *ld_cmd;
    InFile *c_files, *asm_files, *other_files;
    char asm_tmp[] = "/tmp/luxXXXXXX.s";
    char obj_tmp[] = "/tmp/luxXXXXXX.o";
    int target; /* 1: x86, 2: vm */

    prog_name = argv[0];
    if (argc == 1) {
        usage(TRUE);
        exit(0);
    }

    /* use musl libc if it is installed */
    musl_libc_is_installed = file_exist(PATH_TO_MUSL_LIBC);

    driver_args = 0;
    outpath = alt_asm_tmp = NULL;
    c_files = asm_files = other_files = NULL;
    cc_cmd = string_new(32);
    as_cmd = string_new(32);
    ld_cmd = string_new(32);
    string_printf(cc_cmd, get_path(CC));
    target = 1;

    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '-') {
            char *p;
            InFile *newf;

            newf = malloc(sizeof(InFile));
            newf->path = argv[i];
            newf->next = NULL;
            if ((p=strrchr(argv[i], '.')) == NULL)
                other_files = add_file(other_files, newf);
            else if (equal(p, ".c"))
                c_files = add_file(c_files, newf);
            else if (equal(p, ".asm") || equal(p, ".s"))
                asm_files = add_file(asm_files, newf);
            else
                other_files = add_file(other_files, newf);
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
             The rest of the options are equal to both.
            */
            case 'a':
                if (equal(argv[i], "-analyze")) {
                    string_printf(cc_cmd, " -a");
                    driver_args |= DVR_ANALYZE_ONLY;
                } else if (strncmp(argv[i], "-alt-asm-tmp", 12) == 0) {
                    /*
                     * This option is only used when self-compiling in x86. It is necessary
                     * because otherwise the temporary asm file name that will be
                     * embedded into the object files will be different from one invocation
                     * to the other and the comparison of binaries will fail.
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
                driver_args |= DVR_NOLINK;
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
                driver_args |= DVR_PREP_ONLY;
                string_printf(cc_cmd, " -p");
                break;
            case 'e':
            case 'L':
            case 'l':
                string_printf(ld_cmd, " %s", argv[i]);
                if (argv[i][2] == '\0') {
                    if (argv[i+1] == NULL)
                        missing_arg(argv[i]);
                    string_printf(ld_cmd, " %s", argv[++i]);
                }
                break;
            case 'h':
                driver_args |= DVR_HELP;
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
            case 'm':
                string_printf(cc_cmd, " %s", argv[i]);
                if (argv[i][2] == '\0') {
                    if (argv[i+1] == NULL)
                        missing_arg(argv[i]);
                    string_printf(cc_cmd, " %s", argv[++i]);
                    target = equal(argv[i-1], "x86") ? 1 : 2;
                } else {
                    target = equal(argv[i]+2, "x86") ? 1 : 2;
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
                driver_args |= DVR_COMP_ONLY;
                break;
            case 's':
                if (equal(argv[i], "-show-stats"))
                    string_printf(cc_cmd, " -s");
                else
                    unknown_opt(argv[i]);
                break;
            case 'u':
                if (equal(argv[i], "-uncolored"))
                    string_printf(cc_cmd, " -u");
                else
                    unknown_opt(argv[i]);
                break;
            case 'v':
                verbose = TRUE;
                break;
            case '\0': /* stray '-' */
                break;
            default:
                unknown_opt(argv[i]);
                break;
            }
        }
    }
    if (outpath!=NULL && (driver_args & (DVR_PREP_ONLY|DVR_COMP_ONLY|DVR_NOLINK))) {
        if (asm_files != NULL) {
            if (asm_files->next!=NULL || c_files!=NULL)
                goto err_1;
        } else if (c_files!=NULL && c_files->next!=NULL) {
            goto err_1;
        }
        goto ok_1;
err_1:
        fprintf(stderr, "%s: error: -o cannot be used with -E, -S, -c and multiple input files\n", prog_name);
        goto done;
    }
ok_1:
    if (target == 2) {
        string_printf(as_cmd, get_path(VM_AS));
        string_clear(ld_cmd);
        string_printf(ld_cmd, get_path(VM_LD));
        string_printf(ld_cmd, " %s", get_path(VM_LIBC));
    } else {
        char *p;

        if (musl_libc_is_installed)
            string_printf(cc_cmd, " -D_MUSL_LIBC");
        string_printf(as_cmd, get_path(X86_AS));
        p = strdup(strbuf(ld_cmd));
        string_printf(ld_cmd, "%s %s", get_path(X86_LD), p);
        free(p);
    }

    exst = 0;
    if (driver_args & DVR_HELP) {
        usage(FALSE);
        printf("%s", helpstr);
        goto done;
    }
    if (c_files==NULL && asm_files==NULL && other_files==NULL) {
        fprintf(stderr, "%s: error: no input files\n", prog_name);
        exst = 1;
        goto done;
    }
    if (driver_args & DVR_ANALYZE_ONLY) {
        InFile *fi;

        if (c_files == NULL)
            goto done;
        for (fi = c_files; fi != NULL; fi = fi->next) {
            unsigned pos;

            pos = string_get_pos(cc_cmd);
            string_printf(cc_cmd, " %s", fi->path);
            if (exec_cmd(strbuf(cc_cmd)))
                exst = 1;
            string_set_pos(cc_cmd, pos);
        }
    } else if (driver_args & (DVR_PREP_ONLY|DVR_COMP_ONLY)) {
        if (c_files == NULL)
            goto done;
        if (outpath != NULL) { /* there must be a single C input file */
            string_printf(cc_cmd, " %s -o %s", c_files->path, outpath);
            exst = !!exec_cmd(strbuf(cc_cmd));
        } else {
            InFile *fi;

            for (fi = c_files; fi != NULL; fi = fi->next) {
                unsigned pos;

                pos = string_get_pos(cc_cmd);
                string_printf(cc_cmd, " %s", fi->path);
                if (exec_cmd(strbuf(cc_cmd)))
                    exst = 1;
                string_set_pos(cc_cmd, pos);
            }
        }
    } else if (driver_args & DVR_NOLINK) {
        if (c_files==NULL && asm_files==NULL)
            goto done;
        if (c_files != NULL)
            mkstemps(asm_tmp, 2);
        if (outpath != NULL) { /* there must be a single C or ASM input file */
            if (c_files != NULL) {
                string_printf(cc_cmd, " %s -o %s", c_files->path, asm_tmp);
                if (exec_cmd(strbuf(cc_cmd)) == 0) {
                    string_printf(as_cmd, " %s -o %s", asm_tmp, outpath);
                    exst = !!exec_cmd(strbuf(as_cmd));
                } else {
                    exst = 1;
                }
            } else {
                string_printf(as_cmd, " %s -o %s", asm_files->path, outpath);
                exst = !!exec_cmd(strbuf(as_cmd));
            }
        } else {
            char *s;
            InFile *fi;
            unsigned cpos, apos;

            for (fi = c_files; fi != NULL; fi = fi->next) {
                cpos = string_get_pos(cc_cmd);
                string_printf(cc_cmd, " %s -o %s", fi->path, asm_tmp);
                if (exec_cmd(strbuf(cc_cmd)) == 0) {
                    apos = string_get_pos(as_cmd);
                    s = replace_extension(fi->path, ".o");
                    string_printf(as_cmd, " %s -o %s", asm_tmp, s);
                    free(s);
                    if (exec_cmd(strbuf(as_cmd)))
                        exst = 1;
                    string_set_pos(as_cmd, apos);
                } else {
                    exst = 1;
                }
                string_set_pos(cc_cmd, cpos);
            }
            for (fi = asm_files; fi != NULL; fi = fi->next) {
                apos = string_get_pos(as_cmd);
                s = replace_extension(fi->path, ".o");
                string_printf(as_cmd, " %s -o %s", fi->path, s);
                free(s);
                if (exec_cmd(strbuf(as_cmd)))
                    exst = 1;
                string_set_pos(as_cmd, apos);
            }
        }
        if (c_files != NULL)
            unlink(asm_tmp);
    } else {
        int ntmp;
        char *obj_tmps[64];
        unsigned cpos, apos;
        char *asm_tmp_p;
        InFile *fi;

        if (c_files != NULL) {
            if (alt_asm_tmp != NULL) {
                asm_tmp_p = alt_asm_tmp;
            } else {
                asm_tmp_p = asm_tmp;
                mkstemps(asm_tmp_p, 2);
            }
        }
        ntmp = 0;
        for (fi = c_files; fi != NULL; fi = fi->next) {
            cpos = string_get_pos(cc_cmd);
            string_printf(cc_cmd, " %s -o %s", fi->path, asm_tmp_p);
            if (exec_cmd(strbuf(cc_cmd)) == 0) {
                apos = string_get_pos(as_cmd);
                sprintf(obj_tmp, "/tmp/luxXXXXXX.o");
                mkstemps(obj_tmp, 2);
                obj_tmps[ntmp++] = strdup(obj_tmp);
                string_printf(as_cmd, " %s -o %s", asm_tmp_p, obj_tmp);
                if (exec_cmd(strbuf(as_cmd)))
                    exst = 1;
                string_set_pos(as_cmd, apos);
            } else {
                exst = 1;
            }
            string_set_pos(cc_cmd, cpos);
        }
        for (fi = asm_files; fi != NULL; fi = fi->next) {
            apos = string_get_pos(as_cmd);
            sprintf(obj_tmp, "/tmp/luxXXXXXX.o");
            mkstemps(obj_tmp, 2);
            obj_tmps[ntmp++] = strdup(obj_tmp);
            string_printf(as_cmd, " %s -o %s", fi->path, obj_tmps);
            if (exec_cmd(strbuf(as_cmd)))
                exst = 1;
            string_set_pos(as_cmd, apos);
        }
        if (exst == 0) {
            InFile *fp;

            for (fp = other_files; fp != NULL; fp = fp->next)
                string_printf(ld_cmd, " %s", fp->path);
            for (i = 0; i < ntmp; i++)
                string_printf(ld_cmd, " %s", obj_tmps[i]);
            if (outpath != NULL)
                string_printf(ld_cmd, " -o %s", outpath);
            if (target == 1) {
                if (musl_libc_is_installed)
                    string_printf(ld_cmd, " %s", get_path(X86_LIBC));
                else
                    string_printf(ld_cmd, " %s -I/lib/ld-linux.so.2", get_path(X86_LIBC));
            }
            exst = !!exec_cmd(strbuf(ld_cmd));
        }
        if (alt_asm_tmp==NULL && c_files!=NULL)
            unlink(asm_tmp_p);
        for (i = 0; i < ntmp; i++) {
            unlink(obj_tmps[i]);
            free(obj_tmps[i]);
        }
    }
done:
    if (c_files != NULL) {
        InFile *tmp;
        do {
            tmp = c_files;
            c_files = c_files->next;
            free(tmp);
        } while (c_files != NULL);
    }
    if (asm_files != NULL) {
        InFile *tmp;
        do {
            tmp = asm_files;
            asm_files = asm_files->next;
            free(tmp);
        } while (asm_files != NULL);
    }
    if (other_files != NULL) {
        InFile *tmp;
        do {
            tmp = other_files;
            other_files = other_files->next;
            free(tmp);
        } while (other_files != NULL);
    }
    string_free(cc_cmd);
    string_free(as_cmd);
    string_free(ld_cmd);

    return exst;
}
