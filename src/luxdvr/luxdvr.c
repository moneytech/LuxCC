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
#include "../util.h"
#include "../str.h"

int verbose;
char *prog_name;
char helpstr[] =
    "\nGeneral options:\n"
    "  -m<mach>         Target machine <mach>\n"
    "  -o<file>         Write output to <file>\n"
    "  -E               Preprocess only\n"
    "  -S               Compile but do not assemble\n"
    "  -c               Compile and assemble but do not link\n"
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
    "\nLinker options (x86 only):\n"
    "  -Xe<sym>         Set <sym> as the entry point symbol\n"
    "  -Xl<name>        Link against object file/library <name>\n"
    "  -XL<dir>         Add <dir> to the list of directories searched for the -l options\n"
    "  -XI<interp>      Set <interp> as the name of the dynamic linker\n"
;

enum {
    DVR_HELP            = 0x001,
    DVR_PREP_ONLY       = 0x002,
    DVR_COMP_ONLY       = 0x004,
    DVR_NOLINK          = 0x008,
    DVR_ANALYZE_ONLY    = 0x010,
    DVR_VM32_TARGET     = 0x020,
    DVR_VM64_TARGET     = 0x040,
    DVR_X86_TARGET      = 0x080,
    DVR_X64_TARGET      = 0x100,
};
#define DVR_TARGETS (DVR_VM32_TARGET+DVR_VM64_TARGET+DVR_X86_TARGET+DVR_X64_TARGET)

typedef struct PathList PathList;
struct PathList {
    char *path;
    PathList *next;
};

struct {
    char *name;
    PathList *dirs;
} required_files[64];
int nreq;

PathList *add_file(PathList *flist, PathList *newf)
{
    PathList *p;

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
        PathList *pl;

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
            pl = malloc(sizeof(PathList));
            pl->path = strdup(buf2);
            pl->next = required_files[nreq].dirs;
            required_files[nreq].dirs = pl;
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
        PathList *pl;

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
    char *outpath, *alt_asm_tmp, *p;
    String *cc_cmd, *as_cmd, *ld_cmd;
    PathList *c_files, *asm_files, *other_files;
    char asm_tmp[] = "/tmp/luxXXXXXX.s";
    char obj_tmp[] = "/tmp/luxXXXXXX.o";

    prog_name = argv[0];
    if (argc == 1) {
        usage(TRUE);
        exit(0);
    }

    driver_flags = 0;
    driver_flags |= DVR_X86_TARGET;
    outpath = alt_asm_tmp = NULL;
    c_files = asm_files = other_files = NULL;
    cc_cmd = string_new(32); string_printf(cc_cmd, "");
    as_cmd = string_new(32); string_printf(as_cmd, "");
    ld_cmd = string_new(32); string_printf(ld_cmd, "");

    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '-') {
            PathList *newf;

            newf = malloc(sizeof(PathList));
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
                    driver_flags |= DVR_ANALYZE_ONLY;
                } else if (strncmp(argv[i], "-alt-asm-tmp", 12) == 0) {
                    /*
                     * This option is only used when self-compiling in x86/x64.
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
    if (outpath!=NULL && (driver_flags & (DVR_PREP_ONLY|DVR_COMP_ONLY|DVR_NOLINK))) {
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
    exst = 0;
    if (driver_flags & DVR_HELP) {
        usage(FALSE);
        printf("%s", helpstr);
        if (verbose)
            printf("\nCurrent valid arguments for -m:\n"
                   "  vm32\n"
                   "  vm64\n"
                   "  x86\n"
                   "  x64\n");
        else
            printf("\nFor a list of valid arguments to -m, use -h -v.\n");
        goto done;
    }
    if (driver_flags & DVR_VM32_TARGET) {
        parse_conf_file("vm32.conf");
        string_printf(as_cmd, "%s -vm32", search_required("luxvmas", TRUE));
        string_clear(ld_cmd);
        string_printf(ld_cmd, "%s -vm32", search_required("luxvmld", TRUE));
        string_printf(ld_cmd, " %s", search_required("crt32.o", FALSE));
        string_printf(ld_cmd, " %s", search_required("libc.o", FALSE));
    } else if (driver_flags & DVR_VM64_TARGET) {
        parse_conf_file("vm64.conf");
        string_printf(as_cmd, "%s -vm64", search_required("luxvmas", TRUE));
        string_clear(ld_cmd);
        string_printf(ld_cmd, "%s -vm64", search_required("luxvmld", TRUE));
        string_printf(ld_cmd, " %s", search_required("crt64.o", FALSE));
        string_printf(ld_cmd, " %s", search_required("libc.o", FALSE));
    } else if (driver_flags & DVR_X64_TARGET) {
        parse_conf_file("x64.conf");
        string_printf(as_cmd, "%s -m64", search_required("luxas", TRUE));
        p = strdup(strbuf(ld_cmd));
        string_clear(ld_cmd);
        string_printf(ld_cmd, "%s -I/lib64/ld-linux-x86-64.so.2 %s", search_required("ld", TRUE), p);
        free(p);
    } else {
        parse_conf_file("x86.conf");
        string_printf(as_cmd, "%s -m32", search_required("luxas", TRUE));
        p = strdup(strbuf(ld_cmd));
        string_clear(ld_cmd);
        string_printf(ld_cmd, "%s -I/lib/ld-linux.so.2 %s", search_required("ld", TRUE), p);
        string_printf(ld_cmd, " %s", search_required("liblux.o", FALSE));
        free(p);
    }
    p = strdup(strbuf(cc_cmd));
    string_clear(cc_cmd);
    string_printf(cc_cmd, "%s %s", search_required("luxcc", TRUE), p);
    free(p);

    if (c_files==NULL && asm_files==NULL && other_files==NULL) {
        fprintf(stderr, "%s: error: no input files\n", prog_name);
        exst = 1;
        goto done;
    }
    if (driver_flags & DVR_ANALYZE_ONLY) {
        PathList *fi;
        unsigned pos;

        if (c_files == NULL)
            goto done;
        pos = string_get_pos(cc_cmd);
        for (fi = c_files; fi != NULL; fi = fi->next) {
            string_printf(cc_cmd, " %s", fi->path);
            if (exec_cmd(cc_cmd))
                exst = 1;
            string_set_pos(cc_cmd, pos);
        }
    } else if (driver_flags & (DVR_PREP_ONLY|DVR_COMP_ONLY)) {
        if (c_files == NULL)
            goto done;
        if (outpath != NULL) { /* there is a single C input file */
            string_printf(cc_cmd, " %s -o %s", c_files->path, outpath);
            exst = !!exec_cmd(cc_cmd);
        } else {
            PathList *fi;
            unsigned pos;

            pos = string_get_pos(cc_cmd);
            for (fi = c_files; fi != NULL; fi = fi->next) {
                string_printf(cc_cmd, " %s", fi->path);
                if (exec_cmd(cc_cmd))
                    exst = 1;
                string_set_pos(cc_cmd, pos);
            }
        }
    } else if (driver_flags & DVR_NOLINK) {
        if (c_files==NULL && asm_files==NULL)
            goto done;
        if (c_files != NULL)
            mkstemps(asm_tmp, 2);
        if (outpath != NULL) { /* there is a single C/ASM input file */
            if (c_files != NULL) {
                string_printf(cc_cmd, " %s -o %s", c_files->path, asm_tmp);
                if (exec_cmd(cc_cmd) == 0) {
                    string_printf(as_cmd, " %s -o %s", asm_tmp, outpath);
                    exst = !!exec_cmd(as_cmd);
                } else {
                    exst = 1;
                }
            } else {
                string_printf(as_cmd, " %s -o %s", asm_files->path, outpath);
                exst = !!exec_cmd(as_cmd);
            }
        } else {
            char *s;
            PathList *fi;
            unsigned cpos, apos;

            cpos = string_get_pos(cc_cmd);
            apos = string_get_pos(as_cmd);
            for (fi = c_files; fi != NULL; fi = fi->next) {
                string_printf(cc_cmd, " %s -o %s", fi->path, asm_tmp);
                if (exec_cmd(cc_cmd) == 0) {
                    s = replace_extension(fi->path, ".o");
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
            for (fi = asm_files; fi != NULL; fi = fi->next) {
                s = replace_extension(fi->path, ".o");
                string_printf(as_cmd, " %s -o %s", fi->path, s);
                free(s);
                if (exec_cmd(as_cmd))
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
        PathList *fi;

        if (c_files != NULL) {
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
        for (fi = c_files; fi != NULL; fi = fi->next) {
            string_printf(cc_cmd, " %s -o %s", fi->path, asm_tmp_p);
            if (exec_cmd(cc_cmd) == 0) {
                sprintf(obj_tmp, "/tmp/luxXXXXXX.o");
                mkstemps(obj_tmp, 2);
                obj_tmps[ntmp++] = strdup(obj_tmp);
                string_printf(as_cmd, " %s -o %s", asm_tmp_p, obj_tmp);
                if (exec_cmd(as_cmd))
                    exst = 1;
                string_set_pos(as_cmd, apos);
            } else {
                exst = 1;
            }
            string_set_pos(cc_cmd, cpos);
        }
        for (fi = asm_files; fi != NULL; fi = fi->next) {
            sprintf(obj_tmp, "/tmp/luxXXXXXX.o");
            mkstemps(obj_tmp, 2);
            obj_tmps[ntmp++] = strdup(obj_tmp);
            string_printf(as_cmd, " %s -o %s", fi->path, obj_tmps);
            if (exec_cmd(as_cmd))
                exst = 1;
            string_set_pos(as_cmd, apos);
        }
        if (exst == 0) {
            PathList *fp;

            /* TOFIX: the order of the files should remain the same */
            for (i = 0; i < ntmp; i++)
                string_printf(ld_cmd, " %s", obj_tmps[i]);
            for (fp = other_files; fp != NULL; fp = fp->next)
                string_printf(ld_cmd, " %s", fp->path);
            if (outpath != NULL)
                string_printf(ld_cmd, " -o %s", outpath);
            if (driver_flags & (DVR_X86_TARGET+DVR_X64_TARGET)) {
                string_printf(ld_cmd, " %s", search_required("crt1.o", FALSE));
                string_printf(ld_cmd, " %s", search_required("crti.o", FALSE));
                string_printf(ld_cmd, " %s", search_required("crtn.o", FALSE));
                string_printf(ld_cmd, " %s", search_required("libc.so.6", FALSE));
                string_printf(ld_cmd, " %s", search_required("libc_nonshared.a", FALSE));
            }
            exst = !!exec_cmd(ld_cmd);
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
        PathList *tmp;
        do {
            tmp = c_files;
            c_files = c_files->next;
            free(tmp);
        } while (c_files != NULL);
    }
    if (asm_files != NULL) {
        PathList *tmp;
        do {
            tmp = asm_files;
            asm_files = asm_files->next;
            free(tmp);
        } while (asm_files != NULL);
    }
    if (other_files != NULL) {
        PathList *tmp;
        do {
            tmp = other_files;
            other_files = other_files->next;
            free(tmp);
        } while (other_files != NULL);
    }
    for (i = 0; i < nreq; i++) {
        PathList *tmp1, *tmp2;

        free(required_files[i].name);
        tmp1 = required_files[i].dirs;
        do {
            tmp2 = tmp1;
            tmp1 = tmp1->next;
            free(tmp2->path);
            free(tmp2);
        } while (tmp1 != NULL);
    }
    string_free(cc_cmd);
    string_free(as_cmd);
    string_free(ld_cmd);

    return exst;
}
