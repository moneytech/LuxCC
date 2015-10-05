#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <stdlib.h>
#include <assert.h>
#include "parser.h"
#include "ic.h"
#include "vm_cgen/vm_cgen.h"
#include "x86_cgen/x86_cgen.h"
#include "util.h"

unsigned warning_count, error_count;
int disable_warnings;
int colored_diagnostics = 1;

unsigned stat_number_of_pre_tokens;
unsigned stat_number_of_c_tokens;
unsigned stat_number_of_ast_nodes;
static char *program_name;

static void usage(FILE *fp, int more_inf)
{
    fprintf(fp, "USAGE: %s [ OPTIONS ] <file>\n", program_name);
    if (more_inf)
        fprintf(fp, "Type `%s -h' to see command-line options\n", program_name);
}

static void print_options(void)
{
    printf(
    "\nOPTIONS:\n"
    "  -p               Preprocess only\n"
    "  -t               Show tokenized file\n"
    "  -a               Perform static analysis only\n"
    "  -q               Disable all warnings\n"
    "  -o<file>         Write output to <file>\n"
    "  -s               Show compilation stats\n"
    "  -u               Print uncolored diagnostics\n"
    "  -m<mach>         Generate target code for machine <mach>\n"
    "  -I<dir>          Add <dir> to the list of directories searched for #include <...>\n"
    "  -i<dir>          Add <dir> to the list of directories searched for #include \"...\"\n"
    "  -h               Print this help\n"
    );
}

static void missing_arg(char *opt)
{
    fprintf(stderr, "%s: option `%s' requires an argument\n", program_name, opt);
    exit(EXIT_FAILURE);
}

static FILE *open_for_write(char *path)
{
    FILE *fp;

    fp = (path == NULL) ? stdout : fopen(path, "wb");
    if (fp == NULL) {
        fprintf(stderr, "%s: cannot write to file `%s'\n", program_name, path);
        exit(EXIT_FAILURE);
    }
    return fp;
}

enum {
    OPT_PREPROCESS_ONLY = 0x01,
    OPT_DUMP_TOKENS     = 0x02,
    OPT_ANALYZE         = 0x04,
    OPT_SHOW_STATS      = 0x08,
    OPT_X86_TARGET      = 0x10,
    OPT_VM_TARGET       = 0x20,
};

int main(int argc, char *argv[])
{
    int i;
    FILE *fp = NULL;
    unsigned flags = 0;
    char *outpath = NULL, *inpath = NULL;
    PreTokenNode *pre;
    TokenNode *tok;
    PreTokenNode dummy_node = { PRE_TOK_NL };

    program_name = argv[0];
    if (argc == 1) {
        usage(stderr, TRUE);
        exit(EXIT_SUCCESS);
    }
    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '-') {
            inpath = argv[i];
            continue;
        }
        switch (argv[i][1]) {
        case 'a':
            flags |= OPT_ANALYZE;
            break;
        case 'h':
            usage(stdout, FALSE);
            print_options();
            exit(EXIT_SUCCESS);
        case 'I':
            if (argv[i][2] != '\0')
                add_angle_dir(argv[i]+2);
            else if (argv[i+1] == NULL)
                missing_arg(argv[i]);
            else
                add_angle_dir(argv[++i]);
            break;
        case 'i':
            if (argv[i][2] != '\0')
                add_quote_dir(argv[i]+2);
            else if (argv[i+1] == NULL)
                add_quote_dir(argv[i]);
            else
                add_quote_dir(argv[++i]);
            break;
        case 'm': {
            char *targ;

            if (argv[i][2] != '\0')
                targ = argv[i]+2;
            else if (argv[i+1] == NULL)
                missing_arg(argv[i]);
            else
                targ = argv[++i];
            if (equal(targ, "x86"))
                flags |= OPT_X86_TARGET;
            else if (equal(targ, "vm"))
                flags |= OPT_VM_TARGET;
        }
            break;
        case 'o':
            if (argv[i][2] != '\0')
                outpath = argv[i]+2;
            else if (argv[i+1] == NULL)
                missing_arg(argv[i]);
            else
                outpath = argv[++i];
            break;
        case 'p':
            flags |= OPT_PREPROCESS_ONLY;
            break;
        case 'q':
            disable_warnings = TRUE;
            break;
        case 's':
            flags |= OPT_SHOW_STATS;
            break;
        case 't':
            flags |= OPT_DUMP_TOKENS;
            break;
        case 'u':
            colored_diagnostics = 0;
            break;
        case '\0': /* stray '-' */
            break;
        default:
            fprintf(stderr, "%s: unknown option `%s'\n", program_name, argv[i]);
            exit(EXIT_FAILURE);
        }
    }

    if (inpath == NULL) {
        usage(stderr, TRUE);
        exit(EXIT_FAILURE);
    }

    if (flags & OPT_VM_TARGET)
        install_macro(SIMPLE_MACRO, "__LuxVM__", &dummy_node, NULL);
    else /* default target: x86 */
        install_macro(SIMPLE_MACRO, "__x86_32__", &dummy_node, NULL);

    pre = preprocess(inpath);
    if (flags & OPT_PREPROCESS_ONLY) {
        PreTokenNode *p;

        fp = open_for_write(outpath);
        for (p = pre; p != NULL; p = p->next)
            if (!p->deleted || p->token==PRE_TOK_NL)
                fprintf(fp, "%s ", p->lexeme);
        goto done;
    }

    tok = tokenize(pre);
    if (flags & OPT_DUMP_TOKENS) {
        TokenNode *p;

        for (p = tok; p != NULL; p = p->next)
            fprintf(fp, "%s:%d:%-3d =>   token: %-15s lexeme: `%s'\n", p->src_file, p->src_line,
            p->src_column, token_table[p->token*2], p->lexeme);
        goto done;
    }

    parse(tok); /* parse & analyze */

    if (flags & OPT_ANALYZE)
        goto done;

    if (error_count == 0) {
        fp = open_for_write(outpath);
        if (flags & OPT_VM_TARGET)
            vm_cgen(fp);
        else
            x86_cgen(fp);
    } else {
        return 1;
    }
done:
    if (fp!=NULL && fp!=stdout)
        fclose(fp);
    if (flags & OPT_SHOW_STATS) {
        printf("\n=> '%u' preprocessing tokens were created (aprox)\n", stat_number_of_pre_tokens);
        printf("=> '%u' C tokens were created (aprox)\n", stat_number_of_c_tokens);
        printf("=> '%u' AST nodes were created (aprox)\n", stat_number_of_ast_nodes);
    }
    return 0;
}
