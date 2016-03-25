#include "luxcc.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "parser.h"
#include "ic.h"
#include "vm32_cgen/vm32_cgen.h"
#include "vm64_cgen/vm64_cgen.h"
#include "x86_cgen/x86_cgen.h"
#include "x64_cgen/x64_cgen.h"
#include "mips_cgen/mips_cgen.h"
#include "arm_cgen/arm_cgen.h"
#include "util.h"

unsigned warning_count, error_count;
int disable_warnings;
int colored_diagnostics = TRUE;
int targeting_arch64;
int target_arch;
char *cg_outpath;
char *cfg_outpath, *cfg_function_to_print;
char *ic_outpath, *ic_function_to_print;
int include_liblux = TRUE;
int include_libc = TRUE;
int verbose_asm;

unsigned stat_number_of_pre_tokens;
unsigned stat_number_of_c_tokens;
unsigned stat_number_of_ast_nodes;
static char *program_name;

static void usage(FILE *fp)
{
    fprintf(fp, "USAGE: %s [ OPTIONS ] <file>\n", program_name);
}

static void missing_arg(char *opt)
{
    fprintf(stderr, "%s: option `%s' requires an argument\n", program_name, opt);
    exit(EXIT_FAILURE);
}

enum {
    OPT_PREPROCESS_ONLY = 0x001,
    OPT_DUMP_TOKENS     = 0x002,
    OPT_ANALYZE         = 0x004,
    OPT_SHOW_STATS      = 0x008,
    OPT_PRINT_AST       = 0x010,
    OPT_PRINT_CG        = 0x020,
    OPT_X86_TARGET      = 0x040,
    OPT_X64_TARGET      = 0x080,
    OPT_VM32_TARGET     = 0x100,
    OPT_VM64_TARGET     = 0x200,
    OPT_MIPS_TARGET     = 0x400,
    OPT_ARM_TARGET      = 0x800,
};
#define TARGET_MASK (OPT_X86_TARGET|\
                     OPT_X64_TARGET|\
                     OPT_VM32_TARGET|\
                     OPT_VM64_TARGET|\
                     OPT_MIPS_TARGET|\
                     OPT_ARM_TARGET)

int main(int argc, char *argv[])
{
    int i;
    FILE *fp = NULL;
    unsigned flags = 0;
    char *outpath = NULL, *inpath = NULL;
    PreTokenNode *pre;
    TokenNode *tok;
    PreTokenNode newline_node, one_node;
    newline_node.token = PRE_TOK_NL;
    newline_node.lexeme = "\n";
    one_node.token = PRE_TOK_NUM;
    one_node.lexeme = "1";
    one_node.next = &newline_node;

    program_name = argv[0];
    if (argc == 1) {
        usage(stderr);
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
        case 'D':
            if (argv[i][2] != '\0')
                install_macro(SIMPLE_MACRO, argv[i]+2, &one_node, NULL);
            else if (argv[i+1] == NULL)
                missing_arg(argv[i]);
            else
                install_macro(SIMPLE_MACRO, argv[++i], &one_node, NULL);
            break;
        case 'h':
            usage(stdout);
            printf("Run the driver with the `-h' option for more info\n");
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
                missing_arg(argv[i]);
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
            else if (equal(targ, "x64"))
                flags |= OPT_X64_TARGET;
            else if (equal(targ, "vm32"))
                flags |= OPT_VM32_TARGET;
            else if (equal(targ, "vm64"))
                flags |= OPT_VM64_TARGET;
            else if (equal(targ, "mips"))
                flags |= OPT_MIPS_TARGET;
            else if (equal(targ, "arm"))
                flags |= OPT_ARM_TARGET;
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
        case 'u':
            colored_diagnostics = 0;
            break;
        case 'A':
            flags |= OPT_PRINT_AST;
            break;
        case 'C':
            flags |= OPT_PRINT_CG;
            break;
        case 'G':
            if (argv[i][2] != '\0')
                cfg_function_to_print = argv[i]+2;
            else if (argv[i+1] == NULL)
                missing_arg(argv[i]);
            else
                cfg_function_to_print = argv[++i];
            break;
        case 'N':
            if (argv[i][2] != '\0')
                ic_function_to_print = argv[i]+2;
            else if (argv[i+1] == NULL)
                missing_arg(argv[i]);
            else
                ic_function_to_print = argv[++i];
            break;
        case 'T':
            flags |= OPT_DUMP_TOKENS;
            break;
        case 'v':
            verbose_asm = TRUE;
            break;
        case 'z': /* only used when compiling liblux */
            include_liblux = FALSE;
            break;
        case 'Z': /* only used when compiling libc */
            include_libc = FALSE;
            break;
        case '\0': /* stray '-' */
            break;
        default:
            fprintf(stderr, "%s: unknown option `%s'\n", program_name, argv[i]);
            exit(EXIT_FAILURE);
        }
    }

    if (inpath == NULL) {
        usage(stderr);
        exit(EXIT_FAILURE);
    }

    switch (flags & TARGET_MASK) {
    case OPT_VM32_TARGET:
        target_arch = ARCH_VM32;
        install_macro(SIMPLE_MACRO, "__LuxVM__", &one_node, NULL);
        break;
    case OPT_VM64_TARGET:
        target_arch = ARCH_VM64;
        install_macro(SIMPLE_MACRO, "__LuxVM__", &one_node, NULL);
        install_macro(SIMPLE_MACRO, "__LP64__", &one_node, NULL);
        targeting_arch64 = TRUE;
        break;
    case OPT_X64_TARGET:
        target_arch = ARCH_X64;
        install_macro(SIMPLE_MACRO, "__x86_64__", &one_node, NULL);
        install_macro(SIMPLE_MACRO, "__LP64__", &one_node, NULL);
        targeting_arch64 = TRUE;
        break;
    case OPT_MIPS_TARGET:
        target_arch = ARCH_MIPS;
        install_macro(SIMPLE_MACRO, "__mips__", &one_node, NULL);
        break;
    case OPT_ARM_TARGET:
        target_arch = ARCH_ARM;
        install_macro(SIMPLE_MACRO, "__arm__", &one_node, NULL);
        break;
    default:
        target_arch = ARCH_X86;
        flags |= OPT_X86_TARGET;
        install_macro(SIMPLE_MACRO, "__i386__", &one_node, NULL);
        break;
    }
    install_macro(SIMPLE_MACRO, "__unix__", &one_node, NULL);
    install_macro(SIMPLE_MACRO, "__linux__", &one_node, NULL);
    install_macro(SIMPLE_MACRO, "__gnu_linux__", &one_node, NULL);

    pre = preprocess(inpath);
    if (flags & OPT_PREPROCESS_ONLY) {
        PreTokenNode *p;

        fp = (outpath == NULL) ? stdout : fopen(outpath, "wb");
        for (p = pre; p != NULL; p = p->next)
            if (!p->deleted || p->token==PRE_TOK_NL)
                fprintf(fp, "%s ", p->lexeme);
        fprintf(fp, "\n");
        goto done;
    }

    tok = tokenize(pre);
    if (flags & OPT_DUMP_TOKENS) {
        TokenNode *p;
        char *tok_outpath;

        tok_outpath = replace_extension(inpath, ".tok");
        fp = fopen(tok_outpath, "wb");
        for (p = tok; p != NULL; p = p->next)
            fprintf(fp, "%s:%d:%-3d =>   token: %-15s lexeme: `%s'\n", p->src_file, p->src_line,
            p->src_column, token_table[p->token*2], p->lexeme);
        free(tok_outpath);
        fclose(fp);
    }

    /* parse & analyze */
    if (flags & OPT_PRINT_AST) {
        char *ast_outpath;

        ast_outpath = replace_extension(inpath, ".ast.dot");
        parse(tok, ast_outpath);
        free(ast_outpath);
    } else {
        parse(tok, NULL);
    }
    if (flags & OPT_ANALYZE)
        goto done;

    if (error_count == 0) {
        fp = (outpath == NULL) ? stdout : fopen(outpath, "wb");
        switch (flags & TARGET_MASK) {
        case OPT_X86_TARGET:
        case OPT_X64_TARGET:
        case OPT_MIPS_TARGET:
        case OPT_ARM_TARGET:
            if (ic_function_to_print != NULL)  ic_outpath = replace_extension(inpath, ".ic");
            if (cfg_function_to_print != NULL) cfg_outpath = replace_extension(inpath, ".cfg.dot");
            if (flags & OPT_PRINT_CG)          cg_outpath = replace_extension(inpath, ".cg.dot");

            switch (flags & TARGET_MASK) {
            case OPT_X86_TARGET:  x86_cgen(fp); break;
            case OPT_X64_TARGET:  x64_cgen(fp); break;
            case OPT_MIPS_TARGET: mips_cgen(fp); break;
            case OPT_ARM_TARGET: arm_cgen(fp); break;
            }

            if (ic_function_to_print != NULL) free(ic_outpath);
            if (cfg_outpath != NULL)          free(cfg_outpath);
            if (cg_outpath != NULL)           free(cg_outpath);
            break;
        case OPT_VM32_TARGET:
            vm32_cgen(fp);
            break;
        case OPT_VM64_TARGET:
            vm64_cgen(fp);
            break;
        }
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
    return !!error_count;
}
