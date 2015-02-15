#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <stdlib.h>
#include "parser.h"
#include "ic.h"

unsigned warning_count, error_count;
int disable_warnings;


static void usage(char *program_name)
{
    printf("USAGE: %s [-Edaqhs] [-o <path>] <file>\n", program_name);
}

static void print_options(void)
{
    printf("\nOPTIONS:\n"
           "  -E, --preprocess          Preprocess only\n"
           "  -d, --dump-tokens         Show tokenized file\n"
           "  -a, --analyze             Perform static analysis only\n"
           "  -q, --quiet               Disable all warnings\n"
           "  -o, --ouput-file          Write ouput to specified file\n"
           "  -s, --show-stats          Show compilation stats\n"
           "  -h, --help                Print this help\n"
           );
}

int main(int argc, char *argv[])
{
    static PreTokenNode *pre;
    static TokenNode *tok;
    static ExternDecl *tree;

    /*
     * Handle command line options.
     */
    int option_index, c;
    unsigned option_flags;
    enum {
        OPT_PREPROCESS_ONLY = 0x1,
        OPT_DUMP_TOKENS     = 0x2,
        OPT_ANALYZE         = 0x4,
        OPT_SHOW_STATS      = 0x8
    };

    struct option compiler_options[] = {
        {"preprocess",  no_argument,        NULL, 'E'},
        {"dump-tokens", no_argument,        NULL, 'd'},
        {"analyze",     no_argument,        NULL, 'a'},
        {"quiet",       no_argument,        NULL, 'q'},
        {"ouput-file",  required_argument,  0,    'o'},
        {"help",        no_argument,        NULL, 'h'},
        {"show-stats",  no_argument,        NULL, 's'},
        {NULL,          0,                  NULL,  0}
    };

    option_flags = 0;
    for (;;) {
        option_index = 0;

        c = getopt_long(argc, argv, "Edaqo:h", compiler_options, &option_index);
        if (c == -1)
            /* no more options */
            break;

        switch (c) {
            case 'E':
                option_flags |= OPT_PREPROCESS_ONLY;
                break;
            case 'd':
                option_flags |= OPT_DUMP_TOKENS;
                break;
            case 'a':
                option_flags |= OPT_ANALYZE;
                break;
            case 's':
                option_flags |= OPT_SHOW_STATS;
                break;
            case 'q':
                disable_warnings = 1;
                break;
            case 'o':
                printf("redirect output to `%s'\n", optarg);
                break;
            case 'h':
                usage(argv[0]);
                print_options();
                exit(EXIT_SUCCESS);
            case '?':
                /* by default opterr is true, getopt_long already printed a diagnostic */
                // break;
            default:
                exit(EXIT_FAILURE);
        }
    }

    if (optind >= argc) {
        /* the input file is missing */
        usage(argv[0]);
        exit(EXIT_FAILURE);
    }

    /*
     * Here begins the processing of the file.
     */

    pre = preprocess(argv[optind]);
    if (option_flags & OPT_PREPROCESS_ONLY) {
        PreTokenNode *p;

        for (p = pre; p != NULL; p = p->next) {
            if (!p->deleted || p->token==PRE_TOK_NL)
                printf("%s ", p->lexeme);
        }
        exit(EXIT_SUCCESS);
    }

    tok = lexer(pre);
    if (option_flags & OPT_DUMP_TOKENS) {
        TokenNode *p;

        for (p = tok; p != NULL; p = p->next) {
            printf("%s:%d:%-3d =>   token: %-15s lexeme: `%s'\n", p->src_file, p->src_line,
            p->src_column, token_table[p->token*2], p->lexeme);
        }
        exit(EXIT_SUCCESS);
    }

    ic_init();
    tree = parser(tok);

    if (option_flags & OPT_SHOW_STATS) {
        extern unsigned number_of_pre_tokens;
        extern unsigned number_of_c_tokens;
        extern unsigned number_of_ast_nodes;

        printf("%u preprocessing tokens were created (aprox)\n", number_of_pre_tokens);
        printf("%u C tokens were created (aprox)\n", number_of_c_tokens);
        printf("%u AST nodes were created (aprox)\n", number_of_ast_nodes);
    }
    printf("%d warning and %d error generated\n", warning_count, error_count);

    if (option_flags & OPT_ANALYZE)
        exit(EXIT_SUCCESS);

	return 0;
}
