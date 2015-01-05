#include <stdio.h>
#include <string.h>
#include "parser.h"

int main(int argc, char *argv[])
{
    PreTokenNode *pre;
    TokenNode *tok;

    /* only preprocess */
    pre = preprocess(argv[1]); //"src/test.c");
    // while (pre != NULL) {
        // if (!pre->deleted || pre->token==PRE_TOK_NL)
            // printf("%s ", pre->lexeme);
        // pre = pre->next;
    // }
    // extern unsigned number_of_pre_tokens;
    // printf("%u preprocessing tokens were created (aprox)\n", number_of_pre_tokens);

    /* only tokenize */
    tok = lexer(pre);
    // while (tok != NULL) {
        // printf("%s:%d:%-3d =>   token: %-15s lexeme: `%s'\n", tok->src_file, tok->src_line,
        // tok->src_column, token_table[tok->token*2], tok->lexeme);
        // tok = tok->next;
    // }
    // extern unsigned number_of_c_tokens;
    // printf("%u C tokens were created (aprox)\n", number_of_c_tokens);

    /* only parse */
    parser(tok);
    // extern unsigned number_of_ast_nodes;
    // printf("%u AST nodes were created (aprox)\n", number_of_ast_nodes);
	return 0;
}
