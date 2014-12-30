#include <stdio.h>
#include <string.h>
#include "parser.h"

int main(void)
{
    PreTokenNode *pre;
    TokenNode *tok;

    /* only preprocess */
    pre = preprocess("src/test.c");
    // while (pre != NULL) {
        // if (!pre->deleted || pre->token==PRE_TOK_NL)
            // printf("%s ", pre->lexeme);
        // pre = pre->next;
    // }

    /* only tokenize */
    tok = lexer(pre);
    while (tok != NULL) {
        printf("%s:%d:%-3d =>   token: %-15s lexeme: `%s'\n", tok->src_file, tok->src_line,
        tok->src_column, token_table[tok->token*2], tok->lexeme);
        tok = tok->next;
    }

    /* only parse */
    // parser(tok);
    // printf("Parsed OK\n");
	return 0;
}
