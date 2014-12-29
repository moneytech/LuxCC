#include <stdio.h>
#include <string.h>
#include "parser.h"

int main(void)
{
    PreTokenNode *pre;
    TokenNode *tok;

    pre = preprocess("src/test.c");
    tok = lexer(pre);
    while (tok != NULL) {
        printf("%s:%-3d =>   token: %-15s lexeme: `%s'\n", tok->file, tok->src_line,
        token_table[tok->token*2], tok->lexeme);
        tok = tok->next;
    }
    // parser(tok);
	return 0;
}
