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
        printf("%s:%-3d =>   token: %-3d lexeme: `%s'\n", tok->file, tok->src_line,
        tok->token, tok->lexeme);
        tok = tok->next;
    }
    // parser(tok);
	return 0;
}
