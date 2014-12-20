#include <stdio.h>
#include <string.h>
#include "parser.h"

int main(void)
{
    PreTokenNode *pre;
    TokenNode *tok;

    pre = preprocess("src/test.c");
    tok = lexer(pre);
    // while (tok != NULL)
        // printf("file:%s, line:%d, %d, %s\n", tok->file, tok->src_line, tok->token, tok->lexeme),
        // tok = tok->next;
    parser(tok);
	return 0;
}
