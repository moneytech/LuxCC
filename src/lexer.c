#include "lexer.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#define ERROR() ;
#define TRUE  1
#define FALSE 0


#define equal(s, t)     (strcmp(s, t) == 0)
#define not_equal(s, t) (strcmp(s, t) != 0)
int get_esc_seq_val(char **c);
void check_integer_constant(char *ic);


TokenNode *new_token(Token token, char *lexeme)
{
    TokenNode *temp;

    temp = malloc(sizeof(TokenNode));
    temp->token = token;
    temp->lexeme = lexeme;
    temp->next = NULL;

    return temp;
}


/*
 * Convert preprocessing tokens
 * to compiler tokens.
 */
TokenNode *lexer(PreTokenNode *pre_token_list)
{
    PreTokenNode *pre_tok;
    TokenNode *first, *tok;

    pre_tok = pre_token_list;
    first = tok = new_token(0, NULL);
    for (; pre_tok != NULL; pre_tok = pre_tok->next) {
        if (pre_tok->deleted)
            continue;

        switch (pre_tok->token) {
        case PRE_TOK_EOF:
            break;
        case PRE_TOK_PUNCTUATOR:
            if (equal(pre_tok->lexeme, "["))
                tok->next = new_token(TOK_LBRACKET, pre_tok->lexeme);
            else if (equal(pre_tok->lexeme, "]"))
                tok->next = new_token(TOK_RBRACKET, pre_tok->lexeme);
            /* ... */
            break;
        case PRE_TOK_NUM:
            check_integer_constant(pre_tok->lexeme);
            tok->next = new_token(TOK_ICONST, pre_tok->lexeme);
            break;
        case PRE_TOK_ID:
            tok->next = new_token(TOK_ID, pre_tok->lexeme);
            break;
        case PRE_TOK_CHACON: {
            char buf[16], *p;

            /*
             * If the character constant has more than
             * one character, set as its value the value
             * of its rightmost character.
             */
            p = pre_tok->lexeme;
            while (*p != '\0') {
                if (*p == '\\') {
                    ++p; /* skip \ */
                    sprintf(buf, "%d", get_esc_seq_val(&p));
                } else {
                    sprintf(buf, "%d", *p);
                    ++p;
                }
            }
            tok->next = new_token(TOK_ICONST, NULL);
            tok->next->lexeme = malloc(strlen(buf)+1);
            strcpy(tok->next->lexeme, buf);
        }
            break;
        case PRE_TOK_STRLIT: {
            char *src, *dest;

            src = dest = pre_tok->lexeme;
            while (*src != '\0') {
                if (*src == '\\') {
                    ++src; /* skip \ */
                    *dest++ = (char)get_esc_seq_val(&src);
                } else {
                    *dest++ = *src++;
                }
            }
            *dest = *src; /* copy '\0' */
            tok->next = new_token(TOK_STRLIT, pre_tok->lexeme);
        }
            break;
        case PRE_TOK_NL:
            break;
        case PRE_TOK_OTHER:
            break;
        }
        tok = tok->next;
    }
    tok = first->next;
    free(first);
    return tok;
}

int isodigit(int c)
{
    return (c!='8')&&(c!='9')&&(isdigit(c));
}

int get_esc_seq_val(char **c)
{
    switch (*(*c)++) {
    case 'n':
        return '\n';
    case 't':
        return '\t';
    case 'v':
        return '\v';
    case 'b':
        return '\b';
    case 'r':
        return '\r';
    case 'f':
        return '\f';
    case 'a':
        return '\a';
    case '\\':
        return '\\';
    case '\?':
        return '\?';
    case '\'':
        return '\'';
    case '\"':
        return '\"';
    case 'x': {
        char buf[16], *p = buf;
        int val;

        if (isxdigit(**c)) {
            do
                *p++=*(*c)++;
            while (isxdigit(**c));
            *p = '\0';
            sscanf(buf, "%x", &val);
            return (char)val;
        } else {
            // error: expecting hexadecimal digits after \\x
            exit(1);
        }
    }
    default:
        --*c;
        if (isodigit(**c)) {
            char buf[16], *p = buf;
            int val;

            do
                *p++=*(*c)++;
            while (isodigit(**c));
            *p = '\0';
            sscanf(buf, "%o", &val);
            return (char)val;
        } else {
            return *(*c)++; /* unknown escape sequence, the backslash is ignored */
        }
    }
}

/*
 * Check the validity of the integer constant `ic'.
 */
void check_integer_constant(char *ic)
{
    enum {
        START,
        INDEC,
        INZOH, /* zero, octal, or hexadecimal */
        INHEX1,
        INHEX2,
        INOCT,
        INLSUF,
        INUSUF,
        INLUSUF,
        INULSUF,
        INERROR
    };
    int state;
    char *c = ic;

    state = START;
    while (TRUE) {
        switch (state) {
        case START: /* the first char is always a digit */
            state = (*c!='0')?INDEC:INZOH;
            break;
        case INDEC:
            if (isdigit(*c))
                ;
            else if (*c=='l' || *c=='L')
                state = INLSUF;
            else if (*c=='u' || *c=='U')
                state = INUSUF;
            else if (*c=='\0')
                return;
            else
                state = INERROR;
            break;
        case INZOH:
            if (isodigit(*c))
                state = INOCT;
            else if (*c=='x' || *c=='X')
                state = INHEX1;
            else if (*c=='l' || *c=='L')
                state = INLSUF;
            else if (*c=='u' || *c=='U')
                state = INUSUF;
            else if (*c=='\0')
                return;
            else
                state = INERROR;
            break;
        case INHEX1:
            if (isxdigit(*c))
                state = INHEX2;
            else
                state = INERROR;
            break;
        case INHEX2:
            if (isxdigit(*c))
                ;
            else if (*c=='l' || *c=='L')
                state = INLSUF;
            else if (*c=='u' || *c=='U')
                state = INUSUF;
            else if (*c == '\0')
                return;
            else
                state = INERROR;
            break;
        case INOCT:
            if (isodigit(*c))
                ;
            else if (*c=='l' || *c=='L')
                state = INLSUF;
            else if (*c=='u' || *c=='U')
                state = INUSUF;
            else if (*c == '\0')
                return;
            else
                state = INERROR;
            break;
        case INLSUF:
            if (*c=='u' || *c=='U')
                state = INLUSUF;
            else if (*c == '\0')
                return;
            else
                state = INERROR;
            break;
        case INUSUF:
            if (*c=='l' || *c=='L')
                state = INLUSUF;
            else if (*c == '\0')
                return;
            else
                state = INERROR;
            break;
        case INLUSUF:
        case INULSUF:
            if (*c != '\0')
                state = INERROR;
            else
                return;
            break;
        case INERROR:
            fprintf(stderr, "invalid integer constant: '%s'\n", ic);
            exit(1);
            break;
        } /* switch (state) */
        c++;
    } /* while (TRUE) */
}
