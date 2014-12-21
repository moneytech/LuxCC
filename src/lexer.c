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

struct {
    char *str;
    Token tok;
} keywords_table[] = {
    {"auto", TOK_AUTO},
    {"break", TOK_BREAK},
    {"case", TOK_CASE},
    {"char", TOK_CHAR},
    {"const", TOK_CONST},
    {"continue", TOK_CONTINUE},
    {"default", TOK_DEFAULT},
    {"do", TOK_DO},
    {"double", TOK_DOUBLE},
    {"else", TOK_ELSE},
    {"enum", TOK_ENUM},
    {"extern", TOK_EXTERN},
    {"float", TOK_FLOAT},
    {"for", TOK_FOR},
    {"goto", TOK_GOTO},
    {"if", TOK_IF},
    {"inline", TOK_INLINE},
    {"int", TOK_INT},
    {"long", TOK_LONG},
    {"register", TOK_REGISTER},
    {"restrict", TOK_RESTRICT},
    {"return", TOK_RETURN},
    {"short", TOK_SHORT},
    {"signed", TOK_SIGNED},
    {"sizeof", TOK_SIZEOF},
    {"static", TOK_STATIC},
    {"struct", TOK_STRUCT},
    {"switch", TOK_SWITCH},
    {"typedef", TOK_TYPEDEF},
    {"union", TOK_UNION},
    {"unsigned", TOK_UNSIGNED},
    {"void", TOK_VOID},
    {"volatile", TOK_VOLATILE},
    {"while", TOK_WHILE}
};

Token lookup_id(char *s)
{
    static int kt_size = sizeof(keywords_table)/sizeof(keywords_table[0]);

    int i;
    for (i = 0; i < kt_size; i++)
        if (strcmp(s, keywords_table[i].str) == 0)
            return keywords_table[i].tok;
    return TOK_ID;
}

TokenNode *new_token(Token token, PreTokenNode *ptok)
{
    TokenNode *temp;

    temp = malloc(sizeof(TokenNode));
    temp->token = token;
    temp->lexeme = ptok->lexeme;
    temp->src_line = ptok->src_line;
    temp->file = ptok->file;
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
    first = tok = malloc(sizeof(TokenNode)); /* this node is deleted later */
    for (; pre_tok != NULL; pre_tok = pre_tok->next) {
        if (pre_tok->deleted)
            continue;

        switch (pre_tok->token) {
        case PRE_TOK_EOF:
            tok->next = new_token(TOK_EOF, pre_tok);
            break;
        case PRE_TOK_PUNCTUATOR:
            if (equal(pre_tok->lexeme, "["))
                tok->next = new_token(TOK_LBRACKET, pre_tok);
            else if (equal(pre_tok->lexeme, "]"))
                tok->next = new_token(TOK_RBRACKET, pre_tok);
            else if (equal(pre_tok->lexeme, "("))
                tok->next = new_token(TOK_LPAREN, pre_tok);
            else if (equal(pre_tok->lexeme, ")"))
                tok->next = new_token(TOK_RPAREN, pre_tok);
            else if (equal(pre_tok->lexeme, "{"))
                tok->next = new_token(TOK_LBRACE, pre_tok);
            else if (equal(pre_tok->lexeme, "}"))
                tok->next = new_token(TOK_RBRACE, pre_tok);
            // else if (equal(pre_tok->lexeme, "."))
            // else if (equal(pre_tok->lexeme, "->"))
            else if (equal(pre_tok->lexeme, ";"))
                tok->next = new_token(TOK_SEMICOLON, pre_tok);
            else if (equal(pre_tok->lexeme, ","))
                tok->next = new_token(TOK_COMMA, pre_tok);
            else if (equal(pre_tok->lexeme, "*"))
                tok->next = new_token(TOK_ASTERISK, pre_tok);
            else if (equal(pre_tok->lexeme, "="))
                tok->next = new_token(TOK_ASSIGN, pre_tok);
            break;
        case PRE_TOK_NUM:
            check_integer_constant(pre_tok->lexeme);
            tok->next = new_token(TOK_ICONST, pre_tok);
            break;
        case PRE_TOK_ID:
            tok->next = new_token(TOK_ID, pre_tok);
            tok->next->token = lookup_id(pre_tok->lexeme);
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
            tok->next = new_token(TOK_ICONST, pre_tok);
            tok->next->lexeme = malloc(strlen(buf)+1); /* replace prev lexeme */
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
            tok->next = new_token(TOK_STRLIT, pre_tok);
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
