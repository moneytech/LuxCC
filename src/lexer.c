#include "lexer.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#define TRUE  1
#define FALSE 0

#define ERROR(...)\
    fprintf(stderr, "%s:%d: error: ", pre_tok->file, pre_tok->src_line),\
    fprintf(stderr, __VA_ARGS__),\
    fprintf(stderr, "\n"),\
    exit(EXIT_FAILURE)


#define equal(s, t)     (strcmp(s, t) == 0)
#define not_equal(s, t) (strcmp(s, t) != 0)
int get_esc_seq_val(char **c);
void check_integer_constant(char *ic);
static PreTokenNode *pre_tok; /* declared global so ERROR can access its content */

/*
 * Table that contains token-name/lexeme pairs.
 * It's indexed by `Token' enumeration constants.
 * The main use is for diagnostic messages.
 */
const char *token_table[] = {
    "LBRACKET", "[",
    "RBRACKET", "]",
    "LPAREN", "(",
    "RPAREN", ")",
    "LBRACE", "{",
    "RBRACE", "}",
    "DOT", ".",
    "ARROW", "->",
    "INC", "++",
    "DEC", "--",
    "AMPERSAND", "&",
    "ASTERISK", "*",
    "PLUS", "+",
    "MINUS", "-",
    "COMPLEMENT", "~",
    "NEGATION", "!",
    "DIV", "/",
    "MOD", "%",
    "LSHIFT", "<<",
    "RSHIFT", ">>",
    "LT", "<",
    "GT", ">",
    "LET", "<=",
    "GET", ">=",
    "EQ", "==",
    "NEQ", "!=",
    "BW_XOR", "^",
    "BW_OR", "|",
    "AND", "&",
    "OR", "||",
    "CONDITIONAL", "?",
    "COLON", ":",
    "SEMICOLON", ";",
    "ELLIPSIS", "...",
    "COMMA", ",",
    "ASSIGN", "=",
    "MUL_ASSIGN", "*=",
    "DIV_ASSIGN", "/=",
    "MOD_ASSIGN", "%=",
    "PLUS_ASSIGN", "+=",
    "MINUS_ASSIGN", "-=",
    "LSHIFT_ASSIGN", "<<=",
    "RSHIFT_ASSIGN", ">>=",
    "BW_AND_ASSIGN", "&=",
    "BW_XOR_ASSIGN", "^=",
    "BW_OR_ASSIGN", "|=",
    "AUTO", "auto",
    "BREAK", "break",
    "CASE", "case",
    "CHAR", "char",
    "CONST", "const",
    "CONTINUE", "continue",
    "DEFAULT", "default",
    "DO", "do",
    "DOUBLE", "double",
    "ELSE", "else",
    "ENUM", "enum",
    "EXTERN", "extern",
    "FLOAT", "float",
    "FOR", "for",
    "GOTO", "goto",
    "IF", "if",
    "INLINE", "inline",
    "INT", "int",
    "LONG", "long",
    "REGISTER", "register",
    "RESTRICT", "restrict",
    "RETURN", "return",
    "SHORT", "short",
    "SIGNED", "signed",
    "SIZEOF", "sizeof",
    "STATIC", "static",
    "STRUCT", "struct",
    "SWITCH", "switch",
    "TYPEDEF", "typedef",
    "UNION", "union",
    "UNSIGNED", "unsigned",
    "VOID", "void",
    "VOLATILE", "volatile",
    "WHILE", "while",
    "ID", "identifier",
    "STRLIT", "string literal",
    "ICONST", "integer constant",
    "EOF", "end-of-file",
    "TYPEDEFNAME", "typedef-name"
};

const struct Keyword {
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

static const int
number_of_keywords = sizeof(keywords_table)/sizeof(struct Keyword);

int cmp_kw(const void *p1, const void *p2)
{
   struct Keyword *x1 = (struct Keyword *)p1;
   struct Keyword *x2 = (struct Keyword *)p2;

   return strcmp(x1->str, x2->str);
}

Token lookup_id(char *s)
{
    struct Keyword key, *res;

    key.str = s;
    res = bsearch(&key, keywords_table, number_of_keywords, sizeof(struct Keyword), cmp_kw);
    if (res == NULL)
        return TOK_ID;
    else
        return res->tok;
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

const struct Punctuator {
    char *str;
    Token tok;
} punctuators_table[] = {
    {"!", TOK_NEGATION},
    {"!=", TOK_NEQ},
    {"%", TOK_MOD},
    {"%=", TOK_MOD_ASSIGN},
    {"&", TOK_AMPERSAND},
    {"&&", TOK_AND},
    {"&=", TOK_BW_AND_ASSIGN},
    {"(", TOK_LPAREN},
    {")", TOK_RPAREN},
    {"*", TOK_ASTERISK},
    {"*=", TOK_MUL_ASSIGN},
    {"+", TOK_PLUS},
    {"++", TOK_INC},
    {"+=", TOK_PLUS_ASSIGN},
    {",", TOK_COMMA},
    {"-", TOK_MINUS},
    {"--", TOK_DEC},
    {"-=", TOK_MINUS_ASSIGN},
    {"->", TOK_ARROW},
    {".", TOK_DOT},
    {"...", TOK_ELLIPSIS},
    {"/", TOK_DIV},
    {"/=", TOK_DIV_ASSIGN},
    {":", TOK_COLON},
    {";", TOK_SEMICOLON},
    {"<", TOK_LT},
    {"<<", TOK_LSHIFT},
    {"<<=", TOK_LSHIFT_ASSIGN},
    {"<=", TOK_LET},
    {"=", TOK_ASSIGN},
    {"==", TOK_EQ},
    {">", TOK_GT},
    {">=", TOK_GET},
    {">>", TOK_RSHIFT},
    {">>=", TOK_RSHIFT_ASSIGN},
    {"?", TOK_CONDITIONAL},
    {"[", TOK_LBRACKET},
    {"]", TOK_RBRACKET},
    {"^", TOK_BW_XOR},
    {"^=", TOK_BW_XOR_ASSIGN},
    {"{", TOK_LBRACE},
    {"|", TOK_BW_OR},
    {"|=", TOK_BW_OR_ASSIGN},
    {"||", TOK_OR},
    {"}", TOK_RBRACE},
    {"~", TOK_COMPLEMENT},
};

static const int
number_of_punctuators = sizeof(punctuators_table)/sizeof(struct Punctuator);

int cmp_punct(const void *p1, const void *p2)
{
   struct Punctuator *x1 = (struct Punctuator *)p1;
   struct Punctuator *x2 = (struct Punctuator *)p2;

   return strcmp(x1->str, x2->str);
}

/*
 * Convert preprocessing tokens
 * to compiler tokens.
 */
TokenNode *lexer(PreTokenNode *pre_token_list)
{
    TokenNode *first, *tok;
    // PreTokenNode *pre_tok;

    pre_tok = pre_token_list;
    first = tok = malloc(sizeof(TokenNode)); /* this node is deleted later */
    for (; pre_tok != NULL; pre_tok = pre_tok->next) {
        if (pre_tok->deleted)
            continue;

        switch (pre_tok->token) {
        case PRE_TOK_EOF:
            tok->next = new_token(TOK_EOF, pre_tok);
            break;
        case PRE_TOK_PUNCTUATOR: {
            struct Punctuator key, *res;

            key.str = pre_tok->lexeme;
            res = bsearch(&key, punctuators_table, number_of_punctuators, sizeof(struct Punctuator), cmp_punct);
            if (res == NULL) { /* this shouldn't happen! */
                fprintf(stderr, "lexer bug: bsearch returned NULL in PRE_TOK_PUNCTUATOR case\n");
                exit(1);
            }
            tok->next = new_token(res->tok, pre_tok);
            break;
        }
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
            ERROR("expecting hexadecimal digits after \\x");
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
            ERROR("invalid integer constant `%s'", ic);
            break;
        } /* switch (state) */
        c++;
    } /* while (TRUE) */
}
