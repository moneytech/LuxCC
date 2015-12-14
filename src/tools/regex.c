/*
    Regex with backtracking.

        Recognized grammar

    expr     -> term { "|" term }
    term     -> "^" rep-fact { rep-fact } "$"
    rep-fact -> factor [ ( "*" | "+" | "?" ) ]
    factor   -> ordinary_char | "(" expr ")" | "." | "\" char
*/

#include "regex.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>

typedef struct Tree Tree;

static jmp_buf env;
static char *curr_tok;
static char *text_start; /* used for '^' */

static void match(char expected);
static Tree *new_tree_node(int op);

static Tree *expr(void);
static Tree *term(void);
static Tree *rep_fact(void);
static Tree *factor(void);
static Tree *ordinary_char(void);
static Tree *parse(char *regex);
static char *eval(Tree *t, char *text);

enum {
    SYNERR = 1,
    INTERR
};

static int last_error = 0;

static const char *error_messages[] = {
    "",
    "syntax error",     /* SYNERR */
    "internal error"    /* INTERR */
};

const char *regex_get_error(void)
{
    return error_messages[last_error];
}

void match(char expected)
{
    if (*curr_tok == expected)
        ++curr_tok;
    else
        longjmp(env, 1);
}

enum {  /* Tree.op */
    ALTERNATIVE     = 255,  /* "|" */
    ZERO_OR_MORE,           /* "*" */
    ONE_OR_MORE,            /* "+" */
    ZERO_OR_ONE,            /* "?" */
    ANY_CHAR,               /* "." */
    LITERAL_CHAR,           /* literal character */
    /* perl-like backslash sequences */
    DIGIT           = 'd',  /* "\d" (decimal digit) */
    NON_DIGIT       = 'D',  /* "\D" */
    ALPHANUM        = 'w',  /* "\w" (alphanumeric + "_") */
    NON_ALPHANUM    = 'W',  /* "\W" */
    WHITESPACE      = 's',  /* "\s" (any whitespace character) */
    NON_WHITESPACE  = 'S',  /* "\S" */
};

struct Tree {
    int op;
    char c;     /* literal character */
    char anchored_start, anchored_end;
    Tree *child[2];
};

Tree *new_tree_node(int op)
{
    Tree *n;

    n = calloc(1, sizeof(Tree));
    n->op = op;

    return n;
}

/* expr -> term { "|" term } */
Tree *expr(void)
{
    Tree *n;

    n = term();
    while (*curr_tok == '|') {
        Tree *n2;

        match('|');
        n2 = new_tree_node(ALTERNATIVE);
        n2->child[0] = n;
        n2->child[1] = term();
        n = n2;
    }

    return n;
}

/* term -> "^" rep-fact { rep-fact } "$" */
Tree *term(void)
{
    Tree *n, *n2;
    int caret;

    caret = 0;
    if (*curr_tok == '^') {
        caret = 1;
        match('^');
    }
    n  = n2 = rep_fact();
    n->anchored_start = caret;
    while (*curr_tok!=')' && *curr_tok!='|' && *curr_tok!='$' && *curr_tok!='\0') {
        while (n2->child[0] != NULL)
            n2 = n2->child[0];
        n2->child[0] = rep_fact();
        n2 = n2->child[0];
    }
    if (*curr_tok == '$') {
        n2->anchored_end = 1;
        match('$');
    }

    return n;
}

/* rep-fact -> factor [ ( "*" | "+" | "?" ) ] */
Tree *rep_fact(void)
{
    Tree *n, *n2;

    n = factor();
    n2 = NULL;
    if (*curr_tok == '*')
        n2 = new_tree_node(ZERO_OR_MORE);
    else if (*curr_tok == '+')
        n2 = new_tree_node(ONE_OR_MORE);
    else if (*curr_tok == '\?')
        n2 = new_tree_node(ZERO_OR_ONE);
    if (n2 != NULL) {
        match(*curr_tok);
        n2->child[1] = n;
        n = n2;
    }

    return n;
}

/* factor -> ordinary_char | "(" expr ")" | "." | "\" char */
Tree *factor(void)
{
    Tree *n;

    if (*curr_tok == '(') {
        match('(');
        n = expr();
        match(')');
    } else if (*curr_tok == '.') {
        n = new_tree_node(ANY_CHAR);
        match(*curr_tok);
    } else if (*curr_tok == '\\') {
        ++curr_tok;
        switch (*curr_tok) {
        case 'd':
        case 'D':
        case 'w':
        case 'W':
        case 's':
        case 'S':
            n = new_tree_node(*curr_tok);
            match(*curr_tok);
            break;
        default:
            /* if metacharacter, turn off its special meaning */
            n = ordinary_char();
            break;
        }
    } else {
        n = ordinary_char();
    }

    return n;
}

/* ordinary_char -> literal character with no special meaning */
Tree *ordinary_char(void)
{
    Tree *n;

    n = new_tree_node(LITERAL_CHAR);
    n->c = *curr_tok;
    match(*curr_tok);

    return n;
}

/* eval: recursively match the regex t onto text */
char *eval(Tree *t, char *text)
{
    char *p, *q;

    if (t == NULL) /* match */
        return text;

    if (t->anchored_start && text!=text_start)
        return NULL;
    if (t->anchored_end && (*text=='\0'||*(text+1)!='\0'))
        return NULL;

    switch (t->op) {
    case ALTERNATIVE:
        if ((p=eval(t->child[0], text)) == NULL)
            return eval(t->child[1], text);
        return p;
    case ONE_OR_MORE:
        if (eval(t->child[1], text) == NULL)
            return NULL;
        /* fall through */
    case ZERO_OR_MORE:
        /* skip over the longest string that matches the starred operand */
        q = text;
        while ((p=eval(t->child[1], q))!=NULL && p!=q)
            q = p;
        /* backtrack if the rest of the string doesn't match the rest of the pattern */
        while (q >= text) {
            if ((p=eval(t->child[0], q)) != NULL)
                return p;
            --q;
        }
        return NULL;
    case ZERO_OR_ONE:
        /* backtrack if the operand of ? causes the rest of the pattern to fail */
        if ((p=eval(t->child[1], text)) != NULL
        &&  (p=eval(t->child[0], p)) != NULL)
            return p;
        return eval(t->child[0], text);
    case ANY_CHAR:
        return (*text != '\0') ? eval(t->child[0], text+1) : NULL;
    case LITERAL_CHAR:
        return (t->c == *text) ? eval(t->child[0], text+1) : NULL;
    case DIGIT:
        return (isdigit(*text)) ? eval(t->child[0], text+1) : NULL;
    case NON_DIGIT:
        return (*text!='\0' && !isdigit(*text)) ? eval(t->child[0], text+1) : NULL;
    case ALPHANUM:
        return (isalnum(*text) || *text=='_') ? eval(t->child[0], text+1) : NULL;
    case NON_ALPHANUM:
        return (*text!='\0' && !isalnum(*text) && *text!='_') ? eval(t->child[0], text+1) : NULL;
    case WHITESPACE:
        return (isspace(*text)) ? eval(t->child[0], text+1) : NULL;
    case NON_WHITESPACE:
        return (*text!='\0' && !isspace(*text)) ? eval(t->child[0], text+1) : NULL;
    default: /* unreachable */
        return NULL;
    }
}

Tree *parse(char *regex)
{
    Tree *t;

    curr_tok = regex;
    if (setjmp(env) == 0) {
        t = expr();
    } else {
        last_error = SYNERR;
        return NULL;
    }
    if (*curr_tok != '\0') {
        /* something wrong went undetected during parsing */
        last_error = INTERR;
        return NULL;
    }

    return t;
}

void free_tree(Tree *t)
{
    if (t == NULL)
        return;
    free_tree(t->child[0]);
    free_tree(t->child[1]);
    free(t);
}

int regex_match(char *regex, char *text)
{
    Tree *t;
    int found;

    if ((t=parse(regex)) == NULL)
        return -1;
    found = 0;
    text_start = text;
    do {
        if (eval(t, text) != NULL) {
            found = 1;
            goto done;
        }
    } while (*text++ != '\0');
done:
    free_tree(t);
    return found;
}

int regex_match2(char *regex, char *text, char **start, char **end)
{
    char *p;
    Tree *t;
    int found;

    if ((t=parse(regex)) == NULL)
        return -1;
    found = 0;
    text_start = text;
    do {
        if ((p=eval(t, text)) != NULL) {
            *start = text;
            *end = p;

            found = 1;
            goto done;
        }
    } while (*text++ != '\0');
done:
    free_tree(t);
    return found;
}
