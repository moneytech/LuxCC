#include "pre.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "util.h"


#define TRUE  1
#define FALSE 0

#define ERROR(...)\
    fprintf(stderr, "%s:%d: error: ", curr_tok->file, curr_tok->src_line),\
    fprintf(stderr, __VA_ARGS__),\
    fprintf(stderr, "\n"),\
    exit(EXIT_FAILURE)

#define ERROR2(...)\
    fprintf(stderr, "%s:%d: error: ", curr_source_file, curr_line),\
    fprintf(stderr, __VA_ARGS__),\
    fprintf(stderr, "\n"),\
    exit(EXIT_FAILURE)

typedef enum {
    STATE_START,
    STATE_DONE,
    STATE_INID,
    STATE_INNUM,
    STATE_INCHAR,
    STATE_INSTR,
    STATE_INCOMMENT1,
    STATE_INCOMMENT2,
    STATE_INLINECOMMENT,
    STATE_INCMD
} State;

static char *buf, *curr, *curr_source_file;
static char token_string[128];
static PreTokenNode *curr_tok, *token_list;
static int curr_line/*, curr_column*/;


typedef enum {
    SIMPLE,
    PARAMETERIZED
} MacroKind;

typedef struct Macro Macro;
struct Macro {
    char *name;
    MacroKind kind;
    PreTokenNode *rep, *params;
    Macro *next;
};

static
PreTokenNode *new_node(PreToken token, char *lexeme)
{
    PreTokenNode *temp;

    temp = malloc(sizeof(PreTokenNode));
    temp->token = token;
    temp->lexeme = strdup(lexeme);
    temp->next = NULL;
    temp->deleted = FALSE;
    temp->src_line = curr_line;
    temp->file = curr_source_file;

    return temp;
}

static
PreToken lookahead(int i)
{
    PreTokenNode *p;

    p = curr_tok;
    while (--i)
        p = p->next;
    return p->token;
}

static
char *get_lexeme(int i)
{
    PreTokenNode *p;

    p = curr_tok;
    while (--i)
        p = p->next;
    return p->lexeme;
}

PreToken get_token(void);

/*
 * Tokenize the buffer 'buf' and return
 * the resulting chain of tokens.
 */
PreTokenNode *tokenize(void)
{
    PreToken t;
    PreTokenNode *n, *p;

    curr_line = 1; /* initialize line counter */
    t = get_token();
    n = p = new_node(t, token_string);
    p->next_char = *curr;

    if (t == PRE_TOK_EOF)
        return n;

    do {
        t = get_token();
        p->next = new_node(t, token_string);
        p->next->next_char = *curr;
        p = p->next;

    } while (t != PRE_TOK_EOF);

    return n;
}

static char *str_tok[] = {
    "EOF",
    "punctuator"
    "preprocessor number"
    "identifier",
    "character constant",
    "string literal",
    "new line",
    "other"
};

static
void match(PreToken x)
{
    if (curr_tok->token == x)
        curr_tok = curr_tok->next;
    else
        ERROR("expecting: `%s'; found: `%s'", str_tok[x], str_tok[curr_tok->token]);
}

void match2(PreToken x)
{
    if (curr_tok->token == x) {
        curr_tok->deleted = TRUE;
        curr_tok = curr_tok->next;
    } else {
        ERROR("expecting: `%s'; found: `%s'", str_tok[x], str_tok[curr_tok->token]);
    }
}

/*
 * Load the file located at `file_path' into the global buffer `buf',
 * and perform end-of-line replacing (DOS's CRLF are replaced for a
 * single LF) and line splicing.
 */
static
// void init(FILE *fp)
void init(char *file_path)
{
    FILE *fp;
    int buf_size;

    fp = fopen(file_path, "rb");
    if (fp == NULL) {
        fprintf(stderr, "Error reading file `%s'\n", file_path);
        exit(1);
    }

    fseek(fp, 0, SEEK_END);
    buf_size = ftell(fp); /* number of chars of the file */
    ++buf_size; /* makes room for '\0' */
    rewind(fp);
    curr = buf = malloc(buf_size);

    while (fgets(curr, 0x7FFFFFFF, fp) != NULL) {
        int line_len = strlen(curr);
#if WIN_LINE_ENDING
        /* replace CRLFs for LFs */
        if (line_len>1 && curr[line_len-2]=='\r') {
            curr[line_len-2]='\n', curr[line_len-1]='\0';
            --line_len;
        }
#endif
        /* join lines ending in \ with the next line (the last line must not end in \) */
        while (line_len>1 && curr[line_len-2]=='\\') {
            line_len -= 2; /* removes '\\' and '\n' */
            fgets(curr+line_len, 0x7FFFFFFF, fp);
            line_len += strlen(curr+line_len);
#if WIN_LINE_ENDING
            /* again, replace CRLFs for LFs */
            if (curr[line_len-2]=='\r') {
                curr[line_len-2]='\n', curr[line_len-1]='\0';
                --line_len;
            }
#endif
        }

        curr += line_len;
    }

    curr = buf;
    fclose(fp);
    /*
     * Set the current file global var. Each token has attached
     * the name of the file from where it was obtained. This is
     * used for diagnostic messages.
     */
    curr_source_file = strdup(file_path);
}

void preprocessing_file(void);

/*
 * Preprocess the source file and any file
 * included through #include, and return a
 * sequence of preprocessing tokens.
 */
PreTokenNode *preprocess(char *source_file)
{
    // FILE *fp;

    // curr_source_file = source_file;
    // fp = fopen(source_file, "rb");
    // init(fp);
    init(source_file);
    // fclose(fp);
    token_list = curr_tok = tokenize();
    preprocessing_file();
    // while (token_list != NULL) {
        // if (!token_list->deleted)
            // printf("%d, %s\n", token_list->token, token_list->lexeme);
        // token_list = token_list->next;
    // }
    return token_list;
}


PreToken get_token(void)
{
    int tok_str_ind = 0;
    PreToken token;
    State state = STATE_START;
    int save;

    while (state != STATE_DONE) {
        int c = *curr++;
        save = TRUE;

        switch (state) {
        case STATE_START:
            if (c==' ' || c=='\t') {
                save = FALSE;
            } else if (isdigit(c)) {
                state = STATE_INNUM;
            } else if (isalpha(c) || (c=='_')) {
                state = STATE_INID;
            } else if (c == '\'') {
                save = FALSE;
                state = STATE_INCHAR;
            } else if (c == '\"') {
                save = FALSE;
                state = STATE_INSTR;
            } else if ((c=='/') && ((*curr=='/')||(*curr=='*'))) { /* // or /* */
                save = FALSE;
                if (*curr == '/')
                    state = STATE_INLINECOMMENT;
                else if (*curr == '*')
                    state = STATE_INCOMMENT1;
            } else {
                state = STATE_DONE;
                switch (c) {
                case '\0':
                    save = FALSE;
                    token = PRE_TOK_EOF;
                    break;
                case '\n':
                    save = FALSE; // ?
                    token = PRE_TOK_NL;
                    ++curr_line;
                    break;
                /*
                 * single-char punctuators
                 */
                case '(':
                case ')':
                case '[':
                case ']':
                case '{':
                case '}':
                case ';':
                case ',':
                case '\?':
                case ':':
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                /*
                 * multi-char punctuators
                 */
#define SAVE_AND_ADVANCE() \
    token_string[tok_str_ind++] = (char)c, \
    c = *curr++
                case '+': /* +, ++ or += */
                    if (*curr=='+' || *curr=='=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '-': /* -, --, -> or -= */
                    if (*curr=='-' || *curr=='>' || *curr=='=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '*': /* * or *= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '/': /* / or /= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '%': /* % or %= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '^': /* ^ or ^= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '=': /* = or == */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '|': /* |, || or |= */
                    if (*curr=='|' || *curr=='=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '&': /* &, && or &= */
                    if (*curr=='&' || *curr=='=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '!': /* ! or != */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '#':
                    if (*curr == '#')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '<': /* <, <=, << or <<= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    else if (*curr == '<') {
                        SAVE_AND_ADVANCE();
                        if (*curr == '=')
                            SAVE_AND_ADVANCE();
                    }
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '>': /* >, >=, >> or >>= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    else if (*curr == '>') {
                        SAVE_AND_ADVANCE();
                        if (*curr == '=')
                            SAVE_AND_ADVANCE();
                    }
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '.': /* . or ... */
                    if (*curr=='.' && *(curr+1)=='.') {
                        SAVE_AND_ADVANCE();
                        SAVE_AND_ADVANCE();
                    }
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                default:
                    /*
                     * In ASCII, characters included in the 'other' group are @, $, `, and
                     * any control characters other than NUL.
                     */
                    token = PRE_TOK_OTHER;
                    break;
                } /* switch (c) */
            }
            break; /* STATE_START */
        case STATE_INNUM:
        case STATE_INID:
            if (!isalpha(c) && !isdigit(c) && (c!='_')) {
                save = FALSE;
                --curr;
                token = (state==STATE_INNUM)?PRE_TOK_NUM:PRE_TOK_ID;
                state = STATE_DONE;
            }
            break;
        case STATE_INCHAR:
            --curr;
            if (*curr=='\0' || *curr=='\n') {
                ERROR2("missing terminating \"'\" character");
            } else if (*curr == '\'') {
                ERROR2("empty character constant");
            } else {
                token_string[tok_str_ind++] = *curr++;
                while (*curr!='\'' || *(curr-1)=='\\') {
                    if (*curr=='\0' || *curr=='\n')
                        ERROR2("missing terminating \"'\" character");
                    token_string[tok_str_ind++] = *curr++;
                }
                ++curr; /* skip ' */
                save = FALSE;
                state = STATE_DONE;
                token = PRE_TOK_CHACON;
            }
            break;
        case STATE_INSTR:
            --curr;
            if (*curr=='\0' || *curr=='\n') {
                ERROR2("missing terminating '\"' character");
            } else {
                while (*curr!='"' || *(curr-1)=='\\') {
                    if (*curr=='\0' || *curr=='\n')
                        ERROR2("missing terminating '\"' character");
                    token_string[tok_str_ind++] = *curr++;
                }
                ++curr; /* skip " */
                save = FALSE;
                state = STATE_DONE;
                token = PRE_TOK_STRLIT;
            }
            break;
        case STATE_INCOMMENT1:
            save = FALSE;
            if (c == '\0')
                ERROR2("unterminated comment");
            else if (c == '*')
                state = STATE_INCOMMENT2;
            else if (c == '\n')
                ++curr_line;
            break;
        case STATE_INCOMMENT2:
            save = FALSE;
            if (c == '\0')
                ERROR2("unterminated comment");
            else if (c == '/')
                state = STATE_START;
            else if (c != '*') {
                state = STATE_INCOMMENT1;
                if (c == '\n')
                    ++curr_line;
            }
            break;
        case STATE_INLINECOMMENT:
            save = FALSE;
            if (c == '\n') {
                --curr;
                state = STATE_START;
            } else if (c == '\0') {
                --curr;
                state = STATE_START;
            }
            break;
        case STATE_DONE:
        default:
            /* scanner bug */
            break;
        } /* switch (state) */

        if (save)
            token_string[tok_str_ind++] = (char)c;
        if (state == STATE_DONE)
            token_string[tok_str_ind] = '\0';
    } /* while (state != STATE_DONE) */
    return token;
}

/* recursive parser functions */
void group(int skip);
void group_part(int skip);
void if_section(int skip);
int if_group(int skip);
int elif_groups(int skip);
int elif_group(int skip);
void else_group(int skip);
void endif_line(void);
void control_line(int skip);
void pp_tokens(int skip);
void preprocessing_token(int skip);


int is_group_part(void)
{
    if (lookahead(1) == PRE_TOK_EOF)
        return FALSE;

    if (!strcmp(get_lexeme(1), "#")
    && (!strcmp(get_lexeme(2), "elif")
    ||  !strcmp(get_lexeme(2), "else")
    ||  !strcmp(get_lexeme(2), "endif")))
        return FALSE;

    return TRUE;
}

/*
 * preprocessing_file = [ group ] end_of_file
 */
void preprocessing_file(void)
{
    if (lookahead(1) != PRE_TOK_EOF)
        group(FALSE);
    match(PRE_TOK_EOF);
}

/*
 * group = group_part { group_part }
 */
void group(int skip)
{
    group_part(skip);
    while (/*lookahead(1)!=PRE_TOK_EOF && */is_group_part())
        group_part(skip);
}

/*
 * group_part = [ pp_tokens ] new_line |
 *              if_section |
 *              control_line
 */
void group_part(int skip)
{
    if (!strcmp(get_lexeme(1), "#")) {
        if (!strcmp(get_lexeme(2), "if")
        ||  !strcmp(get_lexeme(2), "ifdef")
        ||  !strcmp(get_lexeme(2), "ifndef"))
            if_section(skip);
        else
            control_line(skip);
    } else {
        if (lookahead(1) != PRE_TOK_NL)
            pp_tokens(skip);
        match2(PRE_TOK_NL);
    }
}

/*
 * if_section = if_group [ elif_groups ] [ else_group ] endif_line
 */
void if_section(int skip)
{
    int if_cond, elif_cond = 0;

    if_cond = if_group(skip);
    if (!strcmp(get_lexeme(1), "#") && !strcmp(get_lexeme(2), "elif"))
        elif_cond = elif_groups(skip || if_cond);
    if (!strcmp(get_lexeme(1), "#") && !strcmp(get_lexeme(2), "else"))
        else_group(skip || if_cond || elif_cond);
    endif_line();
}

static Macro *lookup(char *s);

/*
 * if_group = "#" "if" constant_expression new_line [ group ] |
 *            "#" "ifdef" identifier new_line [ group ] |
 *            "#" "ifndef" identifier new_line [ group ]
 */
int if_group(int skip)
{
    int cond_res;

    /*
     * group_part() confirmed either #if, #ifdef or #ifndef
     */
    match2(PRE_TOK_PUNCTUATOR);
    if (!strcmp(get_lexeme(1), "if")) {
        match2(PRE_TOK_ID);

        /*
         * The only constant expressions supported
         * are decimal numbers or macro names that
         * expand directly into decimal numbers.
         */
        if (lookahead(1) == PRE_TOK_ID) {
            Macro *m;
            if ((m=lookup(get_lexeme(1))) != NULL)
                cond_res = atoi(m->rep->lexeme);
            else
                cond_res = 0; /* undefined macro names evaluate to zero */
        } else {
            cond_res = atoi(get_lexeme(1));
        }
        match2(lookahead(1)); /* id or number */
    } else if (!strcmp(get_lexeme(1), "ifdef")) {
        /* */
    } else { /* ifndef */
        match2(PRE_TOK_ID);
        cond_res = (lookup(get_lexeme(1))==NULL)?1:0;
        match2(lookahead(1));
    }
    match2(PRE_TOK_NL);
    /*
     * Skip if the condition evaluated to false or if the
     * if/ifdef/ifndef is inside a block being skipped.
     */
    skip = (cond_res==0)?1:skip;
    if (is_group_part())
        group(skip);

    return cond_res;
}

/*
 * elif_groups = elif_group { elif_group }
 */
int elif_groups(int skip)
{
    int cond_res;

    cond_res = elif_group(skip);
    while (!strcmp(get_lexeme(1), "#") && !strcmp(get_lexeme(2), "elif"))
        if (elif_group(skip || cond_res))
            cond_res = TRUE;
    return cond_res;
}

/*
 * elif_group = "#" "elif" constant_expression new_line [ group ]
 */
int elif_group(int skip)
{
    int cond_res;

    /*
     * if_section()/elif_groups() confirmed # and elif
     */
    match2(PRE_TOK_PUNCTUATOR);
    match2(PRE_TOK_ID);
    /* condition */
    cond_res = atoi(get_lexeme(1));
    match2(PRE_TOK_NUM);
    /* --- */
    match2(PRE_TOK_NL);
    if (is_group_part())
        group(skip || !cond_res);

    return cond_res;
}

/*
 * else_group = "#" "else" new_line [ group ]
 */
void else_group(int skip)
{
    /*
     * if_section() confirmed # and else
     */
    match2(PRE_TOK_PUNCTUATOR); /* # */
    match2(PRE_TOK_ID); /* else */
    match2(PRE_TOK_NL);
    if (is_group_part())
        group(skip);
}

/*
 * endif_line = "#" "endif" new_line
 */
void endif_line(void)
{
    if (!strcmp(get_lexeme(1), "#"))
        match2(PRE_TOK_PUNCTUATOR);
    else
        ERROR("`#endif' expected");
    if (!strcmp(get_lexeme(1), "endif"))
        match2(PRE_TOK_ID);
    else
        ERROR("`#endif' expected");
    match2(PRE_TOK_NL);
}

void simple_define(void);
void parameterized_define(void);

/*
control_line = "#" "include" pp_tokens new_line |
               "#" "define" identifier replacement_list new_line |
               "#" "define" identifier lparen [ identifier_list ] ")" replacement_list new_line |
               "#" "undef" identifier new_line |
               "#" "line" pp_tokens new_line |
               "#" "error" [ pp_tokens ] new_line |
               "#" "pragma" [ pp_tokens ] new_line |
               "#" new_line
*/
void control_line(int skip)
{
    if (skip)
        goto bottom;

    /* by group_part(), we know we have # */
    match2(PRE_TOK_PUNCTUATOR);

    /*
     * Identify and interpret directive.
     */
    if (!strcmp(get_lexeme(1), "include")) {
        /*
         * The only supported forms of the include directive are:
         *      "file.h"
         *      <file.h>
         */
        match2(PRE_TOK_ID);
        if (lookahead(1) == PRE_TOK_STRLIT) {
            /*
             * Search for the file in the same directory as
             * the file that contains the #include directive.
             */
            char *p;
            // FILE *fp;
            PreTokenNode *tokenized_file, *last;

            /*
             * Open the file.
             */
            p = curr_source_file+strlen(curr_source_file);
            while (p!=curr_source_file && *p!='/')
                p--;
            if (p == curr_source_file) {
                /* the file being processed is in the compiler's working directory */
                // fp = fopen(get_lexeme(1), "rb");
                init(get_lexeme(1));
            } else {
                int n;
                char *path;

                n = p-curr_source_file+1;
                path = malloc(n+strlen(get_lexeme(1))+1);
                strncpy(path, curr_source_file, n);
                path[n] = '\0';
                strcat(path, get_lexeme(1));
                // fp = fopen(path, "rb");
                init(path);
                free(path);
            }
            // init(fp);
            // fclose(fp);

            /* match filename */
            match2(lookahead(1));
            /* now at new-line... */

            /*
             * Tokenize the file's content and insert
             * the result right after the filename.
             */
            tokenized_file = last = tokenize();
            while (last->next->token != PRE_TOK_EOF)
                last = last->next;
            /* delete EOF token */
            free(last->next->lexeme);
            free(last->next);
            /* insert */
            last->next = curr_tok->next;
            curr_tok->next = tokenized_file;
        } else if (!strcmp(get_lexeme(1), "<")) {
            /* ... */
        } else {
            /* error */
        }
    } else if (!strcmp(get_lexeme(1), "define")) {
        match2(PRE_TOK_ID);
        if (lookahead(1) == PRE_TOK_ID) {
            if (curr_tok->next_char == '(')
                parameterized_define();
            else
                simple_define();
        } else {
            ERROR("define: name expected");
        }
    } else if (!strcmp(get_lexeme(1), "undef")) {
        ;
    } else if (!strcmp(get_lexeme(1), "\n")) {
        ;
    } else {
        ERROR("unknown directive `%s'", get_lexeme(1));
    }

bottom:
    /*
     * Mark the rest of the line as deleted.
     */
    while (lookahead(1) != PRE_TOK_NL)
        match2(lookahead(1));
    match2(PRE_TOK_NL);
}

#define MACRO_TABLE_SIZE 31

static Macro *macro_table[MACRO_TABLE_SIZE];

static
Macro *lookup(char *s)
{
	Macro *np;
	for(np = macro_table[hash(s)%MACRO_TABLE_SIZE]; np != NULL; np = np->next)
		if(strcmp(s, np->name) == 0)
			return np;
	return NULL;
}

static
void install(MacroKind kind, char *name, PreTokenNode *rep, PreTokenNode *params)
{
    Macro *np;
    unsigned int hashval;

    if ((np=lookup(name)) == NULL) {
        np = malloc(sizeof(Macro));
        np->kind = kind;
        np->name = name;
        np->rep = rep;
        np->params = params;
        hashval = hash(name)%MACRO_TABLE_SIZE;
        np->next = macro_table[hashval];
        macro_table[hashval] = np;
    } else {
        // free(np->rep);
        ERROR("macro `%s' redefined", name);
    }
    // np->rep = copy_string(rep);
}

/* duplicate a replacement list */
PreTokenNode *dup_rep_list(PreTokenNode *r)
{
    PreTokenNode *new_rep_list, *temp;

    /*
     * Test for empty replacement list.
     */
    if (r->token == PRE_TOK_NL)
        return NULL;

    /*
     * Make the duplicate.
     */
    new_rep_list = temp = new_node(r->token, r->lexeme);
    r = r->next;
    while (r->token != PRE_TOK_NL) {
        temp->next = new_node(r->token, r->lexeme);
        temp = temp->next;
        r = r->next;
    }

    return new_rep_list;
}

void simple_define(void)
{
    install(SIMPLE, get_lexeme(1), curr_tok->next, NULL);
}

void parameterized_define(void)
{
    PreTokenNode *rep;
    for (rep = curr_tok; strcmp(rep->lexeme, ")") != 0 ; rep = rep->next);
    install(PARAMETERIZED, get_lexeme(1), rep->next, curr_tok->next->next);
}

/*
 * pp_tokens = preprocessing_token { preprocessing_token }
 */
void pp_tokens(int skip)
{
    preprocessing_token(skip);
    while (lookahead(1) != PRE_TOK_NL)
        preprocessing_token(skip);
}

/*
preprocessing_token = header_name
                      identifier
                      pp_number
                      character_constant
                      string_literal
                      punctuator
                      each non-white-space character that cannot be one of the above
*/
void preprocessing_token(int skip)
{
    if (skip) {
        match2(lookahead(1));
        return;
    }

    Macro *m;
    if (lookahead(1)==PRE_TOK_ID && (m=lookup(get_lexeme(1)))!=NULL) {
        if (m->kind == SIMPLE) {
            /*
             * Object-like macro.
             */
            PreTokenNode *r, *old_next;

            /*
             * Test empty replacement list.
             */
            if ((r=dup_rep_list(m->rep)) == NULL)
                goto empty_rep_list1;
            /*
             * Insert the replacement list between the
             * identifier and the next token.
             */
            old_next = curr_tok->next;
            curr_tok->next = r;
            while (r->next != NULL)
                r = r->next;
            r->next = old_next;
empty_rep_list1:
            /*
             * Mark the identifier as deleted and advance.
             */
            match2(lookahead(1));
        } else {
// #define equal(s, t)     (strcmp(s->lexeme, t->lexeme) == 0)
// #define not_equal(s, t) (strcmp(s->lexeme, t->lexeme) != 0)
            /*
             * Parameterized macro.
             * Assume correct syntax and
             * proper use of the macro.
             */
            int pn; /* parenthesis nesting level counter */
            PreTokenNode *r, *r_prev = NULL, *r_first, *param, *arg;

            /*
             * The name of a function-like macro not followed by left parenthesis,
             * is not an invocation to the macro.
             */
            if (strcmp(get_lexeme(2), "(") != 0) {
                match(lookahead(1));
                return;
            }

            if ((r=r_first=dup_rep_list(m->rep)) == NULL)
                goto empty_rep_list2;

            /*
             * Scan the replacement list and at each ocurrence
             * of an identifier token, see if there is a matching
             * parameter (if so, replace it for corresponding actual
             * argument).
             */
            while (TRUE) {
                if (r->token != PRE_TOK_ID)
                    goto bottom;

                /*
                 * See if there is a matching parameter.
                 */
                param = m->params;
                arg = curr_tok->next->next;
                while (strcmp(param->lexeme, ")") != 0) {
                    if (strcmp(param->lexeme, ",") == 0)
                        param = param->next;

                    /* test for match */
                    if (strcmp(param->lexeme, "...")==0 || strcmp(param->lexeme, r->lexeme)==0)
                        break;

                    /*
                     * There was no match, advance the
                     * argument and parameter pointer.
                     */
                    pn = 0;
                    while (pn>0 || strcmp(arg->lexeme, ",")!=0 && strcmp(arg->lexeme, ")")!=0) {
                        if (strcmp(arg->lexeme, "(") == 0) pn++;
                        else if (strcmp(arg->lexeme, ")") == 0) pn--;
                        arg = arg->next;
                    }
                    if (strcmp(arg->lexeme, ",") == 0)
                        arg = arg->next;
                    param = param->next;
                }

                /*
                 * Replace if there was a match.
                 */
                if (strcmp(param->lexeme, "...") == 0) {
                    if (strcmp(r->lexeme, "__VA_ARGS__") == 0) {
                        /*
                         * __VA_ARGS__ was found
                         * in the replacement list.
                         */
                        if (strcmp(arg->lexeme, ")") != 0) {
                            /*
                             * The argument list corresponding
                             * to ... isn't empty.
                             */
                            PreTokenNode *arg_copy, *last;

                            /* make a copy of the argument */
                            pn = 0;
                            arg_copy = last = new_node(arg->token, arg->lexeme);
                            if (strcmp(arg->lexeme, "(") == 0)
                                pn++;
                            arg = arg->next;
                            while (pn>0 || strcmp(arg->lexeme, ")")!=0) {
                                if (strcmp(arg->lexeme, "(") == 0) pn++;
                                else if (strcmp(arg->lexeme, ")") == 0) pn--;
                                last->next = new_node(arg->token, arg->lexeme);
                                last = last->next;
                                arg = arg->next;
                            }
                            /* insert the argument list in place of __VA_ARGS__ */
                            if (r_prev == NULL) {
                                last->next = r->next;
                                r_first = arg_copy;
                            } else {
                                r_prev->next = arg_copy;
                                last->next = r->next;
                            }
                            free(r->lexeme);
                            free(r);
                            r = last;
                        } else {
                            /*
                             * Make disappear __VA_ARGS__ (there are
                             * no arguments corresponding to ...).
                             */
                            PreTokenNode *r_next;

                            r_next = r->next;
                            if (r_prev == NULL)
                                r_first = r_next;
                            else
                                r_prev->next = r_next;
                            free(r->lexeme);
                            free(r);
                            r = r_next;
                        }
                    }
                } else if (strcmp(param->lexeme, ")") != 0) {
                    /*
                     * A formal parameter was found
                     * in the replacement list.
                     */
                    PreTokenNode *arg_copy, *last;

                    /* make a copy of the argument */
                    pn = 0;
                    arg_copy = last = new_node(arg->token, arg->lexeme);
                    if (strcmp(arg->lexeme, "(") == 0)
                        pn++;
                    arg = arg->next;
                    while (pn>0 || strcmp(arg->lexeme, ",")!=0 && strcmp(arg->lexeme, ")")!=0) {
                        if (strcmp(arg->lexeme, "(") == 0) pn++;
                        else if (strcmp(arg->lexeme, ")") == 0) pn--;
                        last->next = new_node(arg->token, arg->lexeme);
                        last = last->next;
                        arg = arg->next;
                    }
                    /* insert the argument list in place of the identifier */
                    if (r_prev == NULL) {
                        last->next = r->next;
                        r_first = arg_copy;
                    } else {
                        r_prev->next = arg_copy;
                        last->next = r->next;
                    }
                    free(r->lexeme);
                    free(r);
                    r = last;
                }
bottom:
                /*
                 * Exit the loop if this was the last
                 * token of the replacement list.
                 */
                if (r==NULL || r->next==NULL)
                    break;

                r_prev = r;
                r = r->next;
            }
empty_rep_list2:
            /*
             * Mark the macro call as deleted.
             */
            pn = 0;
            while (pn>1 || strcmp(curr_tok->lexeme, ")")!=0) {
                if (strcmp(curr_tok->lexeme, "(") == 0) pn++;
                else if (strcmp(curr_tok->lexeme, ")") == 0) pn--;
                match2(lookahead(1));
            }
            /*
             * Insert the replacement list next
             * to the macro call.
             */
            if (r != NULL) {
                r->next = curr_tok->next;
                curr_tok->next = r_first;
            }
            match2(lookahead(1)); /* ) */
        }
    } else {
        match(lookahead(1));
    }
}

// lparen:
  // "the" "left-parenthesis" "character" "without" "preceding" "white" "space" ;

// replacement_list = [ pp_tokens ]

// identifier_list = identifier { "," identifier }

// header_name = "<" h_char_sequence ">"
              // "\"" q_char_sequence "\""
//
// h_char_sequence = h_char { h_char }
//
// h_char = any member of the source character set except the new_line character and ">"
//
// q_char_sequence = q_char { q_char }
//
// q_char = any member of the source character set except the new_line character and "\""
//
// pp_number = digit { digit | nondigit }
//
// new_line = "the" new_line "character"
