#include "pre.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "util.h"


#define equal(s, t)     (strcmp(s, t) == 0)
#define not_equal(s, t) (strcmp(s, t) != 0)

#define SRC_FILE    curr_source_file
#define SRC_LINE    curr_line
#define SRC_COLUMN  src_column

#define MACRO_TABLE_SIZE 101

typedef enum {
    SIMPLE,
    PARAMETERIZED
} MacroKind;
typedef struct Macro Macro;
struct Macro {
    char *name;
    MacroKind kind;
    PreTokenNode *rep, *params;
    int enabled;
    Macro *next;
};

static void install(MacroKind kind, char *name, PreTokenNode *rep, PreTokenNode *params);
static Macro *macro_table[MACRO_TABLE_SIZE];
static Macro *lookup(char *s);
static void uninstall(char *name);


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
static char token_string[1024];
static PreTokenNode *curr_tok, *token_list;
static int curr_line, src_column;
unsigned number_of_pre_tokens;

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
    temp->src_column = src_column;
    temp->src_file = curr_source_file;
    ++number_of_pre_tokens;

    return temp;
}

static
PreToken lookahead(int i)
{
    PreTokenNode *p;

    p = curr_tok;
    while (/*p!=NULL && */--i)
        p = p->next;
    return p->token;
}

static
char *get_lexeme(int i)
{
    PreTokenNode *p;

    p = curr_tok;
    while (/*p!=NULL && */--i)
        p = p->next;
    return p->lexeme;
}

PreToken get_token(void);

static PreTokenNode *penultimate_node; /* the code for #include use this */

/*
 * Tokenize the buffer `buf' and return
 * the resulting chain of tokens.
 */
static
PreTokenNode *tokenize(void)
{
    PreToken t;
    PreTokenNode *n, *p;

    curr_line = 1; /* initialize line counter */
    t = get_token();
    n = p = penultimate_node = new_node(t, token_string);
    p->next_char = *curr;

    while (t != PRE_TOK_EOF) {
        t = get_token();
        p->next = new_node(t, token_string);
        p->next->next_char = *curr;
        penultimate_node = p;
        p = p->next;
    }

    return n;
}

static char *str_tok[] = {
    "EOF",
    "punctuator",
    "preprocessor number",
    "identifier",
    "character constant",
    "string literal",
    "new line",
    "other"
};

/*
 * Load the file located at `file_path' into the global buffer `buf',
 * and perform end-of-line replacing (DOS's CRLF are replaced for a
 * single LF) and line splicing.
 */
static
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
    ++buf_size; /* make room for '\0' */
    rewind(fp);
    curr = buf = malloc(buf_size);

    while (fgets(curr, /*0x7FFFFFFF*/0x1000, fp) != NULL) {
        int line_len = strlen(curr);
#if WIN_LINE_ENDING
        /* replace CRLFs for LFs */
        if (line_len>1 && curr[line_len-2]=='\r') {
            curr[line_len-2]='\n', curr[line_len-1]='\0';
            --line_len;
        }
#endif
        /* join lines ending in \ with the next line (the last line must not end with \) */
        while (line_len>1 && curr[line_len-2]=='\\') {
            line_len -= 2; /* removes '\\' and '\n' */
            fgets(curr+line_len, /*0x7FFFFFFF*/0x1000, fp);
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

static void preprocessing_file(void);

/*
 * Preprocess the source file and any file
 * included through #include, and return a
 * sequence of preprocessing tokens.
 */
PreTokenNode *preprocess(char *source_file)
{
    init(source_file);
    token_list = curr_tok = tokenize();
    preprocessing_file();

    return token_list;
}

PreToken get_token(void)
{
    PreToken token;
    int tok_str_ind = 0, save;
    State state = STATE_START;
    static int curr_column = 0;

    while (state != STATE_DONE) {
        int c = *curr++;
        save = TRUE;
        ++curr_column;

        switch (state) {
        case STATE_START:
            src_column = curr_column-1;
            if (c==' ' || c=='\t' /* add other desired white space chars here */) {
                save = FALSE;
            } else if (isdigit(c)) {
                state = STATE_INNUM;
            } else if (isalpha(c) || (c=='_')) {
                state = STATE_INID;
            } else if (c == '\'') {
                save = FALSE;
                state = STATE_INCHAR;
            } else if (c == '\"') {
                // save = FALSE;
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
                    // save = FALSE;
                    token = PRE_TOK_NL;
                    ++curr_line, curr_column=0;
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
#define ADVANCE() (++curr_column, *curr++)
#define REWIND()  (--curr_column, --curr)
#define SAVE_AND_ADVANCE()\
    token_string[tok_str_ind++] = (char)c,\
    c = ADVANCE() //*curr++
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
                case '~': /* ~ or ~= */
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
                case '#': /* # or ## */
                    if (*curr == '#')
                        SAVE_AND_ADVANCE();
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '<': /* <, <=, << or <<= */
                    if (*curr == '=') {
                        SAVE_AND_ADVANCE();
                    } else if (*curr == '<') {
                        SAVE_AND_ADVANCE();
                        if (*curr == '=')
                            SAVE_AND_ADVANCE();
                    }
                    token = PRE_TOK_PUNCTUATOR;
                    break;
                case '>': /* >, >=, >> or >>= */
                    if (*curr == '=') {
                        SAVE_AND_ADVANCE();
                    } else if (*curr == '>') {
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
                // --curr;
                REWIND();
                token = (state==STATE_INNUM)?PRE_TOK_NUM:PRE_TOK_ID;
                state = STATE_DONE;
            }
            break;
        case STATE_INCHAR:
            // --curr;
            REWIND();
            if (*curr=='\0' || *curr=='\n') {
                ERROR("missing terminating \"'\" character");
            } else if (*curr == '\'') {
                ERROR("empty character constant");
            } else {
                /*
                 *   If ' is found and it is preceded by:
                 * 1) an even number of backslashes = stop
                 *      'a'
                 *      '\\',
                 *      '\\\\'
                 * 2) an odd number of backslashes = don't stop
                 *      'abc\'def',
                 *      '\'   <-error!
                 *      '\\\' <-error!
                 */
                int cb; /* consecutive backslashes */

                cb = 0;
                if (*curr == '\\')
                    ++cb;
                token_string[tok_str_ind++] = ADVANCE();//*curr++;
                while (*curr!='\'' || cb%2!=0) {
                    if (*curr=='\0' || *curr=='\n')
                        ERROR("missing terminating \"'\" character");
                    else if (*curr == '\\')
                        ++cb;
                    else
                        cb = 0;
                    token_string[tok_str_ind++] = ADVANCE();//*curr++;
                }
                // ++curr; /* skip ' */
                ADVANCE();
                save = FALSE;
                state = STATE_DONE;
                token = PRE_TOK_CHACON;
            }
            break;
        case STATE_INSTR:
            // --curr;
            REWIND();
            if (*curr=='\0' || *curr=='\n') {
                ERROR("missing terminating '\"' character");
            } else {
                /* backslashes: the same rule as for character constants */
                int cb;

                cb = 0;
                while (*curr!='"' || cb%2!=0) {
                    if (*curr=='\0' || *curr=='\n')
                        ERROR("missing terminating '\"' character");
                    else if (*curr == '\\')
                        ++cb;
                    else
                        cb = 0;
                    token_string[tok_str_ind++] = ADVANCE();//*curr++;
                }
                // ++curr; /* skip " */
                // ADVANCE();
                // save = FALSE;
                c = *curr++;
                state = STATE_DONE;
                token = PRE_TOK_STRLIT;
            }
            break;
        case STATE_INCOMMENT1:
            save = FALSE;
            if (c == '\0')
                ERROR("unterminated comment");
            else if (c == '*')
                state = STATE_INCOMMENT2;
            else if (c == '\n')
                ++curr_line, curr_column=0;
            break;
        case STATE_INCOMMENT2:
            save = FALSE;
            if (c == '\0')
                ERROR("unterminated comment");
            else if (c == '/')
                state = STATE_START;
            else if (c != '*') {
                state = STATE_INCOMMENT1;
                if (c == '\n')
                    ++curr_line, curr_column=0;
            }
            break;
        case STATE_INLINECOMMENT:
            save = FALSE;
            if (c == '\n') {
                // --curr;
                REWIND();
                state = STATE_START;
            } else if (c == '\0') {
                // --curr;
                REWIND();
                state = STATE_START;
            }
            break;
        case STATE_DONE:
        default:
            fprintf(stderr, "get_token() bug\n");
            exit(1);
            break;
        } /* switch (state) */

        if (save)
            token_string[tok_str_ind++] = (char)c;
        if (state == STATE_DONE)
            token_string[tok_str_ind] = '\0';
    } /* while (state != STATE_DONE) */

    return token;
}

#undef SRC_FILE
#undef SRC_LINE
#undef SRC_COLUMN
#define SRC_FILE    curr_tok->src_file
#define SRC_LINE    curr_tok->src_line
#define SRC_COLUMN  curr_tok->src_column

static
void match(PreToken x)
{
    if (curr_tok->token == x)
        curr_tok = curr_tok->next;
    else
        ERROR("expecting: `%s'; found: `%s'", str_tok[x], curr_tok->lexeme);
}

static
void match2(PreToken x) /* same as match but mark the token as deleted */
{
    if (curr_tok->token == x) {
        curr_tok->deleted = TRUE;
        curr_tok = curr_tok->next;
    } else {
        ERROR("expecting: `%s'; found: `%s'", str_tok[x], curr_tok->lexeme);
    }
}

/* recursive parser functions */
static void group(int skip);
static void group_part(int skip);
static void if_section(int skip);
static int if_group(int skip);
static int elif_groups(int skip);
static int elif_group(int skip);
static void else_group(int skip);
static void endif_line(void);
static void control_line(int skip);
static void pp_tokens(int skip);
static void preprocessing_token(int skip);


int is_group_part(void)
{
    /*
     * group_part cannot begin with neither EOF
     * nor #elif nor #else nor #endif.
     */
    if (lookahead(1) == PRE_TOK_EOF
    || (equal(get_lexeme(1), "#")
    && (equal(get_lexeme(2), "elif")
    ||  equal(get_lexeme(2), "else")
    ||  equal(get_lexeme(2), "endif"))))
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
    if (equal(get_lexeme(1), "#")) {
        if (equal(get_lexeme(2), "if")
        ||  equal(get_lexeme(2), "ifdef")
        ||  equal(get_lexeme(2), "ifndef"))
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
    if (equal(get_lexeme(1), "#") && equal(get_lexeme(2), "elif"))
        elif_cond = elif_groups(skip || if_cond);
    if (equal(get_lexeme(1), "#") && equal(get_lexeme(2), "else"))
        else_group(skip || if_cond || elif_cond);
    endif_line();
}

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
    match2(PRE_TOK_PUNCTUATOR); /* # */
    if (equal(get_lexeme(1), "if")) {
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
    } else if (equal(get_lexeme(1), "ifdef")) {
        match2(PRE_TOK_ID);
        cond_res = (lookup(get_lexeme(1))!=NULL)?1:0;
        match2(PRE_TOK_ID);
    } else { /* ifndef */
        match2(PRE_TOK_ID);
        cond_res = (lookup(get_lexeme(1))==NULL)?1:0;
        match2(PRE_TOK_ID);
    }
    match2(PRE_TOK_NL);
    /*
     * Skip if the condition evaluated to false or if the
     * if/ifdef/ifndef already is within of a block being skipped.
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
    while (equal(get_lexeme(1), "#") && equal(get_lexeme(2), "elif"))
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
    match2(PRE_TOK_NL);

    skip = (cond_res==0)?1:skip;
    if (is_group_part())
        group(skip);

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
    if (equal(get_lexeme(1), "#"))
        match2(PRE_TOK_PUNCTUATOR);
    else
        ERROR("`#endif' expected");
    if (equal(get_lexeme(1), "endif"))
        match2(PRE_TOK_ID);
    else
        ERROR("`#endif' expected");
    match2(PRE_TOK_NL);
}

void simple_define(void);
void parameterized_define(void);

/*
 * control_line = "#" "include" pp_tokens new_line |
 *                "#" "define" identifier replacement_list new_line |
 *                "#" "define" identifier lparen [ identifier_list ] ")" replacement_list new_line |
 *                "#" "undef" identifier new_line |
 *                "#" "line" pp_tokens new_line |
 *                "#" "error" [ pp_tokens ] new_line |
 *                "#" "pragma" [ pp_tokens ] new_line |
 *                "#" new_line
 */
void control_line(int skip)
{
    if (skip)
        goto bottom;

    /* by group_part() confirmed # */
    match2(PRE_TOK_PUNCTUATOR);

    /*
     * Identify and interpret directive.
     */
    if (equal(get_lexeme(1), "include")) {
        PreTokenNode *tokenized_file;

        match2(PRE_TOK_ID);
        /*
         * The only supported forms of the include directive are:
         *      #include "file.h"
         *      #include <file.h>
         */
        if (lookahead(1) == PRE_TOK_STRLIT) {
            char *p;
            /*
             * Search for the file in the same directory as
             * the file that contains the #include directive.
             */
            curr_source_file = curr_tok->src_file;

            /* remove "" */
            memmove(curr_tok->lexeme, curr_tok->lexeme+1, strlen(curr_tok->lexeme+1)+1);
            curr_tok->lexeme[strlen(curr_tok->lexeme)-1] = '\0';

            /*
             * Open the file.
             */
            p = curr_source_file+strlen(curr_source_file);
            while (p!=curr_source_file && *p!='/') /* search the last '/' of the path */
                p--;
            if (p == curr_source_file) {
                /* the file being processed is in the compiler's working directory */
                init(get_lexeme(1));
            } else {
                /* Construct the path. For example if the file being processed
                   is src/foo.c and the argument of #include is "foo.h", the
                   resulting path is src/foo.h. */
                int n;
                char *path;

                n = p-curr_source_file+1;
                path = malloc(n+strlen(get_lexeme(1))+1);
                strncpy(path, curr_source_file, n);
                path[n] = '\0';
                strcat(path, get_lexeme(1));
                init(path);
                free(path);
            }
            match2(lookahead(1)); /* filename */
        } else if (equal(get_lexeme(1), "<")) {
            char path[128] = "include/";

            match2(PRE_TOK_PUNCTUATOR);
            if (equal(get_lexeme(1), ">"))
                ERROR("include: empty `<>'");
            do {
                if (lookahead(1) == PRE_TOK_NL)
                    ERROR("include: missing closing `>'");
                strcat(path, get_lexeme(1));
                match2(lookahead(1));
            } while (not_equal(get_lexeme(1), ">"));
            init(path);
            match2(PRE_TOK_PUNCTUATOR); /* > */
        } else {
            ERROR("include: \"file.h\" or <file.h> expected");
        }
        /* now at new-line... */

        /*
         * Tokenize the file's content and insert
         * the result right after the filename.
         */
        tokenized_file = tokenize();
        /*
         * `penultimate_node' is set by tokenize()
         * as the node previous to the EOF token
         * just to allow the operations that follow.
         */
        /* delete EOF token */
        free(penultimate_node->next->lexeme);
        free(penultimate_node->next);
        /* insert */
        penultimate_node->next = curr_tok->next;
        curr_tok->next = tokenized_file;
    } else if (equal(get_lexeme(1), "define")) {
        match2(PRE_TOK_ID);
        if (lookahead(1) == PRE_TOK_ID) {
            if (curr_tok->next_char == '(')
                parameterized_define();
            else
                simple_define();
        } else {
            ERROR("define: name expected");
        }
    } else if (equal(get_lexeme(1), "undef")) {
        match2(PRE_TOK_ID);
        if (lookahead(1) == PRE_TOK_ID)
            uninstall(get_lexeme(1));
        else
            ERROR("undef: name expected");
    } else if (equal(get_lexeme(1), "\n")) {
        ; /* NULL directive */
    /* --> add the remaining directives here <-- */
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

void simple_define(void)
{
    install(SIMPLE, get_lexeme(1), curr_tok->next, NULL);
}

void parameterized_define(void) // TODO: check for duplicate parameter names
{
    PreTokenNode *rep;

    /*
     * #define ABC( a1, a2 ... an   )
     *          ^                   ^
     *    we are here           and must go here
     */
    for (rep = curr_tok->next->next; not_equal(rep->lexeme, ")"); ) {
        if (equal(rep->lexeme, "...")) {
            if (not_equal(rep->next->lexeme, ")"))
                ERROR("`...' not at the end of the parameter list");
        } else if (rep->token != PRE_TOK_ID) {
            ERROR("expecting parameter name");
        }

        rep = rep->next;
        if (equal(rep->lexeme, ",")) {
            rep = rep->next;
            if (equal(rep->lexeme, ")")) /* something like #define A(x,) */
                ERROR("missing parameter name");
        } else if (not_equal(rep->lexeme, ")")) {
            /* otherwise (a,b...) would be considered valid */
            ERROR("expecting parameter name");
        }
    }
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

void expand_simple_macro(Macro *m);
void expand_parameterized_macro(Macro *m);
void reenable_macro(char *name);

/*
 * preprocessing_token = header_name
 *                       identifier
 *                       pp_number
 *                       character_constant
 *                       string_literal
 *                       punctuator
 *                       each non-white-space character that cannot be one of the above
 */
void preprocessing_token(int skip)
{
    Macro *m;

    if (skip) {
        match2(lookahead(1));
        return;
    }

    if (lookahead(1)==PRE_TOK_ID && (m=lookup(get_lexeme(1)))!=NULL) {
        DEBUG_PRINTF("found macro `%s'\n", get_lexeme(1));
        if (m->kind == SIMPLE)
            expand_simple_macro(m);
        else
            expand_parameterized_macro(m);
    } else if (lookahead(1) == PRE_TOK_MACRO_REENABLER) {
        DEBUG_PRINTF("reenabling macro `%s'\n", get_lexeme(1));
        reenable_macro(get_lexeme(1));
        match2(PRE_TOK_MACRO_REENABLER);
    } else {
        match(lookahead(1));
    }
}

/*
 * Duplicate a replacement list.
 */
static
PreTokenNode *dup_rep_list(PreTokenNode *r)
{
    PreTokenNode *new_rep_list, *temp;

    /* test for empty replacement list */
    if (r->token == PRE_TOK_NL)
        return NULL;

    /* make the duplicate */
    new_rep_list = temp = new_node(r->token, r->lexeme);
    new_rep_list->src_line = curr_tok->src_line;
    r = r->next;
    while (r->token != PRE_TOK_NL) {
        temp->next = new_node(r->token, r->lexeme);
        temp->next->src_line = curr_tok->src_line;
        temp = temp->next;
        r = r->next;
    }

    return new_rep_list;
}

/*
 * Expand an object-like macro.
 */
void expand_simple_macro(Macro *m)
{
    PreTokenNode *r, *old_next;

    /* test empty replacement list */
    if ((r=dup_rep_list(m->rep)) == NULL)
        goto empty_rep_list;
    /*
     * Insert the replacement list between the
     * identifier and the next token.
     */
    old_next = curr_tok->next;
    curr_tok->next = r;
    while (r->next != NULL)
        r = r->next;
    // r->next = old_next;
    r->next = new_node(PRE_TOK_MACRO_REENABLER, m->name);
    r->next->next = old_next;
    m->enabled = FALSE; /* disable the macro temporally */
empty_rep_list:
    /*
     * Mark the identifier as deleted and advance.
     */
    match2(lookahead(1));
}

typedef enum {
    FIXED_LIST,
    VAR_LIST
} ParaListKind;

/*
 * Support function for parameterized macro calls.
 * Make a copy of the token string that conform an argument.
 * `kind' modifies the main loop stop condition.
 */
PreTokenNode *copy_arg(PreTokenNode **a, ParaListKind kind)
{
    int pn; /* parenthesis nesting level counter */
    PreTokenNode *copy, *temp;

    if (equal((*a)->lexeme, ",") || equal((*a)->lexeme, ")"))
        ERROR("empty macro argument");

    pn = 0;
    if (equal((*a)->lexeme, "("))
        ++pn;
    copy = temp = new_node((*a)->token, (*a)->lexeme);
    copy->src_line = (*a)->src_line;
    (*a) = (*a)->next;

    while (pn>0 || ((kind==VAR_LIST||not_equal((*a)->lexeme, ",")) && not_equal((*a)->lexeme, ")"))) {
        if (equal((*a)->lexeme, "(")) pn++;
        else if (equal((*a)->lexeme, ")")) pn--;
        else if ((*a)->token == PRE_TOK_EOF) ERROR("missing `)' in macro call");
        temp->next = new_node((*a)->token, (*a)->lexeme);
        temp->next->src_line = (*a)->src_line;
        temp = temp->next;
        *a = (*a)->next;
    }

    return copy;
}

/*
 * Second support function. Make a copy of an
 * argument created previously with copy_arg().
 */
PreTokenNode *copy_arg2(PreTokenNode *a, PreTokenNode **last)
{
    PreTokenNode *copy;

    copy = *last = new_node(a->token, a->lexeme);
    copy->src_line = a->src_line;
    a = a->next;

    while (a != NULL) {
        (*last)->next = new_node(a->token, a->lexeme);
        (*last)->next->src_line = a->src_line;
        *last = (*last)->next;
        a = a->next;
    }

    return copy;
}

void expand_parameterized_macro(Macro *m)
{
    PreTokenNode *r, *p, *prev, *param, *arg;
    PreTokenNode *par_arg_tab[16][2] = { { NULL } };
    int tab_size = 0, i;

    /*
     * The name of a function-like macro not followed by left parenthesis,
     * is not an invocation to the macro.
     */
    arg = curr_tok->next;
    while (arg->token==PRE_TOK_NL || arg->token==PRE_TOK_MACRO_REENABLER) arg = arg->next;
    if (not_equal(arg->lexeme, "(")) {
        match(lookahead(1));
        return;
    }

    // if ((r=dup_rep_list(m->rep)) == NULL)
        // goto empty_rep_list;
    r=dup_rep_list(m->rep);

    param = m->params;
    // arg = curr_tok->next->next; /* ID -> "(" -> first-argument */
    arg = arg->next; /* first-argument */

    /*
     * Associate actual argument token strings
     * with the corresponding formal parameter
     * names.
     */
    while (not_equal(param->lexeme, ")") && not_equal(arg->lexeme, ")")) {
        if (equal(param->lexeme, "...")) {
            par_arg_tab[tab_size][0] = new_node(PRE_TOK_ID, "__VA_ARGS__");
            par_arg_tab[tab_size][1] = copy_arg(&arg, VAR_LIST); /* arg is left pointing to ")" */
            ++tab_size;
            param = param->next; /* advance to ")" */
            break;
        }

        par_arg_tab[tab_size][0] = param;
        par_arg_tab[tab_size][1] = copy_arg(&arg, FIXED_LIST); /* arg is left pointing to "," or ")" */
        ++tab_size;

        param = param->next; /* advance to "," or ")" */

        if (not_equal(param->lexeme, arg->lexeme)) {
            /*
             * Check if it's the case where there are not
             * actual arguments corresponding to '...'.
             * For example:
             *      #define ABC(a,...) a+__VA_ARGS__
             *      ABC(123)
             */
            if (equal(param->lexeme, ",") && equal(param->next->lexeme, "...") /*&& equal(arg->lexeme, ")")*/)
                param = param->next;
            else
                ERROR("argument number mismatch in macro call");
        } else if (not_equal(param->lexeme, ")")) {
            param=param->next, arg=arg->next;
        }
    }

    if (not_equal(param->lexeme, arg->lexeme)) { /* both should be equal to ")" */
        if (not_equal(param->lexeme, "...")) { /* or '...' must not have matching arguments */
            ERROR("argument number mismatch in macro call");
        } else {
            par_arg_tab[tab_size][0] = new_node(PRE_TOK_ID, "__VA_ARGS__");
            par_arg_tab[tab_size][1] = NULL;
            ++tab_size;
        }
    }

    /*int i = 0;
    while (i < tab_size) {
        PreTokenNode *p;

        printf("par=%s ==> arg= ", par_arg_tab[i][0]->lexeme);
        p = par_arg_tab[i][1];
        while (p != NULL)
            printf("%s ->> ", p->lexeme), p=p->next;
        printf("\n");
        ++i;
    }*/

    /*
     * Replace every occurrence of a formal parameter
     * for a copy of the actual argument token sequence
     * associated with it (build the 'expansion' of the macro).
     */
    prev=NULL, p=r;
    while (p != NULL) {
        if (p->token != PRE_TOK_ID) {
            prev=p, p=p->next;
            continue;
        }

        for (i = 0; i < tab_size; i++) {
            if (equal(par_arg_tab[i][0]->lexeme, p->lexeme)) {
                PreTokenNode *last;

                if (par_arg_tab[i][1] != NULL) {
                    if (prev != NULL)
                        /* a -> <b -> x> -> c -> NULL */
                        prev->next = copy_arg2(par_arg_tab[i][1], &last);
                    else
                        /* <a -> x> -> b -> c -> NULL */
                        r = copy_arg2(par_arg_tab[i][1], &last);
                    last->next = p->next;
                    free(p->lexeme), free(p);
                    prev = last;
                    p = last->next;
                } else {
                    /*
                     * `...' doesn't correspond to nothing, make
                     * disappear __VA_ARGS__ from the replacement list.
                     */
                    if (prev != NULL) {
                        /* a -> <b -> > -> c -> NULL */
                        prev->next = p->next;
                        free(p->lexeme), free(p);
                        p = prev->next;
                    } else {
                        /* <a -> > -> b -> c -> NULL */
                        r = p->next;
                        free(p->lexeme), free(p);
                        p = r;
                    }
                }
                break;
            }
        }
        if (i == tab_size) /* no match */
            prev=p, p=p->next;
    }

    /*
     * Delete the original copies of the arguments.
     */
    for (i = 0; i < tab_size; i++) {
        PreTokenNode *temp;

        while (par_arg_tab[i][1] != NULL) {
            temp = par_arg_tab[i][1];
            par_arg_tab[i][1] = par_arg_tab[i][1]->next;
            free(temp->lexeme);
            free(temp);
        }
    }
    /*while (r != NULL) {
        printf("r=%s\n", r->lexeme);
        r = r->next;
    }
    exit(0);*/
// empty_rep_list:
    /*
     * Mark the macro call as deleted.
     */
    while (curr_tok != arg) {
        if (curr_tok->token == PRE_TOK_MACRO_REENABLER) {
            DEBUG_PRINTF("reenabling macro `%s'\n", curr_tok->lexeme);
            reenable_macro(curr_tok->lexeme);
        }
        match2(lookahead(1));
    }
    /*
     * Insert the replacement list next
     * to the macro call.
     */
    if (r != NULL) {
        prev->next = new_node(PRE_TOK_MACRO_REENABLER, m->name);
        prev->next->next = curr_tok->next;
        m->enabled = FALSE;
        // prev->next = curr_tok->next;
        curr_tok->next = r;
    }
    match2(lookahead(1)); /* ) */
}

void reenable_macro(char *name)
{
    Macro *m;

    for(m = macro_table[hash(name)%MACRO_TABLE_SIZE]; m != NULL; m = m->next) {
        if(equal(get_lexeme(1), m->name)) {
            m->enabled = TRUE;
            break;
        }
    }
}

Macro *lookup(char *s)
{
	Macro *np;

	for(np = macro_table[hash(s)%MACRO_TABLE_SIZE]; np != NULL; np = np->next)
		if(np->enabled && equal(s, np->name))
			return np;
	return NULL;
}

void install(MacroKind kind, char *name, PreTokenNode *rep, PreTokenNode *params)
{
    Macro *np;
    unsigned int hash_val;

    if ((np=lookup(name)) == NULL) {
        np = malloc(sizeof(Macro));
        np->kind = kind;
        np->name = name;
        np->rep = rep;
        np->enabled = TRUE;
        np->params = params;
        hash_val = hash(name)%MACRO_TABLE_SIZE;
        np->next = macro_table[hash_val];
        macro_table[hash_val] = np;
    } else {
        ERROR("macro `%s' redefined", name);
    }
}

void uninstall(char *name)
{
    /* if ((np=lookup(name)) != NULL)
        / np->enabled = FALSE;*/
    unsigned hash_val;
	Macro *np, *prev;

    hash_val = hash(name)%MACRO_TABLE_SIZE;

	for(np=macro_table[hash_val], prev=NULL;
        np!=NULL&&not_equal(name, np->name);
        prev=np, np=np->next)

	if (np == NULL)
        return; /* not found */
    if (prev == NULL)
        macro_table[hash_val] = np->next;
    else
        prev->next = np->next;
    free(np);
}
