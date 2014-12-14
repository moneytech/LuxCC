#include "pre.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


#define TRUE  1
#define FALSE 0
#define ERROR(...) \
    fprintf(stderr, "Error: %s\n", __VA_ARGS__), \
    exit(EXIT_FAILURE)

typedef enum {
    PRE_TOK_EOF,
    PRE_TOK_PUNCTUATOR,
    PRE_TOK_NUM,
    PRE_TOK_ID,
    PRE_TOK_CHACON,
    PRE_TOK_STRLIT,
    PRE_TOK_NL,
    PRE_TOK_OTHER
} PreToken;

// typedef struct TokenLine TokenLine;
// struct TokenLine {
    // PreToken tok;
    // int line; /* source code line in which the token appears */
// };

PreToken get_token(void);

typedef struct PreTokenNode PreTokenNode;
struct PreTokenNode {
    PreToken token;
    char *lexeme;
    PreTokenNode *next, *prev;
    char next_char; /* needed to distinguish between
                      "name(" and "name (" in #define */
};

char *buf, *curr;
char token_string[128];
PreTokenNode *curr_tok;

PreTokenNode *new_node(PreToken token, char *lexeme)
{
    PreTokenNode *temp;

    temp = malloc(sizeof(PreTokenNode));
    temp->token = token;
    temp->lexeme = strdup(lexeme);
    temp->next_char = *curr; // !!!
    temp->prev = temp->next = NULL;

    return temp;
}

PreToken lookahead(int i)
{
    PreTokenNode *p;

    p = curr_tok;
    while (--i)
        p = p->next;
    return p->token;
}

char *get_lexeme(int i)
{
    PreTokenNode *p;

    p = curr_tok;
    while (--i)
        p = p->next;
    return p->lexeme;
}

/*
 * Tokenize the buffer 'buf' and return the resulting
 * chain of tokens.
 */
PreTokenNode *tokenize(void)
{
    PreToken t;
    PreTokenNode *n, *p;

    t = get_token();
    n = p = new_node(t, token_string);

    /*
     * there was only one token (EOF)
     */
    if (t == PRE_TOK_EOF)
        return n;

    /*
     * make doubly linked list of tokens
     */
    do {
        t = get_token();
        p->next = new_node(t, token_string);
        p->next->prev = p;
        p = p->next;

    } while (t != PRE_TOK_EOF);

    return n;
}

void consume(void)
{
    // lookahead[buf_index].tok = get_token();
    // buf_index = (buf_index+1) % LOOKAHEAD;
}

void match(PreToken x)
{
    if (curr_tok->token == x)
        // consume();
        curr_tok = curr_tok->next;
    else
        ERROR("error at match");
}

void preprocessing_file(void);

/*
 * Load the file pointed to by fp into the global buffer 'buf', and
 * perform end-of-line replacing (DOS's CRLF are replaced for a
 * single LF) and line splicing.
 */
void init(FILE *fp)
{
    int buf_size;
    // char *buf, *curr;

    fseek(fp, 0, SEEK_END);
    buf_size = ftell(fp); /* number of chars of the file */
    ++buf_size; /* makes room for '\0' */
    rewind(fp);
    curr = buf = malloc(buf_size);

    while (fgets(curr, 0x7FFFFFFF, fp) != NULL) {
        int line_len = strlen(curr);

        /* replaces CRLFs for LFs */
        if (line_len>1 && curr[line_len-2]=='\r') {
            curr[line_len-2]='\n', curr[line_len-1]='\0';
            --line_len;
        }

        /* joins lines ending in \ with the next line (the last line must not end in \) */
        while (line_len>1 && curr[line_len-2]=='\\') {
            line_len -= 2; /* removes '\\' and '\n' */
            fgets(curr+line_len, 0x7FFFFFFF, fp);
            line_len += strlen(curr+line_len);

            /* again, replaces CRLFs for LFs */
            if (curr[line_len-2]=='\r') {
                curr[line_len-2]='\n', curr[line_len-1]='\0';
                --line_len;
            }
        }

        curr += line_len;
    }

    curr = buf;
}


void preprocess(char *source_file)
{
    FILE *fp;
    fp = fopen(source_file, "rb");
    init(fp);
    fclose(fp);
    curr_tok = tokenize();
    preprocessing_file();
}


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


PreToken get_token(void)
{
    int tok_str_ind = 0;
    PreToken curr_tok;
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
                    curr_tok = PRE_TOK_EOF;
                    break;
                case '\n':
                    save = FALSE; // ?
                    curr_tok = PRE_TOK_NL;
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
                    curr_tok = PRE_TOK_PUNCTUATOR;
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
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '-': /* -, --, -> or -= */
                    if (*curr=='-' || *curr=='>' || *curr=='=')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '*': /* * or *= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '/': /* / or /= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '%': /* % or %= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '^': /* ^ or ^= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '=': /* = or == */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '|': /* |, || or |= */
                    if (*curr=='|' || *curr=='=')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '&': /* &, && or &= */
                    if (*curr=='&' || *curr=='=')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '!': /* ! or != */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '#':
                    if (*curr == '#')
                        SAVE_AND_ADVANCE();
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '<': /* <, <=, << or <<= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    else if (*curr == '<') {
                        SAVE_AND_ADVANCE();
                        if (*curr == '=')
                            SAVE_AND_ADVANCE();
                    }
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '>': /* >, >=, >> or >>= */
                    if (*curr == '=')
                        SAVE_AND_ADVANCE();
                    else if (*curr == '>') {
                        SAVE_AND_ADVANCE();
                        if (*curr == '=')
                            SAVE_AND_ADVANCE();
                    }
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                case '.': /* . or ... */
                    if (*curr=='.' && *(curr+1)=='.') {
                        SAVE_AND_ADVANCE();
                        SAVE_AND_ADVANCE();
                    }
                    curr_tok = PRE_TOK_PUNCTUATOR;
                    break;
                /*
                 * In ASCII, characters included in the 'other' group are @, $, `, and
                 * any control characters other than NUL.
                 */
                default:
                    curr_tok = PRE_TOK_OTHER;
                    break;
                } /* switch (c) */
            }
            break; /* STATE_START */
        case STATE_INNUM:
        case STATE_INID:
            if (!isalpha(c) && !isdigit(c) && (c!='_')) {
                save = FALSE;
                --curr;
                curr_tok = (state==STATE_INNUM)?PRE_TOK_NUM:PRE_TOK_ID;
                state = STATE_DONE;
            }
            break;
        case STATE_INCHAR:
            --curr;
            if (*curr=='\0' || *curr=='\n') {
                ERROR("missing terminating ' character");
            } else if (*curr == '\'') {
                ERROR("empty character constant");
            } else {
                token_string[tok_str_ind++] = *curr++;
                while (*curr!='\'' || *(curr-1)=='\\') {
                    if (*curr=='\0' || *curr=='\n')
                        ERROR("missing terminating ' character");
                    token_string[tok_str_ind++] = *curr++;
                }
                ++curr; /* skip ' */
                save = FALSE;
                state = STATE_DONE;
                curr_tok = PRE_TOK_CHACON;
            }
            break;
        case STATE_INSTR:
            --curr;
            if (*curr=='\0' || *curr=='\n') {
                ERROR("missing terminating \" character");
            } else {
                while (*curr!='"' || *(curr-1)=='\\') {
                    if (*curr=='\0' || *curr=='\n')
                        ERROR("missing terminating \" character");
                    token_string[tok_str_ind++] = *curr++;
                }
                ++curr; /* skip " */
                save = FALSE;
                state = STATE_DONE;
                curr_tok = PRE_TOK_STRLIT;
            }
            break;
        case STATE_INCOMMENT1:
            save = FALSE;
            if (c == '\0')
                ERROR("unterminated comment");
            else if (c == '*')
                state = STATE_INCOMMENT2;
            else if (c == '\n')
                ;
            break;
        case STATE_INCOMMENT2:
            save = FALSE;
            if (c == '\0')
                ERROR("unterminated comment");
            else if (c == '/')
                state = STATE_START;
            else if (c == '\n')
                ;
            else if (c != '*')
                state = STATE_INCOMMENT1;
            break;
        case STATE_INLINECOMMENT:
            save = FALSE;
            if (c == '\n') {
                // INC_LINE_NUM();
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
    return curr_tok;
}

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
    if (lookahead(1)==PRE_TOK_PUNCTUATOR && !strcmp(get_lexeme(1), "#"))
        if (lookahead(2) == PRE_TOK_ID)
            if (!strcmp(get_lexeme(2), "elif")
            ||  !strcmp(get_lexeme(2), "else")
            ||  !strcmp(get_lexeme(2), "endif"))
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
    while (lookahead(1)!=PRE_TOK_EOF && is_group_part())
        group_part(skip);
}

/*
 * group_part = [ pp_tokens ] new_line |
 *              if_section |
 *              control_line
 */
void group_part(int skip)
{
    if (lookahead(1)==PRE_TOK_PUNCTUATOR && !strcmp(get_lexeme(1), "#")) {
        if (lookahead(2)==PRE_TOK_ID
        && (!strcmp(get_lexeme(2), "if")
        || !strcmp(get_lexeme(2), "ifdef")
        || !strcmp(get_lexeme(2), "ifndef")))
            if_section(skip);
        else
            control_line(skip);
    } else {
        if (lookahead(1) != PRE_TOK_NL)
            pp_tokens(skip);
        match(PRE_TOK_NL);
    }
}

/*
 * if_section = if_group [ elif_groups ] [ else_group ] endif_line
 */
void if_section(int skip)
{
    int if_cond, elif_cond = 0;

    if_cond = if_group(skip);
    if (lookahead(1)==PRE_TOK_PUNCTUATOR && !strcmp(get_lexeme(1), "#")) {
        if (lookahead(2)==PRE_TOK_ID && !strcmp(get_lexeme(2), "elif"))
            elif_cond = elif_groups(skip || if_cond);
    }
    if (lookahead(1)==PRE_TOK_PUNCTUATOR && !strcmp(get_lexeme(1), "#")) {
        if (lookahead(2)==PRE_TOK_ID && !strcmp(get_lexeme(2), "else"))
            else_group(skip || if_cond || elif_cond);
    }
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
    match(PRE_TOK_PUNCTUATOR); /* match # */
    if (!strcmp(get_lexeme(1), "if")) {
        match(PRE_TOK_ID); /* match if */
        /* condition */
        cond_res = atoi(get_lexeme(1));
        match(PRE_TOK_NUM);
        /* --- */
        match(PRE_TOK_NL);
        if (is_group_part())
            group(skip || !cond_res);
    } else if (!strcmp(get_lexeme(1), "ifdef")) {
        /* */
    } else { /* ifndef */
        /* */
    }
    return cond_res;
}

/*
 * elif_groups = elif_group { elif_group }
 */
int elif_groups(int skip)
{
    int cond_res;

    cond_res = elif_group(skip);
    while (lookahead(1)==PRE_TOK_PUNCTUATOR && !strcmp(get_lexeme(1), "#")
    && lookahead(2)==PRE_TOK_ID && !strcmp(get_lexeme(2), "elif")) {
        if (elif_group(skip || cond_res))
            cond_res = TRUE;
    }
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
    match(PRE_TOK_PUNCTUATOR);
    match(PRE_TOK_ID);
    /* condition */
    cond_res = atoi(get_lexeme(1));
    match(PRE_TOK_NUM);
    /* --- */
    match(PRE_TOK_NL);
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
    match(PRE_TOK_PUNCTUATOR); /* # */
    match(PRE_TOK_ID); /* else */
    match(PRE_TOK_NL);
    if (is_group_part())
        group(skip);
}

/*
 * endif_line = "#" "endif" new_line
 */
void endif_line(void)
{
    if (!strcmp(get_lexeme(1), "#"))
        match(PRE_TOK_PUNCTUATOR);
    else
        ERROR("error at endif_line()");
    if (!strcmp(get_lexeme(1), "endif"))
        match(PRE_TOK_ID);
    else
        ERROR("error at endif_line()");
    match(PRE_TOK_NL);
}

void simple_define(void);
void parameterized_define(void);

// control_line = "#" "include" pp_tokens new_line |
               // "#" "define" identifier replacement_list new_line |
               // "#" "define" identifier lparen [ identifier_list ] ")" replacement_list new_line |
               // "#" "undef" identifier new_line |
               // "#" "line" pp_tokens new_line |
               // "#" "error" [ pp_tokens ] new_line |
               // "#" "pragma" [ pp_tokens ] new_line |
               // "#" new_line
void control_line(int skip)
{
    if (skip) {
        while (lookahead(1) != PRE_TOK_NL)
            match(lookahead(1));
        match(PRE_TOK_NL);
        return;
    }

    /* by group_part(), we know we have # */
    match(PRE_TOK_PUNCTUATOR);
    if (!strcmp(get_lexeme(1), "include"))
        ;
    else if (!strcmp(get_lexeme(1), "define")) {
        match(PRE_TOK_ID); /* match define */
        if (lookahead(1) == PRE_TOK_ID) {
            if (curr_tok->next_char == '(')
                parameterized_define();
            else
                simple_define();
            // exit(EXIT_SUCCESS);
        } else {
            ERROR("define: name expected");
        }
    } else if (!strcmp(get_lexeme(1), "undef")) {
        ;
    } else if (!strcmp(get_lexeme(1), "\n")) {
        ;
    } else {
        ERROR("unknow directive");
    }
    match(PRE_TOK_NL);
}

typedef enum {
    SIMPLE,
    PARAMETERIZED
} MacroKind;

typedef struct Macro Macro;
struct Macro {
    char *name;
    MacroKind kind;
    PreTokenNode *rep;
    void *args;
    Macro *next;
};

#define MACRO_TABLE_SIZE 31

Macro *macro_table[MACRO_TABLE_SIZE];

unsigned hash(char *s)
{
    unsigned hash_val;
    for (hash_val = 0; *s != '\0'; s++)
        hash_val = (unsigned)*s + 31*hash_val;
    return hash_val%MACRO_TABLE_SIZE;
}

Macro *lookup(char *s)
{
	Macro *np;
	for(np = macro_table[hash(s)]; np != NULL; np = np->next)
		if(strcmp(s, np->name) == 0)
			return np;
	return NULL;
}

void install(MacroKind kind, char *name, PreTokenNode *rep, void *args)
{
    Macro *np;
    unsigned int hashval;

    if ((np=lookup(name)) == NULL) {
        np = malloc(sizeof(Macro));
        np->kind = kind;
        np->name = name;
        np->rep = rep;
        np->args = args;
        hashval = hash(name);
        np->next = macro_table[hashval];
        macro_table[hashval] = np;
    } else {
        // free(np->rep);
        ERROR("macro redefinition");
    }
    // np->rep = copy_string(rep);
}

void simple_define(void)
{
    install(SIMPLE, curr_tok->lexeme, curr_tok->next, NULL);
    while (lookahead(1) != PRE_TOK_NL)
        match(lookahead(1));

    // PreTokenNode *p = lookup("ABC")->rep;
    // while (p->token != PRE_TOK_NL) {
        // printf("%d, %s\n", p->token, p->lexeme);
        // p = p->next;
    // }
    // exit(EXIT_SUCCESS);
}

void parameterized_define(void)
{
}

// lparen:
  // "the" "left-parenthesis" "character" "without" "preceding" "white" "space" ;

// replacement_list = [ pp_tokens ]

// identifier_list = identifier { "," identifier }

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
        match(lookahead(1));
    } else {
        // /* añadir token a la cadena de tokens que se le pasara al compilador */

        if (lookahead(1) == PRE_TOK_ID) {
            Macro *m;
            if ((m=lookup(get_lexeme(1))) != NULL) {
                if (m->kind == SIMPLE) {
                    PreTokenNode *r, *q, *next, *prev;
                    r = m->rep;
                    next = curr_tok->next;
                    prev = curr_tok->prev;
                    q = curr_tok->prev;
                    while (r->token != PRE_TOK_NL) {
                        q->next = new_node(r->token, r->lexeme);
                        q->next->prev = q;
                        q = q->next;
                        r = r->next;
                    }
                    q->next = next;
                    curr_tok = prev->next;
                    // printf("curr_tok->token=%d, %d\n", curr_tok->token, PRE_TOK_NL);
                } else {
                    /*
                     * The name of a function-like macro not followed by left parenthesis
                     * is not an invocation to the macro.
                     */
                    if (strcmp(get_lexeme(2), "(") != 0)
                        ; /* not a macro call */
                }
            }
        } else {
            printf("%s\n", get_lexeme(1));
            match(lookahead(1));
        }
    }
}

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
// pp_number = digit { ( digit | nondigit ) }
//
// new_line = "the" new_line "character"
