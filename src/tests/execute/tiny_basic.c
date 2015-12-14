/* ===========================================================================
                Tiny BASIC interpreter.

     <program> ::= { <line> } <eof>
    <line> ::= [ Number ] [ <statement> ] <eol>
    <statement> ::= PRINT <expr-list> |
                    INPUT <varlist> |
                    LET var = <expression> |
                    GOTO <expression> |
                    GOSUB <expression> |
                    RETURN |
                    IF <expression> <relop> <expression> THEN <statement> |
                    FOR <var> = <expression> TO <expression> |
                    REM <commentstring> |
                    NEXT |
                    END
    <expr-list> ::= (String | <expression>) { , (String | <expression>) }
    <varlist> ::= <var> { , <var> }
    <expression> ::= [+ | -] <term> { (+ | -) <term> }
    <term> ::= <factor> { (* | / | %) <factor> }
    <factor> ::= <var> | Number | (<expression>)
    <var> ::= A | B | C ... | Y | Z
    <relop> ::= < <= <> > >= >< =
    <eol> ::= '\n'
    <eof> ::= '\0'
=========================================================================== */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#define TRUE                1
#define FALSE               0
#define NUM_VARS            26
#define MAX_LAB_VAL         128
#define MAX_TOK_LEN         128
#define NUM_RESERVED        13
#define NUM_TOKENS          32
#define SUB_NEST            32
#define FOR_NEST            32
/* error codes */
#define UNEXPECTED_TOKEN    256
#define RELOP_IF_STMT       512
#define STMT_EXPECTED       1024

typedef enum {
    PLUS, MINUS, TIMES, OVER, MOD, EQ, LT, GT, LPAREN, RPAREN, COMMA, SEMI, IF,
    FOR, NEXT, TO, THEN, PRINT, INPUT, GOTO, GOSUB, RETURN, END, ASSIGN, ID, NUM,
    STR, LET, GET, NEQ,
    /* special */
    ERROR, ENDFILE, ENDLINE, REM
} TokenType;

typedef enum {
    START,
    INNUM,
    INID,
    INSTR,
    DONE
} ScannerState;

typedef struct ForStack ForStack;
struct ForStack {
    int var; /* counter variable */
    int target; /* target value */
    char *loc;
} fstack[FOR_NEST]; /* stack for FOR/NEXT loop */
int ftos = 0;  /* index to top of FOR stack */
char *label_table[MAX_LAB_VAL];
char *gstack[SUB_NEST]; /* stack for gosub */
int gtos = 0;  /* index to top of GOSUB stack */
int variables[NUM_VARS]; /* 26 variables A-Z */
char *prog, *curr_ch;
TokenType token;
char token_string[MAX_TOK_LEN+1];

char *readfile(const char *path);
void scan_labels(void);
void get_token(void);
TokenType res_lookup(void);
void error(const char *msg);
void serror(int errcode);
void find_eol(void);
void let_stmt(void);
void goto_stmt(void);
void if_stmt(void);
void gosub_stmt(void);
void return_stmt(void);
int expr(void);
int term(void);
int factor(void);
void match(TokenType etoken);
void program(void);
void line(void);
void statement(void);
void for_stmt(void);
void next_stmt(void);
void print_stmt(void);
void input_stmt(void);
void rem_stmt(void);
void flush_input(void);
int is_relop(void);

char test_prog[] =  "LET L = 100\n"
                    "GOSUB 10\n"
                    "END\n"

                    "REM TREAT FIRST TWO PRIMES AS SPECIAL CASE\n"
                    "10 PRINT 2\n"
                    "PRINT 3\n"
                    "LET G = 5\n"
                    "20 IF G>L THEN GOTO 30\n"
                    "    LET F = 3\n"
                    "    40 IF F*F>=G THEN GOTO 50\n"
                    "    IF G%F=0 THEN GOTO 50\n"
                    "        LET F = F+2\n"
                    "    GOTO 40\n"
                    "    50 IF G%F<>0 THEN PRINT G\n"
                    "    LET G = G+2\n"
                    "GOTO 20\n"
                    "30 RETURN\n";

int main(void/*int argc, char *argv[]*/)
{
    /*if (argc != 2) {
        fprintf(stderr, "usage: %s <filename>\n", argv[0]);
        exit(1);
    }
    curr_ch = prog = readfile(argv[1]);*/
    curr_ch = prog = test_prog;
    /*if (prog == NULL) {
        fprintf(stderr, "Error reading file.\n");
        exit(1);
    }*/
    scan_labels();
    get_token(); /* prepares for parsing */
    program();
	return 0;
}

/*
 * <program> ::= { <line> } <eof>
 */
void program(void)
{
    while (token != ENDFILE)
        line();
    match(ENDFILE);
}

/*
 * <line> ::= [ Number ] <statement> <eol>
 */
void line(void)
{
    if (token == NUM)
        match(NUM);
    statement();
    match(ENDLINE);
}

void statement(void)
{
    switch (token) {
    case ASSIGN:
        let_stmt();
        break;
    case PRINT:
        print_stmt();
        break;
    case GOTO:
        goto_stmt();
        break;
    case IF:
        if_stmt();
        break;
    case FOR:
        for_stmt();
        break;
    case NEXT:
        next_stmt();
        break;
    case INPUT:
        input_stmt();
        break;
    case GOSUB:
        gosub_stmt();
        break;
    case RETURN:
        return_stmt();
        break;
    case REM:
        rem_stmt();
        break;
    case END:
        printf("Exiting...\n");
        exit(0);
        break;
    case ENDLINE: /* ignore blank lines (not expressed in the grammar) */
        break;
    default:
        serror(STMT_EXPECTED);
        break;
    };
}

/*
 * REM <commentstring>
 */
void rem_stmt(void)
{
    match(REM);
    find_eol();
}

/*
 * INPUT <varlist>
 * <varlist> ::= <var> { , <var> }
 */
void input_stmt(void)
{
    /*match(INPUT);
    goto read;
    while (token == COMMA) {
        match(COMMA);
read:   if (token == ID) {
            printf("? ");
            scanf("%d", &variables[toupper(token_string[0])-'A']);
            flush_input();
        }
        match(ID);
    }*/
}

/*
 * PRINT <expr-list>
 * <expr-list> ::= (String | <expression>) { , (String | <expression>) }
 */
void print_stmt(void)
{
    int lf = 1;
    match(PRINT);
    goto print;
    while (token == COMMA) {
        match(COMMA);
        if (token != ENDLINE) {
print:      if (token == STR) {
                printf("%s", token_string);
                match(STR);
            } else
                printf("%d", expr());
        } else {
            /* if comma at the EOL: don't print '\n' */
            lf = 0;
            break;
        }
    }
    if (lf) printf("\n");
}

/*
 * NEXT
 */
void next_stmt(void)
{
    match(NEXT);
    if (--ftos < 0)
        error("NEXT without FOR");
    variables[fstack[ftos].var]++; /* increment control variable */
    if (variables[fstack[ftos].var] > fstack[ftos].target) /* all done */
        return;
    /* more iterations */
    curr_ch = fstack[ftos].loc; /* loop */
    ftos++; /* restore info */
}

/*
 * FOR <var> = <expression> TO <expression>
 * BUG: if the program jumps out of a FOR loop, the loop isn't removed from the
 * stack. This can lead to some tricky bugs. Similar situations arise if the
 * program jumps the other way around.
 */
void for_stmt(void)
{
    if (ftos == FOR_NEST)
        error("Too many nested FOR loops");
    match(FOR);
    if (token == ID)
        fstack[ftos].var = toupper(token_string[0])-'A';
    match(ID);
    match(EQ);
    variables[fstack[ftos].var] = expr();
    match(TO);
    fstack[ftos].target = expr();
    /* if loop can execute at least once, push info on stack */
    if (variables[fstack[ftos].var] <= fstack[ftos].target) {
        fstack[ftos].loc = curr_ch;
        ftos++;
    } else { /* otherwise, skip loop code altogether */
        while ((token!=NEXT) && (token!=ENDFILE))
            get_token();
        match(NEXT);
    }
}

/*
 * RETURN
 */
void return_stmt(void)
{
    match(RETURN);
    if (--gtos < 0)
        error("RETURN without GOSUB");
    curr_ch = gstack[gtos];
}

/*
 * GOSUB <expression>
 */
void gosub_stmt(void)
{
    int lab;
    if (gtos == SUB_NEST)
        error("Too many nested GOSUBs");
    match(GOSUB);
    lab = expr();
    if (lab > MAX_LAB_VAL)
        error("Label value too high");
    if (label_table[lab] == NULL)
        error("Undefined label");
    gstack[gtos++] = curr_ch;   /* save place to return to */
    curr_ch = label_table[lab]; /* jump */
}

/*
 * IF <expression> <relop> <expression> THEN <statement>
 */
void if_stmt(void)
{
    int x, y, cond;
    TokenType op;
    match(IF);
    x = expr();
    if (!is_relop())
        serror(RELOP_IF_STMT);
    op = token;
    match(token);
    y = expr();
    switch (op) {
    case LT: cond = x<y; break;
    case LET: cond = x<=y; break;
    case GT: cond = x>y; break;
    case GET: cond = x>=y; break;
    case EQ: cond = x==y; break;
    case NEQ: cond = x!=y; break;
    }
    if (cond) { /* is true so process target of IF */
        match(THEN);
        statement();
    } else /* execution continue in the next line */
        find_eol();
}

/*
 * GOTO <expression>
 */
void goto_stmt(void)
{
    int lab;
    match(GOTO);
    lab = expr();
    if (lab > MAX_LAB_VAL)
        error("Label value too high");
    if ((curr_ch=label_table[lab]) == NULL)
        error("Undefined label");
}

/*
 * LET var = <expression>
 */
void let_stmt(void)
{
    int index;
    match(ASSIGN);
    if (token == ID)
        index = toupper(token_string[0])-'A';
    match(ID);
    match(EQ);
    variables[index] = expr();
}

/*
 * <expression> ::= [+ | -] <term> { (+ | -) <term> }
 */
int expr(void)
{
    int temp, neg = 0;
    if (token==PLUS || token==MINUS) {
        if (token == MINUS)
            neg = 1;
        match(token);
    }
    temp = term();
    while (token==PLUS || token==MINUS) {
        switch (token) {
        case PLUS:
            match(PLUS);
            temp += term();
            break;
        case MINUS:
            match(MINUS);
            temp -= term();
            break;
        }
    }
    return (neg) ? -temp:temp;
}

/*
 * <term> ::= <factor> { (* | / | %) <factor> }
 */
int term(void)
{
    int temp = factor(), val;
    while (token==TIMES || token==OVER || token==MOD) {
        switch (token) {
        case TIMES:
            match(TIMES);
            temp *= factor();
            break;
        case OVER:
            match(OVER);
            val = factor();
            if (val != 0) temp /= val;
            else error("Division by zero");
            break;
        case MOD:
            match(MOD);
            val = factor();
            if (val != 0) temp %= val;
            else error("Division by zero");
            break;
        }
    }
    return temp;
}

/*
 * <factor> ::= <var> | Number | (<expression>)
 */
int factor(void)
{
    int temp;
    if (token == ID) {
        temp = variables[toupper(token_string[0])-'A'];
        match(ID);
    } else if (token == NUM) {
        temp = atoi(token_string);
        match(NUM);
    } else if (token == LPAREN) {
        match(LPAREN);
        temp = expr();
        match(RPAREN);
    } else
        serror(UNEXPECTED_TOKEN);
    return temp;
}

TokenType res_lookup(void)
{
    static struct {
        char *str;
        TokenType tok;
    } reserved[NUM_RESERVED] = {
        {"IF", IF},
        {"FOR", FOR},
        {"PRINT", PRINT},
        {"INPUT", INPUT},
        {"THEN", THEN},
        {"GOTO", GOTO},
        {"NEXT", NEXT},
        {"TO", TO},
        {"GOSUB", GOSUB},
        {"RETURN", RETURN},
        {"END", END},
        {"LET", ASSIGN},
        {"REM", REM}
    };
    int i;
    for (i = 0; i < NUM_RESERVED; i++)
        // if (strcasecmp(reserved[i].str, token_string) == 0)
        if (strcmp(reserved[i].str, token_string) == 0)
            return reserved[i].tok;
    return ID;
}

void get_token(void)
{
    int index = 0; /* index used for save to token_string */
    ScannerState state = START; /* current state */
    int save; /* indicates whether write or not to token_string */
    while (state != DONE) {
        int c = *curr_ch++;
        save = TRUE;
        switch (state) {
        case START:
            if (isdigit(c))
                state = INNUM;
            else if (isalpha(c))
                state = INID;
            else if (c == '"') {
                state = INSTR;
                save = FALSE;
            } else if ((c == ' ') || (c == '\t')) {
                save = FALSE;
            } else {
                state = DONE;
                switch (c) {
                case '\0':
                    save = FALSE;
                    token = ENDFILE;
                    break;
                case '\n':
                    save = FALSE;
                    token = ENDLINE;
                    break;
                case '=':
                    token = EQ;
                    break;
                case '<':
                    token_string[index++] = (char) c;
                    c = *curr_ch++;
                    if (c == '>')
                        token = NEQ; // <>
                    else if (c == '=')
                        token = LET; // <=
                    else {
                        token = LT;  // <
                        --curr_ch;
                        save = FALSE;
                    }
                    break;
                case '>':
                    token_string[index++] = (char) c;
                    c = *curr_ch++;
                    if (c == '=')
                        token = GET;  // >=
                    else if (c == '<')
                        token = NEQ;  // ><
                    else {
                        token = GT;   // >
                        --curr_ch;
                        save = FALSE;
                    }
                    break;
                case '+':
                    token = PLUS;
                    break;
                case '-':
                    token = MINUS;
                    break;
                case '*':
                    token = TIMES;
                    break;
                case '/':
                    token = OVER;
                    break;
                case '%':
                    token = MOD;
                    break;
                case '(':
                    token = LPAREN;
                    break;
                case ')':
                    token = RPAREN;
                    break;
                case ';':
                    token = SEMI;
                    break;
                case ',':
                    token = COMMA;
                    break;
                default:
                    token = ERROR;
                    break;
                }
            }
            break; /* START label */
        case INNUM:
            if (!isdigit(c)) { /* [other] */
                --curr_ch;
                save = FALSE;
                state = DONE;
                token = NUM;
            }
            break;
        case INID:
            if (!isalpha(c)) { /* [other] */
                --curr_ch;
                save = FALSE;
                state = DONE;
                token = ID;
            }
            break;
        case INSTR:
            if (c == '\0') {
                state = DONE;
                token = ENDFILE;
            } else if (c == '"') {
                state = DONE;
                token = STR;
                save = FALSE;
            }
            break;
        case DONE:
        default: /* should never occur */
            printf("Scanner Bug: state = %d\n", state);
            state = DONE;
            token = ERROR;
            exit(0);
            break;
        } /* end state switch */
        if ((save) && (index < MAX_TOK_LEN))
            token_string[index++] = (char) c;
        if (state == DONE) {
            token_string[index] = '\0';
            if (token==ID && index>1) {
                token = res_lookup();
                if (token == ID) /* variable name with more than one char */
                    token = ERROR;
            }
        }
    } /* end while */
}

/*
 * Find all labels.
 */
void scan_labels(void)
{
    int lab;
    /* fist token in the file is a label */
    get_token();
    if (token == NUM) {
        if ((lab=atoi(token_string)) > MAX_LAB_VAL)
            error("Label value too high");
        label_table[lab] = curr_ch;
    }
    if (token != ENDLINE)
        find_eol();
    do {
        get_token();
        if (token == NUM) {
            lab = atoi(token_string);
            if ((lab=atoi(token_string)) > MAX_LAB_VAL)
                error("Label value too high");
            if (label_table[lab] != NULL)
                error("Duplicate label");
            label_table[lab] = curr_ch; /* current point in program */
        }
        /* if not on a blank line, find next line */
        if (token != ENDLINE)
            find_eol();
    } while (token != ENDFILE);
    curr_ch = prog; /* restore */
}

/*
 * Find the start of the next line.
 */
void find_eol(void)
{
    while ((token!=ENDLINE) && (token!=ENDFILE))
        get_token();
}

void match(TokenType etoken)
{
    if (token == etoken) /* token == expected token ? */
        get_token();
    else
        serror(etoken);
}

int is_relop(void)
{
    return (token==LT) || (token==LET) || (token==GT) || (token==GET)
    || (token==EQ) || (token==NEQ);
}

void error(const char *msg)
{
    fprintf(stderr, "Error: %s\n", msg);
    exit(1);
}

/*
 * Report a syntax error.
 */
void serror(int errcode)
{
    static char *str_token[] = {
        "+", "-", "*", "/", "%", "=", "<", "gt", "(", ")", ",", ";", "if", "for",
        "NEXT", "TO", "THEN", "PRINT", "INPUT", "GOTO", "GOSUB", "RETURN", "END",
        "LET", "Identifier", "Number", "String", "<=", ">=", "<>", "Unknown",
        "EOF", "EOL", "REM"
    };
    switch (errcode) {
    case UNEXPECTED_TOKEN: /* error in factor() */
        fprintf(stderr, "Unexpected token '%s'.\n", str_token[token]);
        break;
    case RELOP_IF_STMT: /* error in if_stmt() */
        fprintf(stderr, "Relational operator expected in condition of IF.\n");
        break;
    case STMT_EXPECTED: /* error in statement() */
        fprintf(stderr, "Statement expected (current token: %s).\n",
        str_token[token]);
        break;
    default: /* error in match() */
        fprintf(stderr, "%s expected (current token: %s).\n", str_token[errcode],
        str_token[token]);
        break;
    }
    exit(1);
}

void flush_input(void)
{
    int c;
    while ((c=fgetc(stdin))!='\n' && (c!=EOF));
}
/*
char *readfile(const char *path)
{
    FILE *fp = fopen(path, "rb");
    if (fp != NULL) {
        long count;
        char *src;
        size_t newlen;
        fseek(fp, 0, SEEK_END);
        count = ftell(fp);
        rewind(fp);
        src = (char *)malloc(sizeof(char)*(size_t)(count+1));
        newlen = fread(src, sizeof(char), (size_t)count, fp);
        src[newlen] = '\0';
        fclose(fp);
        return src;
    } else {
        fprintf(stderr, "Error reading file\n");
        return NULL;
    }
}
*/
