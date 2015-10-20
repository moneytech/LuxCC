/*
        ASM GRAMMAR
    program = statement { statement }
    statement = instruction | label | directive
    instruction = operation [ operand ] ";"
    operand = id | number
    label = id ":"
    directive = "." "text" |
                "." "data" |
                "." "bss"  |
                "." "global" id { "," id } |
                "." "extern" id { "," id } |
                "." "byte"  number |
                "." "word"  number |
                "." "dword" ( id [ "+" number ] | number ) |
                "." "res"   number |
                "." "align" number |
                "." "zero"  number
        TOKENS
    id = ( '_' | '@' | [A-Za-z] ) ( '_' | '@' | [0-9A-Za-z] )*
    number = [0-9]+
*/

#include "as.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "vm.h"
#include "../util.h"
#include "operations.h"

typedef enum {
    TOK_ID,
    TOK_NUM,
    TOK_COMMA,
    TOK_COLON,
    TOK_DOT,
    TOK_SEMI,
    TOK_PLUS,
    TOK_EOF
} Token;

char *prog_name;
char *curr, *buf;

int reading_from_stdin;
#define MAX_LINE_BUF    4096
char line_buf[MAX_LINE_BUF];

#define MAX_LEXEME  512
char lexeme[MAX_LEXEME];
Token curr_tok;
Token get_token(void);
int lineno = 1;
void match(Token expected);
void program(void);
void statement(void);
void instruction(char *operation);
void label(char *id);
void directive(void);
#define ASSEMBLER_ERR(...)  fprintf(stderr, "%s: line %d: ", prog_name, lineno), TERMINATE(__VA_ARGS__)
int read_line(void);
void init(char *file_path);

unsigned long get_num(char *s)
{
    char *ep;

    return strtoul(s, &ep, 0);
}

/*
 * Segments.
 */
#define TEXT_SEG_MAX    65536
#define DATA_SEG_MAX    65536
char text_seg[TEXT_SEG_MAX];
char data_seg[DATA_SEG_MAX];
int curr_segment = TEXT_SEG;
int text_size, data_size, bss_size;
#define CURR_OFFS() ((curr_segment==DATA_SEG)?data_size:(curr_segment==TEXT_SEG)?text_size:bss_size)

void write_char(int c)
{
    if (curr_segment == TEXT_SEG)
        text_seg[text_size++] = (char)c;
    else if (curr_segment == DATA_SEG)
        data_seg[data_size++] = (char)c;
    else
        ASSEMBLER_ERR("trying to write into .bss segment");

}

void write_short(int s)
{
    if (curr_segment == TEXT_SEG) {
        *(short *)&text_seg[text_size] = (short)s;
        text_size += 2;
    } else if (curr_segment == DATA_SEG) {
        *(short *)&data_seg[data_size] = (short)s;
        data_size += 2;
    } else {
        ASSEMBLER_ERR("trying to write into .bss segment");
    }
}

void write_int(int i)
{
    if (curr_segment == TEXT_SEG) {
        *(int *)&text_seg[text_size] = i;
        text_size += 4;
    } else if (curr_segment == DATA_SEG) {
        *(int *)&data_seg[data_size] = i;
        data_size += 4;
    } else {
        ASSEMBLER_ERR("trying to write into .bss segment");
    }
}

/*
 * Symbols.
 */
#define HASH_SIZE   1009
#define HASH(s) (hash(s)%HASH_SIZE)

typedef struct Symbol Symbol;
struct Symbol {
    char *name;
    int segment, offset;
    int kind;
    Symbol *next;
} *symbols[HASH_SIZE];
int nsym;

Symbol *lookup_symbol(char *name)
{
    Symbol *np;

    for (np = symbols[HASH(name)]; np != NULL; np = np->next)
        if (equal(np->name, name))
            break;

    return np;
}

Symbol *define_symbol(char *name, int kind, int segment, int offset)
{
    unsigned h;
    Symbol *np;

    h = HASH(name);
    for (np = symbols[h]; np != NULL; np = np->next)
        if (equal(np->name, name))
            break;

    if (np == NULL) {
        np = malloc(sizeof(Symbol));
        /* set attributes */
        np->name = strdup(name);
        np->segment = segment;
        np->offset = offset;
        np->kind = kind;
        /* chain */
        np->next = symbols[h];
        symbols[h] = np;
        ++nsym;
    } else {
        /* allow a symbol to be declared extern more than once */
        if (np->kind==EXTERN_SYM && kind==EXTERN_SYM)
            return np;

        ASSEMBLER_ERR("symbol `%s' redefined", name);
    }

    return np;
}

/*
 * Relocations.
 */
#define RELOC_TABLE_SIZE    4096
typedef struct Reloc Reloc;
struct Reloc {
    int segment, offset; /* segment+offset is where the fix must be made */
    char *symbol;        /* the information about what symbol must be fixed */
} relocation_table[RELOC_TABLE_SIZE];
int nreloc;

void append_reloc(int segment, int offset, char *symbol)
{
    relocation_table[nreloc].segment = segment;
    relocation_table[nreloc].offset = offset;
    relocation_table[nreloc].symbol = strdup(symbol);
    ++nreloc;
}

void err_no_input(void)
{
    fprintf(stderr, "%s: no input file\n", prog_name);
    exit(1);
}

int main(int argc, char *argv[])
{
    /*
                [ Object file format ]

    +-------------------------------------------------+ <-+
    | Number of entries in symbol table (4 bytes)     |   |
    +-------------------------------------------------+   |
    | Bss size in bytes (4 bytes)                     |   |
    +-------------------------------------------------+   |
    | Data size in bytes (4 bytes)                    |   |-> Header
    +-------------------------------------------------+   |
    | Text size in bytes (4 bytes)                    |   |
    +-------------------------------------------------+   |
    | Number of entries in relocation table (4 bytes) |   |
    +-------------------------------------------------+ <-+
    | Symbol table                                    |
    +-------------------------------------------------+
    | Data                                            |
    +-------------------------------------------------+
    | Text                                            |
    +-------------------------------------------------+
    | Relocation table                                |
    +-------------------------------------------------+

    Each entry of the relocation table:
        - segment: indicates if the fix must be made into the data or text segment.
        - offset: the offset from the start of the segment where the fix must be made.
        - symbol: indicates the location of what symbol must be corrected.
    The job of the linker is:
        for each entry of the reloc table do:
            *(segment+offset) = final (static) offset of symbol;
    */

    int i;
    FILE *fout;
    char *outpath, *inpath;

    prog_name = argv[0];
    if (argc == 1)
        err_no_input();
    outpath = NULL;
    for (i = 1; i < argc; i++) {
        if (argv[i][0]!='-' || argv[i][1]=='\0') {
            inpath = argv[i];
            continue;
        }
        switch (argv[i][1]) {
        case 'o':
            if (argv[i][2] != '\0') {
                outpath = argv[i]+2;
            } else if (argv[i+1] == NULL) {
                fprintf(stderr, "%s: option `o' requires an argument\n", prog_name);
                exit(1);
            } else {
                outpath = argv[++i];
            }
            break;
        case 'h':
            printf("usage: %s [ options ] <input-file>\n"
                   "  The available options are:\n"
                   "    -o<file>    write output to <file>\n"
                   "    -h          print this help\n"
                   "\nnote: if the input file is - the program is read from the standard input\n", prog_name);
            exit(0);
            break;
        default:
            fprintf(stderr, "%s: unknown option `%s'\n", prog_name, argv[i]);
            exit(1);
        }
    }
    if (inpath == NULL)
        err_no_input();
    init(equal(inpath, "-") ? NULL : inpath);

    curr_tok = get_token();
    program();
    match(TOK_EOF);

    fout = (outpath != NULL) ? fopen(outpath, "wb") : stdout;
    fwrite(&nsym, sizeof(int), 1, fout);
    fwrite(&bss_size, sizeof(int), 1, fout);
    fwrite(&data_size, sizeof(int), 1, fout);
    fwrite(&text_size, sizeof(int), 1, fout);
    fwrite(&nreloc, sizeof(int), 1, fout);

    for (i = 0; i < HASH_SIZE; i++) {
        if (symbols[i] != NULL) {
            Symbol *np;

            for (np = symbols[i]; np != NULL; np = np->next) {
                fwrite(np->name, strlen(np->name)+1, 1, fout);
                fwrite(&np->segment, sizeof(int), 1, fout);
                fwrite(&np->offset, sizeof(int), 1, fout);
                fwrite(&np->kind, sizeof(int), 1, fout);
            }
        }
    }

    fwrite(data_seg, data_size, 1, fout);
    fwrite(text_seg, text_size, 1, fout);

    for (i = 0; i < nreloc; i++) {
        /*
         * If there is a reloc for a label but that label is not into the module's symbol
         * table, it means that the label was used into the code but not declared anywhere.
         */
        if (lookup_symbol(relocation_table[i].symbol) == NULL)
            TERMINATE("%s: symbol `%s' undefined", prog_name, relocation_table[i].symbol);

        fwrite(&relocation_table[i].segment, sizeof(int), 1, fout);
        fwrite(&relocation_table[i].offset, sizeof(int), 1, fout);
        fwrite(relocation_table[i].symbol, strlen(relocation_table[i].symbol)+1, 1, fout);
    }
    if (fout != stdout)
        fclose(fout);

    return 0;
}

/* program = statement { statement } */
void program(void)
{
    statement();
    while (curr_tok != TOK_EOF)
        statement();
}

/* statement = instruction | label | directive */
void statement(void)
{
    if (curr_tok == TOK_DOT) {
        directive();
    } else if (curr_tok == TOK_ID) {
        char id[MAX_SYM_LEN];

        strcpy(id, lexeme);
        match(TOK_ID);
        if (curr_tok == TOK_COLON)
            label(id);
        else
            instruction(id);
    } else {
        ASSEMBLER_ERR("expecting instruction, label, or directive (got `%s')", lexeme);
    }
}

/* label = id ":" */
void label(char *id)
{
    define_symbol(id, LOCAL_SYM, curr_segment, CURR_OFFS());
    match(TOK_COLON);
}

/* instruction = operation [ operand ] ";" */
void instruction(char *operation)
{
    Operation *op_entry;

    if ((op_entry=lookup_operation(operation)) == NULL)
        ASSEMBLER_ERR("unknown operation `%s'", operation);
    write_char(op_entry->opcode);
    if (op_entry->has_operand) {
        if (curr_tok == TOK_ID) {
            append_reloc(curr_segment, CURR_OFFS(), lexeme);
            match(TOK_ID);
            write_int(0);
        } else if (curr_tok == TOK_NUM) {
            write_int(get_num(lexeme));
            match(TOK_NUM);
        } else if (curr_tok == TOK_SEMI) {
            ASSEMBLER_ERR("operation `%s' requires an operand", operation);
         } else {
            ASSEMBLER_ERR("invalid operand to operation `%s'", operation);
        }
    }
    match(TOK_SEMI);
}

void globalize_symbol(char *name)
{
    Symbol *s;

    if ((s=lookup_symbol(name)) == NULL)
        ASSEMBLER_ERR("global directive applied to undefined symbol `%s'", name);

    if (s->kind == EXTERN_SYM) /* global can only be applied to local symbols */
        ASSEMBLER_ERR("symbol `%s' redefined", name);

    /* allow global to be applied to a symbol more than once */
    s->kind = GLOBAL_SYM;
}

/*
 * directive = "." "text" |
 *             "." "data" |
 *             "." "bss"  |
 *             "." "global" id { "," id } |
 *             "." "extern" id { "," id } |
 *             "." "byte"  number |
 *             "." "word"  number |
 *             "." "dword" ( id [ "+" number ] | number ) |
 *             "." "res"   number |
 *             "." "align" number |
 *             "." "zero"  number
 */
void directive(void)
{
    match(TOK_DOT);
    if (curr_tok != TOK_ID)
        ASSEMBLER_ERR("expecting directive (got `%s')", lexeme);

    /* dispatch directive */
    if (equal(lexeme, "text")) {
        match(TOK_ID);
        curr_segment = TEXT_SEG;
    } else if (equal(lexeme, "data")) {
        match(TOK_ID);
        curr_segment = DATA_SEG;
    } else if (equal(lexeme, "bss")) {
        match(TOK_ID);
        curr_segment = BSS_SEG;
    } else if (equal(lexeme, "global")) {
        match(TOK_ID);
        if (curr_tok == TOK_ID)
            globalize_symbol(lexeme);
        match(TOK_ID);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            if (curr_tok == TOK_ID)
                globalize_symbol(lexeme);
            match(TOK_ID);
        }
    } else if (equal(lexeme, "extern")) {
        match(TOK_ID);
        if (curr_tok == TOK_ID)
            define_symbol(lexeme, EXTERN_SYM, 0, 0);
        match(TOK_ID);
        while (curr_tok == TOK_COMMA) {
            match(TOK_COMMA);
            if (curr_tok == TOK_ID)
                define_symbol(lexeme, EXTERN_SYM, 0, 0);
            match(TOK_ID);
        }
    } else if (equal(lexeme, "byte")) {
        match(TOK_ID);
        if (curr_tok == TOK_NUM)
            write_char(get_num(lexeme));
        match(TOK_NUM);
    } else if (equal(lexeme, "word")) {
        match(TOK_ID);
        if (curr_tok == TOK_NUM)
            write_short(get_num(lexeme));
        match(TOK_NUM);
    } else if (equal(lexeme, "dword")) {
        match(TOK_ID);
        if (curr_tok == TOK_ID) {
            append_reloc(curr_segment, CURR_OFFS(), lexeme);
            match(TOK_ID);
            if (curr_tok == TOK_PLUS) {
                match(TOK_PLUS);
                if (curr_tok == TOK_NUM)
                    write_int(get_num(lexeme));
                match(TOK_NUM);
            } else {
                write_int(0);
            }
        } else if (curr_tok == TOK_NUM) {
            write_int(get_num(lexeme));
            match(TOK_NUM);
        } else {
            ASSEMBLER_ERR("invalid operand to directive `.dword'");
        }
    } else if (equal(lexeme, "res")) {
        match(TOK_ID);
        if (curr_tok == TOK_NUM) {
            if (curr_segment != BSS_SEG)
                ASSEMBLER_ERR("`.res' can only be used in the .bss segment");
            bss_size += get_num(lexeme);
        }
        match(TOK_NUM);
    } else if (equal(lexeme, "align")) {
        match(TOK_ID);
        if (curr_tok == TOK_NUM) {
            int curr_size, new_size;

            curr_size = CURR_OFFS();
            new_size = round_up(curr_size, get_num(lexeme));
            while (curr_size++ < new_size)
                if (curr_segment == BSS_SEG)
                    ++bss_size;
                else
                    write_char(OpNop);
        }
        match(TOK_NUM);
    } else if (equal(lexeme, "zero")) {
        match(TOK_ID);
        if (curr_tok == TOK_NUM) {
            unsigned long n;

            for (n = get_num(lexeme); n != 0; n--)
                write_char(0);
        }
        match(TOK_NUM);
    } else {
        ASSEMBLER_ERR("unknown directive `%s'", lexeme);
    }
}

Token get_token(void)
{
    enum {
        START,
        DONE,
        INID,
        INNUM,
        INCOMMENT
    };
    Token tok;
    int state;
    int save, cind;

    cind = 0;
    state = START;
    while (state != DONE) {
        int c;

        c = *curr++;
        save = 1;
        switch (state) {
        case START:
            if (c==' ' || c=='\t' || c=='\n') {
                save = 0;
                if (c == '\n')
                    ++lineno;
            } else if (isdigit(c)) {
                state = INNUM;
            } else if (isalpha(c) || c=='_' || c=='@') {
                state = INID;
            } else if (c == '#') {
                save = 0;
                state = INCOMMENT;
            } else {
                state = DONE;
                switch (c) {
                case '\0':
                    save = 0;
                    if (reading_from_stdin && read_line())
                        state = START;
                    else
                        tok = TOK_EOF;
                    break;
                case ',':
                    tok = TOK_COMMA;
                    break;
                case ';':
                    tok = TOK_SEMI;
                    break;
                case ':':
                    tok = TOK_COLON;
                    break;
                case '.':
                    tok = TOK_DOT;
                    break;
                case '+':
                    tok = TOK_PLUS;
                    break;
                default:
                    /* ignore stray character */
                    save = 0;
                    state = START;
                    break;
                }
            }
            break;
        case INID:
            if (!isalnum(c) && c!='_' && c!='@') {
                save = 0;
                --curr;
                tok = TOK_ID;
                state = DONE;
            }
            break;
        case INNUM:
            if (!isdigit(c)) {
                save = 0;
                --curr;
                tok = TOK_NUM;
                state = DONE;
            }
            break;
        case INCOMMENT:
            save = 0;
            if (c == '\n') {
                ++lineno;
                state = START;
            } else if (c == '\0') {
                --curr;
                state = START;
            }
            break;
        default: /* unreachable */
            break;
        }
        if (save)
            lexeme[cind++] = (char)c;
        if (state == DONE)
            lexeme[cind] = '\0';
    }

    return tok;
}

void match(Token expected)
{
    if (curr_tok != expected)
        ASSEMBLER_ERR("syntax error: unexpected `%s'", lexeme);
    curr_tok = get_token();
}

int read_line(void)
{
    curr = line_buf;
    return (fgets(line_buf, MAX_LINE_BUF, stdin) != NULL);
}

void init(char *file_path)
{
    if (file_path != NULL) {
        FILE *fp;
        unsigned len;

        fp = fopen(file_path, "rb");
        if (fp == NULL)
            TERMINATE("%s: error reading file `%s'", prog_name, file_path);
        fseek(fp, 0, SEEK_END);
        len = ftell(fp);
        rewind(fp);
        curr = buf = malloc(len+1);
        len = fread(buf, 1, len, fp);
        buf[len] = '\0';
    } else {
        reading_from_stdin = TRUE;
        read_line();
    }
}
