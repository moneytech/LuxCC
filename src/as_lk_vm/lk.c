#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include "as.h"
#include "../arena.h"
#include "../util.h"

char *prog_name;

/*
 * Segments.
 */
#define TEXT_SEG_MAX    524288
#define DATA_SEG_MAX    65536
char text_seg[TEXT_SEG_MAX];
char data_seg[DATA_SEG_MAX];
int text_size, data_size, bss_size;
#define SEG_SIZ(seg) ((seg==DATA_SEG)?data_size:(seg==TEXT_SEG)?text_size:bss_size)
#define MAX_ALIGN       4 /* most restrictive alignment */

/*
 * Symbols.
 */
#define HASH_SIZE   1009
#define HASH(s)     (hash(s)%HASH_SIZE)
typedef struct Symbol Symbol;
struct Symbol {
    char *name;
    int segment, offset;
    int kind;
    Symbol *next;
};
/*
 * Global/Extern symbols.
 */
Symbol *symbols[HASH_SIZE];

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
    } else {
        if (np->kind == EXTERN_SYM) {
            if (kind == GLOBAL_SYM) {
                np->kind = GLOBAL_SYM; /* now defined */
                np->segment = segment;
                np->offset = offset;
            }
        } else if (np->kind == GLOBAL_SYM) {
            if (kind == GLOBAL_SYM)
                TERMINATE("%s: multiple definition of `%s'", prog_name, name);
        }
    }

    return np;
}

Symbol *lookup_global_symbol(char *name)
{
    unsigned h;
    Symbol *np;

    h = HASH(name);
    for (np = symbols[h]; np != NULL; np = np->next)
        if (equal(np->name, name))
             return np;

    assert(0);
}

/*
 * Local symbols.
 */
Symbol *local_symbols[HASH_SIZE]; /* flushed after each module processing */

Arena *local_arena;

void init_local_table(void)
{
    local_arena = arena_new(32768, FALSE);
}

void *new_local_symbol(void)
{
    void *p;

    if ((p=arena_alloc(local_arena, sizeof(Symbol))) == NULL)
        TERMINATE("%s: out of memory", prog_name);

    return p;
}

void reset_local_table(void)
{
    memset(local_symbols, 0, sizeof(Symbol *)*HASH_SIZE);
    arena_reset(local_arena);
}

Symbol *define_local_symbol(char *name, int segment, int offset)
{
    unsigned h;
    Symbol *np;

    h = HASH(name);
    for (np = local_symbols[h]; np != NULL; np = np->next)
        if (equal(np->name, name))
            break;

    if (np == NULL) {
        np = new_local_symbol();
        /* set attributes */
        np->name = strdup(name);
        np->segment = segment;
        np->offset = offset;
        np->kind = LOCAL_SYM;
        /* chain */
        np->next = local_symbols[h];
        local_symbols[h] = np;
    } else {
        /* assembler bug: it should have detected this redefinition */
        assert(0);
    }

    return np;
}

/* general lookup function */
Symbol *lookup_symbol(char *name)
{
    unsigned h;
    Symbol *np;

    h = HASH(name);
    /* local symbols have precedence over extern/global symbols */
    for (np = local_symbols[h]; np != NULL; np = np->next)
        if (equal(np->name, name))
             return np;
    for (np = symbols[h]; np != NULL; np = np->next)
        if (equal(np->name, name))
             return np;

    assert(0);
}

/*
 * Relocations.
 */
#define TEXT_RELOC_TABLE_SIZE   16384
#define DATA_RELOC_TABLE_SIZE   16384
typedef struct Reloc Reloc;
struct Reloc {
    int segment, offset;
    char *symbol;
} text_relocation_table[TEXT_RELOC_TABLE_SIZE], data_relocation_table[DATA_RELOC_TABLE_SIZE];
int ntreloc, ndreloc;

void append_text_reloc(int segment, int offset, char *symbol)
{
    text_relocation_table[ntreloc].segment = segment;
    text_relocation_table[ntreloc].offset = offset;
    text_relocation_table[ntreloc].symbol = symbol;
    ++ntreloc;
}

void append_data_reloc(int segment, int offset, char *symbol)
{
    data_relocation_table[ndreloc].segment = segment;
    data_relocation_table[ndreloc].offset = offset;
    data_relocation_table[ndreloc].symbol = symbol;
    ++ndreloc;
}

void err_no_input(void)
{
    fprintf(stderr, "%s: no input file\n", prog_name);
    exit(1);
}

int main(int argc, char *argv[])
{
    /*
                [ Executable file format ]

    +-------------------------------------------------+ <-+
    | Bss size in bytes (4 bytes)                     |   |
    +-------------------------------------------------+   |
    | Data size in bytes (4 bytes)                    |   |
    +-------------------------------------------------+   |
    | Text size in bytes (4 bytes)                    |   |-> Header
    +-------------------------------------------------+   |
    | Number of entries in data relocation table      |   |
    +-------------------------------------------------+   |
    | Number of entries in text relocation table      |   |
    +-------------------------------------------------+ <-+
    | Data                                            |
    +-------------------------------------------------+
    | Text                                            |
    +-------------------------------------------------+
    | Data relocation table                           |
    +-------------------------------------------------+
    | Text relocation table                           |
    +-------------------------------------------------+


    Each entry of the relocation tables:
        - offset: the offset from the start of the segment (data or text) where the fix
        must be made.
        - segment: indicates if the runtime start address of the bss, data, or text segment
        must be added to do the fix.
    */

    int i;
    FILE *fout;
    char *outpath;
    char *infiles[64];
    int ninf;

    prog_name = argv[0];
    if (argc == 1)
        err_no_input();
    ninf = 1; /* infiles[0] == crt.o */
    outpath = "a.out.vme";
    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '-') {
            infiles[ninf++] = argv[i];
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
            printf("usage: %s [ options ] <input-file> ...\n"
                   "  The available options are:\n"
                   "    -o<file>    write output to <file>\n"
                   "    -h          print this help\n", prog_name);
            exit(0);
            break;
        case '\0':
            break;
        default:
            fprintf(stderr, "%s: unknown option `%s'\n", prog_name, argv[i]);
            exit(1);
        }
    }
    if (ninf == 1)
        err_no_input();

    /*
     * crt.o must be the first file (crt.o's code must be physically at the
     * beginning of the resulting executable). An alternative would be to label
     * crt.o's code as, say, `_start:' and emit an "jmp _start;" before the
     * program's code.
     * crt.o has code to initialize some variables and call main.
     */
    init_local_table();
    // infiles[0] = "/usr/local/lib/luxcc/crt.o";
    infiles[0] = "src/lib/crt.o";
    for (i = 0; i < ninf; i++) {
        FILE *fin;
        char name[MAX_SYM_LEN], *cp;
        int j;
        int nsym;
        int curr_bss_size;
        int curr_text_size;
        int curr_data_size;
        int curr_nreloc;
        int segment, offset, kind;

        if ((fin=fopen(infiles[i], "rb")) == NULL)
            TERMINATE("%s: error reading file `%s'", prog_name, infiles[i]);

        /* header */
        fread(&nsym, sizeof(int), 1, fin);
        fread(&curr_bss_size, sizeof(int), 1, fin);
        fread(&curr_data_size, sizeof(int), 1, fin);
        fread(&curr_text_size, sizeof(int), 1, fin);
        fread(&curr_nreloc, sizeof(int), 1, fin);

        /* symbol table entries */
        for (j = 0; j < nsym; j++) {
            for (cp = name; ; cp++) {
                if ((*cp=(char)fgetc(fin)) == '\0')
                    break;
            }
            fread(&segment, sizeof(int), 1, fin);
            fread(&offset, sizeof(int), 1, fin);
            fread(&kind, sizeof(int), 1, fin);
            if (kind == LOCAL_SYM)
                define_local_symbol(name, segment, SEG_SIZ(segment)+offset);
            else
                define_symbol(name, kind, segment, SEG_SIZ(segment)+offset);
        }

        /* data&text */
        fread(data_seg+data_size, curr_data_size, 1, fin);
        fread(text_seg+text_size, curr_text_size, 1, fin);

        /* relocation table entries */
        for (j = 0; j < curr_nreloc; j++) {
            Symbol *s;

            fread(&segment, sizeof(int), 1, fin);
            fread(&offset, sizeof(int), 1, fin);
            for (cp = name; ; cp++) {
                if ((*cp=(char)fgetc(fin)) == '\0')
                    break;
            }
            s = lookup_symbol(name);
            if (s->kind != EXTERN_SYM) {
                if (segment == TEXT_SEG) {
                    *(int *)&text_seg[SEG_SIZ(segment)+offset] += s->offset;
                    append_text_reloc(s->segment, SEG_SIZ(segment)+offset, NULL);
                } else {
                    *(int *)&data_seg[SEG_SIZ(segment)+offset] += s->offset;
                    append_data_reloc(s->segment, SEG_SIZ(segment)+offset, NULL);
                }
            } else {
                /*
                 * The file that contains the symbol definition
                 * has not been seen yet. Mark it to fix later.
                 */
                if (segment == TEXT_SEG)
                    append_text_reloc(0, SEG_SIZ(segment)+offset, strdup(name));
                else
                    append_data_reloc(0, SEG_SIZ(segment)+offset, strdup(name));
            }
        }

        bss_size  += round_up(curr_bss_size,  MAX_ALIGN);
        data_size += round_up(curr_data_size, MAX_ALIGN);
        text_size += round_up(curr_text_size, MAX_ALIGN);
        reset_local_table();
        fclose(fin);
    }

    /*
     * Try to fix the relocs that couldn't be fixed before
     * because they depended on not-yet-defined extern symbols.
     * Fail if a symbol is still undefined.
     */
    for (i = 0; i < ntreloc; i++) {
        if (text_relocation_table[i].symbol != NULL) {
            Symbol *s;

            if ((s=lookup_global_symbol(text_relocation_table[i].symbol))->kind == EXTERN_SYM)
                TERMINATE("%s: undefined reference to `%s'", prog_name, s->name);
            *(int *)&text_seg[text_relocation_table[i].offset] += s->offset;
            text_relocation_table[i].segment = s->segment;
        }
    }
    for (i = 0; i < ndreloc; i++) {
        if (data_relocation_table[i].symbol != NULL) {
            Symbol *s;

            if ((s=lookup_global_symbol(data_relocation_table[i].symbol))->kind == EXTERN_SYM)
                TERMINATE("%s: undefined reference to `%s'", prog_name, s->name);
            *(int *)&data_seg[data_relocation_table[i].offset] += s->offset;
            data_relocation_table[i].segment = s->segment;
        }
    }

    /* Write the final executable file. */
    fout = fopen(outpath, "wb");
    /* header */
    fwrite(&bss_size, sizeof(int), 1, fout);
    fwrite(&data_size, sizeof(int), 1, fout);
    fwrite(&text_size, sizeof(int), 1, fout);
    fwrite(&ndreloc, sizeof(int), 1, fout);
    fwrite(&ntreloc, sizeof(int), 1, fout);
    /* data&text */
    fwrite(data_seg, data_size, 1, fout);
    fwrite(text_seg, text_size, 1, fout);
    /* data relocation table */
    for (i = 0; i < ndreloc; i++) {
        fwrite(&data_relocation_table[i].segment, sizeof(int), 1, fout);
        fwrite(&data_relocation_table[i].offset, sizeof(int), 1, fout);
    }
    /* text relocation table */
    for (i = 0; i < ntreloc; i++) {
        fwrite(&text_relocation_table[i].segment, sizeof(int), 1, fout);
        fwrite(&text_relocation_table[i].offset, sizeof(int), 1, fout);
    }
    fclose(fout);

    return 0;
}
