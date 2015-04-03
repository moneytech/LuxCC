#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "as.h"
#include "../arena.h"
#include "../util.h"

char *prog_name;

/*
 * Segments.
 */
#define TEXT_SEG_MAX    8192
#define DATA_SEG_MAX    8192
char text_seg[TEXT_SEG_MAX];
char data_seg[DATA_SEG_MAX];
int text_size, data_size, bss_size;
#define SEG_SIZ(seg) ((seg==DATA_SEG)?data_size:(seg==TEXT_SEG)?text_size:bss_size)

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

    my_assert(0, "lookup_global_symbol()");
}

/*
 * Local symbols.
 */
Symbol *local_symbols[HASH_SIZE]; /* flushed after each module processing */

Arena *local_arena;

void init_local_table(void)
{
    local_arena = arena_new(8192);
}

void *new_local_symbol(void)
{
    return arena_alloc(local_arena, sizeof(Symbol));
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
        my_assert(0, "define_local_symbol()");
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

    my_assert(0, "lookup_symbol()");
}

/*
 * Relocations.
 */
#define TEXT_RELOC_TABLE_SIZE   1024
#define DATA_RELOC_TABLE_SIZE   1024
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

int main(int argc, char *argv[])
{
    /*
                [ Executable file format ]

    +-------------------------------------------------+ <-+
    | Bss size in bytes (4 bytes)                     |   |-> Bss stuff
    +-------------------------------------------------+ <-+
    | Data size in bytes (4 bytes)                    |   |
    +-------------------------------------------------+   |
    | Data                                            |   |
    +-------------------------------------------------+   |-> Data stuff
    | Number of entries in data relocation table      |   |
    +-------------------------------------------------+   |
    | Data relocation table                           |   |
    +-------------------------------------------------+ <-+
    | Text size in bytes (4 bytes)                    |   |
    +-------------------------------------------------+   |
    | Text                                            |   |
    +-------------------------------------------------+   |-> Text stuff
    | Number of entries in text relocation table      |   |
    +-------------------------------------------------+   |
    | Text relocation table                           |   |
    +-------------------------------------------------+ <-+

    Each entry of the relocation tables:
        - offset: the offset from the start of the segment (data or text) where the fix
        must be made.
        - segment: indicates if the runtime start address of the bss, data, or text segment
        must be added to do the fix.
    */

    int i;
    FILE *fout;

    prog_name = argv[0];
    if (argc < 3) {
        printf("usage: %s <output-file> <intput-file> { <input-file> }\n", prog_name);
        printf("e.g.:  %s prog.out f1.in f2.in f3.in\n", prog_name);
        exit(0);
    }

    init_local_table();

    for (i = 2; i < argc; i++) { /* object files */
        FILE *fin;
        char name[MAX_SYM_LEN], *cp;
        int j;
        int nsym;
        int curr_bss_size;
        int curr_text_size;
        int curr_data_size;
        int curr_nreloc;
        int segment, offset, kind;

        if ((fin=fopen(argv[i], "rb")) == NULL)
            TERMINATE("%s: error reading file `%s'", prog_name, argv[i]);

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
            /*printf("name=%s, segment=%d, offset=%d, kind=%d\n", name, segment, SEG_SIZ(segment)+offset, kind);*/
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
                    *(int *)&text_seg[SEG_SIZ(segment)+offset] = s->offset;
                    append_text_reloc(s->segment, SEG_SIZ(segment)+offset, NULL);
                } else {
                    *(int *)&data_seg[SEG_SIZ(segment)+offset] = s->offset;
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

        bss_size += curr_bss_size;
        data_size += curr_data_size;
        text_size += curr_text_size;
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
            *(int *)&text_seg[text_relocation_table[i].offset] = s->offset;
            text_relocation_table[i].segment = s->segment;
        }
    }
    for (i = 0; i < ndreloc; i++) {
        if (data_relocation_table[i].symbol != NULL) {
            Symbol *s;

            if ((s=lookup_global_symbol(data_relocation_table[i].symbol))->kind == EXTERN_SYM)
                TERMINATE("%s: undefined reference to `%s'", prog_name, s->name);
            *(int *)&data_seg[data_relocation_table[i].offset] = s->offset;
            data_relocation_table[i].segment = s->segment;
        }
    }

    /*
     * Write the final executable file.
     */
    if ((fout=fopen(argv[1], "wb")) == NULL)
        TERMINATE("%s: error while trying to write to file `%s'", prog_name, argv[1]);
    /* bss */
    fwrite(&bss_size, sizeof(int), 1, fout);
    /* data */
    fwrite(&data_size, sizeof(int), 1, fout);
    fwrite(data_seg, data_size, 1, fout);
    fwrite(&ndreloc, sizeof(int), 1, fout);
    for (i = 0; i < ndreloc; i++) {
        fwrite(&data_relocation_table[i].segment, sizeof(int), 1, fout);
        fwrite(&data_relocation_table[i].offset, sizeof(int), 1, fout);
    }
    /* text */
    fwrite(&text_size, sizeof(int), 1, fout);
    fwrite(text_seg, text_size, 1, fout);
    fwrite(&ntreloc, sizeof(int), 1, fout);
    for (i = 0; i < ntreloc; i++) {
        fwrite(&text_relocation_table[i].segment, sizeof(int), 1, fout);
        fwrite(&text_relocation_table[i].offset, sizeof(int), 1, fout);
    }
    fclose(fout);

    return 0;
}
