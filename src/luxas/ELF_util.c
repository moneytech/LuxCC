#include "ELF_util.h"
#include <stdlib.h>
#include <string.h>

struct StrTab {
    int siz, max;
    char **str;
    int *offs;
};

StrTab *strtab_new(void)
{
    StrTab *n;

    n = malloc(sizeof(StrTab));
    n->max = 16;
    n->str = malloc(sizeof(char *)*n->max);
    n->offs = malloc(sizeof(int)*n->max);
    n->str[0] = "";
    n->offs[0] = 0;
    n->siz = 1;
    return n;
}

void strtab_destroy(StrTab *tab)
{
    free(tab->str);
    free(tab->offs);
    free(tab);
}

int strtab_append(StrTab *tab, char *str)
{
    if (tab->siz >= tab->max) {
        tab->max *= 2;
        tab->str = realloc(tab->str, sizeof(char *)*tab->max);
        tab->offs = realloc(tab->offs, sizeof(int)*tab->max);
    }
    tab->str[tab->siz] = str;
    tab->offs[tab->siz] = tab->offs[tab->siz-1]+strlen(tab->str[tab->siz-1])+1;
    return tab->offs[tab->siz++];
}

int strtab_write(StrTab *tab, FILE *fp)
{
    int i, total = 0;

    for (i = 0; i < tab->siz; i++) {
        int n;

        n = strlen(tab->str[i])+1;
        fwrite(tab->str[i], n, 1, fp);
        total += n;
    }
    return total;
}

unsigned strtab_get_size(StrTab *tab)
{
    int i;
    unsigned n;

    for (i=0, n=0; i < tab->siz; i++)
        n += strlen(tab->str[i])+1;
    return n;
}

int strtab_get_offset(StrTab *tab, char *str)
{
    int i;

    for (i = 0; i < tab->siz; i++)
        if (strcmp(tab->str[i], str) == 0)
            return tab->offs[i];
    return -1;
}

char *strtab_get_string(StrTab *tab, int offs)
{
    int i;

    for (i = 0; i < tab->siz; i++)
        if (tab->offs[i] == offs)
            return tab->str[i];
    return NULL;
}

void strtab_copy(StrTab *tab, char *dest)
{
    int i;
    unsigned n;

    for (i = 0; i < tab->siz; i++) {
        n = strlen(tab->str[i])+1;
        memcpy(dest, tab->str[i], n);
        dest += n;
    }
}
