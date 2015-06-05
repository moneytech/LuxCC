#include "loc.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "util.h"
#include "imp_lim.h"
#include "arena.h"

#define HASH_SIZE   1009
#define HASH_VAL(s) (hash(s)%HASH_SIZE)

static int curr_scope = 0;
static Location *location_table[MAX_NEST][HASH_SIZE]; /* location_table[0] unused */
static Arena *location_arena[MAX_NEST];

void init_location_arena(void)
{
    int i;

    for (i = 0; i < MAX_NEST; i++)
        location_arena[i] = arena_new(8192);
}

static
Location *alloc_location(void)
{
    void *p;

    if ((p=arena_alloc(location_arena[curr_scope], sizeof(Location))) == NULL)
        TERMINATE("Out of memory");

    return p;
}

void location_pop_scope(void)
{
    memset(&location_table[curr_scope][0], 0, sizeof(Location *)*HASH_SIZE);
    arena_reset(location_arena[curr_scope]);
    --curr_scope;
}

void location_push_scope(void)
{
    ++curr_scope;
}

Location *lookup_location(char *id)
{
    int n;
    unsigned h;
    Location *np;

    h = HASH_VAL(id);
    for (n = curr_scope; n >= 0; n--)
        for (np = location_table[n][h]; np != NULL; np = np->next)
            if (equal(id, np->id))
                return np;
    assert(0);
}

Location *new_location(char *id, int offset)
{
    unsigned h;
    Location *np;

    h = HASH_VAL(id);
    np = alloc_location();
    np->id = id;
    np->offset = offset;
    np->next = location_table[curr_scope][h];
    location_table[curr_scope][h] = np;
    return np;
}
