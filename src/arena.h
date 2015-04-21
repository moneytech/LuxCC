#ifndef ARENA_H_
#define ARENA_H_

/* growable memory arena */
typedef struct Arena Arena;

Arena *arena_new(unsigned size);
void *arena_alloc(Arena *a, unsigned n);
void arena_reset(Arena *a);
void arena_destroy(Arena *a);

#endif
