#ifndef LOC_H_
#define LOC_H_

typedef struct Location Location;
struct Location {
    char *id;
    int offset;
    Location *next;
};

void init_location_arena(void);
void location_pop_scope(void);
void location_push_scope(void);
Location *lookup_location(char *id);
Location *new_location(char *id, int offset);

#endif
