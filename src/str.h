#ifndef STR_H_
#define STR_H_

#include <stdio.h>

typedef struct String String;

/*
 * siz is the initial size;
 * the string will grow automatically when necessary.
 */
String *string_new(unsigned siz);
void string_free(String *s);
void string_printf(String *s, char *fmt, ...);
unsigned string_length(String *s); /* including '\0' */
void string_write(String *s, FILE *fp);
void string_clear(String *s);
char *string_curr(String *s);
unsigned string_get_pos(String *s);
void string_set_pos(String *s, unsigned n);

#endif
