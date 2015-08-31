#include "str.h"
#include <stdlib.h>
#include <stdarg.h>

struct String {
    char *buf;
    unsigned buf_max, buf_next;
};

String *string_new(unsigned siz)
{
    String *s;

    s = malloc(sizeof(String));
    s->buf = malloc(siz);
    s->buf_max = siz;
    s->buf_next = 0;
    return s;
}

void string_free(String *s)
{
    free(s->buf);
    free(s);
}

void string_clear(String *s)
{
    s->buf_next = 0;
}

int string_printf(String *s, char *fmt, ...)
{
	va_list args;
    unsigned n, n2;

	va_start(args, fmt);
    n = s->buf_max-s->buf_next;
    if ((n2=vsnprintf(s->buf+s->buf_next, n, fmt, args)) >= n) {
        char *p;

        s->buf_max = s->buf_max*2+n2;
        if ((p=realloc(s->buf, s->buf_max)) == NULL) {
            fprintf(stderr, "string_printf: out of memory\n");
            free(s->buf);
            exit(EXIT_FAILURE);
        }
        s->buf = p;

        va_end(args);
        va_start(args, fmt);
        vsprintf(s->buf+s->buf_next, fmt, args);
    }
    s->buf_next += n2;
	va_end(args);
    return n2;
}

void string_write(String *s, FILE *fp)
{
    fwrite(s->buf, 1, s->buf_next, fp);
}

char *string_curr(String *s)
{
    return s->buf+s->buf_next;
}

unsigned string_get_pos(String *s)
{
    return s->buf_next;
}

void string_set_pos(String *s, unsigned n)
{
    s->buf_next = n;
}
