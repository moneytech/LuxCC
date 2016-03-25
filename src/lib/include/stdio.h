#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h> /* for size_t, NULL */
#include <stdarg.h> /* for va_list */

#define _IOFBF  0x01
#define _IOLBF  0x02
#define _IONBF  0x04

#define EOF             (-1)
#define BUFSIZ          8192
#define FOPEN_MAX       16
#define FILENAME_MAX    4096
#define L_tmpnam        32

#ifndef SEEK_SET
#define SEEK_SET        0
#define SEEK_CUR        1
#define SEEK_END        2
#endif

#define TMP_MAX         5000

typedef struct _FILE FILE;
typedef long fpos_t;

extern FILE *stdin, *stdout, *stderr;
#define stdin   stdin
#define stdout  stdout
#define stderr  stderr

// ===========
// Standard C
// ===========

/* Operations on files */
int remove(const char *filename);
int rename(const char *oldpath, const char *newpath);
FILE *tmpfile(void);
char *tmpnam(char *s);

/* File access functions */
int fclose(FILE *stream);
int fflush(FILE *stream);
FILE *fopen(const char *filename, const char *mode);
FILE *freopen(const char *filename, const char *mode, FILE *stream);
void setbuf(FILE *stream, char *buf);
int setvbuf(FILE *stream, char *buf, int mode, size_t size);

/* Formatted input/output functions */
int fprintf(FILE *stream, const char *format, ...);
int fscanf(FILE *stream, const char *format, ...);
int printf(const char *format, ...);
int scanf(const char *format, ...);
int snprintf(char *str, size_t size, const char *format, ...);
int sprintf(char *s, const char *format, ...);
int sscanf(const char *s, const char *format, ...);
int vfprintf(FILE *stream, const char *format, va_list ap);
int vfscanf(FILE *stream, const char *format, va_list ap);
int vprintf(const char *format, va_list ap);
int vscanf(const char *format, va_list ap);
int vsnprintf(char *str, size_t size, const char *format, va_list ap);
int vsprintf(char *s, const char *format, va_list ap);
int vsscanf(const char *str, const char *format, va_list ap);

/* Character input/output functions */
int fgetc(FILE *stream);
int fputc(int c, FILE *stream);
char *fgets(char *s, int n, FILE *stream);
int fputs(const char *s, FILE *stream);
char *gets(char *s);
int puts(const char *s);
int ungetc(int c, FILE *stream);
int getc(FILE *stream);
int getchar(void);
int putc(int c, FILE *stream);
int putchar(int c);

/* Direct input/output functions */
size_t fread(void *ptr, size_t size, size_t nelem, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nelem, FILE *stream);

/* File positioning functions */
int fgetpos(FILE *stream, fpos_t *pos);
int fseek(FILE *stream, long offset, int whence);
int fsetpos(FILE *stream, const fpos_t *pos);
long ftell(FILE *stream);
void rewind(FILE *stream);

/* Error-handling functions */
void clearerr(FILE *stream);
int feof(FILE *stream);
int ferror(FILE *stream);
void perror(const char *s);

// ===========
// POSIX
// ===========

/*FILE *fdopen(int fd, const char *mode);*/
int fileno(FILE *stream);

#endif
