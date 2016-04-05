#include <stdio.h>
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>
#include <stdarg.h>
#include <errno.h>
#include <ctype.h>
#include <time.h>
#include <termios.h>
#include <sys/stat.h>
#include <sys/syscall.h>

#define O_WRITABLE 3 /* assume O_WRONLY==1 && O_RDWR==2 */

enum {
    _IOEOF    = 0x08,   /* EOF has occurred on the stream */
    /*
     * The policy adopted for _IOERR is that it will be
     * set only when a call to read() or write() fails.
     */
    _IOERR    = 0x10,
    _IORD     = 0x20,   /* last operation on the stream was a read */
    _IOWR     = 0x40,   /* last operation on the stream was a write */
    _IOOPEN   = 0x80,   /* the stream is currently associated with an external file */
    _IOEXTBUF = 0x100,  /* we are using a user-supplied buffer */
    _IOTMP    = 0x200,  /* removed when closed or at exit */
};

struct _FILE {
    int _fd;
    unsigned char *_buf;
    size_t _bufsiz;
    size_t _pos;        /* next buffer position to read or write */
    size_t _cnt;        /* # of characters left to read */
    int _flags;         /* stream's flags */
    int _oflags;        /* open()'s flags */
    int _putback;       /* ungetc()'d char on unbuffered streams */
    char _filename[FILENAME_MAX];
};

FILE __iob[FOPEN_MAX] = {
    { STDIN_FILENO,  NULL, 0, 0, 0, _IOOPEN,        O_RDONLY, EOF },
    { STDOUT_FILENO, NULL, 0, 0, 0, _IOOPEN,        O_WRONLY, EOF },
    { STDERR_FILENO, NULL, 0, 0, 0, _IOOPEN|_IONBF, O_WRONLY, EOF },
};
FILE *stdin  = &__iob[0];
FILE *stdout = &__iob[1];
FILE *stderr = &__iob[2];
static FILE *free_stream;   /* for freopen() */
static int must_not_exist;  /* for tmpfile() */

void __set_std_buffering(void)
{
    /*
     * Set the buffering mode of stdin and stdout (stderr is always
     * unbuffered by default). They are fully buffered if don't refer
     * to an interactive device (a terminal), line buffered otherwise.
     */
    stdin->_flags |= (isatty(STDIN_FILENO))?_IOLBF:_IOFBF;
    stdout->_flags |= (isatty(STDOUT_FILENO))?_IOLBF:_IOFBF;
}

void __close_all_open_streams(void)
{
    int i;

    /*
     * If stdin is a terminal, discard any unread
     * input, otherwise the shell will try to execute
     * that as a command.
     */
    if (isatty(STDIN_FILENO))
        tcflush(STDIN_FILENO, TCIFLUSH);

    for (i = 0; i < FOPEN_MAX; i++)
        if (__iob[i]._flags != 0)
            fclose(&__iob[i]);
}

FILE *fopen(const char *filename, const char *mode)
{
    int open_flags;
    mode_t open_mode;
    FILE *fp;

    if (free_stream == NULL) {
        /* search for a free slot */
        for (fp = __iob; fp < __iob+FOPEN_MAX; fp++)
            if (!(fp->_flags & _IOOPEN))
                break;
        if (fp >= __iob+FOPEN_MAX)
            return NULL;
    } else { /* freopen() called us */
        fp = free_stream;
    }

    open_flags = 0;
    open_mode = S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH;
    switch (mode[0]) {
    case 'r':
        switch (mode[1]) {
        case 'b':
            if (mode[2] == '+') {
                ;
            } else { /* rb */
                open_flags |= O_RDONLY;
                break;
            }
        case '+': /* r+, rb+, r+b */
            open_flags |= O_RDWR;
            break;
        default: /* r */
            open_flags |= O_RDONLY;
            break;
        }
        break;
    case 'w':
        switch (mode[1]) {
        case 'b':
            if (mode[2] == '+') {
                ;
            } else { /* wb */
                open_flags |= O_WRONLY;
                break;
            }
        case '+': /* w+, wb+, w+b */
            open_flags |= O_RDWR;
            break;
        default: /* w */
            open_flags |= O_WRONLY;
            break;
        }
        open_flags |= O_CREAT|O_TRUNC;
        if (must_not_exist) /* tmpfile() called us */
            open_flags |= O_EXCL;
        break;
    case 'a':
        switch (mode[1]) {
        case 'b':
            if (mode[2] == '+') {
                ;
            } else { /* ab */
                open_flags |= O_WRONLY;
                break;
            }
        case '+': /* a+, ab+, a+b */
            open_flags |= O_RDWR;
            break;
        default: /* a */
            open_flags |= O_WRONLY;
            break;
        }
        open_flags |= O_CREAT|O_APPEND;
        break;
    default:
        return NULL;
    }

    if ((fp->_fd=open(filename, open_flags, open_mode)) < 0)
        return NULL;
    fp->_oflags = open_flags;
    fp->_flags = _IOOPEN|_IOFBF;
    fp->_putback = EOF;
    strcpy(fp->_filename, filename);
    return fp;
}

FILE *freopen(const char *filename, const char *mode, FILE *stream)
{
    if (filename == NULL)
        /* we don't support this */
        return stream; /* XXX: or should be NULL? */

    if (stream->_flags & _IOOPEN)
        fclose(stream);
    else
        memset(stream, 0, sizeof(FILE));
    free_stream = stream;
    fopen(filename, mode);
    free_stream = NULL;
    return stream;
}

int fclose(FILE *stream)
{
    int status;

    status = 0;
    if (!(stream->_flags & _IONBF)) {
        if (stream->_oflags & O_WRITABLE)
            status = fflush(stream);
        if (!(stream->_flags & _IOEXTBUF))
            free(stream->_buf);
    }
    if (close(stream->_fd) < 0)
        status = EOF;
    if (stream->_flags & _IOTMP)
        remove(stream->_filename);
    memset(stream, 0, sizeof(FILE));
    return status;
}

static ssize_t _do_read(FILE *stream, void *ptr, size_t size)
{
    ssize_t rn;

    if ((rn=read(stream->_fd, ptr, size)) <= 0) {
        if (rn < 0)
            stream->_flags |= _IOERR;
        else
            stream->_flags |= _IOEOF;
        return EOF;
    }
    return rn;
}

static int _fillbuf(FILE *stream)
{
    ssize_t rn;

    if (stream->_oflags&O_WRONLY    /* write-only stream */
    || stream->_flags&_IOWR         /* last operation was a write */
    || stream->_flags&_IOEOF)       /* EOF was already encountered */
        return EOF;
    stream->_flags |= _IORD;
    if (stream->_buf == NULL) {
        if ((stream->_buf=malloc(BUFSIZ)) == NULL)
            return EOF;
        stream->_bufsiz = BUFSIZ;
    }
    stream->_pos = stream->_cnt = 0;
    if ((rn=_do_read(stream, stream->_buf, stream->_bufsiz)) == EOF)
        return EOF;
    else
        stream->_cnt = rn;
    return 0;
}

int fgetc(FILE *stream)
{
    if (stream->_flags & _IOEOF)
        return EOF;

    if (stream->_flags & _IONBF) {
        unsigned char c;

        if (stream->_putback != EOF) {
            c = stream->_putback;
            stream->_putback = EOF;
        } else if (_do_read(stream, &c, 1) == EOF) {
            return EOF;
        }
        return c;
    }

    if (stream->_cnt==0 && _fillbuf(stream)==EOF)
        /* _fillbuf() should set any flags */
        return EOF;
    --stream->_cnt;
    return (unsigned char)stream->_buf[stream->_pos++];
}

size_t fread(void *ptr, size_t size, size_t nelem, FILE *stream)
{
    size_t nb;

    if ((nb=size*nelem) == 0)
        return 0;

    /* allocate a buffer if we don't have one yet */
    if (stream->_flags&(_IOLBF|_IOFBF) && stream->_buf==NULL) {
        if ((stream->_buf=malloc(BUFSIZ)) == NULL)
            return 0;
        stream->_bufsiz = BUFSIZ;
    }

    if (stream->_flags&_IONBF || nb>stream->_bufsiz) {
        ssize_t rn;

        if (stream->_flags & _IONBF) {
            /* read any ungetc()'d character */
            if (stream->_putback != EOF) {
                *(char *)ptr = stream->_putback;
                stream->_putback = EOF;
                ptr = (char *)ptr+1;
                --nb;
            }
        } else if (stream->_cnt != 0) {
            /* before read()'ing, copy whatever we have buffered */
            assert(!(stream->_flags & _IONBF));
            memcpy(ptr, stream->_buf+stream->_pos, stream->_cnt);
            stream->_pos += stream->_cnt;
            ptr = (char *)ptr+stream->_cnt;
            nb -= stream->_cnt;
            stream->_cnt = 0;
        }
        if ((rn=_do_read(stream, ptr, nb)) == EOF)
            rn = 0;
        nb -= rn;
    } else { /* the request can be satisfied with at most one call to _fillbuf() */
        size_t t;

        if (stream->_cnt != 0) {
            t = (nb<stream->_cnt)?nb:stream->_cnt;
            memcpy(ptr, stream->_buf+stream->_pos, t);
            ptr = (char *)ptr+t;
            stream->_pos += t;
            stream->_cnt -= t;
            nb -= t;
        }
        if (nb != 0) {
            if (_fillbuf(stream) == EOF)
                goto done;
            t = (nb<stream->_cnt)?nb:stream->_cnt;
            memcpy(ptr, stream->_buf, t);
            stream->_pos += t;
            stream->_cnt -= t;
            nb -= t;
        }
    }
done:
    return ((size*nelem)-nb)/size;
}

static int _flushbuf(FILE *stream)
{
    size_t nb;
    ssize_t wn;

    if (!(stream->_oflags & O_WRITABLE)   /* read-only stream */
    || stream->_flags&_IORD)            /* last operation was a read */
        return EOF;
    stream->_flags |= _IOWR;
    if (stream->_buf==NULL || (nb=stream->_pos)==0) /* nothing to write */
        return 0;
    stream->_pos = 0;
    if ((wn=write(stream->_fd, stream->_buf, nb))<0 || wn!=nb) {
        stream->_flags |= _IOERR;
        return EOF;
    }
    return 0;
}

int fputc(int c, FILE *stream)
{
    if (stream->_flags & _IONBF) {
        ssize_t wn;

        if ((wn=write(stream->_fd, &c, 1)) < 1) {
            stream->_flags |= _IOERR;
            return EOF;
        }
        goto done;
    }

    if (stream->_buf == NULL) {
        if ((stream->_buf=malloc(BUFSIZ)) == NULL)
            return EOF;
        stream->_bufsiz = BUFSIZ;
    }
    if (stream->_pos>=stream->_bufsiz && _flushbuf(stream)==EOF)
        /* _flushbuf() should set any flags */
        return EOF;
    stream->_buf[stream->_pos++] = c;

    if (c=='\n' && stream->_flags&_IOLBF && fflush(stream)==EOF)
        return EOF;

done:
    return (unsigned char)c;
}

size_t fwrite(const void *ptr, size_t size, size_t nelem, FILE *stream)
{
    size_t nb;

    if ((nb=size*nelem) == 0)
        return 0;

    /* allocate a buffer if we don't have one yet */
    if (stream->_flags&(_IOLBF|_IOFBF) && stream->_buf==NULL) {
        if ((stream->_buf=malloc(BUFSIZ)) == NULL)
            return 0;
        stream->_bufsiz = BUFSIZ;
    }

    if (stream->_flags & _IOLBF) {
        unsigned char *ptr2;

        for (ptr2 = (unsigned char *)ptr; nb != 0; nb--)
            if (fputc(*ptr2++, stream) == EOF)
                break;
    } else if (stream->_flags&_IONBF || nb>stream->_bufsiz) {
        ssize_t wn;

        if (_flushbuf(stream) == EOF)
            return 0;
        if ((wn=write(stream->_fd, ptr, nb))<0 || wn!=nb) {
            stream->_flags |= _IOERR;
            if (wn < 0)
                wn = 0;
        }
        nb -= wn;
    } else {
        if ((stream->_pos+nb > stream->_bufsiz) && _flushbuf(stream)==EOF)
            return 0;
        memcpy(stream->_buf+stream->_pos, ptr, nb);
        stream->_pos += nb;
        nb = 0;
    }

    return ((size*nelem)-nb)/size;
}

int ungetc(int c, FILE *stream)
{
    if (c == EOF)
        return EOF;
    /*
     * Note that the standard requires
     * only one character of pushback.
     */
    if (stream->_flags & _IONBF) {
        /*
         * For unbuffered streams, only the
         * most recent pushed-back character
         * will be returned by subsequent reads.
         */
        stream->_putback = (unsigned char)c;
    } else if (stream->_pos > 0) {
        /*
         * For buffered streams, at most
         * stream->_bufsize characters will
         * be pushed-back.
         */
        stream->_buf[--stream->_pos] = c;
        ++stream->_cnt;
    } else {
        return EOF;
    }
    stream->_flags &= ~_IOEOF;
    return (unsigned char)c;
}

int getc(FILE *stream)
{
    fgetc(stream);
}

int getchar(void)
{
    getc(stdin);
}

int putc(int c, FILE *stream)
{
    fputc(c, stream);
}

int putchar(int c)
{
    putc(c, stdout);
}

int fflush(FILE *stream)
{
    int res;

    res = 0;
    if (stream != NULL) {
        if (stream->_flags&_IONBF || stream->_flags&_IORD)
            return 0;
        res = _flushbuf(stream);
        if (stream->_oflags & O_RDWR)
            stream->_flags &= ~(_IOWR|_IORD); /* so the next operation can
                                                 be either a read or a write */
    } else {
        int i;

        for (i = 0; i < FOPEN_MAX; i++)
            if (__iob[i]._flags&_IOOPEN && fflush(&__iob[i])==EOF)
                res = EOF;
    }
    return res;
}

int fseek(FILE *stream, long offset, int whence)
{
    if (whence!=SEEK_SET && whence!=SEEK_CUR && whence!=SEEK_END)
        return EOF;
    stream->_flags &= ~_IOEOF;
    stream->_putback = EOF; /* any ungetc()'d char is gone */
    fflush(stream);
    stream->_pos = stream->_cnt = 0; /* note that the buffer is empty now */
    if (lseek(stream->_fd, offset, whence) < 0)
        return EOF;
    /*
     * On update streams, the next operation after
     * a successful fseek() can be read or write.
     */
    if (stream->_oflags & O_RDWR)
        stream->_flags &= ~(_IORD|_IOWR);
    return 0;
}

long ftell(FILE *stream)
{
    long pos;

    if ((pos=lseek(stream->_fd, 0L, SEEK_CUR)) < 0)
        return -1L;
    if (!(stream->_flags & _IONBF)) {
        /* compensate for buffering */
        if (stream->_flags & _IORD)
            pos -= stream->_cnt;
        else if (stream->_flags & _IOWR)
            pos += stream->_pos;
    }
    return pos;
}

int fgetpos(FILE *stream, fpos_t *pos)
{
    long n;

    if ((n=ftell(stream)) < 0)
        return n;
    *pos = n;
    return 0;
}

int fsetpos(FILE *stream, const fpos_t *pos)
{
    return fseek(stream, *pos, SEEK_SET);
}

void rewind(FILE *stream)
{
    fseek(stream, 0L, SEEK_SET);
    stream->_flags &= ~_IOERR;
}

void perror(const char *s)
{
    if (s!=NULL && s[0]!='\0')
        fprintf(stderr, "%s: ", s);
    fprintf(stderr, "%s\n", strerror(errno));
}

void clearerr(FILE *stream)
{
    stream->_flags &= ~(_IOERR|_IOEOF);
}

int feof(FILE *stream)
{
    return stream->_flags&_IOEOF;
}

int ferror(FILE *stream)
{
    return stream->_flags&_IOERR;
}

void setbuf(FILE *stream, char *buf)
{
    if (buf == NULL)
        setvbuf(stream, NULL, _IONBF, 0);
    else
        setvbuf(stream, buf, _IOFBF, BUFSIZ);
}

int setvbuf(FILE *stream, char *buf, int mode, size_t size)
{
    if (mode!=_IOFBF && mode!=_IOLBF && mode!=_IONBF)
        return EOF;

    if (mode == _IONBF) {
        buf = NULL;
        size = 0;
    } else if (buf == NULL) {
        if ((buf=malloc(size)) == NULL)
            return EOF;
    } else {
        stream->_flags |= _IOEXTBUF;
    }
    stream->_flags &= ~(_IOFBF|_IOLBF|_IONBF);
    stream->_flags |= mode;
    stream->_buf = (unsigned char *)buf;
    stream->_bufsiz = size;
    return 0;
}

int fputs(const char *s, FILE *stream)
{
    size_t len;

    len = strlen(s);
    if (stream->_flags & _IONBF) {
        while (len-- && fputc(*s++, stream)!=EOF)
            ;
        if (*s != '\0')
            return EOF;
    } else {
        if (fwrite(s, 1, len, stream) != len)
            return EOF;
    }
    return 0;
}

char *fgets(char *s, int n, FILE *stream)
{
    int c;
    char *p;

    if (n <= 0)
        return NULL;

    p = s;
    while (--n) {
        if ((c=fgetc(stream)) == EOF) {
            if (p==s || stream->_flags&_IOERR) /* EOF and nothing read, or reading error */
                return NULL;
            break;
        }
        if ((*p++=c) == '\n')
            break;
    }
    *p = '\0';
    return s;
}

char *gets(char *s)
{
    int c;
    char *p;

    p = s;
    while (1) {
        switch (c = fgetc(stdin)) {
        case EOF:
            if (p==s || stdin->_flags&_IOERR)
                return NULL;
            goto done;
        case '\n':
            goto done;
        default:
            *p++ = c;
            break;
        }
    }
done:
    *p = '\0';
    return s;
}

int puts(const char *s)
{
    if (fputs(s, stdout)==EOF || fputc('\n', stdout)==EOF)
        return EOF;
    return 0;
}

int fileno(FILE *stream)
{
    return stream->_fd;
}

int remove(const char *filename)
{
    return unlink(filename);
}

int rename(const char *oldpath, const char *newpath)
{
    return syscall(SYS_rename, oldpath, newpath);
}

static int tmpfile_count;
static unsigned long int tmp_next/* = 1*/;

static int _tmp_rand(void)
{
    tmp_next = tmp_next*1103515245+12345;
    return (unsigned int)(tmp_next/65536)%32768;
}

/*
 * Dummy function that tries to generate a unique (unique
 * at the moment stat() is issued) temporary file name.
 * The names generated are of the form:
 *   /tmp/[PRN][-][0..TMP_MAX-1][.][program-pid]
 */
char *_gen_tmpnam(char *s)
{
    int i;
    enum {
        MAX_ATTEMPTS = 10,
    };
    struct stat st;
    static int pid = -1;

    if (pid < 0) {
        pid = getpid();
        tmp_next = (unsigned long int)time(NULL);
    }
    for (i = 0; i < MAX_ATTEMPTS; i++) {
        snprintf(s, L_tmpnam, "/tmp/%.5d-%d.%.4d", _tmp_rand(), tmpfile_count, pid);
        if (stat(s, &st) != 0)
            break;
    }
    return (i >= MAX_ATTEMPTS) ? NULL : s;
}

static char tmpnam_buf[L_tmpnam];

char *tmpnam(char *s)
{
    if (tmpfile_count >= TMP_MAX)
        return NULL;
    if (s == NULL)
        s = tmpnam_buf;
    if ((s=_gen_tmpnam(s)) != NULL)
        ++tmpfile_count;
    return s;
}

FILE *tmpfile(void)
{
    FILE *fp;
    char *s, buf[L_tmpnam];

    if (tmpfile_count >= TMP_MAX)
        return NULL;

    must_not_exist = 1; /* tell fopen() that is must add O_EXCL to the open() flags */
    if ((s=_gen_tmpnam(buf))==NULL || (fp=fopen(s, "wb+"))==NULL) {
        fp = NULL;
    } else {
        fp->_flags |= _IOTMP;
        ++tmpfile_count;
    }
    must_not_exist = 0;
    return fp;
}

/* ================================================ */
/* Formatted I/O functions.                         */
/* ================================================ */

/* flags */
#define F_LEFT      0x00000001  /* - */
#define F_SIGN      0x00000002  /* + */
#define F_SPACE     0x00000004  /*   */
#define F_ALT       0x00000008  /* # */
#define F_ZERO      0x00000010  /* 0 */
/* length modifiers */
#define L_CHAR      0x00000020  /* hh */
#define L_SHORT     0x00000040  /* h */
#define L_LONG      0x00000080  /* l */
#define L_LLONG     0x00000100  /* ll */
#define L_IMAX      0x00000200  /* j */
#define L_SIZE      0x00000400  /* z */
#define L_PTRD      0x00000800  /* t */
#define L_MASK      (L_CHAR|L_SHORT|L_LONG|L_LLONG|L_IMAX|L_SIZE|L_PTRD)

#define HAS_PREC    0x00001000  /* precision has been given */
#define HAS_MINWID  0x00002000  /* minimum field width has been given */
#define PREF_HEX    0x00004000  /* needs '0x' prefix */
#define PREF_MINUS  0x00008000  /* needs '-' prefix */
#define PREF_PLUS   0x00010000  /* needs '+' prefix */
#define PREF_SPACE  0x00020000  /* needs ' ' prefix */
#define PREF_MASK   (PREF_HEX|PREF_MINUS|PREF_PLUS|PREF_SPACE)
#define SIGNED      0x00040000  /* d/i conversion specifier */
#define BASE_OCT    0x00080000  /* o */
#define BASE_HEX1   0x00100000  /* x */
#define BASE_HEX2   0x00200000  /* X */
#define BASE_HEX    (BASE_HEX1|BASE_HEX2)

/*
 * Current stream or string from which
 * we are reading the input or in which
 * we are writing the formatted output.
 * These variables are set by each one of the
 * formatted input/output functions before
 * calling _scan_formatted()/_print_formatted().
 */
static FILE *curr_stream;
static char *curr_s;

static int _get_num(const char *s, const char **end)
{
    int n;

    n = 0;
    while (*s!='\0' && (*s>='0' && *s<='9')) {
        n = n*10+(*s-'0');
        s++;
    }
    *end = s;
    return n;
}

/* =========================== */
/* Formatted output functions. */
/* =========================== */

/*
 * Temporal buffer where the result of a
 * single conversion specifier is written.
 */
static char *conv_buf;
static size_t conv_buf_max, conv_buf_pos;

static void _write_to_conv_buf(const char *s, size_t size)
{
    assert(conv_buf_pos == 0);
    if (conv_buf == NULL) {
        conv_buf_max = (size<4096)?4096:size;
        conv_buf = malloc(conv_buf_max); /* XXX */
    } else if (size > conv_buf_max) {
        conv_buf_max = size;
        conv_buf = realloc(conv_buf, conv_buf_max); /* XXX */
    }
    memcpy(conv_buf, s, size);
    conv_buf_pos = size;
}

/*
 * Current and maximum (without counting '\0') number
 * of characters to be transmited.
 */
static size_t curr_count, curr_lim;

static void _write_to_final_buf(const char *ptr, size_t size)
{
    if (curr_stream != NULL) {
        fwrite(ptr, size, 1, curr_stream);
    } else if (curr_count < curr_lim) {
        size_t avail, nb;

        assert(curr_s != NULL);
        avail = curr_lim-curr_count;
        nb = (avail<size)?avail:size;
        memcpy((void *)curr_s, ptr, nb);
        curr_s += nb;
    }
    curr_count += size;
}

static void _number(long long n, int base, int upper, int precision, int alzr)
{
    int i, j;
    char buf[32], buf2[32];
    char *digits;

    if (precision > 32)
        precision = 32;
    else if (precision==0 && n==0)
        return;

    i = j = 0;
    digits = upper?"0123456789ABCDEF"
                  :"0123456789abcdef";
    do {
        buf[i++] = digits[n%(unsigned long long)base];
        n /= (unsigned long long)base;
    } while (n != 0);
    if (alzr && precision<=i)
        precision = i+1; /* this is for '#' along with 'o' */
    while (i < precision)
        buf[i++] = '0';
    do
        buf2[j++] = buf[--i];
    while (i != 0);

    _write_to_conv_buf(buf2, j);
}

static void _print_formatted(const char *format, va_list arg)
{
    const char *p;
    unsigned conv;
    size_t min_width, precision;

    conv = 0;
    for (p = format; *p != '\0'; p++) {
        if (*p != '%') {
            _write_to_final_buf(p, 1);
            continue;
        }
        ++p;

        /*
         * The format of a conversion specification is as follows:
         *   %[flags][field-width][precision][length-modifier][conversion-specifier]
         */

        /* flags */
        while (1) {
            switch (*p) {
            case '-': conv |= F_LEFT;   ++p; break;
            case '+': conv |= F_SIGN;   ++p; break;
            case ' ': conv |= F_SPACE;  ++p; break;
            case '#': conv |= F_ALT;    ++p; break;
            case '0': conv |= F_ZERO;   ++p; break;
            default: goto done_flags;
            }
        }
done_flags:
        /* take care of mutually exclusive flags */
        if (conv&F_SIGN && conv&F_SPACE) /* "+ " == "+" */
            conv &= ~F_SPACE;
        else if (conv&F_LEFT && conv&F_ZERO) /* "-0" == "-" */
            conv &= ~F_ZERO;

        /* field width */
        if (*p=='*' || (*p>='1' && *p<='9')) {
            if (*p == '*') {
                int n;

                if ((n=va_arg(arg, int)) < 0) {
                    conv |= F_LEFT;
                    n = -n;
                }
                min_width = (size_t)n;
                ++p;
            } else {
                min_width = _get_num(p, &p);
            }
            conv |= HAS_MINWID;
        }

        /* precision */
        if (*p == '.') {
            ++p;
            if (*p == '*') {
                int n;

                if ((n=va_arg(arg, int)) > 0) {
                    precision = (size_t)n;
                    conv |= HAS_PREC;
                }
                ++p;
            } else if (*p>='0' && *p<='9') {
                precision = _get_num(p, &p);
                conv |= HAS_PREC;
            } else {
                precision = 0;
                conv |= HAS_PREC;
            }
        }

        /* length modifier */
        switch (*p) {
        case 'h':
            ++p;
            if (*p == 'h') {
                conv |= L_CHAR;
                ++p;
            } else {
                conv |= L_SHORT;
            }
            break;
        case 'l':
            ++p;
            if (*p == 'l') {
                conv |= L_LLONG;
                ++p;
            } else {
                conv |= L_LONG;
            }
            break;
        case 'j':
            conv |= L_IMAX;
            ++p;
            break;
        case 'z':
            conv |= L_SIZE;
            ++p;
            break;
        case 't':
            conv |= L_PTRD;
            ++p;
            break;
        }

        /* conversion specifier */
        switch (*p) {
        case 'd':
        case 'i':
            conv |= SIGNED;
            goto intconv;
        case 'u':
            goto intconv;
        case 'o':
            conv |= BASE_OCT;
            goto intconv;
        case 'x':
            conv |= BASE_HEX1;
            goto intconv;
        case 'X':
            conv |= BASE_HEX2;
        intconv: {
            long long n;

            if (conv & HAS_PREC)
                conv &= ~F_ZERO;
            switch (conv & L_MASK) {
            case L_CHAR:    /* hh */
                n = (unsigned char)va_arg(arg, int);
                if (conv & SIGNED)
                    n = (signed char)n;
                break;
            case L_SHORT:   /* h */
                n = (unsigned short)va_arg(arg, int);
                if (conv & SIGNED)
                    n = (signed short)n;
                break;
            case L_LLONG:   /* ll */
                n = va_arg(arg, long long);
                break;
            case L_LONG:    /* l */
            case L_IMAX:    /* j */
            case L_SIZE:    /* z */
            case L_PTRD:    /* t */
                n = (unsigned long)va_arg(arg, unsigned long);
                if (conv & SIGNED)
                    n = (long)n;
                break;
            default:
                n = va_arg(arg, unsigned int);
                if (conv & SIGNED)
                    n = (int)n;
                break;
            }
            if (conv & SIGNED) {
                if (n < 0) {
                    conv |= PREF_MINUS;
                    n = -n;
                } else if (conv & F_SIGN) {
                    conv |= PREF_PLUS;
                } else if (conv & F_SPACE) {
                    conv |= PREF_SPACE;
                }
            } else if (conv&BASE_HEX && conv&F_ALT && n!=0) {
                conv |= PREF_HEX;
            }
            _number(n, (conv&BASE_OCT)?8:(conv&BASE_HEX)?16:10, conv&BASE_HEX2,
            (conv&HAS_PREC)?precision:1, conv&BASE_OCT && conv&F_ALT && n!=0);
        }
            break;
        case 'c': {
            int c;

            c = va_arg(arg, int);
            _write_to_conv_buf((const char *)&c, 1);
        }
            break;
        case 's': {
            char *a;
            size_t len;

            a = va_arg(arg, char *);
            len = strlen(a);
            if (conv&HAS_PREC && precision<len)
                len = precision;
            _write_to_conv_buf(a, len);
        }
            break;
        case 'p':
            _number((unsigned long)va_arg(arg, void *), 16, 0, 1, 0);
            conv |= BASE_HEX1|PREF_HEX;
            break;
        case 'n':
            switch (conv & L_MASK) {
            case L_CHAR: {
                signed char *np;

                np = va_arg(arg, signed char *);
                *np = curr_count;
            }
                break;
            case L_SHORT: {
                short int *np;

                np = va_arg(arg, short int *);
                *np = curr_count;
            }
                break;
            case L_LLONG: {
                long long int *np;

                np = va_arg(arg, long long int *);
                *np = curr_count;
            }
                break;
            case L_LONG:
            case L_IMAX:
            case L_SIZE:
            case L_PTRD: {
                long int *np;

                np = va_arg(arg, long int *);
                *np = curr_count;
            }
                break;
            default: {
                int *np;

                np = va_arg(arg, int *);
                *np = curr_count;
            }
                break;
            }
            conv = 0;
            continue;
        case '%':
            _write_to_conv_buf(p, 1);
            break;
        }

        /* write any prefix that can be written at this point */
        if (!(conv&HAS_MINWID) || conv&F_LEFT || conv_buf_pos>=min_width || conv&F_ZERO) {
            switch (conv & PREF_MASK) {
            case PREF_MINUS:
            case PREF_PLUS:
            case PREF_SPACE:
                _write_to_final_buf((conv&PREF_MINUS)?"-":(conv&PREF_PLUS)?"+":" ", 1);
                --min_width;
                break;
            case PREF_HEX:
                _write_to_final_buf((conv&BASE_HEX1)?"0x":"0X", 2);
                min_width -= 2;
                break;
            }
        }

        if (conv&HAS_MINWID && conv_buf_pos<min_width) {
            if (conv & F_LEFT) {
                _write_to_final_buf(conv_buf, conv_buf_pos);
                /* pad on the right */
                while (conv_buf_pos < min_width) {
                    _write_to_final_buf(" ", 1); /* never pad with zeroes here! */
                    ++conv_buf_pos;
                }
            } else {
                size_t t;
                char *padch;

                /* subtract the length of the prefix (if any) from the amount to pad */
                if (!(conv & F_ZERO)) {
                    if (conv & (PREF_MINUS|PREF_PLUS|PREF_SPACE))
                        --min_width;
                    else if (conv & PREF_HEX)
                        min_width -= 2;
                }

                /* pad on the left */
                t = conv_buf_pos;
                padch = (conv&F_ZERO)?"0":" ";
                while (conv_buf_pos < min_width) {
                    _write_to_final_buf(padch, 1);
                    ++conv_buf_pos;
                }

                /* write the prefix (if any) */
                if (!(conv & F_ZERO)) {
                    switch (conv & PREF_MASK) {
                    case PREF_MINUS:
                    case PREF_PLUS:
                    case PREF_SPACE:
                        _write_to_final_buf((conv&PREF_MINUS)?"-":(conv&PREF_PLUS)?"+":" ", 1);
                        break;
                    case PREF_HEX:
                        _write_to_final_buf((conv&BASE_HEX1)?"0x":"0X", 2);
                        break;
                    }
                }

                /* write the conversion */
                _write_to_final_buf(conv_buf, t);
            }
        } else {
            _write_to_final_buf(conv_buf, conv_buf_pos);
        }

        conv_buf_pos = 0;
        conv = 0;
    }
}

int vfprintf(FILE *stream, const char *format, va_list arg)
{
    curr_count = 0;
    curr_stream = stream;
    curr_s = NULL;

    _print_formatted(format, arg);

    return curr_count;
}

int vprintf(const char *format, va_list arg)
{
    return vfprintf(stdout, format, arg);
}

int printf(const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    return vfprintf(stdout, format, ap);
}

int fprintf(FILE *stream, const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    return vfprintf(stream, format, ap);
}

int vsprintf(char *s, const char *format, va_list arg)
{
    curr_count = 0;
    curr_lim = (size_t)-1;
    curr_stream = NULL;
    curr_s = s;

    _print_formatted(format, arg);
    *curr_s = '\0';

    return curr_count;
}

int sprintf(char *s, const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    return vsprintf(s, format, ap);
}

int vsnprintf(char *s, size_t n, const char *format, va_list arg)
{
    if (n == 0)
        return 0;

    curr_count = 0;
    curr_lim = n-1; /* we always write '\0' */
    curr_stream = NULL;
    curr_s = s;

    _print_formatted(format, arg);
    *curr_s = '\0';

    return curr_count;
}

int snprintf(char *s, size_t n, const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    return vsnprintf(s, n, format, ap);
}

/* ========================== */
/* Formatted input functions. */
/* ========================== */

#define HAS_MAXWID HAS_MINWID

typedef struct {
    int neg; /* ^ */
    uint32_t m[8];
} ScanSet;
#define SS_INSERT(s, e) ((s)->m[(e)/32] |= (1U << ((e)%32)))
#define SS_DELETE(s, e) ((s)->m[(e)/32] &= ~(1U << ((e)%32)))
#define SS_MEMBER(s, e) ((s)->m[(e)/32] & (1U << ((e)%32)))

static int scan_char_count, scan_eof_reached;

static int _get_char(void)
{
    int c;

    if (scan_eof_reached)
        return EOF;
    if (curr_stream != NULL) {
        if ((c=fgetc(curr_stream)) == EOF)
            scan_eof_reached = 1;
    } else {
        if ((c=*curr_s++) == '\0') {
            scan_eof_reached = 1;
            c = EOF;
        }
    }
    ++scan_char_count;
    return c;
}

static void _unget_char(int c)
{
    if (c == EOF) {
        if (curr_stream != NULL)
            ungetc(c, curr_stream);
        else
            --curr_s;
    }
    --scan_char_count;
}

#define NUMBUFSIZ 64
static char numbuf[NUMBUFSIZ];

/*
 * Get an integer input item (7.19.6.2, #9 & #12).
 * Note that scanf() behavior for integer items is similar
 * to that of strtoX(), but not identical.
 * For example, with the following calls:
 *      (1) scanf("%x", &n); // with input "0xz"
 *      (2) strtol("0xz", &endp, 0);
 * if you do
 *      printf("%c", getchar())
 * after the first call you will get 'z' (scanf() consumed 2 chars).
 * If you do
 *      printf("%c", *endp)
 * after the second call you will get 'x' (strtol() consumed 1 char).
 * This is because scanf() can push-back at most one input character (when
 * it encounters the 'z', the only thing it can do is push it back and fail).
 * strtoX() functions don't have any lookahed limitation, so they can
 * determine that the longest subsequence of the input string that is of the
 * expected form is just "0" ("xz" being the final string).
 */
static int _fill_numbuf(int base, int max_width)
{
    int i, c;

    if (max_width <= 0) {
        numbuf[0] = '\0';
        return 0;
    } else if (max_width > NUMBUFSIZ-1) {
        max_width = NUMBUFSIZ-1;
    }

    /* skip white-space chars (they don't contribute to the item's width) */
    while (isspace(c = _get_char()))
        ;

    i = 0;
    if (c=='+' || c=='-') {
        numbuf[i++] = c;
        c = _get_char();
        if (i >= max_width)
            goto done;
    }

    if (base==0 || base==16) {
        if (c == '0') {
            numbuf[i++] = c;
            c = _get_char();
            if (i >= max_width)
                goto done;
            if (c=='x' || c=='X') {
                numbuf[i++] = c;
                c = _get_char();
                if (i >= max_width)
                    goto done;
                base = 16;
            } else if (base == 0) {
                base = 8;
            }
        } else if (base == 0) {
            base = 10;
        }
    }
    while (i < max_width) {
        int val, uc;

        uc = toupper(c);
        if (isdigit(uc))
            val = uc-'0';
        else if (uc>='A' && uc<='Z')
            val = uc-'A'+10;
        else
            break;
        if (val >= base)
            break;
        numbuf[i++] = c;
        c = _get_char();
    }
done:
    _unget_char(c);
    numbuf[i] = '\0';
    return i;
}

/*
 * Parse a scanset of the form:
 *  scanset = "[" [ "^" ] [ ( "]" | "-" ) ] chars_other_than_right_bracket_and_hyphen [ "-" ] "]"
 * Return a pointer to the last parsed character (which will be ']' for a correct scanset).
 */
static char *_parse_scanset(const char *s, ScanSet *p)
{
    memset(p, 0, sizeof(ScanSet));

    ++s; /* skip "[" */
    if (s[0] == '^') {
        p->neg = 1;
        ++s;
    }
    if (s[0]==']' || s[0]=='-') {
        SS_INSERT(p, s[0]);
        ++s;
    }
    while (s[0]!=']' && s[0]!='\0') {
        if (s[0]=='-' && s[1]!=']' && s[1]!='\0') {
            int start, end;

            start = s[-1]; /* already added to the set in the previous iteration */
            end = s[1];
            if (start > end) {
                for (--start; start >= end; start--)
                    SS_INSERT(p, start);
            } else if (start < end) {
                for (++start; start <= end; start++)
                    SS_INSERT(p, start);
            }
            s += 2; /* skip '-' and end */
        } else {
            SS_INSERT(p, s[0]);
            ++s;
        }
    }
    return (char *)s;
}

static int _scan_formatted(const char *format, va_list arg)
{
    int c;
    const char *p;
    unsigned conv;
    int max_width, assign, count;

    conv = 0;
    count = 0; /* # of assigned items */
    for (p = format; *p != '\0'; p++) {
        if (*p != '%') {
            if (isspace(*p)) {
                while (isspace(c = _get_char()))
                    ;
                _unget_char(c);
                for (++p; isspace(*p); p++)
                    ;
                --p;
            } else if ((c=_get_char()) != *p) {
                _unget_char(c);
                goto done;
            }
            continue;
        }
        ++p;
        assign = 1;

        /*
         * The format of a conversion specification is as follows:
         *   %[*][field-width][length-modifier][conversion-specifier]
         */

        /* * (assignment-suppressing character) */
        if (*p == '*') {
            assign = 0;
            ++p;
        }

        /* field width */
        if (*p>='1' && *p<='9') {
            max_width = _get_num(p, &p);
            conv |= HAS_MAXWID;
        }

        /* length modifier */
        switch (*p) {
        case 'h':
            ++p;
            if (*p == 'h') {
                conv |= L_CHAR;
                ++p;
            } else {
                conv |= L_SHORT;
            }
            break;
        case 'l':
            ++p;
            if (*p == 'l') {
                conv |= L_LLONG;
                ++p;
            } else {
                conv |= L_LONG;
            }
            break;
        case 'j':
            conv |= L_IMAX;
            ++p;
            break;
        case 'z':
            conv |= L_SIZE;
            ++p;
            break;
        case 't':
            conv |= L_PTRD;
            ++p;
            break;
        }

        /* conversion specifier */
        switch (*p) {
            int base;
            long long n;
        case 'd':
            base = 10;
            conv |= SIGNED;
            goto intconv;
        case 'i':
            base = 0;
            conv |= SIGNED;
            goto intconv;
        case 'o':
            base = 8;
            goto intconv;
        case 'u':
            base = 10;
            goto intconv;
        case 'x':
            base = 16;
        intconv:
            if (_fill_numbuf(base, (conv&HAS_MAXWID)?max_width:INT_MAX) == 0)
                goto done;
            if (!assign)
                break;
            if (conv & SIGNED)
                n = strtoll(numbuf, NULL, base);
            else
                n = strtoull(numbuf, NULL, base);
            switch (conv & L_MASK) {
            case L_CHAR: {
                signed char *ptr;

                ptr = va_arg(arg, signed char *);
                *ptr = (signed char)n;
            }
                break;
            case L_SHORT: {
                short int *ptr;

                ptr = va_arg(arg, short int *);
                *ptr = (short int)n;
            }
                break;
            case L_LLONG: {
                long long int *ptr;

                ptr = va_arg(arg, long long int *);
                *ptr = n;
            }
                break;
            case L_LONG:
            case L_IMAX:
            case L_SIZE:
            case L_PTRD: {
                long int *ptr;

                ptr = va_arg(arg, long int *);
                *ptr = (long int)n;
            }
                break;
            default: {
                int *ptr;

                ptr = va_arg(arg, int *);
                *ptr = (int)n;
            }
                break;
            }
            ++count;
            break;
        case 'c': {
            int i;
            char *sp;

            if (!(conv & HAS_MAXWID))
                max_width = 1;
            if (assign)
                sp = va_arg(arg, char *);
            c = _get_char();
            for (i = 0; i < max_width; i++) {
                if (c == EOF)
                    break;
                if (assign)
                    sp[i] = c;
                c = _get_char();
            }
            if (assign)
                ++count;
            _unget_char(c);
        }
            break;
        case 's': {
            int i;
            char *sp;

            while (isspace(c = _get_char()))
                ;
            if (assign)
                sp = va_arg(arg, char *);
            for (i = 0; !(conv&HAS_MAXWID) || i<max_width; i++) {
                if (c==EOF || isspace(c))
                    break;
                if (assign)
                    sp[i] = c;
                c = _get_char();
            }
            if (assign) {
                sp[i] = '\0';
                ++count;
            }
            _unget_char(c);
        }
            break;
        case '[': {
            int i;
            char *sp;
            ScanSet ss;

            p = _parse_scanset(p, &ss);
            if (*p != ']')
                /*
                 * Bad scanset (most likely _parse_scanset()
                 * couldn't find the terminating ']').
                 */
                goto done;
            if (assign)
                sp = va_arg(arg, char *);
            c = _get_char();
            for (i = 0; !(conv&HAS_MAXWID) || i<max_width; i++) {
                if (c==EOF || (!ss.neg && !SS_MEMBER(&ss, c)) || (ss.neg && SS_MEMBER(&ss, c)))
                    break;
                if (assign)
                    sp[i] = c;
                c = _get_char();
            }
            if (assign) {
                sp[i] = '\0';
                if (i != 0) /* we matched at least one character */
                    ++count;
            }
            _unget_char(c);
        }
            break;
        case 'p': {
            void **pp;

            if (_fill_numbuf(16, (conv&HAS_MAXWID)?max_width:INT_MAX) == 0)
                goto done;
            if (!assign)
                break;
            pp = va_arg(arg, void **);
            *pp = (void *)strtoul(numbuf, NULL, 16);
            ++count;
        }
            break;
        case 'n':
            switch (conv & L_MASK) {
            case L_CHAR: {
                signed char *np;

                np = va_arg(arg, signed char *);
                *np = scan_char_count;
            }
                break;
            case L_SHORT: {
                short int *np;

                np = va_arg(arg, short int *);
                *np = scan_char_count;
            }
                break;
            case L_LLONG: {
                long long int *np;

                np = va_arg(arg, long long int *);
                *np = scan_char_count;
            }
                break;
            case L_LONG:
            case L_IMAX:
            case L_SIZE:
            case L_PTRD: {
                long int *np;

                np = va_arg(arg, long int *);
                *np = scan_char_count;
            }
                break;
            default: {
                int *np;

                np = va_arg(arg, int *);
                *np = scan_char_count;
            }
                break;
            }
            break;
        case '%':
            if ((c=_get_char()) != '%') {
                _unget_char(c);
                goto done;
            }
            break;
        }

        conv = 0;
    }
done:
    return (count==0 && scan_eof_reached)?EOF:count;
}

int vfscanf(FILE *stream, const char *format, va_list arg)
{
    scan_char_count = 0;
    scan_eof_reached = 0;

    curr_stream = stream;
    curr_s = NULL;

    return _scan_formatted(format, arg);
}

int fscanf(FILE *stream, const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    return vfscanf(stream, format, ap);
}

int vscanf(const char *format, va_list arg)
{
    return vfscanf(stdin, format, arg);
}

int scanf(const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    return vfscanf(stdin, format, ap);
}

int vsscanf(const char *s, const char *format, va_list arg)
{
    scan_char_count = 0;
    scan_eof_reached = 0;

    curr_stream = NULL;
    curr_s = (char *)s;

    return _scan_formatted(format, arg);
}

int sscanf(const char *s, const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    return vsscanf(s, format, ap);
}
