#include <string.h>
#include <stdlib.h>
#include <ctype.h>

static char *sys_errlist[] = {
    "No error",                                       // ENOERROR          0
    "Operation not permitted",                        // EPERM             1
    "No such file or directory",                      // ENOENT            2
    "No such process",                                // ESRCH             3
    "Interrupted system call",                        // EINTR             4
    "I/O error",                                      // EIO               5
    "No such device or address",                      // ENXIO             6
    "Argument list too long",                         // E2BIG             7
    "Exec format error",                              // ENOEXEC           8
    "Bad file number",                                // EBADF             9
    "No child processes",                             // ECHILD           10
    "Try again",                                      // EAGAIN           11
    "Out of memory",                                  // ENOMEM           12
    "Permission denied",                              // EACCES           13
    "Bad address",                                    // EFAULT           14
    "Block device required",                          // ENOTBLK          15
    "Device or resource busy",                        // EBUSY            16
    "File exists",                                    // EEXIST           17
    "Cross-device link",                              // EXDEV            18
    "No such device",                                 // ENODEV           19
    "Not a directory",                                // ENOTDIR          20
    "Is a directory",                                 // EISDIR           21
    "Invalid argument",                               // EINVAL           22
    "File table overflow",                            // ENFILE           23
    "Too many open files",                            // EMFILE           24
    "Not a typewriter",                               // ENOTTY           25
    "Text file busy",                                 // ETXTBSY          26
    "File too large",                                 // EFBIG            27
    "No space left on device",                        // ENOSPC           28
    "Illegal seek",                                   // ESPIPE           29
    "Read-only file system",                          // EROFS            30
    "Too many links",                                 // EMLINK           31
    "Broken pipe",                                    // EPIPE            32
    "Math argument out of domain of func",            // EDOM             33
    "Math result not representable",                  // ERANGE           34
    "Resource deadlock would occur",                  // EDEADLK          35
    "File name too long",                             // ENAMETOOLONG     36
    "No record locks available",                      // ENOLCK           37
    "Function not implemented",                       // ENOSYS           38
    "Directory not empty",                            // ENOTEMPTY        39
    "Too many symbolic links encountered",            // ELOOP            40
    "Operation would block",                          // EWOULDBLOCK  EAGAIN
    "No message of desired type",                     // ENOMSG           42
    "Identifier removed",                             // EIDRM            43
    "Channel number out of range",                    // ECHRNG           44
    "Level 2 not synchronized",                       // EL2NSYNC         45
    "Level 3 halted",                                 // EL3HLT           46
    "Level 3 reset",                                  // EL3RST           47
    "Link number out of range",                       // ELNRNG           48
    "Protocol driver not attached",                   // EUNATCH          49
    "No CSI structure available",                     // ENOCSI           50
    "Level 2 halted",                                 // EL2HLT           51
    "Invalid exchange",                               // EBADE            52
    "Invalid request descriptor",                     // EBADR            53
    "Exchange full",                                  // EXFULL           54
    "No anode",                                       // ENOANO           55
    "Invalid request code",                           // EBADRQC          56
    "Invalid slot",                                   // EBADSLT          57
    "Resource deadlock would occur",                  // EDEADLOCK   EDEADLK
    "Bad font file format",                           // EBFONT           59
    "Device not a stream",                            // ENOSTR           60
    "No data available",                              // ENODATA          61
    "Timer expired",                                  // ETIME            62
    "Out of streams resources",                       // ENOSR            63
    "Machine is not on the network",                  // ENONET           64
    "Package not installed",                          // ENOPKG           65
    "Object is remote",                               // EREMOTE          66
    "Link has been severed",                          // ENOLINK          67
    "Advertise error",                                // EADV             68
    "Srmount error",                                  // ESRMNT           69
    "Communication error on send",                    // ECOMM            70
    "Protocol error",                                 // EPROTO           71
    "Multihop attempted",                             // EMULTIHOP        72
    "RFS specific error",                             // EDOTDOT          73
    "Not a data message",                             // EBADMSG          74
    "Value too large for defined data type",          // EOVERFLOW        75
    "Name not unique on network",                     // ENOTUNIQ         76
    "File descriptor in bad state",                   // EBADFD           77
    "Remote address changed",                         // EREMCHG          78
    "Can not access a needed shared library",         // ELIBACC          79
    "Accessing a corrupted shared library",           // ELIBBAD          80
    ".lib section in a.out corrupted",                // ELIBSCN          81
    "Attempting to link in too many shared libraries",// ELIBMAX          82
    "Cannot exec a shared library directly",          // ELIBEXEC         83
    "Illegal byte sequence",                          // EILSEQ           84
    "Interrupted system call should be restarted",    // ERESTART         85
    "Streams pipe error",                             // ESTRPIPE         86
    "Too many users",                                 // EUSERS           87
    "Socket operation on non-socket",                 // ENOTSOCK         88
    "Destination address required",                   // EDESTADDRREQ     89
    "Message too long",                               // EMSGSIZE         90
    "Protocol wrong type for socket",                 // EPROTOTYPE       91
    "Protocol not available",                         // ENOPROTOOPT      92
    "Protocol not supported",                         // EPROTONOSUPPORT  93
    "Socket type not supported",                      // ESOCKTNOSUPPORT  94
    "Operation not supported on transport endpoint",  // EOPNOTSUPP       95
    "Protocol family not supported",                  // EPFNOSUPPORT     96
    "Address family not supported by protocol",       // EAFNOSUPPORT     97
    "Address already in use",                         // EADDRINUSE       98
    "Cannot assign requested address",                // EADDRNOTAVAIL    99
    "Network is down",                                // ENETDOWN        100
    "Network is unreachable",                         // ENETUNREACH     101
    "Network dropped connection because of reset",    // ENETRESET       102
    "Software caused connection abort",               // ECONNABORTED    103
    "Connection reset by peer",                       // ECONNRESET      104
    "No buffer space available",                      // ENOBUFS         105
    "Transport endpoint is already connected",        // EISCONN         106
    "Transport endpoint is not connected",            // ENOTCONN        107
    "Cannot send after transport endpoint shutdown",  // ESHUTDOWN       108
    "Too many references: cannot splice",             // ETOOMANYREFS    109
    "Connection timed out",                           // ETIMEDOUT       110
    "Connection refused",                             // ECONNREFUSED    111
    "Host is down",                                   // EHOSTDOWN       112
    "No route to host",                               // EHOSTUNREACH    113
    "Operation already in progress",                  // EALREADY        114
    "Operation now in progress",                      // EINPROGRESS     115
    "Stale NFS file handle",                          // ESTALE          116
    "Structure needs cleaning",                       // EUCLEAN         117
    "Not a XENIX named type file",                    // ENOTNAM         118
    "No XENIX semaphores available",                  // ENAVAIL         119
    "Is a named type file",                           // EISNAM          120
    "Remote I/O error",                               // EREMOTEIO       121
    "Quota exceeded",                                 // EDQUOT          122
    "No medium found",                                // ENOMEDIUM       123
    "Wrong medium type",                              // EMEDIUMTYPE     124
    "Operation Canceled",                             // ECANCELED       125
    "Required key not available",                     // ENOKEY          126
    "Key has expired",                                // EKEYEXPIRED     127
    "Key has been revoked",                           // EKEYREVOKED     128
    "Key was rejected by service",                    // EKEYREJECTED    129
    "Owner died",                                     // EOWNERDEAD      130
    "State not recoverable",                          // ENOTRECOVERABLE 131
    "Operation not possible due to RF-kill",          // ERFKILL         132
    "Memory page has hardware error",                 // EHWPOISON       133
};

static int sys_nerr = sizeof(sys_errlist)/sizeof(sys_errlist[0]);

char *strerror(int errnum)
{
    if (errnum < 0)
        errnum = -errnum;
    if (errnum >= sys_nerr)
        return "Unknown error";
    return sys_errlist[errnum];
}

void *memcpy(void *s1, const void *s2, size_t n)
{
    unsigned char *t1, *t2;

    t1 = s1;
    t2 = (unsigned char *)s2;
    while (n--)
        *t1++ = *t2++;
    return s1;
}

void *memmove(void *s1, const void *s2, size_t n)
{
    unsigned char *t1, *t2;

    t1 = s1;
    t2 = (unsigned char *)s2;
    if (t1 <= t2) {
        while (n--)
            *t1++ = *t2++;
    } else {
        t1 += n;
        t2 += n;
        while (n--)
            *--t1 = *--t2;
    }
    return s1;
}

char *strcpy(char *s1, const char *s2)
{
    char *t;

    t = s1;
    while ((*s1++=*s2++) != '\0')
        ;
    return t;
}

char *strncpy(char *s1, const char *s2, size_t n)
{
    size_t i;

    for (i = 0; i<n && s2[i]!='\0'; i++)
        s1[i] = s2[i];
    for ( ; i < n; i++)
        s1[i] = '\0';
    return s1;
}

char *strcat(char *s1, const char *s2)
{
    char *t;

    t = s1;
    while (*s1 != '\0')
        ++s1;
    while ((*s1++=*s2++) != '\0')
        ;
    return t;
}

char *strncat(char *s1, const char *s2, size_t n)
{
    char *t;

    t = s1;
    if (n) {
        while (*s1 != '\0')
            ++s1;
        while ((*s1++=*s2++) != '\0') {
            if (--n == 0) {
                *s1 = '\0';
                break;
            }
        }
    }
    return t;
}

int memcmp(const void *s1, const void *s2, size_t n)
{
    int res;
    const unsigned char *t1, *t2;

    t1 = s1;
    t2 = s2;
    for (res = 0; n != 0; n--)
        if ((res=*t1++-*t2++) != 0)
            break;
    return res;
}

int strcmp(const char *s1, const char *s2)
{
    int res;

    while (1) {
        if ((res=*s1-*s2)!=0 || *s1=='\0')
            break;
        s1++, s2++;
    }
    return res;
}

int strncmp(const char *s1, const char *s2, size_t n)
{
    int res;

    res = 0; /* two prefixes of length zero are equal */
    while (n--) {
        if ((res=*s1-*s2)!=0 || *s1=='\0')
            break;
        s1++, s2++;
    }
    return res;
}

int strcasecmp(const char *s1, const char *s2)
{
    int res;

    while (1) {
        if ((res=tolower(*s1)-tolower(*s2))!=0 || *s1=='\0')
            break;
        s1++, s2++;
    }
    return res;
}

int strncasecmp(const char *s1, const char *s2, size_t n)
{
    int res;

    while (n--) {
        if ((res=tolower(*s1)-tolower(*s2))!=0 || *s1=='\0')
            break;
        s1++, s2++;
    }
    return res;
}

void *memchr(const void *s, int c, size_t n)
{
    while (n && (*(unsigned char *)s != (unsigned char)c)) {
        s = (unsigned char *)s+1;
        --n;
    }
    if (n == 0)
        return NULL;
    return (void *)s;
}

char *strchr(const char *s, int c)
{
    while (*s != (char)c)
        if (*s++ == '\0')
            return NULL;
    return (char *)s;
}

size_t strcspn(const char *s1, const char *s2)
{
    size_t n;

    for (n = 0; *s1 != '\0'; s1++) {
        const char *p;

        for (p = s2; *p != '\0'; p++)
            if (*s1 == *p)
                return n;
        ++n;
    }
    return n;
}

char *strpbrk(const char *s1, const char *s2)
{
    for (; *s1 != '\0'; s1++) {
        const char *t;

        for (t = s2; *t != '\0'; t++)
            if (*s1 == *t)
                return (char *)s1;
    }
    return NULL;
}

char *strrchr(const char *s, int c)
{
    const char *t;

    t = s;
    while (*t++ != '\0')
        ;
    while (--t!=s && *t!=(char)c)
        ;
    if (*t == (char)c)
        return (char *)t;
    return NULL;
}

size_t strspn(const char *s1, const char *s2)
{
    size_t n;

    for (n = 0; *s1 != '\0'; s1++) {
        const char *p;

        for (p = s2; *p != '\0'; p++)
            if (*s1 == *p)
                break;
        if (*p == '\0')
            return n;
        ++n;
    }
    return n;
}

char *strstr(const char *s1, const char *s2)
{
    if (*s2 == '\0')
        return (char *)s1;

    while (*s1 != '\0') {
        char *t1, *t2;

        t1 = (char *)s1;
        t2 = (char *)s2;
        while (*t1!='\0' && *t2!='\0' && *t1==*t2)
            ++t1, ++t2;
        if (*t2 == '\0')
            return (char *)s1;
        ++s1;
    }
    return NULL;
}

char *strtok(char *s1, const char *s2)
{
    char *sbegin, *send;
    static char *next;

    sbegin = (s1!=NULL)?s1:next;
    if (sbegin == NULL)
        return NULL;
    sbegin += strspn(sbegin, s2);
    if (*sbegin == '\0') {
        next = NULL;
        return NULL;
    }
    send = strpbrk(sbegin, s2);
    if (send != NULL)
        *send++ = '\0';
    next = send;
    return sbegin;
}

void *memset(void *s, int c, size_t n)
{
    unsigned char *t;

    t = s;
    while (n-- != 0)
        *t++ = (unsigned char)c;
    return s;
}

size_t strlen(const char *s)
{
    size_t n;

    n = 0;
    while (*s++ != '\0')
        ++n;
    return n;
}

char *strdup(const char *s)
{
    char *t;

    if (s==NULL || (t=malloc(strlen(s)+1))==NULL)
        return NULL;
    strcpy(t, s);
    return t;
}

size_t strnlen(const char *s, size_t maxlen)
{
    size_t n1, n2;

    n1 = 0;
    n2 = maxlen;
    while (*s++!='\0' && n2--)
        ++n1;
    return (n1<maxlen)?n1:maxlen;
}
