/*
    Simple program to automate the testing of compiler diagnostics.

    Take as argument a list of .c files and check that diagnostics are emitted where
    they are expected and that no diagnostic is emitted where it is not expected.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <assert.h>
#include <unistd.h>
#include <stdarg.h>
#include "regex.h"

typedef struct Directive Directive;
typedef int bool;

enum {
    WarningMsg,
    ErrorMsg,
};

struct Directive {
    int msg_kind;
    char *msg;
    char *linep;
    int lineno;
    Directive *next;
} *directives;

char *prog_name;
char *curr_file;
char *buf, *curr;
int bufsz;

void err(bool fatal, char *fmt, ...)
{
    va_list args;

    fprintf(stderr, "%s: file %s: ", prog_name, curr_file);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    if (fatal)
        exit(1);
}

void scan_directives(void)
{
    FILE *fp;
    long len;
    int lineno;

    fp = fopen(curr_file, "rb");
    fseek(fp, 0, SEEK_END);
    len = ftell(fp);
    rewind(fp);
    bufsz = len+1;
    buf = curr = malloc(bufsz);

    lineno = 1;
    while (fgets(curr, 0x7FFFFFFF, fp) != NULL) {
        char *p, *q;

        /* directive general format: @ error|warning "msg" */
        if ((p=strrchr(curr, '@')) != NULL) {
            unsigned n;
            Directive *dir;

            dir = malloc(sizeof(Directive));
            p += 2;
            if (strncmp(p, "error", 5) == 0)
                dir->msg_kind = ErrorMsg;
            else if (strncmp(p, "warning", 7) == 0)
                dir->msg_kind = WarningMsg;
            else
                err(1, "line %d: expecting `error' or `warning'", lineno);
            if ((p=strchr(p, '\"')) == NULL)
                err(1, "line %d: missing message", lineno);
            if ((q=strchr(++p, '\"')) == NULL)
                err(1, "line %d: missing terminating \"", lineno);
            n = q-p;
            dir->msg = malloc(n+1);
            strncpy(dir->msg, p, n);
            dir->msg[n] = '\0';
            dir->linep = curr;
            dir->lineno = lineno;
            dir->next = NULL;
            if (directives == NULL) {
                directives = dir;
            } else {
                Directive *tmp;

                for (tmp = directives; tmp->next != NULL; tmp = tmp->next)
                    ;
                tmp->next = dir;
            }
        }
        curr += strlen(curr);
        ++lineno;
    }
    fclose(fp);
}

int exec_directives(void)
{
    int res;
    FILE *fp, *pp;
    Directive *dir;
    char tmpf[] = "/tmp/testXXXXXX.c";
    char cmd[128], linebuf[BUFSIZ];

    res = 0;
    mkstemps(tmpf, 2);
    fp = fopen(tmpf, "wb");
    fwrite(buf, bufsz-1, 1, fp);
    fflush(fp);

    sprintf(cmd, "src/luxcc -a -u %s 2>&1", tmpf);
    if ((pp=popen(cmd, "r")) == NULL)
        err(1, "failed to execute command");

    for (dir = directives; dir != NULL; dir = dir->next) {
        char *p;

        if (fgets(linebuf, sizeof(linebuf), pp) == NULL) {
            err(0, "line %d: missing diagnostic", dir->lineno);
            res = 1;
            goto done;
        }

        /* error/warning general format: file:line:column: error|warning: msg */
        p = strchr(linebuf, ':')+1;
        if (atoi(p) != dir->lineno) {
            err(0, "expecting diagnostic for line %d, got diagnostic for line %d", dir->lineno, atoi(p));
            res = 1;
            goto done;
        }
        p = strchr(p, ':')+1;
        p = strchr(p, ':')+1;
        p = strchr(p, ':')+1;

        /* compare messages */
        if (!regex_match(dir->msg, p)) {
            err(0, "line %d: mismatching diagnostics", dir->lineno);
            res = 1;
            goto done;
        }

        /* comment out the offending line */
        dir->linep[0] = '/';
        dir->linep[1] = '/';

        if (dir->msg_kind == ErrorMsg) {
            /* re-run */
            rewind(fp);
            fwrite(buf, bufsz-1, 1, fp);
            fflush(fp);
            pclose(pp);
            if ((pp=popen(cmd, "r")) == NULL)
                err(1, "failed to execute command");
        }
    }
    if (fgets(linebuf, sizeof(linebuf), pp) != NULL) {
        err(0, "line %d: unexpected diagnostic", atoi(strchr(linebuf, ':')+1));
        res = 1;
    }
done:
    pclose(pp);
    fclose(fp);
    unlink(tmpf);
    return res;
}

int main(int argc, char *argv[])
{
    int i, res;

    res = 0;
    prog_name = argv[0];
    for (i = 1; i < argc; i++) {
        Directive *dir, *tmp;

        curr_file = argv[i];
        scan_directives();
        if (exec_directives())
            res = 1;

        for (dir = directives; dir != NULL; ) {
            free(dir->msg);
            tmp = dir;
            dir = dir->next;
            free(tmp);
        }
        directives = NULL;
        free(buf);
    }
    return res;
}
