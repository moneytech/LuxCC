#include <getopt.h>
#include <string.h>
#include <stdio.h>

int optind = 1, opterr = 1, optopt;
char *optarg;

/*
 * Parse command-line options. getopt_long() is a GNU function.
 * TODO: add support for several short options in a single argv
 * element (e.g. "-abc" instead of "-a" "-b" "-c").
 */
int getopt_long(int argc, char *const *argv, const char *shortopts, const struct option *longopts, int *indexptr)
{
    if (optind>=argc || argv[optind]==NULL)
        return -1;
    optarg = NULL;
    if (argv[optind][0]=='-' && argv[optind][1]=='-') { /* long option */
        int i;
        size_t namlen;

        if (argv[optind][2] == '\0') /* "--" */
            return -1;
        for (i = 0; longopts[i].name != NULL; i++) {
            if (longopts[i].has_arg) {
                /*
                 * If there is an option 'abc' that takes an argument
                 * and also another option 'abcdef', then the following
                 *   "--abcdef"
                 * must match the second option and must not be interpreted
                 * as the first option with a 'def' argument (long options
                 * must be separated from their arguments by whitespace or '=').
                 */
                if (strncmp(longopts[i].name, argv[optind]+2, namlen=strlen(longopts[i].name))==0
                && (argv[optind][namlen+2]=='\0' || argv[optind][namlen+2]=='='))
                    break;
            } else {
                if (strcmp(longopts[i].name, argv[optind]+2) == 0)
                    break;
            }
        }
        if (longopts[i].name == NULL) { /* unknown option name */
            optopt = argv[optind][2];
            if (opterr)
                fprintf(stderr, "%s: illegal option: %s\n", argv[0], argv[optind]);
            ++optind;
            return '?';
        }
        if (indexptr != NULL)
            *indexptr = i;
        if (longopts[i].has_arg) {
            if (argv[optind][namlen+2] == '=') { /* "--<opt-nam>=<arg>" */
                optarg = argv[optind++]+2+namlen+1;
            } else { /* "--<opt-nam>" "<arg>" */
                ++optind;
                if (optind == argc) { /* missing arg */
                    if (opterr)
                        fprintf(stderr, "%s: option requires an argument: %s\n", argv[0], argv[optind-1]);
                    return '?'; /* TBD: should we consider shortopts[0]? */
                }
                optarg = argv[optind++];
            }
        } else {
            ++optind;
        }
        if (longopts[i].flag == NULL) {
            return longopts[i].val;
        } else {
            *longopts[i].flag = longopts[i].val;
            return 0;
        }
    } else if (argv[optind][0] == '-') { /* short option */
        char *cp;

        if (argv[optind][1] == '\0') /* "-" */
            return -1;
        if ((cp=strchr(shortopts, argv[optind][1])) == NULL) { /* unknown option character */
            optopt = argv[optind][1];
            if (opterr)
                fprintf(stderr, "%s: illegal option: -%c\n", argv[0], optopt);
            ++optind;
            return '?';
        }
        if (cp[1] == ':') {
            if (argv[optind][2] != '\0') { /* "-<opt-char><arg>" */
                optarg = argv[optind++]+2;
            } else { /* "-<opt-char>" "<arg>" */
                ++optind;
                if (optind == argc) { /* missing arg */
                    optopt = cp[0];
                    if (opterr)
                        fprintf(stderr, "%s: option requires an argument: -%c\n", argv[0], optopt);
                    return (shortopts[0] == ':')?':':'?';
                }
                optarg = argv[optind++];
            }
        } else {
            ++optind;
        }
        return cp[0];
    } else { /* non-option */
        return -1;
    }
}
