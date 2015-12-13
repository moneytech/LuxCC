#ifndef _GETOPT_H
#define _GETOPT_H

#define no_argument		    0
#define required_argument	1
#define optional_argument	2

#ifdef __i386__
extern char *optarg;
extern int optind;
#endif

#ifdef __LuxVM__
#ifndef _HAS_OPT_VARS_
extern char **optarg;
extern int *optind;
#define optarg  (*optarg)
#define optind  (*optind)
#define _HAS_OPT_VARS_
#endif
#endif

struct option {
    const char *name;
    int has_arg;
    int *flag;
    int val;
};

int getopt_long(int argc, char *const argv[],
                const char *optstring, const struct option *longopts, int *longindex);
/* int getopt_long_only(int argc, char *const argv[], const char *optstring,
                    const struct option *longopts, int *longindex); */

#endif
