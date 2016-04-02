#ifndef _GETOPT_H
#define _GETOPT_H

#define no_argument         0
#define required_argument   1
/*#define optional_argument   2*/

extern char *optarg;
extern int optind, opterr, optopt;

struct option {
    const char *name;
    int has_arg;
    int *flag;
    int val;
};

int getopt_long(int argc, char *const *argv, const char *shortopts, const struct option *longopts, int *indexptr);
/*int getopt_long_only(int argc, char *const *argv, const char *shortopts, const struct option *longopts, int *indexptr);*/

#endif
