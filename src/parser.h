#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"

#if 0
typedef struct TypeConstructor TypeConstructor;
typedef struct Declaration Declaration;

struct TypeConstructor {
    Token constructor;
    char *str; /* struct/union/enum tag or id */
    union {
        Declaration *d; /* struct declaration list or function parameters */
        ExecNode *exp; /* array size exp or initializer */
    } xyz;
    TypeConstructor *child, *sibling;
};

struct Declaration {
    TypeConstructor *decl_specs;
    TypeConstructor *idl; /* init declarator list */
    Declaration *sibling;
};
#endif

void parser(TokenNode *);

#endif
