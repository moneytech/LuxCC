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

typedef struct TypeExp TypeExp;
typedef struct Declaration Declaration;
typedef struct DeclList DeclList;
typedef struct FuncDef FuncDef;
typedef struct ExternDecl ExternDecl;
typedef struct ExecNode ExecNode;

struct ExecNode {
    int xyz;
};

struct TypeExp {
    Token op;
    char *str;
    union {
        DeclList *dl;
        ExecNode *e;
    } attr;
    TypeExp *child, *sibling;
};

struct Declaration {
    TypeExp *decl_specs;
    TypeExp *idl;
};

struct DeclList {
    Declaration *decl;
    DeclList *next;
};

struct FuncDef {
    TypeExp *decl_specs;
    TypeExp *header; /* declarator */
    ExecNode *body;
};

typedef enum {
    DECLARATION,
    FUNCTION_DEFINITION
} EDKind;

struct ExternDecl {
    EDKind kind;
    union {
        Declaration d;
        FuncDef f;
    } ed;
};

void parser(TokenNode *);

#endif
