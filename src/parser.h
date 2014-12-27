#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"

typedef struct TypeExp TypeExp;
typedef struct Declaration Declaration;
typedef struct DeclList DeclList;
typedef struct FuncDef FuncDef;
typedef struct ExternDecl ExternDecl;
typedef struct ExecNode ExecNode;

struct ExecNode {
    int xyz;
    DeclList *locals;
};

struct TypeExp {
    Token op;
    char *str;        /* struct/union/enum tag or id */
    union {
        DeclList *dl; /* struct declaration list or function parameters */
        ExecNode *e;  /* array size exp or initializer */
        TypeExp *el;  /* enumerator list */
    } attr;
    TypeExp *child, *sibling;
};

struct Declaration {
    TypeExp *decl_specs;
    TypeExp *idl;     /* init declarator list */
};

struct DeclList {
    Declaration *decl;
    DeclList *next;
};

struct FuncDef {
    TypeExp *decl_specs;
    TypeExp *header;  /* declarator */
    ExecNode *body;
};

typedef enum {
    DECLARATION,
    FUNCTION_DEFINITION
} EDKind;

struct ExternDecl {
    EDKind kind;
    union {
        Declaration *d;
        FuncDef *f;
    } ed;
    ExternDecl *sibling;
};

void parser(TokenNode *);

#endif
