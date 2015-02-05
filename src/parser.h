#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"

typedef struct ExecNode ExecNode;

/*
 * Nodes that represent non-executable code.
 */
typedef struct TypeExp TypeExp;
typedef struct Declaration Declaration;
typedef struct DeclList DeclList;
typedef struct FuncDef FuncDef;
typedef struct ExternDecl ExternDecl;

struct TypeExp {
    Token op;
    char *str;        /* struct/union/enum tag or id */
    union {
        DeclList *dl; /* struct declaration list or function parameters */
        ExecNode *e;  /* array size exp or initializer */
        TypeExp *el;  /* enumerator list or pointer qualifiers */
    } attr;
    TypeExp *child, *sibling;
    TokenNode *info;
};

struct DeclList {
    Declaration *decl;
    DeclList *next;
};

struct Declaration {
    TypeExp *decl_specs;
    TypeExp *idl;     /* init declarator list */
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


/*
 * Nodes representing executable code.
 */
typedef enum {
    StmtNode,
    ExpNode
} ENKind;

typedef enum {
    IfStmt, WhileStmt, DoStmt, ForStmt, BreakStmt, ContinueStmt,
    ReturnStmt, ExpStmt, CmpndStmt, SwitchStmt, CaseStmt, DefaultStmt,
    LabelStmt, GotoStmt
} StmtKind;

typedef enum {
    OpExp, IConstExp, StrLitExp, IdExp
} ExpKind;

struct ExecNode {
    ExecNode *child[4];
    ExecNode *sibling;
    DeclList *locals;
    ENKind node_kind;
    union {
        StmtKind stmt;
        ExpKind exp;
    } kind;
    union {
        Token op;
        char *str;
        long val;
        unsigned long uval;
    } attr;
    Declaration type;
    TokenNode *info;
};

// void parser(TokenNode *);
ExternDecl *parser(TokenNode *tokens);

#endif
