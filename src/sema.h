#ifndef SEMA_H_
#define SEMA_H_

#include "parser.h"

typedef struct Symbol Symbol;
struct Symbol {
    // char *id;
    // Token tok;
    TypeExp *decl_specs;
    TypeExp *declarator;
    Symbol *next;
};

void push_scope(void);
void pop_scope(void);
Symbol *lookup(char *id, int all);
// void install(char *id, Token tok);
void install(TypeExp *decl_specs, TypeExp *declarator);
void analyze_init_declarator(TypeExp *decl_specs, TypeExp *declarator, int is_func_def);
void analyze_declarator(TypeExp *decl_specs, TypeExp *declarator);
void restore_scope(void);
void analyze_decl_specs(TypeExp *d);
int is_typedef_name(char *id);
Token get_id_token(char *id);
char *stringify_type_exp(Declaration *d);
void install_tag(TypeExp *t);
typedef struct TypeTag TypeTag;
struct TypeTag {
    TypeExp *type;
    TypeTag *next;
};
TypeTag *lookup_tag(char *id, int all);
void analyze_enumerator(TypeExp *e);
void analyze_parameter_declaration(Declaration *d);
void analyze_function_definition(FuncDef *f);

#endif
