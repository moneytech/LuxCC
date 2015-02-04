#ifndef DECL_H_
#define DECL_H_

#include "parser.h"

typedef struct Symbol Symbol;
struct Symbol {
    TypeExp *decl_specs;
    TypeExp *declarator;
    Symbol *next;
};

typedef struct TypeTag TypeTag;
struct TypeTag {
    TypeExp *type;
    TypeTag *next;
};

void analyze_init_declarator(TypeExp *decl_specs, TypeExp *declarator, int is_func_def);
int analyze_declarator(TypeExp *decl_specs, TypeExp *declarator, int inst_sym);
void analyze_decl_specs(TypeExp *d);
void analyze_enumerator(TypeExp *e);
void analyze_parameter_declaration(Declaration *d);
void analyze_function_definition(FuncDef *f);
void analyze_struct_declarator(TypeExp *sql, TypeExp *declarator);
void check_for_dup_member(DeclList *d);
void analyze_type_name(Declaration *tn);

void push_scope(void);
void pop_scope(void);
void restore_scope(void);
Symbol *lookup(char *id, int all);
void install_tag(TypeExp *t);
TypeTag *lookup_tag(char *id, int all);
int is_typedef_name(char *id);
char *stringify_type_exp(Declaration *d, int show_decayed);
TypeExp *get_sto_class_spec(TypeExp *d);
TypeExp *get_type_spec(TypeExp *d);
TypeExp *get_type_qual(TypeExp *d);
TypeExp *dup_declarator(TypeExp *d);
int are_compatible(TypeExp *ds1, TypeExp *dct1, TypeExp *ds2, TypeExp *dct2, int qualified, int compose);
int is_complete(char *tag);
int is_struct_union_enum(Token t);
int is_external_id(char *id);

#endif
