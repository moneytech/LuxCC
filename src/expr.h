#ifndef EXPR_H_
#define EXPR_H_

#include "parser.h"

void analyze_primary_expression(ExecNode *e);
void analyze_postfix_expression(ExecNode *e);
void analyze_unary_expression(ExecNode *e);
void analyze_cast_expression(ExecNode *e);
void analyze_multiplicative_expression(ExecNode *e);
void analyze_additive_expression(ExecNode *e);
void analyze_assignment_expression(ExecNode *e);
void analyze_bitwise_operator(ExecNode *e);
void analyze_relational_equality_expression(ExecNode *e);
void analyze_logical_operator(ExecNode *e);
void analyze_conditional_expression(ExecNode *e);
void analyze_expression(ExecNode *e);

int can_assign_to(Declaration *dest_ty, ExecNode *e);
long eval_const_expr(ExecNode *e, int is_addr);
Token get_type_category(Declaration *d);
TypeExp *get_type_node(Token ty);
void reset_enum_val(void);
int is_unsigned_int(Token ty);
int is_integer(Token ty);
int get_rank(Token ty);

#endif
