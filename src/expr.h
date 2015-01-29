#ifndef STMT_EXPR_H_
#define STMT_EXPR_H_

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
void analyze_initializer(TypeExp *ds, TypeExp *dct, ExecNode *e, int const_expr);

long eval_const_expr(ExecNode *e, int is_addr);
void analyze_array_size_expr(TypeExp *arr);
void analyze_enumeration_expr(TypeExp *en);
void reset_enum_val(void);

#endif
