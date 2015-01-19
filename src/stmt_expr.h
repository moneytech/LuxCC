#ifndef STMT_EXPR_H_
#define STMT_EXPR_H_

#include "parser.h"

void analyze_expression(ExecNode *e);
void analyze_primary_expression(ExecNode *e);
void analyze_postfix_expression(ExecNode *e);
void analyze_unary_expression(ExecNode *e);
void analyze_cast_expression(ExecNode *e);

#endif
