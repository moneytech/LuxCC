#include "stmt.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "util.h"
#include "decl.h"
#include "expr.h"
#include "error.h"

#define ERROR(tok, ...) emit_error(FALSE, (tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column, __VA_ARGS__)

#define ERROR_R(tok, ...)\
    do {\
        ERROR(tok, __VA_ARGS__);\
        return;\
    } while (0)

#define WARNING(tok, ...) emit_warning((tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column, __VA_ARGS__)

#define HASH_SIZE           4093
#define MAX_SWITCH_NEST     16
#define HASH_VAL(s)         (hash(s)%HASH_SIZE)
#define HASH_VAL2(x)        (hash2(x)%HASH_SIZE)


typedef struct UnresolvedGoto UnresolvedGoto;
struct UnresolvedGoto {
    ExecNode *s;
    UnresolvedGoto *next;
} *unresolved_gotos_list;


typedef struct SwitchLabel SwitchLabel;
static struct SwitchLabel {
    int val;
    int is_default;
    SwitchLabel *next;
} *switch_labels[MAX_SWITCH_NEST][HASH_SIZE];

static int switch_nesting_level = -1;

static
int install_switch_label(long val, int is_default)
{
    SwitchLabel *np;
    unsigned long h;

    h = is_default?0:HASH_VAL2((unsigned long)val); /* 'default' labels are always at bucket zero */
    np = switch_labels[switch_nesting_level][h];
    while (np != NULL) {
        if (is_default) {
            if (np->is_default)
                break;
        } else if (np->val == val) {
            break;
        }
        np = np->next;
    }

    if (np == NULL) {
        /* not found in this switch nesting level */
        np = malloc(sizeof(SwitchLabel));
        np->val = val;
        np->is_default = is_default;
        np->next = switch_labels[switch_nesting_level][h];
        switch_labels[switch_nesting_level][h] = np;
        return TRUE; /* success */
    } else {
        return FALSE; /* failure */
    }
}

void increase_switch_nesting_level(void)
{
    ++switch_nesting_level;
}

void decrease_switch_nesting_level(void)
{
    int i;
    SwitchLabel *np, *temp;

    for (i = 0; i < HASH_SIZE; i++) {
        if (switch_labels[switch_nesting_level][i] != NULL) {
            for (np = switch_labels[switch_nesting_level][i]; np != NULL;) {
                temp = np;
                np = np->next;
                free(temp);
            }
            switch_labels[switch_nesting_level][i] = NULL;
        }
    }
    --switch_nesting_level;
}


typedef struct LabelName LabelName;
static struct LabelName {
    char *name;
    LabelName *next;
} *label_names[HASH_SIZE];

static
LabelName *lookup_label_name(char *name)
{
    LabelName *np;

    for (np = label_names[HASH_VAL(name)]; np != NULL; np = np->next)
        if (equal(name, np->name))
            return np;
    return NULL; /* not found */
}

static
int install_label_name(char *name)
{
    unsigned h;
    LabelName *np;

    h = HASH_VAL(name);
    for (np = label_names[h]; np != NULL; np = np->next)
        if (equal(name, np->name))
            break;

    if (np == NULL) {
        np = malloc(sizeof(LabelName));
        np->name = strdup(name);
        np->next = label_names[h];
        label_names[h] = np;
        return TRUE; /* success */
    } else {
        return FALSE; /* failure */
    }
}

void empty_label_table(void)
{
    int i;
    LabelName *np, *temp;

    for (i = 0; i < HASH_SIZE; i++) {
        if (label_names[i] != NULL) {
            for (np = label_names[i]; np != NULL;) {
                temp = np;
                np = np->next;
                free(temp);
            }
            label_names[i] = NULL;
        }
    }
}

/*
 * Resolve any forward jumps.
 */
void resolve_gotos(void)
{
    UnresolvedGoto *p;

    p = unresolved_gotos_list;
    while (p != NULL) {
        LabelName *lab;
        UnresolvedGoto *temp;

        if ((lab=lookup_label_name(p->s->attr.str)) == NULL) {
            ERROR(p->s, "use of undefined label `%s'", p->s->attr.str);
        }

        temp = p;
        p = p->next;
        free(temp);
    }

    unresolved_gotos_list = NULL;
}

/*
 * Return type of the current function being processed.
 * Used for return statement.
 */
static Declaration ret_ty;

void set_return_type(TypeExp *ds, TypeExp *dct)
{
    ret_ty.decl_specs = ds;
    ret_ty.idl = dct;
}

/*
 * This functions is similar to expr.c's one, but also considers
 * as scalars to array and function expressions.
 */
static
int is_scalar(Token op)
{
    return (op==TOK_STAR || op==TOK_SUBSCRIPT || op==TOK_FUNCTION || is_integer(op));
}

void analyze_labeled_statement(ExecNode *s, int in_switch)
{
    /*
     * 6.8.1
     * #2 A case or default label shall appear only in a switch statement.
     * #3 Label names shall be unique within a function.
     */

    switch (s->kind.stmt) {
    case LabelStmt:
        /*
         * 6.8.1
         * #3 Label names shall be unique within a function.
         */
        if (!install_label_name(s->attr.str))
            ERROR(s, "duplicate label `%s'", s->attr.str);

        break;
    /*
     * 6.8.4.2
     * #3 The expression of each case label shall be an integer constant expression and no two of
     * the case constant expressions in the same switch statement shall have the same value
     * after conversion. There may be at most one default label in a switch statement.
     * (Any enclosed switch statement may have a default label or case constant
     * expressions with values that duplicate case constant expressions in the enclosing
     * switch statement).
     */
    case CaseStmt: {
        Token ty;

        if (!in_switch)
            ERROR(s, "case label not within a switch statement");

        ty = get_type_category(&s->child[0]->type);
        if (ty == TOK_ERROR)
            return;
        if (!is_integer(ty))
            ERROR_R(s->child[0], "case label expression has non-integer type");
        s->child[0]->attr.val = eval_int_const_expr(s->child[0]);

        if (!install_switch_label(s->child[0]->attr.val, FALSE))
            ERROR(s, "duplicate case value `%ld'", s->child[0]->attr.val);
        break;
    }
    case DefaultStmt:
        if (!in_switch)
            ERROR_R(s, "default label not within a switch statement");

        if (!install_switch_label(0, TRUE))
            ERROR(s, "multiple default labels in one switch");
        break;
    }
}

void analyze_selection_statement(ExecNode *s)
{
    Token ty;

    ty = get_type_category(&s->child[0]->type);
    if (ty == TOK_ERROR)
        return;

    if (s->kind.stmt == IfStmt) {
        /*
         * 6.8.4.1
         * #1 The controlling expression of an if statement shall have scalar type.
         */
        if (!is_scalar(ty))
            ERROR(s, "controlling expression of if statement has non-scalar type");
    } else /* if (s->kind.stmt == SwitchStmt) */ {
        /*
         * 6.8.4.2
         * #1 The controlling expression of a switch statement shall have integer type.
         */
        if (!is_integer(ty))
            ERROR(s, "controlling expression of switch statement has non-integer type");
    }
}

void analyze_iteration_statement(ExecNode *s)
{
    /*
     * 6.8.5
     * #2 The controlling expression of an iteration statement shall have scalar type.
     */
    Token ty;

    /* the controlling expression of a for statement can be missing */
    if (s->child[0] == NULL)
        return; /* OK */

    ty = get_type_category(&s->child[0]->type);
    if (ty == TOK_ERROR)
        return;

    if (!is_scalar(ty))
        ERROR(s, "controlling expression of %s statement has non-scalar type",
        (s->kind.stmt==WhileStmt)?"while":(s->kind.stmt==DoStmt)?"do":"for");
}

void analyze_jump_statement(ExecNode *s, int in_loop, int in_switch)
{
    switch (s->kind.stmt) {
    case GotoStmt: {
        /*
         * 6.8.6.1
         * #1 The identifier in a goto statement shall name a label located
         * somewhere in the enclosing function.
         */
        LabelName *lab;

        if ((lab=lookup_label_name(s->attr.str)) == NULL) {
            /* forward jump */
            UnresolvedGoto *new_node;

            new_node = malloc(sizeof(UnresolvedGoto));
            new_node->s = s;
            new_node->next = unresolved_gotos_list;
            unresolved_gotos_list = new_node;
        }
        break;
    }
    case ContinueStmt:
        if (!in_loop)
            ERROR(s, "continue statement not within a loop");
        break;
    case BreakStmt:
        if (!in_loop && !in_switch)
            ERROR(s, "break statement no within loop or switch");
        break;
    case ReturnStmt:
        /*
         * 6.8.6.4
         * #1 A return statement with an expression shall not appear in a function whose return
         * type is void. A return statement without an expression shall only appear in a function
         * whose return type is void.
         */
        if (s->child[0] != NULL) {
            /* return <expression>; */
            if (ret_ty.idl==NULL && get_type_spec(ret_ty.decl_specs)->op==TOK_VOID)
                ERROR_R(s, "return statement with an expression in void function");

            if (get_type_category(&s->child[0]->type) == TOK_ERROR)
                return;
            /*
             * #3 If a return statement with an expression is executed, the value of the expression is
             * returned to the caller as the value of the function call expression. If the expression has a
             * type different from the return type of the function in which it appears, the value is
             * converted as if by assignment to an object having the return type of the function.
             */
            if (!can_assign_to(&ret_ty, s->child[0])) {
                char *ty1, *ty2;

                ty1 = stringify_type_exp(&s->child[0]->type, TRUE);
                ty2 = stringify_type_exp(&ret_ty, FALSE);
                ERROR(s, "incompatible types when returning type `%s' but `%s' was expected", ty1, ty2);
                free(ty1), free(ty2);
            }
            /* save return type for later use */
            s->child[1] = (ExecNode *)ret_ty.decl_specs;
            s->child[2] = (ExecNode *)ret_ty.idl;
        } else {
            /* return; */
            if (ret_ty.idl!=NULL || get_type_spec(ret_ty.decl_specs)->op!=TOK_VOID)
                ERROR(s, "return statement without an expression in non-void function");
        }
        break;
    }
}
