#include "expr.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#define DEBUG 1
#include "util.h"
#include "decl.h"
#undef ERROR

#define ERROR(tok, ...) fprintf(stderr, INFO_COLOR "%s:%d:%d: " ERROR_COLOR "error: " RESET_ATTR, (tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column),fprintf(stderr, __VA_ARGS__),fprintf(stderr, "\n"),exit(EXIT_FAILURE)
#define WARNING(tok, ...) fprintf(stderr, INFO_COLOR "%s:%d:%d: " WARNING_COLOR "warning: " RESET_ATTR, (tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column),fprintf(stderr, __VA_ARGS__), fprintf(stderr, "\n")


// static
Token get_type_category(Declaration *d)
{
    if (d->idl != NULL)
        return d->idl->op;
    else
        return get_type_spec(d->decl_specs)->op;
}

int is_integer(Token ty)
{
    // return (t==TOK_CHAR || t==TOK_INT || t==TOK_LONG/* || t==TOK_ENUM_CONST*/);
    switch (ty) {
    case TOK_LONG: case TOK_UNSIGNED_LONG:
    case TOK_INT: case TOK_UNSIGNED:
    case TOK_SHORT: case TOK_UNSIGNED_SHORT:
    case TOK_CHAR: case TOK_SIGNED_CHAR: case TOK_UNSIGNED_CHAR:
    case TOK_ENUM:
        return TRUE;
    default:
        return FALSE;
    }
}

void analyze_array_size_expr(TypeExp *arr)
{
    long size;

    /*
     * 6.7.5.2#1
     * [...] If they delimit an expression (which specifies the size of an array), the
     * expression shall have an integer type. If the expression is a constant expression,
     * it shall have a value greater than zero. [...] Note: VLAs are not supported, so the
     * expression must always be constant.
     */
    if (!is_integer(get_type_category(&arr->attr.e->type)))
        ERROR(arr, "size of array has non-integer type");

    size = eval_const_expr(arr->attr.e, FALSE);
    if (size <= 0)
        ERROR(arr, "size of array not greater than zero");

    /* store the computed size in the root of the expression tree */
    arr->attr.e->attr.val = size; /* maybe overwrites the operator */
}

static long en_val = -1;

void reset_enum_val(void)
{
    en_val = -1;
}

void analyze_enumeration_expr(TypeExp *en)
{
    /*
     * 6.7.2.2#2
     * The expression that defines the value of an enumeration constant shall be an integer
     * constant expression that has a value representable as an int.
     */
    if (en->attr.e != NULL) {
        if (!is_integer(get_type_category(&en->attr.e->type)))
            ERROR(en, "enumerator value is not an integer constant");
        en_val = eval_const_expr(en->attr.e, FALSE);
    } else {
        en->attr.e = calloc(1, sizeof(ExecNode));
        ++en_val;
    }

    /*if (en_val > 2147483647)
        ...*/

    en->attr.e->attr.val = en_val;
}

static int is_pointer(Token op)
{
    return (op==TOK_STAR || op==TOK_SUBSCRIPT);
}

static int is_scalar(Token op)
{
    return (is_integer(op) || op==TOK_STAR);
}

/*
 *    The C expressions that can be lvalues are:
 * Expression             Additional requirements
 *  name                   name must be a variable
 *  e[k]                   none
 *  (e)                    e must be an lvalue
 *  e.name                 e must be an lvalue
 *  e->name                none
 *  *e                     none
 *  string-constant        none
 */
static int is_lvalue(ExecNode *e)
{
    if (e->kind.exp == IdExp) {
        if (e->type.idl!=NULL && (/*e->type.idl->op==TOK_ENUM_CONST||*/e->type.idl->op==TOK_FUNCTION))
            return FALSE;
        return TRUE;
    } else if (e->kind.exp == OpExp) {
        switch (e->attr.op) {
        case TOK_SUBSCRIPT:
        case TOK_ARROW:
        case TOK_INDIRECTION:
            return TRUE;
        case TOK_DOT:
            return is_lvalue(e->child[0]);
        default:
            return FALSE;
        }
    } else if (e->kind.exp == StrLitExp) {
        return TRUE;
    } else if (e->kind.exp == IConstExp) {
        return FALSE;
    }

    fprintf(stderr, "is_lvalue()\n");
    exit(EXIT_FAILURE);
}

static int is_modif_struct_union(TypeExp *type)
{
    DeclList *d;
    TypeExp *dct;

    for (d = type->attr.dl; d != NULL; d = d->next) {
        int modifiable;
        TypeExp *ts, *tq;

        tq = get_type_qual(d->decl->decl_specs);
        ts = get_type_spec(d->decl->decl_specs);

        modifiable = TRUE;
        if (ts->op == TOK_STRUCT) {
            if (ts->attr.dl == NULL)
                ts = lookup_tag(ts->str, TRUE)->type;
            modifiable = is_modif_struct_union(ts);
        }

        for (dct = d->decl->idl; dct != NULL; dct = dct->sibling) {
            TypeExp *p;

            p = dct->child;
            if (p!=NULL && p->op==TOK_SUBSCRIPT)
                for (; p!=NULL && p->op==TOK_SUBSCRIPT; p = p->child);

            if (p == NULL) {
                /* the member type is not a derived declarator type */
                if ((tq!=NULL && (tq->op==TOK_CONST||tq->op==TOK_CONST_VOLATILE))
                || !modifiable)
                    return FALSE;
            } else if (p->op == TOK_STAR) {
                if (p->attr.el != NULL
                && (p->attr.el->op==TOK_CONST||p->attr.el->op==TOK_CONST_VOLATILE))
                    return FALSE;
            }
        }
    }

    return TRUE;
}

static int is_modif_lvalue(ExecNode *e)
{
    /*
     * 6.3.2.1#1
     * A modifiable lvalue is an lvalue that does not have array type, does not have an
     * incomplete type, does not have a const-qualified type, and if it is a structure
     * or union, does not have any member (including, recursively, any member or element
     * of all contained aggregates or unions) with a const-qualified type.
     */
    Token ty;

    if (!is_lvalue(e))
        return FALSE;

    ty = get_type_category(&e->type);
    if (ty == TOK_SUBSCRIPT) {
        return FALSE;
    } else if (ty == TOK_STAR) {
        if (e->type.idl->attr.el != NULL
        && (e->type.idl->attr.el->op==TOK_CONST||e->type.idl->attr.el->op==TOK_CONST_VOLATILE))
            return FALSE;
    } else if (ty == TOK_VOID) {
        return FALSE;
    } else {
        TypeExp *tq;

        if ((tq=get_type_qual(e->type.decl_specs))!=NULL && (tq->op==TOK_CONST||tq->op==TOK_CONST_VOLATILE))
            return FALSE;

        if (ty==TOK_STRUCT||ty==TOK_UNION||ty==TOK_ENUM) {
            TypeExp *ts;

            ts = get_type_spec(e->type.decl_specs);
            if (!is_complete(ts->str))
                return FALSE;
            if (ty!=TOK_ENUM && !is_modif_struct_union(ts))
                return FALSE;
        }
    }

    return TRUE;
}

TypeExp *dup_decl_specs_list(TypeExp *ds)
{
    TypeExp *new_list, *temp;

    new_list = temp = malloc(sizeof(TypeExp));
    *new_list = *ds;
    ds = ds->child;
    while (ds != NULL) {
        temp->child = malloc(sizeof(TypeExp));
        *temp->child = *ds;
        temp=temp->child, ds=ds->child;
    }

    return new_list;
}

Token get_promoted_type(Token int_ty)
{
    switch (int_ty) {
    case TOK_CHAR: case TOK_UNSIGNED_CHAR: case TOK_SIGNED_CHAR:
    case TOK_SHORT: case TOK_UNSIGNED_SHORT:
        return TOK_INT;
    default:
        return int_ty;
    }
}

/*
 * Every integer type has an integer conversion rank.
 * Integer conversion ranks from highest to lowest
 * 1) long long int, unsigned long long int
 * 2) long int, unsigned long int
 * 3) int, unsigned int
 * 4) short int, unsigned short int
 * 5) char, signed char, unsigned char
 * 6) _Bool
 */
int get_rank(Token ty)
{
    switch (ty) {
    case TOK_LONG:
    case TOK_UNSIGNED_LONG:
        return 4;
    case TOK_INT:
    case TOK_UNSIGNED:
        return 3;
    case TOK_SHORT:
    case TOK_UNSIGNED_SHORT:
        return 2;
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
    case TOK_UNSIGNED_CHAR:
        return 1;
    }
}

int is_signed_int(Token ty)
{
    switch (ty) {
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
    case TOK_SHORT:
    case TOK_INT:
    case TOK_LONG:
    // case TOK_ENUM:
        return TRUE;
    default:
        return FALSE;
    }
}

int is_unsigned_int(Token ty)
{
    switch (ty) {
    case TOK_UNSIGNED_CHAR:
    case TOK_UNSIGNED_SHORT:
    case TOK_UNSIGNED:
    case TOK_UNSIGNED_LONG:
        return TRUE;
    default:
        return FALSE;
    }
}

/*
 * ty1, ty2: promoted operands.
 */
Token get_result_type(Token ty1, Token ty2)
{
    /*
     * 6.3.1.8 Usual arithmetic conversions.
     * [...]
     * If both operands have the same type, then no further conversion is needed.
     */

    /*
     * Otherwise, the integer promotions are performed on both operands. Then the
     * following rules are applied to the promoted operands:
     */
    int rank1, rank2;
    int sign1, sign2;

    /*
     * If both operands have the same type, then no further conversion is needed.
     */
    if (ty1 == ty2)
        return ty1;

    /* fetch rank and sign */
    rank1 = get_rank(ty1);
    rank2 = get_rank(ty2);
    sign1 = is_signed_int(ty1);
    sign2 = is_signed_int(ty2);

    /*
     * Otherwise, if both operands have signed integer types or both have unsigned
     * integer types, the operand with the type of lesser integer conversion rank is
     * converted to the type of the operand with greater rank.
     */
    if (sign1 == sign2)
        return (rank1>rank2)?ty1:ty2;

    /*
     * Otherwise, if the operand that has unsigned integer type has rank greater or
     * equal to the rank of the type of the other operand, then the operand with
     * signed integer type is converted to the type of the operand with unsigned
     * integer type.
     */
    if (!sign1 && rank1>=rank2)
        return ty1;
    if (!sign2 && rank2>=rank1)
        return ty2;

    /*
     * Otherwise, if the type of the operand with signed integer type can represent
     * all of the values of the type of the operand with unsigned integer type, then
     * the operand with unsigned integer type is converted to the type of the
     * operand with signed integer type.
     */
    /* nothing of this applies with sizeof(int)==sizeof(long) */

    /*
     * Otherwise, both operands are converted to the unsigned integer type
     * corresponding to the type of the operand with signed integer type.
     */
    return TOK_UNSIGNED_LONG;
}

TypeExp *get_type_node(Token ty)
{
    static TypeExp ty_char = { TOK_CHAR };
    static TypeExp ty_int = { TOK_INT };
    static TypeExp ty_unsigned = { TOK_UNSIGNED };
    static TypeExp ty_long = { TOK_LONG };
    static TypeExp ty_unsigned_long = { TOK_UNSIGNED_LONG };

    switch (ty) {
    case TOK_CHAR: return &ty_char;
    case TOK_INT: case TOK_ENUM: return &ty_int;
    case TOK_UNSIGNED: return &ty_unsigned;
    case TOK_LONG: return &ty_long;
    case TOK_UNSIGNED_LONG: return &ty_unsigned_long;
    }

    fprintf(stderr, "get_type_node()\n");
    exit(EXIT_FAILURE);
}

void binary_op_error(ExecNode *op)
{
    /*
     * Convert arrays and functions to pointers
     * to array and function respectively.
     */
    int idx;

    for (idx = 0; idx < 2; idx++) {
        if (op->child[idx]->type.idl != NULL) {
            if (op->child[idx]->type.idl->op == TOK_FUNCTION) {
                TypeExp *ptr_node;

                ptr_node = calloc(1, sizeof(TypeExp));
                ptr_node->op = TOK_STAR;
                ptr_node->child = op->child[idx]->type.idl;
                op->child[idx]->type.idl = ptr_node;
            } else if (op->child[idx]->type.idl->op == TOK_SUBSCRIPT) {
                op->child[idx]->type.idl->op = TOK_STAR;
                op->child[idx]->type.idl->attr.e = NULL;
            }
        }
    }

    ERROR(op, "invalid operands to binary %s (`%s' and `%s')", token_table[op->attr.op*2+1],
    stringify_type_exp(&op->child[0]->type), stringify_type_exp(&op->child[1]->type));
}

int is_ptr2obj(Declaration *p)
{
    if (p->idl->child != NULL) {
        if (p->idl->child->op == TOK_FUNCTION)
            return FALSE; /* pointer to function */
        if (p->idl->child->op==TOK_SUBSCRIPT && p->idl->child->attr.e==NULL)
            return FALSE; /* pointer to incomplete type */
    } else {
        TypeExp *ts;

        ts = get_type_spec(p->decl_specs);
        if (is_struct_union_enum(ts->op)&&!is_complete(ts->str) || ts->op==TOK_VOID)
            return FALSE; /* pointer to incomplete type */
    }

    return TRUE;
}

/*
 * See if the expression `e' can be stored in a variable of type `dest_ty'.
 */
// static
// int can_assign_to(ExecNode *e, Declaration *left_op, Declaration *right_op)
int can_assign_to(Declaration *dest_ty, ExecNode *e)
{
    /*
     * 6.5.16.1 Simple assignment
     * Constraints
     * 1# One of the following shall hold:
     * — the left operand has qualified or unqualified arithmetic type and the right has
     * arithmetic type;
     * — the left operand has a qualified or unqualified version of a structure or union type
     * compatible with the type of the right;
     * — both operands are pointers to qualified or unqualified versions of compatible types,
     * and the type pointed to by the left has all the qualifiers of the type pointed to by the
     * right;
     * — one operand is a pointer to an object or incomplete type and the other is a pointer to a
     * qualified or unqualified version of void, and the type pointed to by the left has all
     * the qualifiers of the type pointed to by the right;
     * — the left operand is a pointer and the right is a null pointer constant;
     */
    Token cat_d, cat_s;
    Declaration *src_ty;

    src_ty = &e->type;

    cat_d = get_type_category(dest_ty);
    cat_s = get_type_category(src_ty);

    if (is_integer(cat_d)) {
        if (is_integer(cat_s)) {
            int rank_d, rank_s;

            /*
             * If the src expression is an integer constant, just see if said
             * constant fits into the dest type, and emit a warning if it doesn't.
             */
            if (e->kind.exp == IConstExp) {
                /*int overflow;

                overflow = FALSE;
                switch (cat_d) {
                case TOK_UNSIGNED_LONG:
                case TOK_UNSIGNED:
                    break;
                case TOK_LONG:
                case TOK_INT:
                    if (e->attr.uval > 2147483647)
                        overflow = TRUE;
                    break;
                case TOK_SHORT:
                    if (e->attr.uval > 32767)
                        overflow = TRUE;
                    break;
                case TOK_UNSIGNED_SHORT:
                    if (e->attr.uval > 65535)
                        overflow = TRUE;
                    break;
                case TOK_CHAR:
                case TOK_SIGNED_CHAR:
                    if (e->attr.uval > 127)
                        overflow = TRUE;
                    break;
                case TOK_UNSIGNED_CHAR:
                    if (e->attr.uval > 255)
                        overflow = TRUE;
                    break;
                }

                if (overflow)
                    WARNING(e, "integer constant too large for `%s' type", token_table[cat_d*2+1]);*/

                return TRUE;
            }

            /* int and long have the same rank for assignment purposes */
            rank_d = get_rank(cat_d);
            rank_d = (rank_d==4)?3:rank_d;
            rank_s = get_rank(cat_s);
            rank_s = (rank_s==4)?3:rank_s;

            /*
             * Emit a warning when the destination type is narrower than the source type.
             */
            if (rank_s > rank_d)
                WARNING(e, "implicit conversion loses integer precision: `%s' to `%s'",
                token_table[cat_s*2+1], token_table[cat_d*2+1]);
            /*
             * Otherwise, emit a warning if the source and destination types
             * do not have the same signedness.
             */
            else if (rank_d==rank_s && is_signed_int(cat_d)!=is_signed_int(cat_s))
                WARNING(e, "implicit conversion changes signedness: `%s' to `%s'",
                token_table[cat_s*2+1], token_table[cat_d*2+1]);
        } else if (is_pointer(cat_s) || cat_s==TOK_FUNCTION) {
            WARNING(e, "pointer to integer conversion without a cast");
        } else {
            return FALSE;
        }
    } else if (cat_d==TOK_STRUCT || cat_d==TOK_UNION) {
        TypeExp *ts_d, *ts_s;

        if (cat_d != cat_s)
            return FALSE;

        ts_d = get_type_spec(dest_ty->decl_specs);
        ts_s = get_type_spec(src_ty->decl_specs);
        if (ts_d->str != ts_s->str)
            return FALSE;
    } else if (cat_d == TOK_STAR) {
        if (is_pointer(cat_s) || cat_s==TOK_FUNCTION) {
            TypeExp *ts_d, *ts_s;

            /*
             * Check if the pointers are compatible. If they are, continue
             * and check for the additional requirement of type qualifiers;
             * otherwise, emit a warning and return.
             */
            if (dest_ty->idl->child==NULL && get_type_spec(dest_ty->decl_specs)->op==TOK_VOID) {
                if (cat_s==TOK_FUNCTION || src_ty->idl->child!=NULL&&src_ty->idl->child->op==TOK_FUNCTION) {
                    WARNING(e, "function pointer implicitly converted to void pointer");
                    return TRUE;
                }
            } else if (cat_s!=TOK_FUNCTION && src_ty->idl->child==NULL
            && get_type_spec(src_ty->decl_specs)->op==TOK_VOID) {
                if (dest_ty->idl->child!=NULL && dest_ty->idl->child->op==TOK_FUNCTION) {
                    WARNING(e, "void pointer implicitly converted to function pointer");
                    return TRUE;
                }
            } else if (!are_compatible(dest_ty->decl_specs, dest_ty->idl->child,
                src_ty->decl_specs, (cat_s!=TOK_FUNCTION)?src_ty->idl->child:src_ty->idl, FALSE, FALSE)) {
                    WARNING(e, "assignment from incompatible pointer type");
                    return TRUE;
            }

            /* OK, they are compatible, go on... */

            /*
             * Verify that the type pointed to by the left operand has
             * all the qualifiers of the type pointed to by the right.
             */
            ts_d = ts_s = NULL;

            /* fetch qualifiers of left pointed to type */
            if (dest_ty->idl->child == NULL)
                ts_d = get_type_qual(dest_ty->decl_specs);
            else if (dest_ty->idl->child->op == TOK_STAR)
                ts_d = dest_ty->idl->child->attr.el;
            /* fetch qualifiers of right pointed to type */
            if (src_ty->idl->child == NULL)
                ts_s = get_type_qual(src_ty->decl_specs);
            else if (src_ty->idl->child->op == TOK_STAR)
                ts_s = src_ty->idl->child->attr.el;

            if (ts_s != NULL) {
                char *discarded;

                discarded = NULL;
                if (ts_s->op == TOK_CONST_VOLATILE) {
                    if (ts_d == NULL)
                        discarded = "const volatile";
                    else if (ts_d->op == TOK_CONST)
                        discarded = "volatile";
                    else if (ts_d->op == TOK_VOLATILE)
                        discarded = "const";
                } else if (ts_s->op == TOK_CONST) {
                    if (ts_d==NULL || ts_d->op==TOK_VOLATILE)
                        discarded = "const";
                } else if (ts_s->op == TOK_VOLATILE) {
                    if (ts_d==NULL || ts_d->op==TOK_CONST)
                        discarded = "volatile";
                }
                if (discarded != NULL)
                    WARNING(e, "assignment discards `%s' qualifier from pointer target type", discarded);
            }
        } else if (is_integer(cat_s)) {
            if (e->kind.exp!=IConstExp || e->attr.val!=0)
                WARNING(e, "integer to pointer conversion without a cast");
        } else {
            return FALSE;
        }
    }

    return TRUE;
}

void analyze_expression(ExecNode *e)
{
    /*
     * 6.5.17
     * #2 The left operand of a comma operator is evaluated as a void expression; there is a
     * sequence point after its evaluation. Then the right operand is evaluated; the result has its
     * type and value.97) If an attempt is made to modify the result of a comma operator or to
     * access it after the next sequence point, the behavior is undefined.
     */
    e->type = e->child[1]->type;
}

void analyze_assignment_expression(ExecNode *e)
{
    /*
     * 6.5.16
     * #2 An assignment operator shall have a modifiable lvalue as its left operand.
     */
    if (!is_modif_lvalue(e->child[0]))
        ERROR(e, "expression is not assignable");

    /*
     * #3 An assignment operator stores a value in the object designated by the left operand. An
     * assignment expression has the value of the left operand after the assignment, but is not an
     * lvalue. The type of an assignment expression is the type of the left operand unless the
     * left operand has qualified type, in which case it is the unqualified version of the type of
     * the left operand. The side effect of updating the stored value of the left operand shall
     * occur between the previous and the next sequence point.
     */
    if (e->attr.op == TOK_ASSIGN) {
        // if (!can_assign_to(e, &e->child[0]->type, &e->child[1]->type))
        if (!can_assign_to(&e->child[0]->type, e->child[1]))
            ERROR(e, "incompatible types when assigning to type `%s' from type `%s'",
            stringify_type_exp(&e->child[0]->type), stringify_type_exp(&e->child[1]->type));
    } else {
        /* E1 op= E2 ==> E1 = E1 op (E2) ; with E1 evaluated only once */
        ExecNode temp;

        temp = *e;
        switch (e->attr.op) {
        case TOK_MUL_ASSIGN:
            temp.attr.op = TOK_MUL;
            analyze_multiplicative_expression(&temp);
            break;
        case TOK_DIV_ASSIGN:
            temp.attr.op = TOK_DIV;
            analyze_multiplicative_expression(&temp);
            break;
        case TOK_MOD_ASSIGN:
            temp.attr.op = TOK_MOD;
            analyze_multiplicative_expression(&temp);
            break;
        case TOK_PLUS_ASSIGN:
            temp.attr.op = TOK_PLUS;
            analyze_additive_expression(&temp);
            break;
        case TOK_MINUS_ASSIGN:
            temp.attr.op = TOK_MINUS;
            analyze_additive_expression(&temp);
            break;
        case TOK_LSHIFT_ASSIGN:
            temp.attr.op = TOK_LSHIFT;
            analyze_bitwise_operator(&temp);
            break;
        case TOK_RSHIFT_ASSIGN:
            temp.attr.op = TOK_RSHIFT;
            analyze_bitwise_operator(&temp);
            break;
        case TOK_BW_AND_ASSIGN:
            temp.attr.op = TOK_BW_AND;
            analyze_bitwise_operator(&temp);
            break;
        case TOK_BW_XOR_ASSIGN:
            temp.attr.op = TOK_BW_XOR;
            analyze_bitwise_operator(&temp);
            break;
        case TOK_BW_OR_ASSIGN:
            temp.attr.op = TOK_BW_OR;
            analyze_bitwise_operator(&temp);
            break;
        }
        // if (!can_assign_to(e, &e->child[0]->type, &temp.type))
        if (!can_assign_to(&e->child[0]->type, &temp))
            ERROR(e, "incompatible types when assigning to type `%s' from type `%s'",
            stringify_type_exp(&e->child[0]->type), stringify_type_exp(&temp.type));
    }

    e->type = e->child[0]->type;

    printf("assign: %s\n", stringify_type_exp(&e->type));
}

void analyze_conditional_expression(ExecNode *e)
{
    /*
     * 6.5.15
     *
     * #2 The first operand shall have scalar type.
     * #3 One of the following shall hold for the second and third operands:
     * — both operands have arithmetic type;
     * — both operands have the same structure or union type;
     * — both operands have void type;
     * — both operands are pointers to qualified or unqualified versions of compatible types;
     * — one operand is a pointer and the other is a null pointer constant; or
     * — one operand is a pointer to an object or incomplete type and the other is a pointer to
     * a qualified or unqualified version of void.
     */
    Token ty1, ty2, ty3;

    /*
     * Check that the first operand has scalar type.
     */
    ty1 = get_type_category(&e->child[0]->type);
    if (!is_scalar(ty1) && ty1!=TOK_SUBSCRIPT && ty1!=TOK_FUNCTION)
        ERROR(e, "invalid first operand for conditional operator");

    /*
     * Check that the second and third operands can
     * be brought to a common type for the result.
     */
    ty2 = get_type_category(&e->child[1]->type);
    ty3 = get_type_category(&e->child[2]->type);
    if (is_integer(ty2)) {
        if (is_integer(ty3)) {
            e->type.decl_specs = get_type_node(get_result_type(get_promoted_type(ty2), get_promoted_type(ty3)));
        } else if (is_pointer(ty3) || ty3==TOK_FUNCTION) {
            /*
             * Set the type of the pointer operand as the type of the result.
             */
            e->type = e->child[2]->type;
            if (e->child[1]->kind.exp!=IConstExp || e->child[1]->attr.val!=0)
                WARNING(e, "pointer/integer type mismatch in conditional expression");
        } else {
            goto type_mismatch;
        }
    } else if (ty2==TOK_STRUCT || ty2==TOK_UNION) {
        TypeExp *ts2, *ts3;

        if (ty3 != ty2)
            goto type_mismatch;

        ts2 = get_type_spec(e->child[1]->type.decl_specs);
        ts3 = get_type_spec(e->child[2]->type.decl_specs);
        if (ts2->str != ts3->str)
            goto type_mismatch;
        e->type = e->child[1]->type;
    } else if (is_pointer(ty2) || ty2==TOK_FUNCTION) {
        if (is_integer(ty3)) {
            e->type = e->child[1]->type;
            if (e->child[2]->kind.exp!=IConstExp || e->child[2]->attr.val!=0)
                WARNING(e, "pointer/integer type mismatch in conditional expression");
        } else if (is_pointer(ty3) || ty3==TOK_FUNCTION) {
            // TODO: form the result type as indicated in 6.5.15#6
            e->type = e->child[1]->type; /* for now, just set result type==2nd op type */
        } else {
            goto type_mismatch;
        }
    } else if (ty2 == TOK_VOID) {
        if (ty3 != TOK_VOID)
            goto type_mismatch;
        e->type = e->child[1]->type;
    }

    printf("conditional: %s\n", stringify_type_exp(&e->type));
    return;
type_mismatch:
    ERROR(e, "type mismatch in conditional expression (`%s' and `%s')",
    stringify_type_exp(&e->child[1]->type), stringify_type_exp(&e->child[2]->type));
}

void analyze_logical_operator(ExecNode *e)
{
    Token ty1, ty2;

    /*
     * 6.5.13/14
     * #2 Each of the operands shall have scalar type.
     */
    ty1 = get_type_category(&e->child[0]->type);
    ty2 = get_type_category(&e->child[1]->type);
    if (!is_scalar(ty1) && ty1!=TOK_SUBSCRIPT && ty1!=TOK_FUNCTION
    || !is_scalar(ty2) && ty2!=TOK_SUBSCRIPT && ty2!=TOK_FUNCTION)
        binary_op_error(e);

    /* the result has type int */
    e->type.decl_specs = get_type_node(TOK_INT);
}

void analyze_relational_equality_expression(ExecNode *e)
{
#define is_eq_op(op) ((op)==TOK_EQ||(op)==TOK_NEQ)
    /*
     * 6.5.8 Relational operators
     * #2 One of the following shall hold:
     * — both operands have real type;
     * — both operands are pointers to qualified or unqualified versions of compatible object
     * types; or
     * — both operands are pointers to qualified or unqualified versions of compatible
     * incomplete types.
     */

     /*
      * 6.5.9 Equality operators
      * One of the following shall hold:
      * — both operands have arithmetic type;
      * — both operands are pointers to qualified or unqualified versions of compatible types;
      * — one operand is a pointer to an object or incomplete type and the other is a pointer to a
      * qualified or unqualified version of void; or
      * — one operand is a pointer and the other is a null pointer constant.
      */
    Token ty1, ty2;

    ty1 = get_type_category(&e->child[0]->type);
    ty2 = get_type_category(&e->child[1]->type);
    if (is_integer(ty1)) {
        if (is_integer(ty2)) {
            ; /* OK */
        } else if (is_pointer(ty2) || ty2==TOK_FUNCTION) {
            if (!is_eq_op(e->attr.op) || e->child[0]->kind.exp!=IConstExp || e->child[0]->attr.val!=0)
                WARNING(e, "comparison between pointer and integer");
        } else {
            binary_op_error(e);
        }
    } else if (is_pointer(ty1) || ty1==TOK_FUNCTION) {
        if (is_integer(ty2)) {
            if (!is_eq_op(e->attr.op) || e->child[1]->kind.exp!=IConstExp || e->child[1]->attr.val!=0)
                WARNING(e, "comparison between pointer and integer");
        } else if (is_pointer(ty2) || ty2==TOK_FUNCTION) {
            TypeExp *p1, *p2;

            /*
             * Check for the case where one of the operands
             * (or both) is `void *' (only for ==/!=)
             */
            if (is_eq_op(e->attr.op)) {
                if (ty1!=TOK_FUNCTION && e->child[0]->type.idl->child==NULL
                && get_type_spec(e->child[0]->type.decl_specs)->op==TOK_VOID) {
                    /* the left operand is a void pointer */
                    if (ty2==TOK_FUNCTION
                    || e->child[1]->type.idl->child!=NULL&&e->child[1]->type.idl->child->op==TOK_FUNCTION)
                        WARNING(e, "comparison of `void *' with function pointer");

                    goto done;
                } else if (ty2!=TOK_FUNCTION && e->child[1]->type.idl->child==NULL
                && get_type_spec(e->child[1]->type.decl_specs)->op==TOK_VOID) {
                    /* the right operand is a void pointer */
                    if (ty1==TOK_FUNCTION
                    || e->child[0]->type.idl->child!=NULL&&e->child[0]->type.idl->child->op==TOK_FUNCTION)
                        WARNING(e, "comparison of `void *' with function pointer");

                    goto done;
                }
            }

            p1 = (ty1!=TOK_FUNCTION)?e->child[0]->type.idl->child:e->child[0]->type.idl;
            p2 = (ty2!=TOK_FUNCTION)?e->child[1]->type.idl->child:e->child[1]->type.idl;

            if (!are_compatible(e->child[0]->type.decl_specs, p1, e->child[1]->type.decl_specs, p2, FALSE, FALSE))
                WARNING(e, "comparison of distinct pointer types");
            else if (!is_eq_op(e->attr.op) && p1!=NULL && p1->op==TOK_FUNCTION)
                WARNING(e, "comparison of function pointers");
        } else {
            binary_op_error(e);
        }
    } else {
        binary_op_error(e);
    }

done:
    /* the result has type int */
    e->type.decl_specs = get_type_node(TOK_INT);
}

void analyze_bitwise_operator(ExecNode *e)
{
    /*
     * These operators are required to have operands that have integer type.
     * The integer promotions are performed on each of the operands.
     */
    Token ty1, ty2;

    ty1 = get_type_category(&e->child[0]->type);
    ty2 = get_type_category(&e->child[1]->type);
    if (!is_integer(ty1) || !is_integer(ty2))
        binary_op_error(e);

    if (e->attr.op==TOK_LSHIFT || e->attr.op==TOK_RSHIFT)
        /*
         * The usual arithmetic conversions do not apply to <</>>.
         * The type of the result is that of the promoted left operand.
         */
        e->type.decl_specs = get_type_node(get_promoted_type(ty1));
    else
        e->type.decl_specs = get_type_node(get_result_type(get_promoted_type(ty1), get_promoted_type(ty2)));

    printf("bitwise: %s\n", stringify_type_exp(&e->type));
}

void analyze_additive_expression(ExecNode *e)
{
    Token ty_l, ty_r;

    ty_l = get_type_category(&e->child[0]->type);
    ty_r = get_type_category(&e->child[1]->type);

    /*
     * 6.5.6
     * #8 When an expression that has integer type is added to or subtracted from a pointer, the
     * result has the type of the pointer operand.
     */

    if (e->attr.op == TOK_PLUS) {
        /*
         * 6.5.6
         * #2 For addition, either both operands shall have arithmetic type, or one operand shall be a
         * pointer to an object type and the other shall have integer type. (Incrementing is
         * equivalent to adding 1.)
         */
        if (is_integer(ty_l)) {
            if (is_integer(ty_r)) {
                /* integer + integer */
                e->type.decl_specs = get_type_node(get_result_type(get_promoted_type(ty_l), get_promoted_type(ty_r)));
            } else if (is_pointer(ty_r)) {
                /* integer + pointer */
                if (!is_ptr2obj(&e->child[1]->type))
                    binary_op_error(e);
                e->type = e->child[1]->type;
            } else {
                binary_op_error(e);
            }
        } else if (is_pointer(ty_l)) {
            if (!is_integer(ty_r) || !is_ptr2obj(&e->child[0]->type))
                binary_op_error(e);
            /* pointer + integer */
            e->type = e->child[0]->type;
        } else {
            binary_op_error(e);
        }
    } else {
        /*
         * 6.5.6
         * #3 For subtraction, one of the following shall hold:
         * — both operands have arithmetic type;
         * — both operands are pointers to qualified or unqualified versions of compatible object
         * types; or
         * — the left operand is a pointer to an object type and the right operand has integer type.
         * (Decrementing is equivalent to subtracting 1.)
         */
        if (is_integer(ty_l)) {
            if (is_integer(ty_r))
                /* integer - integer */
                e->type.decl_specs = get_type_node(get_result_type(get_promoted_type(ty_l), get_promoted_type(ty_r)));
            else
                binary_op_error(e);
        } else if (is_pointer(ty_l)) {
            if (is_integer(ty_r)) {
                /* pointer - integer */
                if (!is_ptr2obj(&e->child[0]->type))
                    binary_op_error(e);
                e->type = e->child[0]->type;
            } else if (is_pointer(ty_r)) {
                /* pointer - pointer */
                if (!is_ptr2obj(&e->child[0]->type) || !is_ptr2obj(&e->child[1]->type)
                || !are_compatible(e->child[0]->type.decl_specs, e->child[0]->type.idl->child,
                e->child[1]->type.decl_specs, e->child[1]->type.idl->child, FALSE, FALSE))
                    binary_op_error(e);
                e->type.decl_specs = get_type_node(TOK_LONG); /* ptrdiff_t */
            } else {
                binary_op_error(e);
            }
        } else {
            binary_op_error(e);
        }
    }

    printf("add/sub: %s\n", stringify_type_exp(&e->type));
}

void analyze_multiplicative_expression(ExecNode *e)
{
    Token ty1, ty2;

    /*
     * 6.5.5
     * #2 Each of the operands shall have arithmetic type. The operands of the % operator shall
     * have integer type.
     */
    ty1 = get_type_category(&e->child[0]->type);
    ty2 = get_type_category(&e->child[1]->type);

    if (!is_integer(ty1) || !is_integer(ty2))
        binary_op_error(e);

    e->type.decl_specs = get_type_node(get_result_type(get_promoted_type(ty1), get_promoted_type(ty2)));

    printf("mul: %s\n", stringify_type_exp(&e->type));
}

void analyze_cast_expression(ExecNode *e)
{
    Token ty_src, ty_tgt;

    /*
     * 6.5.4
     * #2 Unless the type name specifies a void type, the type name shall specify qualified
     * or unqualified scalar type and the operand shall have scalar type.
     */
    /* source type */
    ty_src = get_type_category(&e->child[0]->type);
    if (!is_scalar(ty_src) && ty_src!=TOK_SUBSCRIPT
    && ty_src!=TOK_FUNCTION && ty_src!=TOK_VOID)
        ERROR(e, "cast operand does not have scalar type");

    /* target type */
    ty_tgt = get_type_category((Declaration *)e->child[1]);
    if (!is_scalar(ty_tgt) && ty_tgt!=TOK_VOID)
        ERROR(e, "cast specifies conversion to non-scalar type");

    /* check for void ==> non-void */
    if (ty_src==TOK_VOID && ty_tgt!=TOK_VOID)
        ERROR(e, "invalid cast of void expression to non-void type");

    e->type = *(Declaration *)e->child[1];

    printf("(%s)\n", stringify_type_exp(&e->type));
}

static
void analyze_inc_dec_operator(ExecNode *e)
{
    /*
     * 6.5.2.4#1/6.5.3.1#1
     * The operand of the postfix/prefix increment or decrement operator shall have qualified or
     * unqualified real or pointer type and shall be a modifiable lvalue.
     */
    Token ty;

    ty = get_type_category(&e->child[0]->type);
    if (!is_integer(ty) && !is_pointer(ty))
        ERROR(e, "wrong type argument to increment");
    if (!is_modif_lvalue(e->child[0]))
        ERROR(e, "expression is not modifiable");

    e->type = e->child[0]->type;
}

void analyze_unary_expression(ExecNode *e)
{
    switch (e->attr.op) {
    case TOK_PRE_INC:
    case TOK_PRE_DEC:
        analyze_inc_dec_operator(e);
        break;
    case TOK_SIZEOF: {
        Token ty;
        TypeExp *ts, *dct;

        /*
         * 6.5.3.4
         * #1 The sizeof operator shall not be applied to an expression that has function type or an
         * incomplete type, to the parenthesized name of such a type, or to an expression that
         * designates a bit-field member.
         */
        if (e->child[1] != NULL) {
            /* "sizeof" "(" type_name ")" */
            ts = get_type_spec(((Declaration *)e->child[1])->decl_specs);
            dct = ((Declaration *)e->child[1])->idl;
            ty = get_type_category((Declaration *)e->child[1]);
        } else {
            /* "sizeof" unary_expression */
            ts = get_type_spec(e->child[0]->type.decl_specs);
            dct = e->child[0]->type.idl;
            ty = get_type_category(&e->child[0]->type);
        }

        if (ty == TOK_FUNCTION)
            ERROR(e, "invalid application of `sizeof' to a function type");
        else if (ty==TOK_SUBSCRIPT && dct->attr.e==NULL
        || is_struct_union_enum(ty) && !is_complete(ts->str))
            ERROR(e, "invalid application of `sizeof' to incomplete type");

        /*
         * #2 The sizeof operator yields the size (in bytes) of its operand, which may be an
         * expression or the parenthesized name of a type. The size is determined from the type of
         * the operand. The result is an integer. If the type of the operand is a variable length array
         * type, the operand is evaluated; otherwise, the operand is not evaluated and the result is an
         * integer constant.
         * #3 When applied to an operand that has type char, unsigned char, or signed char,
         * (or a qualified version thereof) the result is 1. When applied to an operand that has array
         * type, the result is the total number of bytes in the array. When applied to an operand
         * that has structure or union type, the result is the total number of bytes in such an object,
         * including internal and trailing padding.
         */
        /* >> calculate value here << */

        e->type.decl_specs = get_type_node(TOK_UNSIGNED);
        break;
    }
    case TOK_ADDRESS_OF: {
        TypeExp *temp;

        /*
         * 6.5.3.2
         * 1# The operand of the unary & operator shall be either a function designator, the result of a
         * [] or unary * operator, or an lvalue that designates an object that is not a bit-field and is
         * not declared with the register storage-class specifier.
         */
        if (!is_lvalue(e->child[0]) && get_type_category(&e->child[0]->type)!=TOK_FUNCTION)
            ERROR(e, "invalid operand to &");
        if ((temp=get_sto_class_spec(e->child[0]->type.decl_specs))!=NULL && temp->op==TOK_REGISTER)
            ERROR(e, "address of register variable requested");

        /*
         * 6.5.3.2
         * #3 The unary & operator yields the address of its operand. If the operand has type ‘‘type’’,
         * the result has type ‘‘pointer to type’’.
         */
        temp = calloc(1, sizeof(TypeExp));
        temp->op = TOK_STAR;
        temp->child = e->child[0]->type.idl;

        /* set the type of the & node */
        e->type.decl_specs = e->child[0]->type.decl_specs;
        e->type.idl = temp;

        printf("& result type: %s\n", stringify_type_exp(&e->type));
        break;
    }
    case TOK_INDIRECTION: {
        Token ty;

        /*
         * 6.5.3.2
         * #2 The operand of the unary * operator shall have pointer type.
         */
        ty = get_type_category(&e->child[0]->type);
        if (!is_pointer(ty) && ty!=TOK_FUNCTION)
            ERROR(e, "invalid operand to *");

        /* make sure that the pointer does not point to an incomplete struct/union/enum */
        if (ty != TOK_FUNCTION) {
            TypeExp *ts;

            ts = get_type_spec(e->child[0]->type.decl_specs);
            if (is_struct_union_enum(ts->op) && !is_complete(ts->str))
                ERROR(e, "dereferencing pointer to incomplete type");
        }

        /*
         * 6.5.3.2
         * #4 The unary * operator denotes indirection. If the operand points to a function, the result is
         * a function designator; if it points to an object, the result is an lvalue designating the
         * object. If the operand has type ‘‘pointer to type’’, the result has type ‘‘type’’. If an
         * invalid value has been assigned to the pointer, the behavior of the unary * operator is
         * undefined.
         */
        e->type.decl_specs = e->child[0]->type.decl_specs;
        e->type.idl = (ty!=TOK_FUNCTION)?e->child[0]->type.idl->child:e->child[0]->type.idl;

        printf("* result type: %s\n", stringify_type_exp(&e->type));
        break;
    }
    case TOK_UNARY_PLUS:
    case TOK_UNARY_MINUS:
    case TOK_COMPLEMENT: {
        Token ty;

        /*
         * 6.5.3.3
         * #1 The operand of the unary + or - operator shall have arithmetic type;
         * of the ~ operator, integer type.
         */
        ty = get_type_category(&e->child[0]->type);
        if (!is_integer(ty))
            ERROR(e, "invalid operand to %s", token_table[e->attr.op*2+1]);

        /*if ((prom_ty=get_promoted_type(ty)) != ty) {
            TypeExp *ts;

            e->type.decl_specs = dup_decl_specs_list(e->child[0]->type.decl_specs);
            ts = get_type_spec(e->type.decl_specs);
            ts->op = prom_ty;
        } else {
            e->type.decl_specs = e->child[0]->type.decl_specs;
        }
        e->type.idl = e->child[0]->type.idl;*/
        e->type.decl_specs = get_type_node(get_promoted_type(ty));

        printf("+- result type: %s\n", stringify_type_exp(&e->type));
        break;
    }
    case TOK_NEGATION: {
        Token ty;

        /*
         * 6.5.3.3
         * #1 The operand of the unary ! operator shall have scalar type.
         */
        ty = get_type_category(&e->child[0]->type);
        if (!is_scalar(ty) && ty!=TOK_FUNCTION && ty!=TOK_SUBSCRIPT)
            ERROR(e, "invalid operand to !");

        /* the result has type int */
        e->type.decl_specs = get_type_node(TOK_INT);
        break;
    }
    }
}

void analyze_postfix_expression(ExecNode *e)
{
    switch (e->attr.op) {
    case TOK_SUBSCRIPT: {
        /*
         * 6.5.2.1#1
         * One of the expressions shall have type ‘‘pointer to object type’’, the other expression
         * shall have integer type, and the result has type ‘‘type’’.
         */
        int ch_idx; /* index of the pointer child */
        Token ty1, ty2;
        TypeExp *ptr_operand;

        ty1 = get_type_category(&e->child[0]->type);
        ty2 = get_type_category(&e->child[1]->type);

        if (is_pointer(ty1)) {
            if (!is_integer(ty2))
                goto non_int_sub;
            ptr_operand = e->child[0]->type.idl;
            ch_idx = 0;
        } else if (is_pointer(ty2)) {
            if (!is_integer(ty1))
                goto non_int_sub;
            ptr_operand = e->child[1]->type.idl;
            ch_idx = 1;
        } else {
            ERROR(e, "subscripted value is neither array nor pointer");
        }

        /*
         * Check that the pointer points to an object
         * type (and not to a function or incomplete type).
         */
        if (ptr_operand->child == NULL) {
            /* the pointed to type is not a derived declarator type */
            TypeExp *ts;

            ts = get_type_spec(e->child[ch_idx]->type.decl_specs);
            if (is_struct_union_enum(ts->op) && !is_complete(ts->str))
                goto subs_incomp;
        } else if (ptr_operand->child->op==TOK_SUBSCRIPT && ptr_operand->child->attr.e==NULL) {
            goto subs_incomp;
        } else if (ptr_operand->child->op == TOK_FUNCTION) {
            ERROR(e, "subscripting pointer to function");
        }

        /* set as the type of the [] node the element type */
        if (is_pointer(ty1)) {
            e->type.decl_specs = e->child[ch_idx]->type.decl_specs;
            e->type.idl = e->child[ch_idx]->type.idl->child;
        }
        break;
non_int_sub:
        ERROR(e, "array subscript is not an integer");
subs_incomp:
        ERROR(e, "subscripting pointer to incomplete type");
    }
    case TOK_FUNCTION: {
        /*
         * 6.5.2.2
         * #1 The expression that denotes the called function shall have type pointer to function
         * returning void or returning an object type other than an array type.
         */
        /*
         * 6.5.2.2
         * #5 If the expression that denotes the called function has type pointer to function returning an
         * object type, the function call expression has the same type as that object type, and has the
         * value determined as specified in 6.8.6.4. Otherwise, the function call has type void. [...]
         * 6.7.2.3#footnote
         * [...] The specification has to be complete before such a function is called or defined.
         */
        int n;
        DeclList *p;
        ExecNode *a;
        TypeExp *ty;

        ty = e->child[0]->type.idl;

        if (ty == NULL)
            goto non_callable;
        else if (ty->op == TOK_FUNCTION)
            ;
        else if (ty->op==TOK_STAR && ty->child!=NULL && ty->child->op==TOK_FUNCTION)
            ty = ty->child;
        else
            goto non_callable;
        /*
         * Functions cannot be declared as returning an array or function, so what remains
         * to check is that the return type is not an incomplete enum/struct/union type.
         */
        if (ty->child == NULL) {
            /* the return type is not a derived declarator type */
            TypeExp *ts;

            ts = get_type_spec(e->child[0]->type.decl_specs);
            if (is_struct_union_enum(ts->op) && !is_complete(ts->str))
                ERROR(e, "calling function with incomplete return type `%s %s'", token_table[ts->op*2+1], ts->str);
        }

        /*
         * 6.5.2.2
         * #2 If the expression that denotes the called function has a type that includes a prototype, the
         * number of arguments shall agree with the number of parameters. Each argument shall
         * have a type such that its value may be assigned to an object with the unqualified version
         * of the type of its corresponding parameter.
         *
         * #7 If the expression that denotes the called function has a type that does include a prototype,
         * the arguments are implicitly converted, as if by assignment, to the types of the
         * corresponding parameters, taking the type of each parameter to be the unqualified version
         * of its declared type. The ellipsis notation in a function prototype declarator causes
         * argument type conversion to stop after the last declared parameter. The default argument
         * promotions are performed on trailing arguments.
         */
        n = 1;
        p = ty->attr.dl;
        if (get_type_spec(p->decl->decl_specs)->op==TOK_VOID && p->decl->idl==NULL)
            p = NULL;
        a = e->child[1];
        while (p!=NULL && a!=NULL) {
            if (p->decl->idl!=NULL && p->decl->idl->op==TOK_ELLIPSIS)
                break;
            // if (!can_assign_to(a, p->decl, &a->type))
            if (!can_assign_to(p->decl, a))
                ERROR(a, "parameter/argument type mismatch (parameter #%d; expected `%s', "
                "given `%s')", n, stringify_type_exp(p->decl),
                stringify_type_exp(&a->type));

            ++n;
            p = p->next;
            a = a->sibling;
        }
        if (a!=NULL || p!=NULL) {
            if (p!=NULL && p->decl->idl!=NULL && p->decl->idl->op==TOK_ELLIPSIS)
                ; /* OK */
            else
                ERROR(e, "parameter/argument number mismatch");
        }

        /* set as the type of the node () the return type of the function */
        e->type.decl_specs = e->child[0]->type.decl_specs;
        e->type.idl = ty->child;
        break;
non_callable:
        ERROR(e, "called object is not a function");
    }
    case TOK_DOT:
    case TOK_ARROW: {
        char *id;
        DeclList *d;
        TypeExp *ts, *tq_l, *tq_r, *dct;

        /*
         * 6.5.2.3
         * #1 The first operand of the . operator shall have a qualified or unqualified structure
         * or union type, and the second operand shall name a member of that type.
         *
         * #2 The first operand of the -> operator shall have type ‘‘pointer to qualified or unqualified
         * structure’’ or ‘‘pointer to qualified or unqualified union’’, and the second operand shall
         * name a member of the type pointed to.
         */
        ts = get_type_spec(e->child[0]->type.decl_specs);

        if (ts->op!=TOK_STRUCT && ts->op!=TOK_UNION)
            ERROR(e, "left operand of %s has neither structure nor union type", token_table[e->attr.op*2+1]);
        if (e->attr.op == TOK_DOT) {
            if (e->child[0]->type.idl != NULL)
                ERROR(e, "invalid operand to .");
        } else {
            if (e->child[0]->type.idl==NULL || !is_pointer(e->child[0]->type.idl->op))
                ERROR(e, "invalid operand to ->");
        }

        /* fetch the name of the requested member */
        id = e->child[1]->attr.str;

        if (ts->str != NULL) {
            /* non-anonymous struct/union */
            if (ts->attr.dl == NULL) {
                TypeTag *np;

                if ((np=lookup_tag(ts->str, TRUE))->type->attr.dl == NULL)
                    ERROR(e, "left operand of . has incomplete type");
                ts = np->type;
            }
        }

        /* search for the member */
        for (d = ts->attr.dl; d != NULL; d = d->next) {
            for (dct = d->decl->idl; dct != NULL; dct = dct->sibling) {
                if (strcmp(id, dct->str) == 0)
                    goto mem_found;
            }
        }
        ERROR(e, "`%s %s' has no member named `%s'", token_table[ts->op*2+1],
        (ts->str==NULL)?"<anonymous>":ts->str, id);
mem_found:
        /*
         * 6.5.2.3
         * #3 A postfix expression followed by the . operator and an identifier designates a member of
         * a structure or union object. The value is that of the named member, and is an lvalue if
         * the first expression is an lvalue. If the first expression has qualified type, the result has
         * the so-qualified version of the type of the designated member.
         *
         * #4 A postfix expression followed by the -> operator and an identifier designates a member
         * of a structure or union object. The value is that of the named member of the object to
         * which the first expression points, and is an lvalue. If the first expression is a pointer to
         * a qualified type, the result has the so-qualified version of the type of the designated
         * member.
         */
        if ((tq_l=get_type_qual(e->child[0]->type.decl_specs)) != NULL) {
            /*
             * The first expression has qualified type.
             */
            if (dct->child != NULL) {
                /* derived declarator type (struct members cannot
                   have function type, so ignore that case) */
                if (dct->child->op == TOK_STAR) {
                    TypeExp *new_ptr_node;

                    new_ptr_node = calloc(1, sizeof(TypeExp));
                    *new_ptr_node = *dct->child;

                    if (dct->child->attr.el == NULL) {
                        /* non-qualified pointer */
                        new_ptr_node->attr.el = tq_l;
                    } else if (dct->child->attr.el->op!=tq_l->op
                    && dct->child->attr.el->op!=TOK_CONST_VOLATILE) {
                        /* qualified pointer (by const or volatile, but not both) */
                        new_ptr_node->attr.el = calloc(1, sizeof(TypeExp));
                        new_ptr_node->attr.el->op = TOK_CONST_VOLATILE;
                    } /*else {
                        free(new_ptr_node);
                        new_ptr_node = dct->child;
                    }*/
                    e->type.idl = new_ptr_node;
                } else if (dct->child->op == TOK_SUBSCRIPT) {
                    int n;
                    TypeExp *p;

                    /* search the element type */
                    for (p=dct->child, n=0; p!=NULL && p->op==TOK_SUBSCRIPT; p=p->child, n++);
                    if (p != NULL) {
                        /* array of pointers, qualify the pointer element type */
                        TypeExp *new_dct_list;

                        new_dct_list = dup_declarator(dct->child);

                        if (p->attr.el == NULL) {
                            /* non-qualified pointer */
                            for (p = new_dct_list; n != 0; p=p->child, --n);
                            p->attr.el = tq_l;
                        } else if (p->attr.el->op!=tq_l->op && p->attr.el->op!=TOK_CONST_VOLATILE) {
                            /* qualified pointer (by const or volatile, but not both) */
                            for (p = new_dct_list; n != 0; p=p->child, --n);
                            p->attr.el = calloc(1, sizeof(TypeExp));
                            p->attr.el->op = TOK_CONST_VOLATILE;
                        } /*else {
                        }*/
                        e->type.idl = new_dct_list;
                    } else {
                        /* array of non-derived declarator types, just add a new
                           qualifier (if required) to the declaration specifiers list */
                        goto decl_specs_qualif;
                    }
                }
                e->type.decl_specs = d->decl->decl_specs;
            } else {
decl_specs_qualif:
                if ((tq_r=get_type_qual(d->decl->decl_specs)) != NULL) {
                    /* the member is already qualified */
                    if (tq_r->op!=tq_l->op && tq_r->op!=TOK_CONST_VOLATILE) {
                        tq_r = calloc(1, sizeof(TypeExp));
                        tq_r->op = TOK_CONST_VOLATILE;
                        tq_r->child = calloc(1, sizeof(TypeExp));
                        *tq_r->child = *get_type_spec(d->decl->decl_specs);
                        tq_r->child->child = NULL;
                    }
                } else {
                    /* there is not type qualifier between
                       the member's declaration specifiers */
                    tq_r = calloc(1, sizeof(TypeExp));
                    tq_r->op = tq_l->op;
                    tq_r->child = d->decl->decl_specs;
                }
                e->type.decl_specs = tq_r;
                e->type.idl = dct->child;
            }
        } else {
            /*
             * The first expression has unqualified type.
             */
            e->type.decl_specs = d->decl->decl_specs;
            e->type.idl = dct->child;
        }
        // printf("%s: %s\n", id, stringify_type_exp(&e->type));
        break;
    }
    case TOK_POS_INC:
    case TOK_POS_DEC: {
        analyze_inc_dec_operator(e);
        break;
    }
    }
}

void analyze_primary_expression(ExecNode *e)
{
    switch (e->kind.exp) {
    case IdExp:
        if (e->type.idl!=NULL && e->type.idl->op==TOK_ENUM_CONST) {
            e->kind.exp = IConstExp;
            e->attr.val = e->type.idl->attr.e->attr.val;
            /* >>free<< */
            e->type.idl = NULL;
        }
        break;
    case IConstExp: {
        /*
         * This code relies on the equality sizeof(int)==sizeof(long).
         */
        int len;
        char *ep, *ic;

        ic = e->attr.str;

        /* check for unsigned suffix */
        len = strlen(ic);
        if (len>1 && (tolower(ic[len-1])=='u'||tolower(ic[len-2])=='u'))
            goto unsigned_ty;

        /* try int/long */
        errno = 0;
        e->attr.val = strtol(ic, &ep, 0);
        if (errno == ERANGE)
            goto unsigned_ty;
        e->type.decl_specs = get_type_node(TOK_INT);
        break;

unsigned_ty:
        /* try unsigned/unsigned long */
        errno = 0;
        e->attr.uval = strtoul(ic, &ep, 0);
        if (errno == ERANGE)
            /* strtoul() saturates the result, that is, we end up with 0xFFFFFFFF */
            WARNING(e, "integer constant is too large for `unsigned long' type");
        e->type.decl_specs = get_type_node(TOK_UNSIGNED);
        break;
    }
    case StrLitExp: {
        static TypeExp lit_dct = { TOK_SUBSCRIPT };

        e->type.decl_specs = get_type_node(TOK_CHAR);
        e->type.idl = &lit_dct;
        break;
    }
    }
}

unsigned compute_sizeof(Declaration *ty)
{
    Token cat;
    unsigned size;
    Declaration new_ty;

    size = 0;

    cat = get_type_category(ty);
    switch (cat) {
    case TOK_STRUCT:
    case TOK_UNION: {
        TypeExp *ts;
        DeclList *d;

        /* fetch declaration list */
        ts = get_type_spec(ty->decl_specs);
        if (ts->attr.dl == NULL)
            ts = lookup_tag(ts->str, TRUE)->type;
        d = ts->attr.dl;

        for (; d != NULL; d = d->next) {
            TypeExp *dct;

            for (dct = d->decl->idl; dct != NULL; dct = dct->sibling) {
                new_ty.decl_specs = d->decl->decl_specs;
                new_ty.idl = dct->child;
                if (cat == TOK_STRUCT) {
                    size += compute_sizeof(&new_ty);
                } else {
                    unsigned new_size;

                    new_size = compute_sizeof(&new_ty);
                    size = (new_size>size)?new_size:size;
                }
            }
        }
        break;
    }
    case TOK_SUBSCRIPT:
        new_ty.decl_specs = ty->decl_specs;
        new_ty.idl = ty->idl->child;
        size = ty->idl->attr.e->attr.val * compute_sizeof(&new_ty);
        break;
    case TOK_STAR:
        size = 4;
        break;
    case TOK_ENUM:
    case TOK_LONG: case TOK_UNSIGNED_LONG:
    case TOK_INT: case TOK_UNSIGNED:
        size = 4;
        break;
    case TOK_SHORT: case TOK_UNSIGNED_SHORT:
        size = 2;
        break;
    case TOK_CHAR: case TOK_SIGNED_CHAR: case TOK_UNSIGNED_CHAR:
        size = 1;
        break;
    }

    return size;
}

long eval_const_expr(ExecNode *e, int is_addr)
{
    /*
     * This function is a work in progress. It handles
     * basic stuff just fine, but may not work properly
     * with expressions involving pointers and so on.
     */

    switch (e->kind.exp) {
    case OpExp:
        switch (e->attr.op) {
        /*
         * Allow expressions like
         *  &arr[5]; // if 'arr' has static storage duration
         * and
         *  &s.x; and &s.x[5]; // if 's' has static storage duration
         */
        case TOK_SUBSCRIPT:
            /* []'s operand has to be a real array (and not a pointer) */
            if (e->child[0]->type.idl==NULL || e->child[0]->type.idl->op!=TOK_SUBSCRIPT)
                break;
        case TOK_DOT:
            if (!is_addr)
                break;
            return eval_const_expr(e->child[0], is_addr);

        case TOK_SIZEOF:
            if (e->child[1] != NULL)
                return compute_sizeof((Declaration *)e->child[1]);
            else
                return compute_sizeof(&e->child[0]->type);
        case TOK_ADDRESS_OF:
            return eval_const_expr(e->child[0], TRUE);
        case TOK_ARROW:
        case TOK_INDIRECTION:
            break;
        case TOK_UNARY_PLUS:
            return +eval_const_expr(e->child[0], FALSE);
        case TOK_UNARY_MINUS:
            return -eval_const_expr(e->child[0], FALSE);
        case TOK_COMPLEMENT:
            return ~eval_const_expr(e->child[0], FALSE);
        case TOK_NEGATION:
            return !eval_const_expr(e->child[0], FALSE);

        case TOK_CAST: {
            Token ty;

            ty = get_type_category((Declaration *)e->child[1]);
            switch (ty) {
            case TOK_SHORT:
                return (short)eval_const_expr(e->child[0], FALSE);
            case TOK_UNSIGNED_SHORT:
                return (unsigned short)eval_const_expr(e->child[0], FALSE);
            case TOK_CHAR:
            case TOK_SIGNED_CHAR:
                return (char)eval_const_expr(e->child[0], FALSE);
            case TOK_UNSIGNED_CHAR:
                return (unsigned char)eval_const_expr(e->child[0], FALSE);
            }
            return eval_const_expr(e->child[0], FALSE);
        }

#define L eval_const_expr(e->child[0], FALSE)
#define R eval_const_expr(e->child[1], FALSE)
        case TOK_MUL:
            return L * R;
        case TOK_DIV:
            return L / R;
        case TOK_MOD:
            return L % R;
        case TOK_PLUS:
            // escalate
            /*if (is_pointer(get_type_category(&e->child[0]->type)))
                return ;
            else if (is_pointer(get_type_category(&e->child[0]->type)))
                return ;
            else
                return L + R;*/
            return L + R;
        case TOK_MINUS:
            if (!is_integer(get_type_category(&e->child[1]->type)))
                break;
            return L - R;
        case TOK_LSHIFT:
            return L << R;
        case TOK_RSHIFT:
            if (is_signed_int(get_type_spec(e->child[0]->type.decl_specs)->op))
                return L >> R;
            else
                return (unsigned)L >> R;
        case TOK_LT:
            return L < R;
        case TOK_GT:
            return L > R;
        case TOK_LET:
            return L <= R;
        case TOK_GET:
            return L >= R;
        case TOK_EQ:
            return L == R;
        case TOK_NEQ:
            return L != R;
        case TOK_BW_AND:
            return L & R;
        case TOK_BW_XOR:
            return L ^ R;
        case TOK_BW_OR:
            return L | R;
        case TOK_AND:
            return L && R;
        case TOK_OR:
            return L || R;
        case TOK_CONDITIONAL:
            if (eval_const_expr(e->child[0], FALSE))
                return eval_const_expr(e->child[1], FALSE);
            else
                return eval_const_expr(e->child[2], FALSE);
        }
        break;
    case IConstExp:
        return e->attr.val;
    case StrLitExp:
        return 0; /* OK */
    case IdExp: {
        /*
         * An identifier can only appears in a constant expression
         * if its address is being computed or the address of one
         * of its elements (arrays) or members (unions/structs) is.
         * being computed. The address can be computed implicitly
         * if the identifier denotes an array or function designator.
         */
        if (!is_addr
        && (e->type.idl==NULL||e->type.idl->op!=TOK_FUNCTION&&e->type.idl->op!=TOK_SUBSCRIPT))
            break;

        /*
         * Moreover, the identifier must have static storage
         * duration (it was declared at file scope or has one
         * of the storage class specifiers extern or static).
         */
        if (!is_external_id(e->attr.str)) {
            TypeExp *scs;

            scs = get_sto_class_spec(e->type.decl_specs);
            if (scs==NULL || scs->op!=TOK_STATIC&&scs->op!=TOK_EXTERN)
                break;
        }

        return 0; /* OK */
    }
    }

    ERROR(e, "invalid constant expression");
}

/*
 * Currently only fully bracketed initialization is handled.
 */
void analyze_initializer(TypeExp *ds, TypeExp *dct, ExecNode *e, int const_expr)
{
    TypeExp *ts;

    if (dct != NULL) {
        int i;

        if (dct->op != TOK_SUBSCRIPT)
            goto scalar; /* must be a pointer, functions cannot have initializer */

        /*
         * Array.
         */
        if (e->kind.exp == StrLitExp) {
            /* see for character array initialized by string literal */
            int size;
            Token ty;

            /* make sure the element type is a character type */
            ty = get_type_spec(ds)->op;
            if (dct->child!=NULL || !is_integer(ty) || get_rank(ty)!=1)
                ERROR(e, "array of inappropriate type initialized from string literal");

            size = strlen(e->attr.str);

            if (dct->attr.e != NULL) {
                /* array with specified bounds */
                if (dct->attr.e->attr.val < size) /* '\0' optionally stored if there is room */
                    WARNING(e, "initializer-string for char array is too long");
            } else {
                /* array with unspecified bounds */

                /* complete the array type */
                dct->attr.e = calloc(1, sizeof(ExecNode));
                dct->attr.e->attr.val = size+1; /* make room for '\0' */
            }
        } else {
            if (e->attr.op != TOK_INIT_LIST)
                ERROR(e, "invalid array initializer");
            e = e->child[0];

            if (dct->attr.e != NULL) {
                /* array with specified bounds */
                i = dct->attr.e->attr.val;
                for (; e!=NULL && i!=0; e=e->sibling, --i)
                    analyze_initializer(ds, dct->child, e, const_expr);

                if (e != NULL)
                    ERROR(e, "excess elements in array initializer");
            } else {
                /* array with unspecified bounds */
                i = 0;
                for (; e != NULL; e = e->sibling, ++i)
                    analyze_initializer(ds, dct->child, e, const_expr);

                /* complete the array type */
                dct->attr.e = calloc(1, sizeof(ExecNode));
                dct->attr.e->attr.val = i;
            }
        }
    } else if ((ts=get_type_spec(ds))->op == TOK_STRUCT) {
        /*
         * Struct.
         */
        DeclList *d;


        /*
         * See if the struct is being initialized by a single
         * expression of compatible type, like
         *  struct A x = y; // valid if y has struct A type
         * or an initializer list
         *  struct A x = { 1, 2 };
         */
        if (e->attr.op != TOK_INIT_LIST)
            goto scalar;

        /* initialized by initializer list */
        e = e->child[0];

        if (ts->attr.dl == NULL)
            ts = lookup_tag(ts->str, TRUE)->type;
        d = ts->attr.dl;
        for (; d != NULL; d = d->next) {
            dct = d->decl->idl;
            for (; e!=NULL && dct!=NULL; e=e->sibling, dct=dct->sibling)
                analyze_initializer(d->decl->decl_specs, dct->child, e, const_expr);

            if (e == NULL)
                break;
        }

        if (e != NULL)
            ERROR(e, "excess elements in struct initializer");
    } else if (ts->op == TOK_UNION) {
        /*
         * Union.
         */

        /* the same as for structs */
        if (e->attr.op != TOK_INIT_LIST)
            goto scalar;

        e = e->child[0];

        if (ts->attr.dl == NULL)
            ts = lookup_tag(ts->str, TRUE)->type;
        /* only the first member of the union can be initialized */
        analyze_initializer(ts->attr.dl->decl->decl_specs, ts->attr.dl->decl->idl->child, e, const_expr);

        if (e->sibling != NULL)
            ERROR(e->sibling, "excess elements in union initializer");
    } else {
        /*
         * Scalar.
         */
        Declaration dest_ty;
scalar:

        if (e->attr.op == TOK_INIT_LIST)
            ERROR(e, "braces around scalar initializer");

        if (const_expr)
            /* make sure the initializer is computable at compile time */
            (void)eval_const_expr(e, FALSE);

        /* the same rules as for simple assignment apply */
        dest_ty.decl_specs = ds;
        dest_ty.idl = dct;
        // if (!can_assign_to(e, &dest_ty, &e->type))
        if (!can_assign_to(&dest_ty, e))
            ERROR(e, "initializing `%s' with an expression of incompatible type `%s'",
            stringify_type_exp(&dest_ty), stringify_type_exp(&e->type));
    }
}
