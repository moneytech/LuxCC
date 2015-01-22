#include "stmt_expr.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#define DEBUG 1
#include "util.h"
#include "decl.h"
#undef ERROR
#define ERROR(tok, ...) fprintf(stderr, "%s:%d:%d: error: ", (tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column),fprintf(stderr, __VA_ARGS__),fprintf(stderr, "\n"),exit(EXIT_FAILURE)
#define WARNING(tok, ...) fprintf(stderr, "%s:%d:%d: warning: ", (tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column),fprintf(stderr, __VA_ARGS__), fprintf(stderr, "\n")


static Token get_type_category(Declaration *d)
{
    if (d->idl != NULL)
        return d->idl->op;
    else
        return get_type_spec(d->decl_specs)->op;
}

static int is_integer(Token ty)
{
    // return (t==TOK_CHAR || t==TOK_INT || t==TOK_LONG/* || t==TOK_ENUM_CONST*/);
    switch (ty) {
    case TOK_LONG: case TOK_UNSIGNED_LONG:
    case TOK_INT: case TOK_UNSIGNED:
    case TOK_SHORT: case TOK_UNSIGNED_SHORT:
    case TOK_CHAR: case TOK_SIGNED_CHAR: case TOK_UNSIGNED_CHAR:
    // case TOK_ENUM: ???
        return TRUE;
    default:
        return FALSE;
    }
}

static int is_pointer(Token op)
{
    return (op==TOK_STAR || op==TOK_SUBSCRIPT);
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
        if (ts->op == TOK_STRUCT)
            modifiable = is_modif_struct_union(ts);

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

/*
#define QUAL_F 0x1
#define STOR_F 0x2
#define TYPE_F 0x4
static TypeExp *dup_decl_specs(unsigned flags)
{
    TypeExp *new_ds;

    new_ds = NULL;
    if (flags & QUAL_F)
        new_ds = calloc(1, sizeof(TypeExp));
        new_ds
    if (flags & STOR_F)
        printf("s\n");
    if (flags & TYPE_F)
        printf("t\n");
}
*/

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

// Every integer type has an integer conversion rank.
// Integer conversion ranks from highest to lowest
    // 1) long long int, unsigned long long int
    // 2) long int, unsigned long int
    // 3) int, unsigned int
    // 4) short int, unsigned short int
    // 5) char, signed char, unsigned char
    // 6) _Bool
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
// Otherwise, the integer promotions are performed on both operands. Then the
// following rules are applied to the promoted operands:
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

// Otherwise, if the type of the operand with signed integer type can represent
// all of the values of the type of the operand with unsigned integer type, then
// the operand with unsigned integer type is converted to the type of the
// operand with signed integer type.
    /* nothing of this applies with sizeof(int)==sizeof(long) */

    /*
     * Otherwise, both operands are converted to the unsigned integer type
     * corresponding to the type of the operand with signed integer type.
     */
    return TOK_UNSIGNED_LONG;
}

TypeExp *get_type_node(Token ty)
{
    static TypeExp ty_int = { TOK_INT };
    static TypeExp ty_unsigned = { TOK_UNSIGNED };
    static TypeExp ty_long = { TOK_LONG };
    static TypeExp ty_unsigned_long = { TOK_UNSIGNED_LONG };

    switch (ty) {
    case TOK_INT: return &ty_int;
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
 * The `e' is used only for the warnings (it contains the file/line/column information).
 */
static
int can_assign_to(ExecNode *e, Declaration *left_op, Declaration *right_op)
{
/* 6.5.16.1 Simple assignment
Constraints
1
One of the following shall hold:96)
— the left operand has qualified or unqualified arithmetic type and the right has
arithmetic type;
— the left operand has a qualified or unqualified version of a structure or union type
compatible with the type of the right;
— both operands are pointers to qualified or unqualified versions of compatible types,
and the type pointed to by the left has all the qualifiers of the type pointed to by the
right;
— one operand is a pointer to an object or incomplete type and the other is a pointer to a
qualified or unqualified version of void, and the type pointed to by the left has all
the qualifiers of the type pointed to by the right;
— the left operand is a pointer and the right is a null pointer constant; */
    Token ty_l, ty_r;

    ty_l = get_type_category(left_op);
    ty_r = get_type_category(right_op);

    if (is_integer(ty_l)) {
        if (is_integer(ty_r)) {
            int rank_l, rank_r;

            /* int and long have the same rank for assignment purposes */
            rank_l = get_rank(ty_l);
            rank_l = (rank_l==4)?3:rank_l;
            rank_r = get_rank(ty_r);
            rank_r = (rank_r==4)?3:rank_r;

            /*
             * Emit a warning when the destination type is narrower than
             * the source type.
             */
            if (rank_r > rank_l)
                WARNING(e, "implicit conversion loses integer precision: `%s' to `%s'",
                token_table[ty_r*2+1], token_table[ty_l*2+1]);
            /*
             * Otherwise, emit a warning if the source and destination types
             * do not have the same signedness.
             */
            else if (rank_l==rank_r && is_signed_int(ty_l)!=is_signed_int(ty_r))
                WARNING(e, "implicit conversion changes signedness: `%s' to `%s'",
                token_table[ty_r*2+1], token_table[ty_l*2+1]);
        } else if (is_pointer(ty_r)) {
            WARNING(e, "pointer to integer conversion without a cast");
        } else {
            return FALSE;
        }
    } else if (ty_l==TOK_STRUCT || ty_l==TOK_UNION) {
        TypeExp *ts_l, *ts_r;

        if (ty_l != ty_r)
            return FALSE;

        ts_l = get_type_spec(left_op->decl_specs);
        ts_r = get_type_spec(right_op->decl_specs);
        if (ts_l->str != ts_r->str)
            return FALSE;
    } else if (ty_l == TOK_STAR) {
        if (is_pointer(ty_r)) {
            /*
             * Check if the pointers are compatible. If they are, continue
             * and check for the additional requirement of type qualifiers;
             * otherwise, emit a warning and return to the caller.
             */
            if (left_op->idl->child==NULL && get_type_spec(left_op->decl_specs)->op==TOK_VOID) {
                if (right_op->idl->child!=NULL && right_op->idl->child->op==TOK_FUNCTION) {
                    WARNING(e, "conversion of function pointer to void pointer");
                    return TRUE;
                }
            } else if (right_op->idl->child==NULL && get_type_spec(right_op->decl_specs)->op==TOK_VOID) {
                if (left_op->idl->child!=NULL && left_op->idl->child->op==TOK_FUNCTION) {
                    WARNING(e, "conversion of void pointer to function pointer");
                    return TRUE;
                }
            } else if (!compare_and_compose(left_op->decl_specs, left_op->idl->child,
                right_op->decl_specs, right_op->idl->child, FALSE)) {
                    WARNING(e, "assignment from incompatible pointer type");
                    return TRUE;
            }

            /*
             * Verify that the type pointed to by the left operand has
             * all the qualifiers of the type pointed to by the right.
             */
            TypeExp *tq_l, *tq_r;

            /* fetch the qualifiers of the type pointed
               to by the left and right operands */
            if (left_op->idl->child == NULL) {
                tq_l = get_type_qual(left_op->decl_specs);
                tq_r = get_type_qual(right_op->decl_specs);
            } else if (left_op->idl->child->op == TOK_STAR) {
                tq_l = left_op->idl->child->attr.el;
                tq_r = right_op->idl->child->attr.el;
            }

            if (tq_r != NULL) {
                char *discarded;

                discarded = NULL;
                if (tq_r->op == TOK_CONST_VOLATILE) {
                    if (tq_l == NULL)
                        discarded = "const volatile";
                    else if (tq_l->op == TOK_CONST)
                        discarded = "volatile";
                    else if (tq_l->op == TOK_VOLATILE)
                        discarded = "const";
                } else if (tq_r->op == TOK_CONST) {
                    if (tq_l==NULL || tq_l->op==TOK_VOLATILE)
                        discarded = "const";
                } else if (tq_r->op == TOK_VOLATILE) {
                    if (tq_l==NULL || tq_l->op==TOK_CONST)
                        discarded = "volatile";
                }
                if (discarded != NULL)
                    WARNING(e, "assignment discards `%s' qualifier from pointer target type", discarded);
            }
        } else if (is_integer(ty_r)) {
            WARNING(e, "integer to pointer conversion without a cast");
        } else {
            return FALSE;
        }
    }

    return TRUE;
}

void analyze_multiplicative_expression(ExecNode *e);

void analyze_assignment_expression(ExecNode *e)
{
// 6.5.16
// #2 An assignment operator shall have a modifiable lvalue as its left operand.
    if (!is_modif_lvalue(e->child[0]))
        ERROR(e, "expression is not assignable");
// #3 An assignment operator stores a value in the object designated by the left operand. An
// assignment expression has the value of the left operand after the assignment, but is not an
// lvalue. The type of an assignment expression is the type of the left operand unless the
// left operand has qualified type, in which case it is the unqualified version of the type of
// the left operand. The side effect of updating the stored value of the left operand shall
// occur between the previous and the next sequence point.

    if (e->attr.op == TOK_ASSIGN) {
        if (!can_assign_to(e, &e->child[0]->type, &e->child[1]->type))
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
        // case TOK_LSHIFT_ASSIGN:
        // case TOK_RSHIFT_ASSIGN:
        // case TOK_BW_AND_ASSIGN:
        // case TOK_BW_XOR_ASSIGN:
        // case TOK_BW_OR_ASSIGN:
            // break;
        }
        if (!can_assign_to(e, &e->child[0]->type, &temp.type))
            ERROR(e, "incompatible types when assigning to type `%s' from type `%s'",
            stringify_type_exp(&e->child[0]->type), stringify_type_exp(&temp.type));
    }

    e->type = e->child[0]->type;

    printf("assign: %s\n", stringify_type_exp(&e->type));
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
                || !compare_and_compose(e->child[0]->type.decl_specs, e->child[0]->type.idl->child,
                e->child[1]->type.decl_specs, e->child[1]->type.idl->child, FALSE))
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
     * #2 Unless the type name specifies a void type, the type name shall specify qualified or
     * unqualified scalar type and the operand shall have scalar type.
     */
    /* source type */
    ty_src = get_type_category(&e->child[0]->type);
    if (!is_integer(ty_src) && !is_pointer(ty_src)
    && ty_src!=TOK_FUNCTION && ty_src!=TOK_VOID)
        ERROR(e, "cast operand does not have scalar type");

    /* target type */
    ty_tgt = get_type_category((Declaration *)e->child[1]);
    if (!is_integer(ty_tgt) && ty_tgt!=TOK_STAR && ty_tgt!=TOK_VOID)
        ERROR(e, "cast specifies conversion to non-scalar type");

    /* check for void ==> non-void */
    if (ty_src==TOK_VOID && ty_tgt!=TOK_VOID)
        ERROR(e, "invalid cast of void expression to non-void type");

    e->type.decl_specs = ((Declaration *)e->child[1])->decl_specs;
    e->type.idl = ((Declaration *)e->child[1])->idl;

    printf("(%s)\n", stringify_type_exp(&e->type));
}

void analyze_unary_expression(ExecNode *e)
{
    switch (e->attr.op) {
    case TOK_PRE_INC:
        printf("pre_inc\n");
        break;
    case TOK_PRE_DEC:
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
    case TOK_UNARY_MINUS: {
        Token ty, prom_ty;

        /*
         * 6.5.3.3
         * #1 The operand of the unary + or - operator shall have arithmetic type;
         * #2 The result of the unary + operator is the value of its (promoted) operand. The integer
         * promotions are performed on the operand, and the result has the promoted type.
         * #3 The result of the unary - operator is the negative of its (promoted) operand. The integer
         * promotions are performed on the operand, and the result has the promoted type.
         */
        ty = get_type_category(&e->child[0]->type);
        if (!is_integer(ty))
            ERROR(e, "invalid operand to %s", token_table[e->attr.op*2+1]);

        if ((prom_ty=get_promoted_type(ty)) != ty) {
            TypeExp *ts;

            e->type.decl_specs = dup_decl_specs_list(e->child[0]->type.decl_specs);
            ts = get_type_spec(e->type.decl_specs);
            ts->op = prom_ty;
        } else {
            e->type.decl_specs = e->child[0]->type.decl_specs;
        }
        e->type.idl = e->child[0]->type.idl;

        printf("+- result type: %s\n", stringify_type_exp(&e->type));
        break;
    }
    case TOK_COMPLEMENT:
    case TOK_NEGATION:
        break;
    default:
        printf("non_unary_expr\n");
        break;
    }
}

void analyze_postfix_expression(ExecNode *e)
{
    /*if (e->kind.exp != OpExp)
        return;

    analyze_postfix_expression(e->child[0]);*/

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
         * 6.5.2.2#1
         * The expression that denotes the called function shall have type pointer to function
         * returning void or returning an object type other than an array type.
         */
        /*
         * 6.5.2.2#5
         * If the expression that denotes the called function has type pointer to function returning an
         * object type, the function call expression has the same type as that object type, and has the
         * value determined as specified in 6.8.6.4. Otherwise, the function call has type void. [...]
         * 6.7.2.3#footnote
         * [...] The specification has to be complete before such a function is called or defined.
         */
        TypeExp *ty;

        ty = e->child[0]->type.idl;

        if (ty == NULL)
            goto non_callable;
        else if (ty->op == TOK_FUNCTION)
            ty = ty->child;
        else if (ty->op==TOK_STAR && ty->child!=NULL && ty->child->op==TOK_FUNCTION)
            ty = ty->child->child;
        else
            goto non_callable;
        /*
         * Functions cannot be declared as returning an array or function, so what remains
         * to check is that the return type is not an incomplete enum/struct/union type.
         */
        if (ty == NULL) {
            /* the return type is not a derived declarator type */
            TypeExp *ts;

            ts = get_type_spec(e->child[0]->type.decl_specs);
            if (is_struct_union_enum(ts->op) && !is_complete(ts->str))
                ERROR(e, "calling function with incomplete return type `%s %s'", token_table[ts->op*2+1], ts->str);
        }

        /* set as the type of the node () the return type of the function */
        e->type.decl_specs = e->child[0]->type.decl_specs;
        e->type.idl = ty;
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
        /*
         * 6.5.2.4#1
         * The operand of the postfix increment or decrement operator shall have qualified or
         * unqualified real or pointer type and shall be a modifiable lvalue.
         */
        Token ty;

        ty = get_type_category(&e->child[0]->type);
        if (!is_integer(ty) && !is_pointer(ty))
            ERROR(e, "wrong type argument to increment");
        if (!is_modif_lvalue(e->child[0]))
            ERROR(e, "expression is not modifiable");

        /* set the type of the ++ node */
        e->type = e->child[0]->type;
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
            e->type.idl = NULL;
        }
        break;
    case IConstExp: {
        // static TypeExp ty = { TOK_INT };
// #if 0
        char *ep, *ic;

        errno = 0;
        ic = e->attr.str;
        e->attr.val = strtol(ic, &ep, 0);
        if (errno == ERANGE) {
            /* try with unsigned long */
            printf("trying ul\n");
            errno = 0;
            e->attr.uval = strtoul(ic, &ep, 0);
            if (errno ==  ERANGE) {
                DEBUG_PRINTF("non-representable integer constant (saturated result)\n");
            }
        } /*else {

        }*/
        // printf("res=%x\n", e->attr.val);
        // printf("res=%x\n", e->attr.uval);
// #endif
        // e->type.decl_specs = &ty;
        e->type.decl_specs = get_type_node(TOK_INT);
        break;
    }
    case StrLitExp: {
        // static TypeExp lit_ds = { TOK_CHAR };
        static TypeExp lit_dct = { TOK_SUBSCRIPT };

        // e->type.decl_specs = &lit_ds;
        e->type.decl_specs = get_type_node(TOK_CHAR);
        e->type.idl = &lit_dct;
        break;
    }
    default: /* expression between parenthesis that is not a primary expression */
        break;
    }
}
