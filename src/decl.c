#include "decl.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#define DEBUG 0
#include "util.h"
#include "expr.h"
#include "stmt.h"

extern unsigned error_count, warning_count;

#define ERROR(tok, ...)\
    do {\
        PRINT_ERROR((tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column, __VA_ARGS__);\
        ++error_count;\
    } while (0)

#define ERROR_R(tok, ...)\
    do {\
        ERROR(tok, __VA_ARGS__);\
        return;\
    } while (0)

#define ERROR_RF(tok, ...)\
    do {\
        ERROR(tok, __VA_ARGS__);\
        return FALSE;\
    } while (0)

#define WARNING(tok, ...)\
    PRINT_WARNING((tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column, __VA_ARGS__),\
    ++warning_count

#define FATAL_ERROR(tok, ...)\
    PRINT_ERROR((tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column, __VA_ARGS__),\
    exit(EXIT_FAILURE)

#define HASH_SIZE 101
#define MAX_NEST  16

#define FILE_SCOPE 0

static void delete_scope(void);

typedef enum {
    DEFINED,            /* int x = 0; or void foo(void){...} */
    REFERENCED,         /* extern int x; or void foo(void); */
    TENTATIVELY_DEFINED /* int x; */
} ExtIdStatus;

typedef struct ExternId ExternId;
struct ExternId {
    TypeExp *decl_specs;
    TypeExp *declarator;
    ExtIdStatus status;
    ExternId *next;
};
static ExternId *external_declarations[HASH_SIZE];

static Symbol *ordinary_identifiers[HASH_SIZE][MAX_NEST];
static int curr_scope = 0;
static int delayed_delete = FALSE;

static TypeTag *tags[HASH_SIZE][MAX_NEST];

TypeTag *lookup_tag(char *id, int all)
{
    TypeTag *np;
    int n = curr_scope;

    if (delayed_delete)
        delete_scope();

    if (all == TRUE) {
        for (; n >= 0; n--)
            for (np = tags[hash(id)%HASH_SIZE][n]; np != NULL; np = np->next)
                if (strcmp(id, np->type->str) == 0)
                    return np;
        return NULL; /* not found */
    } else {
        for (np = tags[hash(id)%HASH_SIZE][n]; np != NULL; np = np->next)
            if (strcmp(id, np->type->str) == 0)
                return np;
        return NULL; /* not found */
    }
}

void install_tag(TypeExp *t)
{
    TypeTag *np;
    unsigned hash_val;

    DEBUG_PRINTF("new tag `%s', scope: %d\n", t->str, curr_scope);

    if (delayed_delete)
        delete_scope();

    /*if ((np=lookup_tag(t->str, FALSE)) == NULL) {*/ /* not found in the current scope */
        np = malloc(sizeof(TypeTag));
        np->type = t;
        hash_val = hash(t->str)%HASH_SIZE;
        np->next = tags[hash_val][curr_scope];
        tags[hash_val][curr_scope] = np;
    /*}*/
}

int is_sto_class_spec(Token t)
{
    return (t==TOK_TYPEDEF||t==TOK_EXTERN||t==TOK_STATIC||t==TOK_AUTO||t==TOK_REGISTER);
}

/*
 * Return TRUE if `t' is a type specifier.
 */
int is_type_spec(Token t)
{
    switch (t) {
    case TOK_VOID: case TOK_CHAR: case TOK_SHORT: case TOK_INT: case TOK_LONG:
    case TOK_SIGNED: case TOK_UNSIGNED: case TOK_STRUCT: case TOK_UNION:
    case TOK_ENUM: case TOK_TYPEDEFNAME:
        return TRUE;
    default:
        return FALSE;
    }
}

/*
 * Same as is_type_spec() but recognizes additional specifiers that appear
 * after analyze_decl_specs() processes the declaration specifiers list.
 */
int is_type_spec2(Token t)
{
    switch (t) {
    case TOK_VOID: case TOK_CHAR: case TOK_SIGNED_CHAR: case TOK_UNSIGNED_CHAR:
    case TOK_SHORT: case TOK_UNSIGNED_SHORT: case TOK_INT: case TOK_UNSIGNED:
    case TOK_LONG: case TOK_UNSIGNED_LONG: case TOK_STRUCT: case TOK_UNION:
    case TOK_ENUM: case TOK_TYPEDEFNAME: case TOK_ERROR:
        return TRUE;
    default:
        return FALSE;
    }
}

#define is_type_qualifier(t) (t==TOK_CONST || t==TOK_VOLATILE)

int is_struct_union_enum(Token t)
{
    return (t==TOK_STRUCT||t==TOK_UNION||t==TOK_ENUM);
}

TypeExp *get_sto_class_spec(TypeExp *d)
{
    while (d != NULL) {
        if (is_sto_class_spec(d->op))
            return d;
        d = d->child;
    }

    return NULL;
}

TypeExp *get_type_spec(TypeExp *d)
{
    while (d != NULL) {
        if (is_type_spec2(d->op))
            return d;
        d = d->child;
    }

    /* a type specifier is required */
    if (d == NULL)
        my_assert(stderr, "get_type_spec()");

    return NULL; /* just to avoid warning */
}

TypeExp *get_type_qual(TypeExp *d)
{
    while (d != NULL) {
        if (is_type_qualifier(d->op) || d->op==TOK_CONST_VOLATILE)
            return d;
        d = d->child;
    }

    return NULL;
}

void analyze_decl_specs(TypeExp *d)
{
    /*
     * Some notes:
     * - If there is more than one storage class specifier, the rest of the code will
     *   only see the first one because of the way in which get_sto_class_spec() works.
     * - Something similar occurs with type specifiers.
     * - Total lack of type specifiers is a fatal error.
     */

    enum {
        START,
        CHAR,
        SIZE, SIGN, INT,
        CHAR_SIGN, SIGN_CHAR,
        SIZE_SIGN, SIZE_INT, SIGN_SIZE, SIGN_INT, INT_SIGN, INT_SIZE,
        END
    };
    int state;
    TypeExp *scs; /* storage class specifier */
    TypeExp *first_tq; /* first type qualifier */
    TypeExp *first_ts; /* first type specifier */
    TypeExp *temp, *prev;

    temp = d;
    scs = NULL;
    first_tq = NULL;
    state = START;
    while (TRUE) {
        while (d!=NULL && !is_type_spec(d->op)) {
            int del_node = FALSE;

            if (is_sto_class_spec(d->op)) {
                if (scs == NULL)
                    scs = d;
                else
                    ERROR_R(d, "more than one storage class specifier");
            } else if (is_type_qualifier(d->op)) {
                /*
                 * type qualifier nodes are merged into a single node.
                 */
                if (first_tq == NULL) {
                    first_tq = d;
                } else {
                    if (first_tq->op != d->op)
                        first_tq->op = TOK_CONST_VOLATILE;
                    del_node = TRUE;
                }
            }
            if (del_node) {
                /* delete a type qualifier node */
                prev->child = d->child;
                free(d);
                d = prev->child;
            } else {
                prev = d;
                d = d->child;
            }
        }

        if (d == NULL) {
            if (state==START) {
                d = temp;
                FATAL_ERROR(d, "missing type specifier");
            } else {
                return;
            }
        }

        switch (state) {
        case START:
            switch (d->op) {
            case TOK_CHAR:
                state = CHAR;
                break;
            case TOK_SHORT:
            case TOK_LONG:
                state = SIZE;
                break;
            case TOK_SIGNED:
            case TOK_UNSIGNED:
                state = SIGN;
                if (d->op == TOK_SIGNED)
                    d->op = TOK_INT;
                break;
            case TOK_INT:
                state = INT;
                break;
            case TOK_VOID:
            case TOK_UNION:
            case TOK_STRUCT:
            case TOK_ENUM:
            case TOK_TYPEDEFNAME:
                state = END;
                break;
            }
            first_ts = prev = d;
            d = d->child;
            continue;
        /*
         * Types that can be denoted in different ways are brought
         * to a single common form. For example `short', `signed short',
         * and `signed short int' are converted to simply `short'.
         * The single type specifier left is one of:
         *      void,
         *      char, signed char, unsigned char,
         *      short, unsigned short,
         *      int, unsigned,
         *      long, unsigned long
         *      union,
         *      struct,
         *      enum,
         *      typedef-name
         */
        case CHAR:
            if (d->op==TOK_SIGNED || d->op==TOK_UNSIGNED) {
                state = END;
                first_ts->op = (d->op==TOK_SIGNED)?TOK_SIGNED_CHAR:TOK_UNSIGNED_CHAR;
            } else {
                ERROR_R(d, "more than one type specifier");
            }
            break;
        case SIZE:
            if (d->op==TOK_SIGNED || d->op==TOK_UNSIGNED) {
                state = SIZE_SIGN;
                if (d->op == TOK_UNSIGNED)
                    first_ts->op = (first_ts->op==TOK_SHORT)?TOK_UNSIGNED_SHORT:TOK_UNSIGNED_LONG;
            } else if (d->op == TOK_INT) {
                state = SIZE_INT;
            } else {
                ERROR_R(d, "more than one type specifier");
            }
            break;
        case SIGN:
            if (d->op==TOK_SHORT || d->op==TOK_LONG) {
                state = SIGN_SIZE;
                if (first_ts->op == TOK_UNSIGNED)
                    first_ts->op = (d->op==TOK_SHORT)?TOK_UNSIGNED_SHORT:TOK_UNSIGNED_LONG;
                else
                    first_ts->op = d->op;
            } else if (d->op == TOK_INT) {
                state = SIGN_INT;
            } else if (d->op == TOK_CHAR) {
                state = END;
                first_ts->op = (first_ts->op==TOK_UNSIGNED)?TOK_UNSIGNED_CHAR:TOK_SIGNED_CHAR;
            } else {
                ERROR_R(d, "more than one type specifier");
            }
            break;
        case INT:
            if (d->op==TOK_SIGNED || d->op==TOK_UNSIGNED) {
                state = INT_SIGN;
                if (d->op == TOK_UNSIGNED)
                    first_ts->op = TOK_UNSIGNED;
            } else if (d->op==TOK_SHORT || d->op==TOK_LONG) {
                state = INT_SIZE;
                first_ts->op = d->op;
            } else {
                ERROR_R(d, "more than one type specifier");
            }
            break;
        case SIZE_SIGN:
        case SIGN_SIZE:
            if (d->op == TOK_INT)
                state = END;
            else
                ERROR_R(d, "more than one type specifier");
            break;
        case SIZE_INT:
        case INT_SIZE:
            if (d->op==TOK_SIGNED || d->op==TOK_UNSIGNED) {
                state = END;
                if (d->op == TOK_UNSIGNED)
                    first_ts->op = (first_ts->op==TOK_SHORT)?TOK_UNSIGNED_SHORT:TOK_UNSIGNED_LONG;
            } else {
                ERROR_R(d, "more than one type specifier");
            }
            break;
        case SIGN_INT:
        case INT_SIGN:
            if (d->op==TOK_SHORT || d->op==TOK_LONG) {
                state = END;
                if (first_ts->op == TOK_UNSIGNED)
                    first_ts->op = (d->op==TOK_SHORT)?TOK_UNSIGNED_SHORT:TOK_UNSIGNED_LONG;
                else
                    first_ts->op = d->op;
            } else {
                ERROR_R(d, "more than one type specifier");
            }
            break;
        case END:
            ERROR_R(d, "more than one type specifier");
            break;
        } /* switch (state) */
        prev->child = d->child;
        free(d);
        d = prev->child;
    } /* while (TRUE) */
}

static void delete_scope(void)
{
    int i;
    Symbol *np, *temp;
    TypeTag *np2, *temp2;

    if (curr_scope < 0) {
        fprintf(stderr, "Underflow in delete_scope().\n");
        exit(EXIT_FAILURE);
    }

    for (i = 0; i < HASH_SIZE; i++) {
        if (ordinary_identifiers[i][curr_scope] != NULL) {
            for (np = ordinary_identifiers[i][curr_scope]; np != NULL;) { /* delete chain */
                temp = np;
                np = np->next;
                free(temp);
            }
            ordinary_identifiers[i][curr_scope] = NULL;
        }
        if (tags[i][curr_scope] != NULL) {
            for (np2 = tags[i][curr_scope]; np2 != NULL;) { /* delete chain */
                temp2 = np2;
                np2 = np2->next;
                free(temp2);
            }
            tags[i][curr_scope] = NULL;
        }
    }
    --curr_scope;
    // DEBUG_PRINTF("Popping scope, nest_level=%d\n", nest_level);
    delayed_delete = FALSE;
}

void restore_scope(void)
{
    delayed_delete = FALSE;
}

void push_scope(void)
{
    if (delayed_delete)
        delete_scope();

    if (++curr_scope == MAX_NEST) {
        fprintf(stderr, "Too many nested scopes.\n");
        exit(EXIT_FAILURE);
    }
    // DEBUG_PRINTF("Pushing scope, nest_level=%d\n", nest_level);
}

void pop_scope(void)
{
    if (delayed_delete)
        delete_scope();

    delayed_delete = TRUE;
}

Symbol *lookup(char *id, int all)
{
    Symbol *np;
    int n = curr_scope;

    if (delayed_delete)
        delete_scope();

    if (all == TRUE) {
        for (; n >= 0; n--)
            for (np = ordinary_identifiers[hash(id)%HASH_SIZE][n]; np != NULL; np = np->next)
                if (strcmp(id, np->declarator->str) == 0)
                    return np;
        return NULL; /* not found */
    } else {
        for (np = ordinary_identifiers[hash(id)%HASH_SIZE][n]; np != NULL; np = np->next)
            if (strcmp(id, np->declarator->str) == 0)
                return np;
        return NULL; /* not found */
    }
}
/*
Token get_id_token(char *id)
{
    Symbol *np;

    if ((np=lookup(id, TRUE)) != NULL) {
        TypeExp *scs;
        if ((scs=get_sto_class_spec(np->decl_specs))!=NULL && scs->op==TOK_TYPEDEF)
            return TOK_TYPEDEFNAME;
        return TOK_ID;
    } else {
        return 0; // undeclared
    }
}
*/
int is_typedef_name(char *id)
{
    // return (get_id_token(id) == TOK_TYPEDEFNAME);
    Symbol *np;

    if ((np=lookup(id, TRUE)) != NULL) {
        TypeExp *scs;

        if ((scs=get_sto_class_spec(np->decl_specs))!=NULL && scs->op==TOK_TYPEDEF)
            return TRUE;
    }
    return FALSE;
}

// static
void install(TypeExp *decl_specs, TypeExp *declarator)
{
    Symbol *np;
    unsigned hash_val;

    if (delayed_delete)
        delete_scope();

    if ((np=lookup(declarator->str, FALSE)) == NULL) { /* not found */
        np = malloc(sizeof(Symbol));
        // np->id = id;
        // np->tok = tok;
        np->decl_specs = decl_specs;
        np->declarator = declarator;
        hash_val = hash(declarator->str)%HASH_SIZE;
        np->next = ordinary_identifiers[hash_val][curr_scope];
        ordinary_identifiers[hash_val][curr_scope] = np;
    } else { /* already in this scope */
        TypeExp *scs;
        Token curr_scs, prev_scs;

        if (decl_specs->op == TOK_ERROR)
            return;

        /* get the storage class specifier of the current and previous declaration */
        curr_scs = (scs=get_sto_class_spec(decl_specs))!=NULL ? scs->op:0;
        prev_scs = (scs=get_sto_class_spec(np->decl_specs))!=NULL ? scs->op:0;

        /* diagnose depending on the situation */
        if (declarator->op==TOK_ENUM_CONST || curr_scs==TOK_TYPEDEF) {
            /*
             * Clash while trying to install an enumeration constant or typedef name.
             */
            if (declarator->op==TOK_ENUM_CONST && np->declarator->op==TOK_ENUM_CONST)
                ERROR_R(declarator, "redeclaration of enumerator `%s'", declarator->str);
            else if (curr_scs==TOK_TYPEDEF && prev_scs==TOK_TYPEDEF)
                ERROR_R(declarator, "redefinition of typedef `%s'", declarator->str);
            else
                goto diff_kind_of_sym;
        } else if (np->declarator->op==TOK_ENUM_CONST || prev_scs==TOK_TYPEDEF) {
            /*
             * Clash with previously declared enumeration constant or typedef name.
             */
            goto diff_kind_of_sym;
        } else if (curr_scope != FILE_SCOPE) {
            int is_curr_func, is_prev_func;

            is_curr_func = declarator->child!=NULL && declarator->child->op==TOK_FUNCTION;
            is_prev_func = np->declarator->child!=NULL && np->declarator->child->op==TOK_FUNCTION;

            if (is_curr_func || is_prev_func) {
                if (is_curr_func != is_prev_func)
                    goto diff_kind_of_sym;
                else
                    return; /* OK by now */
            }

            /*
             * Check linkage.
             */
            if (!curr_scs || curr_scs!=TOK_EXTERN) {
                if (!prev_scs || prev_scs!=TOK_EXTERN)
                    /* e.g. int x; ==> int x; */
                    ERROR_R(declarator, "redeclaration of `%s' with no linkage", declarator->str);
                else /* if (prev_scs == TOK_EXTERN) */
                    /* e.g. extern int x; ==> int x; */
                    ERROR_R(declarator, "declaration of `%s' with no linkage follows extern declaration", declarator->str);
            } else {
                if (!prev_scs || prev_scs!=TOK_EXTERN)
                    /* e.g. int x; ==> extern int x; */
                    ERROR_R(declarator, "extern declaration of `%s' follows declaration with no linkage", declarator->str);
            }
        }
        return;
diff_kind_of_sym:
        ERROR_R(declarator, "`%s' redeclared as different kind of symbol", declarator->str);
    }
}

static long en_val = -1;

void reset_enum_val(void)
{
    en_val = -1;
}

void analyze_enumerator(TypeExp *e)
{
    static TypeExp enum_ds = { TOK_INT };

    /*
     * 6.7.2.2#2
     * The expression that defines the value of an enumeration constant shall be an integer
     * constant expression that has a value representable as an int.
     */
    if (e->attr.e != NULL) {
        if (!is_integer(get_type_category(&e->attr.e->type))) {
            ERROR(e, "enumerator value is not an integer constant");
            goto error;
        }
        en_val = eval_const_expr(e->attr.e, FALSE);
    } else {
        e->attr.e = calloc(1, sizeof(ExecNode));
        ++en_val;
    }
    /*if (en_val > 2147483647)
        ...*/
    e->attr.e->attr.val = en_val;
    // printf("en_val=%ld\n", en_val);
error:
    install(&enum_ds, e);
}

static
ExternId *lookup_external_id(char *id)
{
    ExternId *np;

    for (np = external_declarations[hash(id)%HASH_SIZE]; np != NULL; np = np->next)
        if (strcmp(id, np->declarator->str) == 0)
            return np;
    return NULL; /* not found */
}

int is_external_id(char *id)
{
    return lookup_external_id(id)!=NULL;
}

static void
install_external_id(TypeExp *decl_specs, TypeExp *declarator, ExtIdStatus status)
{
    ExternId *np;
    unsigned hash_val;

    np = malloc(sizeof(ExternId));
    np->decl_specs = decl_specs;
    np->declarator = declarator;
    np->status = status;
    hash_val = hash(declarator->str)%HASH_SIZE;
    np->next = external_declarations[hash_val];
    external_declarations[hash_val] = np;
}

int compare_decl_specs(TypeExp *ds1, TypeExp *ds2, int qualified)
{
    TypeExp *temp1, *temp2;

    /* type specifiers */
    temp1 = get_type_spec(ds1);
    temp2 = get_type_spec(ds2);
    if (temp1->op!=temp2->op || is_struct_union_enum(temp1->op)&&temp1->str!=temp2->str)
        return FALSE;

    /* type qualifiers */
    if (qualified) {
        temp1 = get_type_qual(ds1);
        temp2 = get_type_qual(ds2);
        if (temp1==NULL && temp2!=NULL
        || temp2==NULL && temp1!=NULL
        || temp1!=NULL && temp1->op!=temp2->op) {
            DEBUG_PRINTF("type qualifiers conflict\n");
            return FALSE;
        }
    }

    return TRUE;
}

/*
 * Return TRUE if the two types are compatibles, FALSE otherwise.
 * If 'qualified' is TRUE, type qualifiers are taken into account.
 * If 'compose' is TRUE, array types are composed.
 */
int are_compatible(TypeExp *ds1, TypeExp *dct1,
                   TypeExp *ds2, TypeExp *dct2,
                   int qualified, int compose)
{
    /* identifiers are non-significant */
    if (dct1!=NULL && dct1->op==TOK_ID)
        dct1 = dct1->child;
    if (dct2!=NULL && dct2->op==TOK_ID)
        dct2 = dct2->child;

    if (dct1==NULL || dct2==NULL) {
        if (dct1 != dct2)
            return FALSE;
        return compare_decl_specs(ds1, ds2, qualified);
    }

    if (dct1->op != dct2->op)
        return FALSE;

    switch (dct1->op) {
    case TOK_ELLIPSIS:
        return TRUE;
    case TOK_STAR:
        if (qualified
        && (dct1->attr.el==NULL && dct2->attr.el!=NULL
        || dct1->attr.el!=NULL && dct2->attr.el==NULL
        || dct1->attr.el!=NULL && dct1->attr.el->op!=dct2->attr.el->op))
            return FALSE;
        break;
    case TOK_SUBSCRIPT:
        if (dct1->attr.e!=NULL && dct2->attr.e!=NULL) {
            if (dct1->attr.e->attr.val != dct2->attr.e->attr.val)
                return FALSE;
        } else if (compose) {
            if (dct1->attr.e==NULL && dct2->attr.e!=NULL) {
                dct1->attr.e = dct2->attr.e;
            } else if (dct2->attr.e==NULL && dct1->attr.e!=NULL) {
                dct2->attr.e = dct1->attr.e;
            }
        }
        break;
    case TOK_FUNCTION: {
        DeclList *p1, *p2;

        p1 = dct1->attr.dl;
        p2 = dct2->attr.dl;
        while (p1!=NULL && p2!=NULL) {
            /*
             * 6.7.6#15
             * [...]In the determination of type compatibility and of a composite type, each
             * parameter declared [...] with qualified type is taken as having the unqualified
             * version of its declared type.
             */
            if (!are_compatible(p1->decl->decl_specs, p1->decl->idl,
            p2->decl->decl_specs, p2->decl->idl, FALSE, compose))
                return FALSE;
            p1 = p1->next;
            p2 = p2->next;
        }
        if (p1 != p2)
            return FALSE;
        break;
    }
    }

    return are_compatible(ds1, dct1->child, ds2, dct2->child, TRUE, compose);
}

int is_complete(char *tag)
{
    TypeTag *tp;

    if (tag == NULL) /* anonymous struct/union/enum */
        return TRUE;

    tp = lookup_tag(tag, TRUE);
    if (tp != NULL) {
        if (tp->type->op == TOK_ENUM)
            return tp->type->attr.el!=NULL;
        else
            return tp->type->attr.dl!=NULL;
    } else {
        my_assert(0, "is_complete()");
    }
}

static
int examine_declarator(TypeExp *decl_specs, TypeExp *declarator)
{
    if (declarator == NULL)
        return TRUE;

    switch (declarator->op) {
    case TOK_ID:
    case TOK_STAR:
        break;
    case TOK_SUBSCRIPT:
        /*
         * 6.7.5.2#1
         * The element type shall not be an incomplete or function type.
         */
        if (declarator->child != NULL) {
            if (declarator->child->op == TOK_FUNCTION)
                ERROR_RF(declarator, "array of functions");
            else if (declarator->child->op==TOK_SUBSCRIPT && declarator->child->attr.e==NULL)
                ERROR_RF(declarator, "array has incomplete element type");
        } else {
            TypeExp *ts;

            ts = get_type_spec(decl_specs);
            if (is_struct_union_enum(ts->op) && !is_complete(ts->str)
            || ts->op==TOK_VOID)
                ERROR_RF(declarator, "array has incomplete element type");
        }
        if (declarator->attr.e != NULL) {
            // analyze_array_size_expr(declarator);
            long size;

            /*
             * 6.7.5.2#1
             * [...] If they delimit an expression (which specifies the size of an array), the
             * expression shall have an integer type. If the expression is a constant expression,
             * it shall have a value greater than zero. [...] Note: VLAs are not supported, so the
             * expression must always be constant.
             */
            if (!is_integer(get_type_category(&declarator->attr.e->type)))
                ERROR_RF(declarator, "size of array has non-integer type");

            size = eval_const_expr(declarator->attr.e, FALSE);
            if (size <= 0)
                ERROR_RF(declarator, "size of array not greater than zero");

            /* store the computed size in the root of the expression tree */
            declarator->attr.e->attr.val = size; /* maybe overwrites some operator */
        }
        break;
    case TOK_FUNCTION: {
        DeclList *p;

        /*
         * 6.5.7.3#1
         * A function declarator shall not specify a return type that is a function type
         * or an array type.
         */
        if (declarator->child != NULL) {
            if (declarator->child->op == TOK_FUNCTION)
                ERROR_RF(declarator, "function returning a function");
            else if (declarator->child->op == TOK_SUBSCRIPT)
                ERROR_RF(declarator, "function returning an array");
        }

        /*
         * Make sure that, if there is a void parameter,
         * it is the first and only one.
         */
        p = declarator->attr.dl;
        while (p != NULL) {
            if (p->decl->idl!=NULL && p->decl->idl->op==TOK_ELLIPSIS)
                break;

            if (get_type_spec(p->decl->decl_specs)->op == TOK_VOID) {
                TypeExp *temp;

                if (p->decl->idl!=NULL && p->decl->idl->op==TOK_ID)
                    temp = p->decl->idl->child;
                else
                    temp = p->decl->idl;

                if (temp==NULL && (p!=declarator->attr.dl || p->next!=NULL))
                    ERROR_RF(declarator, "`void' must be the first and only parameter");
            }
            p = p->next;
        }
        break;
    }
    }
    return examine_declarator(decl_specs, declarator->child);
}

DeclList *new_param_decl(TypeExp *decl_specs, TypeExp *declarator)
{
    DeclList *new_node;

    new_node = malloc(sizeof(DeclList));
    new_node->decl = malloc(sizeof(Declaration));
    new_node->decl->decl_specs = decl_specs;
    new_node->decl->idl = declarator;
    new_node->next = NULL;

    return new_node;
}

TypeExp *dup_declarator(TypeExp *d)
{
    TypeExp *new_node;

    if (d == NULL)
        return d;

    new_node = malloc(sizeof(TypeExp));
    *new_node = *d;
    if (d->op == TOK_FUNCTION) {
        DeclList *p, *temp;

        p = d->attr.dl;
        new_node->attr.dl = temp = new_param_decl(p->decl->decl_specs, dup_declarator(p->decl->idl));
        p = p->next;
        while (p != NULL) {
            temp->next = new_param_decl(p->decl->decl_specs, dup_declarator(p->decl->idl));
            temp=temp->next, p=p->next;
        }
    }
    new_node->child = dup_declarator(d->child);

    return new_node;
}

// static
void replace_typedef_name(Declaration *decl)
{
    /*
     * TODO: fix the file & line/column number mess.
     */
    Symbol *s;
    TypeExp *temp, *tq, *ts, *decl_specs, *declarator;

    decl_specs = decl->decl_specs;
    declarator = decl->idl;

    if ((ts=get_type_spec(decl_specs))->op != TOK_TYPEDEFNAME)
        return;

    /*
     * Type specifiers
     */
    s = lookup(ts->str, TRUE);
    /* replace the typedef name node for the type specifier node of the typedef name */
    temp = ts->child;
    *ts = *get_type_spec(s->decl_specs);
    ts->child = temp;

    /*
     * Append the declarator part (if present) of the typedef name.
     * A copy of the typedef name's declarator is used to make the replacement
     * because of the risk of modifying the typedef definition during type composition.
     * Suppose the declarations
     *  typedef int a[];
     *  extern a x;
     *  extern int x[10]; // (1), completes x
     * if later the typedef name is used again, as in
     *  a x2;
     * x2 will have type int[10] instead of int[] because (1) completed directly the
     * typedef definition.
     */
    if (s->declarator->child != NULL) {
        if (declarator != NULL) {
            while (declarator->child != NULL)
                declarator = declarator->child;
            declarator->child = dup_declarator(s->declarator->child);
            declarator = declarator->child;
        } else {
            /* empty abstract declarator */
            decl->idl = declarator = dup_declarator(s->declarator->child);
        }
    }

    /*
     * Type qualifiers (if any)
     * Situations:
     * (1)
     *  typedef int *t;
     *  const t x;
     * after replacement
     *  int *const x; // const removed, new node for pointer's const
     * (2)
     *  typedef const int t;
     *  t x;
     * after replacement
     *  const int x; // new const node
     */

    /* check for (1) */
    temp = get_type_qual(decl_specs);
    if (temp==NULL || declarator==NULL)
        goto nothing;
    if (declarator->op==TOK_STAR || declarator->op==TOK_FUNCTION) {
        goto common;
    } else if (declarator->op == TOK_SUBSCRIPT) {
        /*
         *  6.7.3#8
         * If the specification of an array type includes any type qualifiers,
         * the element type is so-qualified, not the array type.
         */
        do
            declarator = declarator->child;
        while (declarator!=NULL && declarator->op==TOK_SUBSCRIPT); /* search the element type */
        if (declarator != NULL)
            /* the element type of the array is a derived type (pointer or function) */
            goto common;
    }
    goto nothing;
common:
    if (declarator->op == TOK_FUNCTION) {
        WARNING(declarator, "qualifier on function type `%s' has undefined behavior", s->declarator->str);
        temp->op = 0; /* just ignore the qualifier */
        goto nothing;
    }
    /* qualify the pointer */
    if (declarator->attr.el == NULL) {
        declarator->attr.el = malloc(sizeof(TypeExp));
        declarator->attr.el->op = temp->op;
    } else if (declarator->attr.el->op != temp->op) {
        declarator->attr.el->op = TOK_CONST_VOLATILE;
    }
    temp->op = 0; /* no declaration specifier has value zero. TODO: find a way of directly
                     delete this node so it doesn't stay around bothering */
nothing:
    /* check for (2) */
    tq = get_type_qual(s->decl_specs);
    if (tq != NULL) {
        if (temp != NULL) {
            if (!temp->op)
                temp->op = tq->op; /* utilize node removed in (1) */
            else if (temp->op != tq->op)
                temp->op = TOK_CONST_VOLATILE;
        } else {
            /* no type qualifier between the declaration specifiers of
               the original declaration, append a new node at the end */
            temp = decl_specs;
            while (temp->child != NULL)
                temp = temp->child;
            temp->child = calloc(1, sizeof(TypeExp));
            temp->child->op = tq->op;
        }
    }
}

int analyze_declarator(TypeExp *decl_specs, TypeExp *declarator, int inst_sym)
{
    int good;
    Declaration d;

    d.decl_specs = decl_specs;
    d.idl = declarator;
    replace_typedef_name(&d);

    good = examine_declarator(decl_specs, declarator);
    if (inst_sym) {
        /*
         * If an identifier has an invalid type, install it into the symbol
         * table as having erroneous type. The rest of the code ignores everything
         * that has 'error' type so error cascades are avoided.
         * For example, in the following declaration
         *
         *      int x[10](void); // x has the invalid type `array of functions'
         *
         * subsequent code that checks for expressions correctness will ignore the
         * whole expression where the identifier appears instead of emit spurious errors
         * derived from x's erroneous type. For example if the identifier is encountered
         * in the expression `x * 2', no error will be reported about 'invalid operands to *'.
         * Other functions in this module act in a similar way.
         */
        if (good)
            install(decl_specs, declarator);
        else
            install(get_type_node(TOK_ERROR), declarator);
    }

    return good;
}

void analyze_type_name(Declaration *tn)
{
    analyze_decl_specs(tn->decl_specs);
    replace_typedef_name(tn);
    if (!examine_declarator(tn->decl_specs, tn->idl)) {
        /* the type is faulty */
        TypeExp *err_ty;

        err_ty = malloc(sizeof(TypeExp));
        err_ty->op = TOK_ERROR;
        err_ty->child = tn->decl_specs;
        tn->decl_specs = err_ty;
    }
}

void analyze_parameter_declaration(Declaration *d)
{
    TypeExp *scs;

    analyze_decl_specs(d->decl_specs);

    /* 6.7.5.3#2 */
    if ((scs=get_sto_class_spec(d->decl_specs))!=NULL && scs->op!=TOK_REGISTER)
        ERROR(scs, "invalid storage class specifier in parameter declaration");

    replace_typedef_name(d);
    if (d->idl != NULL) {
        int good;
        TypeExp *p;

        good = examine_declarator(d->decl_specs, d->idl);

        /*
         * Perform adjustment on array and function parameters.
         */
        if (d->idl->op == TOK_ID) {
            p = d->idl->child;
            if (p==NULL && get_type_spec(d->decl_specs)->op==TOK_VOID) {
                ERROR(d->idl, "parameter has void type");
                good = FALSE;
            }
            if (good)
                install(d->decl_specs, d->idl);
            else
                install(get_type_node(TOK_ERROR), d->idl);
        } else {
            p = d->idl;
        }
        /* if the type is faulty don't do nothing */
        if (!good)
            return;
        if (p != NULL) {
            if (p->op == TOK_SUBSCRIPT) {
                /* 6.7.5.3#7 */
                p->op = TOK_STAR;
                if (p->attr.e != NULL) {
                    /* >>free the expression corresponding to the size here<< */
                    p->attr.e = NULL;
                }
            } else if (p->op == TOK_FUNCTION) {
                /* 6.7.5.3#8 */
                TypeExp *temp;

                temp = malloc(sizeof(TypeExp));
                *temp = *p;
                p->child = temp;
                p->op = TOK_STAR;
                p->attr.el = NULL;
            }
        }
    }
}

void analyze_function_definition(FuncDef *f)
{
    DeclList *p;
    TypeExp *spec;
    /*Declaration d;*/

    /* 6.9.1#2 (check this before replace typedef names because it is not allowed for the
    identifier of a function definition to inherit its 'functionness' from a typedef name) */
    if (f->header->child==NULL || f->header->child->op!=TOK_FUNCTION)
        ERROR(f->header, "declarator of function definition does not specify a function type");

    /* temporally switch to file scope */
    /*--curr_scope,*/ curr_scope=0, delayed_delete=FALSE;

    /*d.decl_specs = f->decl_specs;
    d.idl =  f->header;
    replace_typedef_name(&d);

    examine_declarator(f->decl_specs, f->header);
    install(f->decl_specs, f->header);*/
    analyze_declarator(f->decl_specs, f->header, TRUE);
    analyze_init_declarator(f->decl_specs, f->header, TRUE);

    /* switch back */
    /*++curr_scope;*/ curr_scope = 1;

    /* 6.9.1#4 */
    if ((spec=get_sto_class_spec(f->decl_specs))!=NULL && spec->op!=TOK_EXTERN && spec->op!=TOK_STATIC)
        ERROR(spec, "invalid storage class `%s' in function definition", token_table[spec->op*2+1]);

    /* check that the function doesn't return an incomplete type */
    if (f->header->child->child == NULL) {
        /* the return type is not a derived declarator type */
        spec = get_type_spec(f->decl_specs);
        if (is_struct_union_enum(spec->op) && !is_complete(spec->str))
            ERROR(spec, "return type is an incomplete type");
    }

    /*
     *  6.9.1#5
     * If the declarator includes a parameter type list, the declaration of each parameter shall
     * include an identifier, except for the special case of a parameter list consisting of a single
     * parameter of type void, in which case there shall not be an identifier.
     *
     *  6.7.5.3#4
     * After adjustment, the parameters in a parameter type list in a function declarator that is
     * part of a definition of that function shall not have incomplete type.
     */
    p = f->header->child->attr.dl;
    if (get_type_spec(p->decl->decl_specs)->op==TOK_VOID) {
        /* foo(void... */
        if (p->decl->idl == NULL) {
            // if (p->next != NULL)
                /* foo(void, more parameters   WRONG */
                // ERROR(p->decl->decl_specs, "`void' must be the first and only parameter");
            // else
                /* foo(void)   OK */
                goto no_params;
        }
    }
    do {
        if (p->decl->idl==NULL || p->decl->idl->op!=TOK_ID)
            ERROR(p->decl->decl_specs, "missing parameter name in function definition");

        if (p->decl->idl->child == NULL) {
            /* not a derived declarator type */
            TypeExp *ts;

            ts = get_type_spec(p->decl->decl_specs);
            if (/*ts->op==TOK_VOID
            || */is_struct_union_enum(ts->op) && !is_complete(ts->str))
                ERROR(p->decl->idl, "parameter `%s' has incomplete type", p->decl->idl->str);
        }
        // printf("%s\n", stringify_type_exp(p->decl));
        p = p->next;
    } while (p!=NULL && p->decl->idl->op!=TOK_ELLIPSIS);
no_params:

    set_return_type(f->decl_specs, f->header->child->child);
}

void enforce_type_compatibility(TypeExp *prev_ds, TypeExp *prev_dct, TypeExp *ds, TypeExp *dct)
{
    char *t1, *t2;
    Declaration d1, d2;

    if (are_compatible(prev_ds, prev_dct, ds, dct, TRUE, TRUE))
        return; /* OK */

    /*
     * Print a pretty diagnostic with the conflicting
     * types of the previous and current declaration.
     */
    d1.decl_specs = prev_ds;
    d1.idl = prev_dct;
    d2.decl_specs = ds;
    d2.idl = dct;

    t1 = stringify_type_exp(&d1, FALSE);
    t2 = stringify_type_exp(&d2, FALSE);

    fprintf(stderr, INFO_COLOR "%s:%d:%d: " ERROR_COLOR "error: " RESET_ATTR,
    dct->info->src_file, dct->info->src_line, dct->info->src_column);
    fprintf(stderr, "conflicting types for `%s'\n", dct->str);
    fprintf(stderr, "\x1b[1;34m=> " RESET_ATTR "previously declared with type `%s'\n", t1);
    fprintf(stderr, "\x1b[1;34m=> " RESET_ATTR "now redeclared with type `%s'\n", t2);
    // exit(EXIT_FAILURE);
}

void analyze_initializer(TypeExp *ds, TypeExp *dct, ExecNode *e, int const_expr)
{
    /*
     * Note: Currently only fully bracketed initialization is handled.
     */

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
                ERROR_R(e, "non-char array initialized from string literal");

            size = strlen(e->attr.str);

            if (dct->attr.e != NULL) {
                /* array with specified bounds */
                if (dct->attr.e->attr.val < size) /* '\0' is only stored when there is room */
                    WARNING(e, "initializer-string for char array is too long");
            } else {
                /* array with unspecified bounds */

                /* complete the array type */
                dct->attr.e = calloc(1, sizeof(ExecNode));
                dct->attr.e->attr.val = size+1; /* make room for '\0' */
            }
        } else {
            if (e->attr.op != TOK_INIT_LIST)
                ERROR_R(e, "invalid array initializer");
            e = e->child[0];

            if (dct->attr.e != NULL) {
                /* array with specified bounds */
                i = dct->attr.e->attr.val;
                for (; e!=NULL && i!=0; e=e->sibling, --i)
                    analyze_initializer(ds, dct->child, e, const_expr);

                if (e != NULL)
                    ERROR_R(e, "excess elements in array initializer");
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
            ERROR_R(e, "excess elements in struct initializer");
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
            ERROR_R(e->sibling, "excess elements in union initializer");
    } else {
        /*
         * Scalar.
         */
        Declaration dest_ty;
scalar:

        if (e->attr.op == TOK_INIT_LIST)
            ERROR_R(e, "braces around scalar initializer");

        if (const_expr)
            /* make sure the initializer is computable at compile time */
            (void)eval_const_expr(e, FALSE);

        /* the same rules as for simple assignment apply */
        dest_ty.decl_specs = ds;
        dest_ty.idl = dct;
        // if (!can_assign_to(e, &dest_ty, &e->type))
        if (!can_assign_to(&dest_ty, e))
            ERROR_R(e, "initializing `%s' with an expression of incompatible type `%s'",
            stringify_type_exp(&dest_ty, FALSE), stringify_type_exp(&e->type, FALSE));
    }
}

/*
int is_incomplete(TypeExp *decl_specs, TypeExp *declarator)
{
    if (declarator->child == NULL) {
        TypeExp *ts;

        ts = get_type_spec(decl_specs);
        if (ts->op==TOK_VOID || is_struct_union_enum(ts->op)&&!is_complete(ts->str))
            return TRUE;
    } else if (declarator->child->op==TOK_SUBSCRIPT && declarator->child->attr.e==NULL) {
        return TRUE;
    }
}
*/
void analyze_init_declarator(TypeExp *decl_specs, TypeExp *declarator, int is_func_def)
{
    TypeExp *scs;
    ExternId *prev;
    int is_func_decl, is_initialized;

    is_func_decl = declarator->child!=NULL && declarator->child->op==TOK_FUNCTION;
    is_initialized = declarator->attr.e!=NULL;
    scs = get_sto_class_spec(decl_specs);

    /*
     * 6.7.8
     * #3 The type of the entity to be initialized shall be an array of unknown size ("[]")
     * or an object type that is not a variable length array type.
     */
    if (is_initialized) {
        if (scs!=NULL &&  scs->op==TOK_TYPEDEF) {
            ERROR(declarator, "trying to initialize typedef");
        } else if (is_func_decl) {
            ERROR(declarator->child, "trying to initialize function type");
        } else if (declarator->child == NULL) {
            TypeExp *ts;

            ts = get_type_spec(decl_specs);
            if (ts->op==TOK_VOID || is_struct_union_enum(ts->op)&&!is_complete(ts->str))
                ERROR(declarator, "trying to initialize variable with incomplete non-array type");
        }
    }

    /* typedef definition? */
    if (scs!=NULL && scs->op==TOK_TYPEDEF)
        return; /* OK */

    if (curr_scope != FILE_SCOPE)
        goto block_scope;

    /*
     * File scope.
     */

    /*
     * 6.9
     * #2 The storage-class specifiers auto and register shall not appear in the declaration
     * specifiers in an external declaration.
     */
    if (scs!=NULL && (scs->op==TOK_AUTO||scs->op==TOK_REGISTER))
        ERROR(scs, "file-scope declaration of `%s' specifies `%s'", declarator->str, token_table[scs->op*2+1]);

    /*
     * 6.7
     * #4 All the expressions in an initializer for an object that has static storage
     * duration shall be constant expressions or string literals.
     */
    if (is_initialized)
        analyze_initializer(decl_specs, declarator->child, declarator->attr.e, TRUE);

    /*
     * Check for redefinition, linkage, and type compatibility.
     */
    if ((prev=lookup_external_id(declarator->str)) == NULL) {
        /* first time seeing this identifier */
        if (is_initialized || is_func_def)
            install_external_id(decl_specs, declarator, DEFINED);
        else if (is_func_decl || scs!=NULL&&scs->op==TOK_EXTERN)
            install_external_id(decl_specs, declarator, REFERENCED);
        else
            install_external_id(decl_specs, declarator, TENTATIVELY_DEFINED);
    } else {
        TypeExp *prev_scs;

        /* check for redefinition */
        if (is_initialized || is_func_def) {
            if (prev->status == DEFINED)
                ERROR_R(declarator, "redefinition of `%s'", declarator->str);
            // prev->declarator->attr.e = declarator->attr.e;
            prev->status = DEFINED;
        }

        /* check linkage */
        prev_scs = get_sto_class_spec(prev->decl_specs);
        if (prev_scs == NULL) {
            if (scs!=NULL && scs->op==TOK_STATIC)
                /* e.g. int x; ==> static int x; */
                goto static_follows_non_static;
        } else if (prev_scs->op == TOK_EXTERN) {
            if (scs != NULL) {
                if (scs->op==TOK_STATIC)
                    /* e.g. extern int x; ==> static int x; */
                    goto static_follows_non_static;
            } else if (!is_func_decl && prev->status!=DEFINED) {
                /* e.g. extern int x; ==> int x; */
                prev->status = TENTATIVELY_DEFINED;
            }
        } else if (prev_scs->op == TOK_STATIC) {
            if (scs==NULL && !is_func_decl)
                /* e.g. static int x; ==> int x; */
                goto non_static_follows_static;
        }

        /* check type compatibility */
        enforce_type_compatibility(prev->decl_specs, prev->declarator, decl_specs, declarator);
    }

    /*
     * 6.9.2
     * #3 If the declaration of an identifier for an object is a tentative definition and has internal
     * linkage, the declared type shall not be an incomplete type.
     */
    // if (scs!=NULL && scs->op==TOK_STATIC

    return; /* end file scope */
static_follows_non_static:
    ERROR_R(declarator, "static declaration of `%s' follows non-static declaration", declarator->str);
non_static_follows_static:
    ERROR_R(declarator, "non-static declaration of `%s' follows static declaration", declarator->str);

block_scope:
    /*
     * Block scope.
     */

    /*
     * 6.7.1
     * #5 The declaration of an identifier for a function that has block scope shall have
     * no explicit storage-class specifier other than extern.
     */
    if (is_func_decl && scs!=NULL && scs->op!=TOK_TYPEDEF && scs->op!=TOK_EXTERN)
        ERROR(declarator->child, "function `%s' declared in block scope cannot have `%s' storage class",
        declarator->str, token_table[scs->op*2+1]);

    if (is_initialized) {
        /*
         * 6.7.8
         * #5 If the declaration of an identifier has block scope, and the identifier has
         * external or internal linkage, the declaration shall have no initializer for the
         * identifier.
         */
        if (scs!=NULL && scs->op==TOK_EXTERN)
            ERROR(declarator, "`extern' variable cannot have an initializer");

        analyze_initializer(decl_specs, declarator->child, declarator->attr.e,
        (scs!=NULL&&scs->op==TOK_STATIC)?TRUE:FALSE);
    }

    if (scs!=NULL && scs->op==TOK_EXTERN || is_func_decl) {
        if ((prev=lookup_external_id(declarator->str)) == NULL)
            /* first time seeing this identifier */
            install_external_id(decl_specs, declarator, REFERENCED);
        else
            enforce_type_compatibility(prev->decl_specs, prev->declarator, decl_specs, declarator);
    } else {
        /*
         * 6.7
         * #7 If an identifier for an object is declared with no linkage, the type for
         * the object shall be complete by the end of its declarator, or by the end of
         * its init-declarator if it has an initializer;
         */
        if (declarator->child == NULL) {
            TypeExp *ts;

            ts = get_type_spec(decl_specs);
            if (ts->op==TOK_VOID || is_struct_union_enum(ts->op)&&!is_complete(ts->str))
                goto no_link_incomp;
        } else if (declarator->child->op==TOK_SUBSCRIPT && declarator->child->attr.e==NULL) {
            goto no_link_incomp;
        }
    }

    return; /* end block scope */
no_link_incomp:
    ERROR_R(declarator, "`%s' has no linkage and incomplete type", declarator->str);
}

void analyze_struct_declarator(TypeExp *sql, TypeExp *declarator)
{
    /* 6.7.2.1
     * #2 A structure or union shall not contain a member with incomplete or function type (hence,
     * a structure shall not contain an instance of itself, but may contain a pointer to an instance
     * of itself) [we don't support flexible array members, so what follows is not important to us]
     */
    if (!analyze_declarator(sql, declarator, FALSE))
        exit(EXIT_FAILURE);
    if (declarator->child == NULL) {
        /* not a derived declarator type */
        TypeExp *ts;

        ts = get_type_spec(sql);
        if (is_struct_union_enum(ts->op) && !is_complete(ts->str))
            goto incomp_error;
    } else if (declarator->child->op == TOK_SUBSCRIPT) {
        /* the type category is array, the size expression cannot be missing */
        if (declarator->child->attr.e == NULL)
            goto incomp_error;
    } else if (declarator->child->op == TOK_FUNCTION) {
        FATAL_ERROR(declarator, "member `%s' declared as a function", declarator->str);
    }
    return; /* OK */
incomp_error:
    FATAL_ERROR(declarator, "member `%s' has incomplete type", declarator->str);
}

void check_for_dup_member(DeclList *d)
{
#define MEM_TAB_SIZE 53
    typedef struct Member Member;
    static struct Member {
        char *id;
        Member *next;
    } *members[MEM_TAB_SIZE], *np, *temp;
    TypeExp *dct;
    unsigned h;

    // memset(members, 0, sizeof(Member *)*MEM_TAB_SIZE);
    /* traverse the struct declaration list */
    while (d != NULL) {
        /* traverse the struct declarator list */
        for (dct = d->decl->idl; dct != NULL; dct = dct->sibling) {
            /* search member */
            h = hash(dct->str)%MEM_TAB_SIZE;
            for (np = members[h]; np != NULL; np = np->next)
                if (strcmp(dct->str, np->id) == 0)
                    FATAL_ERROR(dct, "duplicate member `%s'", dct->str);
            /* not found */
            np = malloc(sizeof(Member));
            np->id = dct->str;
            np->next = members[h];
            members[h] = np;
        }
        d = d->next;
    }

    /* empty table */
    for (h = 0; h < MEM_TAB_SIZE; h++) {
        if (members[h] != NULL) {
            for (np = members[h]; np != NULL;) {
                temp = np;
                np = np->next;
                free(temp);
            }
            members[h] = NULL;
        }
    }
}

char *stringify_type_exp(Declaration *d, int show_decayed)
{
    TypeExp *e;
    char out[256], temp[256], ds[128], *s;

    out[0]='\0', temp[0]='\0', ds[0]='\0';

    e = d->decl_specs;
    while (e != NULL) {
        if (e->op) {
            strcat(ds, token_table[e->op*2+1]);
            if (is_struct_union_enum(e->op)) {
                strcat(ds, " ");
                strcat(ds, (e->str!=NULL)?e->str:"<anonymous>");
            }
            if (e->child != NULL) {
                strcat(ds, " ");
            } else if (d->idl != NULL) {
                if (d->idl->op == TOK_ID) {
                    if (d->idl->child != NULL)
                        strcat(ds, " ");
                } else {
                    strcat(ds, " ");
                }
            }
        }
        e = e->child;
    }

    e = d->idl;
    while (e != NULL) {
        if (e->op == TOK_FUNCTION) {
            DeclList *p;

            if (show_decayed)
                strcat(out, "(*)(");
            else
                strcat(out, "(");
            p = e->attr.dl;
            while (p != NULL) {
                char *param;

                param = stringify_type_exp(p->decl, FALSE);
                strcat(out, param);
                free(param);

                p = p->next;
                if (p != NULL)
                    strcat(out, ", ");
            }
            strcat(out, ")");
        } else if (e->op == TOK_SUBSCRIPT) {
            /*if (e->attr.e != NULL)
                sprintf(temp, "%s[%ld]", out, e->attr.e->attr.val);
            else
                sprintf(temp, "%s[]", out);
            strcpy(out, temp);*/
            if (show_decayed) {
                if (e->child!=NULL && (e->child->op==TOK_SUBSCRIPT || e->child->op==TOK_FUNCTION))
                    strcpy(temp, "(*)");
                else
                    strcpy(temp, "*");
            } else {
                if (e->attr.e != NULL)
                    sprintf(temp, "[%ld]", e->attr.e->attr.val);
                else
                    strcpy(temp, "[]");
            }
            strcat(out, temp);
        } else if (e->op == TOK_STAR) {
            if (e->child!=NULL && (e->child->op==TOK_SUBSCRIPT || e->child->op==TOK_FUNCTION)) {
                if (e->attr.el != NULL)
                    sprintf(temp, "(*%s%s)", token_table[e->attr.el->op*2+1], out);
                else
                    sprintf(temp, "(*%s)", out);
            } else {
                if (e->attr.el != NULL)
                    sprintf(temp, "*%s%s", token_table[e->attr.el->op*2+1], out);
                else
                    sprintf(temp, "*%s", out);
            }
            strcpy(out, temp);
        } else if (e->op == TOK_ID) {
            /* ... */
        } else if (e->op == TOK_ELLIPSIS) {
            strcpy(out, "...");
        }
        e = e->child;
        show_decayed = FALSE;
    }
    s = malloc(strlen(ds)+strlen(out)+1);
    strcpy(s, ds);
    strcat(s, out);

    return s;
}
