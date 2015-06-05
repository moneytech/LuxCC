#include "decl.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "util.h"
#include "expr.h"
#include "stmt.h"
#include "arena.h"
#include "imp_lim.h"
#include "error.h"

extern unsigned error_count;
extern int colored_diagnostics;

#define ERROR(tok, ...) emit_error(FALSE, (tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column, __VA_ARGS__)

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

#define WARNING(tok, ...) emit_warning((tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column, __VA_ARGS__)
#define FATAL_ERROR(tok, ...) emit_error(TRUE, (tok)->info->src_file, (tok)->info->src_line, (tok)->info->src_column, __VA_ARGS__)

#define HASH_SIZE       4093
#define FILE_SCOPE      0
#define OUTERMOST_LEVEL 0
#define HASH_VAL(s)     (hash(s)%HASH_SIZE)
#define HASH_VAL2(x)    (hash2(x)%HASH_SIZE)

static ExternId *external_declarations[HASH_SIZE];

/* Note: messes up the table! */
ExternId *get_extern_symtab(void)
{
    int i;
    ExternId *first, *last;

    for (i = 0; i < HASH_SIZE; i++) {
        if (external_declarations[i] != NULL) {
            first = external_declarations[i];
            for (last = external_declarations[i]; last->next != NULL; last = last->next);
            break;
        }
    }

    for (i = i+1; i < HASH_SIZE; i++)
        if (external_declarations[i] != NULL)
            for (last->next = external_declarations[i]; last->next != NULL; last = last->next);

    return first;
}

/*
 * Lexically scoped symbol tables that implement ordinary
 * identifiers and tags name spaces.
 * The symbol table that implements label names is in stmt.c.
 * Structure and union members are handled in a special way.
 */
static Symbol *ordinary_identifiers[MAX_NEST][HASH_SIZE];
static TypeTag *tags[MAX_NEST][HASH_SIZE];

static int nesting_level = OUTERMOST_LEVEL;
static int delayed_delete = FALSE;
static int scope_id;

/* memory arenas used to maintain identifier and tag scopes */
static Arena *oids_arena[MAX_NEST];
static Arena *tags_arena[MAX_NEST];

void init_symbol_tables(void)
{
    enum {
        OIDS_ARENA_SIZE = 8192,
        TAGS_ARENA_SIZE = 4096
    };
    int i;

    for (i = 0; i < MAX_NEST; i++) {
        oids_arena[i] = arena_new(OIDS_ARENA_SIZE);
        tags_arena[i] = arena_new(TAGS_ARENA_SIZE);
    }
}

static Symbol *new_symbol(void)
{
    void *p;

    if ((p=arena_alloc(oids_arena[nesting_level], sizeof(Symbol))) == NULL)
        assert(0); /* TODO: print "out of memory" or something */

    return p;
}

static TypeTag *new_tag(void)
{
    void *p;

    if ((p=arena_alloc(tags_arena[nesting_level], sizeof(TypeTag))) == NULL)
        assert(0); /* TODO: print "out of memory" or something */

    return p;
}

static int is_sto_class_spec(Token t)
{
    switch (t) {
    case TOK_TYPEDEF:
    case TOK_EXTERN:
    case TOK_STATIC:
    case TOK_AUTO:
    case TOK_REGISTER:
        return TRUE;
    default:
        return FALSE;
    }
}

/*
 * Return TRUE if `t' is a type specifier.
 */
static int is_type_spec(Token t)
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
static int is_type_spec2(Token t)
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

static int is_type_qualifier(Token t)
{
    return (t==TOK_CONST || t==TOK_VOLATILE);
}

int is_struct_union_enum(Token t)
{
    return (t==TOK_STRUCT||t==TOK_UNION||t==TOK_ENUM);
}

TypeExp *get_sto_class_spec(TypeExp *d)
{
    while (d != NULL) {
        if (is_sto_class_spec(d->op))
            break;
        d = d->child;
    }

    return d;
}

TypeExp *get_type_spec(TypeExp *d)
{
    while (d != NULL) {
        if (is_type_spec2(d->op))
            break;
        d = d->child;
    }

    assert(d != NULL); /* a type specifier is required */

    return d;
}

TypeExp *get_type_qual(TypeExp *d)
{
    while (d != NULL) {
        if (is_type_qualifier(d->op) || d->op==TOK_CONST_VOLATILE)
            break;
        d = d->child;
    }

    return d;
}

/* pop_scope() just set a flag. This function performs the actual delete. */
static void delete_scope(void)
{
    /*int i;
    Symbol *np, *temp;
    TypeTag *np2, *temp2;*/

    /* test for underflow */
    assert(nesting_level >= 0);

    memset(&ordinary_identifiers[nesting_level][0], 0, sizeof(Symbol *)*HASH_SIZE);
    arena_reset(oids_arena[nesting_level]);

    memset(&tags[nesting_level][0], 0, sizeof(TypeTag *)*HASH_SIZE);
    arena_reset(tags_arena[nesting_level]);

    /*for (i = 0; i < HASH_SIZE; i++) {
        if (ordinary_identifiers[nesting_level][i] != NULL) {
            for (np = ordinary_identifiers[nesting_level][i]; np != NULL;) {
                temp = np;
                np = np->next;
                free(temp);
            }
            ordinary_identifiers[nesting_level][i] = NULL;
        }
        if (tags[nesting_level][i] != NULL) {
            for (np2 = tags[nesting_level][i]; np2 != NULL;) {
                temp2 = np2;
                np2 = np2->next;
                free(temp2);
            }
            tags[nesting_level][i] = NULL;
        }
    }*/
    --nesting_level;
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

    if (++nesting_level == MAX_NEST) /* overflow */
        TERMINATE("Error: too many nested scopes (>= %d)", MAX_NEST);

    ++scope_id; /* create a new ID for this scope */
}

void pop_scope(void)
{
    if (delayed_delete)
        delete_scope();

    delayed_delete = TRUE;
}


TypeTag *lookup_tag(char *id, int all)
{
    int n;
    unsigned h;
    TypeTag *np;

    if (delayed_delete)
        delete_scope();

    n = nesting_level;
    h = HASH_VAL(id);
    if (all == TRUE) {
        for (; n >= 0; n--)
            for (np = tags[n][h]; np != NULL; np = np->next)
                if (equal(id, np->type->str))
                    return np;
        return NULL; /* not found */
    } else {
        for (np = tags[n][h]; np != NULL; np = np->next)
            if (equal(id, np->type->str))
                return np;
        return NULL; /* not found */
    }
}

void install_tag(TypeExp *ty)
{
    /* Note: the parser already take care of redefinitions */

    unsigned h;
    TypeTag *np;

    DEBUG_PRINTF("new tag `%s', nesting level: %d\n", ty->str, nesting_level);

    if (delayed_delete)
        delete_scope();

    // np = malloc(sizeof(TypeTag));
    np = new_tag();
    np->type = ty;
    h = HASH_VAL(ty->str);
    np->next = tags[nesting_level][h];
    tags[nesting_level][h] = np;
}


Symbol *lookup(char *id, int all)
{
    int n;
    Symbol *np;
    unsigned h;

    if (delayed_delete)
        delete_scope();

    n = nesting_level;
    h = HASH_VAL(id);
    if (all == TRUE) {
        for (; n >= 0; n--)
            for (np = ordinary_identifiers[n][h]; np != NULL; np = np->next)
                if (equal(id, np->declarator->str))
                    return np;
        return NULL; /* not found */
    } else {
        for (np = ordinary_identifiers[n][h]; np != NULL; np = np->next)
            if (equal(id, np->declarator->str))
                return np;
        return NULL; /* not found */
    }
}

static
void install(TypeExp *decl_specs, TypeExp *declarator, int is_param)
{
    Symbol *np;
    unsigned h;
    TypeExp *scs;
    Token curr_scs, prev_scs;

    if (delayed_delete)
        delete_scope();

    h = HASH_VAL(declarator->str);
    for (np = ordinary_identifiers[nesting_level][h]; np != NULL; np = np->next)
        if (equal(declarator->str, np->declarator->str))
            break;

    if (np == NULL) {
        /* not found in this scope */
        // np = malloc(sizeof(Symbol));
        np = new_symbol();
        np->decl_specs = decl_specs;
        np->declarator = declarator;
        np->is_param = (short)is_param;
        np->nesting_level = (short)nesting_level;
        np->scope = scope_id;
        np->next = ordinary_identifiers[nesting_level][h];
        ordinary_identifiers[nesting_level][h] = np;
        return;
    }

    /*
     * Already in this scope.
     * Just check for errors.
     * Do not replace the previous declaration.
     */

    if (decl_specs->op == TOK_ERROR)
        /* the type of the new declaration is faulty */
        return;

    curr_scs = (scs=get_sto_class_spec(decl_specs))!=NULL ? scs->op:0;
    prev_scs = (scs=get_sto_class_spec(np->decl_specs))!=NULL ? scs->op:0;

    if (declarator->op==TOK_ENUM_CONST || curr_scs==TOK_TYPEDEF) {
        /*
         * Clash while trying to install an enumeration constant or typedef name.
         */
        if (declarator->op==TOK_ENUM_CONST && np->declarator->op==TOK_ENUM_CONST)
            ERROR_R(declarator, "redeclaration of enumerator `%s'", declarator->str);
        else if (curr_scs==TOK_TYPEDEF && prev_scs==TOK_TYPEDEF)
            ERROR_R(declarator, "redefinition of typedef `%s'", declarator->str);
        else
            goto redecl_as_diff_kind;
    } else if (np->declarator->op==TOK_ENUM_CONST || prev_scs==TOK_TYPEDEF) {
        /*
         * Clash with previously declared enumeration constant or typedef name.
         */
        goto redecl_as_diff_kind;
    } else if (nesting_level != OUTERMOST_LEVEL) {
        int curr_is_func, prev_is_func;

        /*
         * If one of the declarations declares a function, just make sure the
         * other one declares a function too and return. analyze_init_declarator()
         * will check for linkage and type agreement.
         */
        curr_is_func = declarator->child!=NULL && declarator->child->op==TOK_FUNCTION;
        prev_is_func = np->declarator->child!=NULL && np->declarator->child->op==TOK_FUNCTION;
        if (curr_is_func || prev_is_func) {
            if (curr_is_func != prev_is_func)
                goto redecl_as_diff_kind;
            else
                return; /* OK by now */
        }

        /*
         * Check linkage of objects.
         */
        if (!curr_scs || curr_scs!=TOK_EXTERN) {
            if (!prev_scs || prev_scs!=TOK_EXTERN)
                /* e.g. int x; ==> int x; */
                ERROR_R(declarator, "redeclaration of `%s' with no linkage", declarator->str);
            else /* if (prev_scs == TOK_EXTERN) */
                /* e.g. extern int x; ==> int x; */
                ERROR_R(declarator, "declaration of `%s' with no linkage follows extern declaration",
                declarator->str);
        } else if (!prev_scs || prev_scs!=TOK_EXTERN) {
            /* e.g. int x; ==> extern int x; */
            ERROR_R(declarator, "extern declaration of `%s' follows declaration with no linkage",
            declarator->str);
        }
    }
    return;
redecl_as_diff_kind:
    /*
     * This is the diagnostic given by gcc and clang
     * when an ordinary identifier that denotes
     * - A function,
     * - An enumeration constant,
     * - A typedef name, or
     * - An object/variable
     * is redeclared as denoting a different kind.
     */
    ERROR_R(declarator, "`%s' redeclared as different kind of symbol", declarator->str);
}


ExternId *lookup_external_id(char *id)
{
    ExternId *np;

    for (np = external_declarations[HASH_VAL(id)]; np != NULL; np = np->next)
        if (equal(id, np->declarator->str))
            return np;
    return NULL; /* not found */
}

static
void install_external_id(TypeExp *decl_specs, TypeExp *declarator, ExtIdStatus status)
{
    ExternId *np;
    unsigned h;

    np = malloc(sizeof(ExternId));
    np->decl_specs = decl_specs;
    np->declarator = declarator;
    np->status = status;
    h = HASH_VAL(declarator->str);
    np->next = external_declarations[h];
    external_declarations[h] = np;
}

int is_external_id(char *id)
{
    return lookup_external_id(id)!=NULL;
}

/* set attributes to an identifier node */
void set_attributes(ExecNode *e, Symbol *sym)
{
    TypeExp *scs;

    /* set type */
    e->type.decl_specs = sym->decl_specs;
    e->type.idl = (sym->declarator->op!=TOK_ENUM_CONST)?sym->declarator->child:sym->declarator;

    /* set scope (the identifier/scope pair is used to unambiguously identify the object) */
    if (sym->nesting_level != OUTERMOST_LEVEL)
        e->attr.var.scope = sym->scope;
    else
        e->attr.var.scope = FILE_SCOPE;

    scs = get_sto_class_spec(e->type.decl_specs);

    /* set storage duration */
    if (sym->nesting_level==OUTERMOST_LEVEL || scs!=NULL&&(scs->op==TOK_EXTERN||scs->op==TOK_STATIC))
        e->attr.var.duration = DURATION_STATIC;
    else
        e->attr.var.duration = DURATION_AUTO;

    /* set linkage */
    if (scs == NULL) {
        if (sym->nesting_level==OUTERMOST_LEVEL || get_type_category(&e->type)==TOK_FUNCTION)
            e->attr.var.linkage = LINKAGE_EXTERNAL;
        else
            e->attr.var.linkage = LINKAGE_NONE;
    } else if (scs->op == TOK_EXTERN) {
        e->attr.var.linkage = LINKAGE_EXTERNAL;
    } else if (scs->op==TOK_STATIC && sym->nesting_level==OUTERMOST_LEVEL) {
        e->attr.var.linkage = LINKAGE_INTERNAL;
    } else {
        e->attr.var.linkage = LINKAGE_NONE;
    }

    e->attr.var.is_param = (char)sym->is_param;
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

int is_typedef_name(char *id)
{
    Symbol *np;

    if ((np=lookup(id, TRUE)) != NULL) {
        TypeExp *scs;

        if ((scs=get_sto_class_spec(np->decl_specs))!=NULL && scs->op==TOK_TYPEDEF)
            return TRUE;
    }
    return FALSE;
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
        Token ty;

        ty = get_type_category(&e->attr.e->type);
        if (!is_integer(ty)) {
            ERROR(e, "enumerator value is not an integer constant");
            goto error;
        }
        en_val = eval_int_const_expr(e->attr.e);
    } else {
        e->attr.e = calloc(1, sizeof(ExecNode));
        if (en_val+1 < en_val)
            WARNING(e, "overflow in enumeration value");
        ++en_val;
    }

    e->attr.e->attr.val = en_val;
    // printf("en_val=%ld\n", en_val);
error:
    install(&enum_ds, e, FALSE);
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
            if (dct1->attr.e==NULL && dct2->attr.e!=NULL)
                dct1->attr.e = dct2->attr.e;
            else if (dct2->attr.e==NULL && dct1->attr.e!=NULL)
                dct2->attr.e = dct1->attr.e;
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

    if (tag[0] == '<') /* <anonymous> struct/union/enum */
        return TRUE;

    tp = lookup_tag(tag, TRUE);
    if (tp != NULL) {
        if (tp->type->op == TOK_ENUM)
            return tp->type->attr.el!=NULL;
        else
            return tp->type->attr.dl!=NULL;
    } else {
        assert(0);
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

            size = eval_int_const_expr(declarator->attr.e);
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

static TokenNode *typedef_name_info;

static
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
    new_node->info = typedef_name_info;
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

static
void replace_typedef_name(Declaration *decl)
{
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
    /* new created nodes will have the same file/line/column information as the typedef name node */
    typedef_name_info = ts->info;
    /* replace the typedef name node for the type specifier node of the typedef name */
    temp = ts->child;
    *ts = *get_type_spec(s->decl_specs);
    ts->child = temp;
    ts->info = typedef_name_info;

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
    temp->op = 0; /* no declaration specifier has value zero */
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
            install(decl_specs, declarator, FALSE);
        else
            install(get_type_node(TOK_ERROR), declarator, FALSE);
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
                install(d->decl_specs, d->idl, TRUE);
            else
                install(get_type_node(TOK_ERROR), d->idl, TRUE);
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
                    free_expression_tree(p->attr.e);
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

/*
 * Variable used to implement __func__.
 * Modified only by analyze_function_definition().
 * Used only by primary_expression().
 */
char *current_function_name;

void analyze_function_definition(Declaration *f)
{
    int good;
    DeclList *p;
    TypeExp *spec;

    /*
     * 6.9.1#2
     * The identifier declared in a function definition (which is the name of the function) shall
     * have a function type, as specified by the declarator portion of the function definition.
     *
     * Note: check this before replace typedef names because it is not allowed for the identifier
     * of a function definition to inherit its 'functionness' from a typedef name.
     */
    if (f->idl->child==NULL || f->idl->child->op!=TOK_FUNCTION) {
        set_return_type(get_type_node(TOK_ERROR), NULL);
        ERROR_R(f->idl, "declarator of function definition does not specify a function type");
    }
    current_function_name = f->idl->str;

    assert(nesting_level == OUTERMOST_LEVEL+1);

    /* temporally switch to file scope */
    nesting_level=OUTERMOST_LEVEL, delayed_delete=FALSE;

    good = analyze_declarator(f->decl_specs, f->idl, TRUE);
    if (good)
        analyze_init_declarator(f->decl_specs, f->idl, TRUE);

    /* switch back */
    nesting_level = OUTERMOST_LEVEL+1;

    if (!good)
        return;

    /*
     * 6.9.1#4
     * The storage-class specifier, if any, in the declaration specifiers shall be either
     * extern or static.
     */
    if ((spec=get_sto_class_spec(f->decl_specs))!=NULL && spec->op!=TOK_EXTERN && spec->op!=TOK_STATIC)
        ERROR(spec, "invalid storage class `%s' in function definition", token_table[spec->op*2+1]);

    /* check that the function doesn't return an incomplete type */
    if (f->idl->child->child == NULL) {
        /* the return type is not a derived declarator type */
        spec = get_type_spec(f->decl_specs);
        if (is_struct_union_enum(spec->op) && !is_complete(spec->str)) {
            ERROR(spec, "return type is an incomplete type");
            good = FALSE;
        }
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
    p = f->idl->child->attr.dl;
    if (get_type_spec(p->decl->decl_specs)->op==TOK_VOID)
        if (p->decl->idl == NULL)
            goto no_params;
    /*
     * The function has parameters, enforce the above constraints.
     */
    do {
        if (p->decl->idl==NULL || p->decl->idl->op!=TOK_ID)
            ERROR(p->decl->decl_specs, "missing parameter name in function definition");

        if (p->decl->idl->child == NULL) {
            /* not a derived declarator type */
            TypeExp *ts;

            ts = get_type_spec(p->decl->decl_specs);
            if (is_struct_union_enum(ts->op) && !is_complete(ts->str))
                ERROR(p->decl->idl, "parameter `%s' has incomplete type", p->decl->idl->str);
        }
        p = p->next;
    } while (p!=NULL && p->decl->idl->op!=TOK_ELLIPSIS);
no_params:
    set_return_type(f->decl_specs, f->idl->child->child);
}

static
void enforce_type_compatibility(TypeExp *prev_ds, TypeExp *prev_dct, TypeExp *ds, TypeExp *dct)
{
    char *ty1, *ty2;
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

    ty1 = stringify_type_exp(&d1, FALSE);
    ty2 = stringify_type_exp(&d2, FALSE);

    if (colored_diagnostics) {
        fprintf(stderr, INFO_COLOR "%s:%d:%d: " ERROR_COLOR "error: " RESET_ATTR,
        dct->info->src_file, dct->info->src_line, dct->info->src_column);
        fprintf(stderr, "conflicting types for `%s'\n", dct->str);
        fprintf(stderr, "\x1b[1;34m=> " RESET_ATTR "previously declared with type `%s'\n", ty1);
        fprintf(stderr, "\x1b[1;34m=> " RESET_ATTR "now redeclared with type      `%s'\n", ty2);
    } else {
        fprintf(stderr, "%s:%d:%d: error: ", dct->info->src_file, dct->info->src_line, dct->info->src_column);
        fprintf(stderr, "conflicting types for `%s'\n", dct->str);
        fprintf(stderr, "=> previously declared with type `%s'\n", ty1);
        fprintf(stderr, "=> now redeclared with type      `%s'\n", ty2);
    }
    free(ty1), free(ty2);

    ++error_count;
}

/*
 * See if the expression `e' is adequate to initialize
 * an object with static storage duration.
 * Return TRUE if OK, FALSE otherwise.
 * Note: this function is incomplete.
 */
static int analyze_static_initializer(ExecNode *e, int is_addr)
{
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
            return analyze_static_initializer(e->child[0], is_addr);

        case TOK_SIZEOF:
            return TRUE;
        case TOK_ADDRESS_OF:
            return analyze_static_initializer(e->child[0], TRUE);
        case TOK_ARROW:
        case TOK_INDIRECTION:
            break;
        case TOK_UNARY_PLUS:
        case TOK_UNARY_MINUS:
        case TOK_COMPLEMENT:
        case TOK_NEGATION:
        case TOK_CAST:
            return analyze_static_initializer(e->child[0], FALSE);

#define C0() analyze_static_initializer(e->child[0], FALSE)
#define C1() analyze_static_initializer(e->child[1], FALSE)
#define C2() analyze_static_initializer(e->child[2], FALSE)
        case TOK_MUL:
        case TOK_DIV:
        case TOK_MOD:
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_LSHIFT:
        case TOK_RSHIFT:
        case TOK_LT:
        case TOK_GT:
        case TOK_LET:
        case TOK_GET:
        case TOK_EQ:
        case TOK_NEQ:
        case TOK_BW_AND:
        case TOK_BW_XOR:
        case TOK_BW_OR:
        case TOK_AND: /* TOFIX: short-circuit the analysis */
        case TOK_OR:  /* TOFIX: short-circuit the analysis */
            return C0() && C1();
        case TOK_CONDITIONAL: /* TOFIX: only analyze C1 or C2 */
            return C0() && C1() && C2();
#undef C0
#undef C1
#undef C2
        }
        break;
    case IConstExp:
    case StrLitExp:
        return TRUE;
    case IdExp:
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
        return TRUE;
    }

    emit_error(FALSE, e->info->src_file, e->info->src_line, e->info->src_column,
    "invalid initializer for static object");
    return FALSE;
}

static
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
            // ts = lookup_tag(ts->str, TRUE)->type;
            ts->attr.dl = lookup_tag(ts->str, TRUE)->type->attr.dl;
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
            // ts = lookup_tag(ts->str, TRUE)->type;
            ts->attr.dl = lookup_tag(ts->str, TRUE)->type->attr.dl;
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

        if (const_expr && !analyze_static_initializer(e, FALSE))
            return;

        if (get_type_category(&e->type) == TOK_ERROR)
            return;

        /* the same rules as for simple assignment apply */
        dest_ty.decl_specs = ds;
        dest_ty.idl = dct;
        if (!can_assign_to(&dest_ty, e)) {
            char *ty1, *ty2;

            ty1 = stringify_type_exp(&dest_ty, FALSE);
            ty2 = stringify_type_exp(&e->type, FALSE);
            ERROR(e, "initializing `%s' with an expression of incompatible type `%s'", ty1, ty2);
            free(ty1), free(ty2);
            return;
        }
    }
}

void analyze_init_declarator(TypeExp *decl_specs, TypeExp *declarator, int is_func_def)
{
    TypeExp *scs;
    ExternId *prev;
    int is_func_decl, is_initialized;

    is_func_decl = declarator->child!=NULL && declarator->child->op==TOK_FUNCTION;
    is_initialized = declarator->attr.e!=NULL && !is_func_def;
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

    if (nesting_level != OUTERMOST_LEVEL)
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
            // prev->declarator = declarator;
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

        /* update previous declaration */
        if (is_initialized || is_func_def)
            prev->declarator = declarator;
    }

    /*
     * 6.9.2
     * #3 If the declaration of an identifier for an object is a tentative definition and has internal
     * linkage, the declared type shall not be an incomplete type.
     *
     * See: http://www.open-std.org/jtc1/sc22/wg14/www/docs/dr_016.html
     * TODO: after the whole translation unit is processed, a final traverse
     * over the external symbols table should check that the type of the
     * non-extern identifiers is not an incomplete type.
     */

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


/*
 *
 * TODO: handle errors more gracefully.
 *
 */

#define MAX_DESCR_STACK 64

static StructDescriptor *struct_descriptor_table[HASH_SIZE];
static StructDescriptor *descriptor_stack[MAX_DESCR_STACK];
static int descr_stack_top = -1;

/*
 * Add a new member to the top descriptor.
 */
void new_struct_member(TypeExp *decl_specs, TypeExp *declarator)
{
    unsigned alignment;
    StructMember *n, *p;

    /* before add, check for duplicate */
    for (p = descriptor_stack[descr_stack_top]->members; p != NULL; p = p->next)
        if (equal(declarator->str, p->id))
            FATAL_ERROR(declarator, "duplicate member `%s'", declarator->str);

    n = malloc(sizeof(StructMember));
    /* set tag and type */
    n->id = declarator->str;
    n->type.decl_specs = decl_specs;
    n->type.idl = declarator->child;
    /* set size */
    n->size = compute_sizeof(&n->type);
    /* set an offset that met with the alignment requirements of the member's type */
    alignment = get_alignment(&n->type);
    n->offset = round_up(descriptor_stack[descr_stack_top]->size, alignment);
    /* update struct's overall size */
    descriptor_stack[descr_stack_top]->size = n->offset+n->size;
    /* update struct's alignment */
    if (alignment > descriptor_stack[descr_stack_top]->alignment)
        descriptor_stack[descr_stack_top]->alignment = alignment;
    /* append member */
    n->next = descriptor_stack[descr_stack_top]->members;
    descriptor_stack[descr_stack_top]->members = n;
}

void push_struct_descriptor(TypeExp *ty)
{
    int i;
    char *tag;
    StructDescriptor *n;

    tag = ty->str;

    /*
     * Check for the tricky case where the struct is redefined inside itself.
     * For example
     *      struct A {
     *          struct A {
     *              int x;
     *          } m;
     *      };
     */
    for (i = descr_stack_top; i >= 0; i--)
        if (*tag!='<' && equal(tag, descriptor_stack[i]->tag))
            FATAL_ERROR(ty, "nested redefinition of `%s %s'", token_table[ty->op*2+1], tag);

    /* push new descriptor */
    n = malloc(sizeof(StructDescriptor));
    n->tag = tag;
    n->size = n->alignment = 0;
    n->members = NULL;
    descriptor_stack[++descr_stack_top] = n;
}

void pop_struct_descriptor(void)
{
    char *tag;
    unsigned h;
    StructDescriptor *n;

    /*
     * Before pop, fetch the descriptor to add it to the struct descriptor table.
     */
    n = descriptor_stack[descr_stack_top];
    --descr_stack_top;
    tag = n->tag;

    /* adjust the overall size to met with alignment requirements */
    n->size = round_up(n->size, n->alignment);

    /*
     * Note that the address itself is hashed and not the string it points to.
     * Because of scope rules the tag does not uniquely identify the struct type.
     */
    h = HASH_VAL2((unsigned long)tag);
    n->next = struct_descriptor_table[h];
    struct_descriptor_table[h] = n;
}

StructDescriptor *lookup_struct_descriptor(char *tag)
{
    StructDescriptor *np;

    for (np = struct_descriptor_table[HASH_VAL2((unsigned long)tag)]; np != NULL; np = np->next)
        if (tag == np->tag)
            break;

    assert(np != NULL);

    return np;
}

StructMember *get_member_descriptor(TypeExp *ty, char *id)
{
    StructMember *m;

    m = lookup_struct_descriptor(ty->str)->members;
    while (m != NULL) {
        if (equal(id, m->id))
            return m;
        m = m->next;
    }

    assert(0);
}

void analyze_struct_declarator(TypeExp *sql, TypeExp *declarator)
{
    /* 6.7.2.1
     * #2 A structure or union shall not contain a member with incomplete or function type (hence,
     * a structure shall not contain an instance of itself, but may contain a pointer to an instance
     * of itself) [we don't support flexible array members, so what follows is not important to us]
     */
    if (!analyze_declarator(sql, declarator, FALSE))
        FATAL_ERROR(declarator, "faulty struct/union member");
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
    new_struct_member(sql, declarator);
    return; /* OK */
incomp_error:
    FATAL_ERROR(declarator, "member `%s' has incomplete type", declarator->str);
}

/*
 * If show_decayed is TRUE, array are shown as pointers to the element
 * type and function designators are shown as pointers to the function
 * they designate.
 */
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
                strcat(ds, e->str);
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
