#include "parser.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "util.h"
#include "decl.h"
#include "expr.h"
#include "stmt.h"
#include "error.h"

static TokenNode *curr_tok;
unsigned number_of_ast_nodes;

/*
 * All syntax errors are fatal.
 * No attempt of recovery is made.
 */
#define ERROR(...) emit_error(TRUE, curr_tok->src_file, curr_tok->src_line, curr_tok->src_column, __VA_ARGS__)

/*
 * Recursive parsing functions.
 */
static ExternDecl *translation_unit(void);
static ExternDecl *external_declaration(void);
static Declaration *function_definition(TypeExp *decl_specs, TypeExp *header);
static Declaration *declaration(TypeExp *decl_specs, TypeExp *first_declarator);
static TypeExp *declaration_specifiers(int type_spec_seen);
static TypeExp *init_declarator_list(TypeExp *decl_specs, TypeExp *first_declarator);
static TypeExp *init_declarator(TypeExp *decl_specs, TypeExp *first_declarator);
static TypeExp *storage_class_specifier(void);
static TypeExp *type_specifier(void);
static TypeExp *struct_or_union_specifier(void);
static TypeExp *struct_or_union(void);
static DeclList *struct_declaration_list(void);
static Declaration *struct_declaration(void);
static TypeExp *specifier_qualifier_list(int type_spec_seen);
static TypeExp *struct_declarator_list(TypeExp *sql);
static TypeExp *struct_declarator(TypeExp *sql);
static TypeExp *enum_specifier(void);
static TypeExp *enumerator_list(void);
static TypeExp *enumerator(void);
static TypeExp *enumeration_constant(void);
static TypeExp *type_qualifier(void);
static TypeExp *direct_declarator_postfix(void);
static TypeExp *pointer(void);
static TypeExp *type_qualifier_list(void);
static DeclList *parameter_type_list(void);
static DeclList *parameter_list(void);
static Declaration *parameter_declaration(void);
// void identifier_list(void);
static Declaration *type_name(void);
static TypeExp *typedef_name(void);
static ExecNode *initializer(void);
static ExecNode *initializer_list(void);

typedef enum {
    ABSTRACT_DECLARATOR,
    CONCRETE_DECLARATOR,
    EITHER_DECLARATOR
} DeclaratorCategory;
static TypeExp *concrete_declarator(void);
static TypeExp *abstract_declarator(void);
static TypeExp *declarator(DeclaratorCategory dc);
static TypeExp *direct_declarator(DeclaratorCategory dc);

static ExecNode *statement(int in_loop, int in_switch);
static ExecNode *labeled_statement(int in_loop, int in_switch);
static ExecNode *compound_statement(int new_scope, int in_loop, int in_switch);
static DeclList *declaration_list(void);
static ExecNode *statement_list(int in_loop, int in_switch);
static ExecNode *expression_statement(void);
static ExecNode *selection_statement(int in_loop, int in_switch);
static ExecNode *iteration_statement(int in_switch);
static ExecNode *jump_statement(int in_loop, int in_switch);

static ExecNode *conditional_expression(void);
static ExecNode *assignment_expression(void);
static ExecNode *constant_expression(void);
static ExecNode *expression(void);
static ExecNode *logical_OR_expression(void);
static ExecNode *logical_AND_expression(void);
static ExecNode *inclusive_OR_expression(void);
static ExecNode *exclusive_OR_expression(void);
static ExecNode *AND_expression(void);
static ExecNode *equality_expression(void);
static ExecNode *relational_expression(void);
static ExecNode *shift_expression(void);
static ExecNode *additive_expression(void);
static ExecNode *multiplicative_expression(void);
static ExecNode *cast_expression(void);
static ExecNode *unary_expression(void);
static ExecNode *postfix_expression(void);
static ExecNode *argument_expression_list(void);
static ExecNode *primary_expression(void);
static ExecNode *postfix(void);


/*
 * Helper functions.
 */
static Token lookahead(int i)
{
    TokenNode *p;

    p = curr_tok;
    while (--i /*&& p->token!=TOK_EOF*/)
        p = p->next;
    return p->token;
}

static char *get_lexeme(int i)
{
    TokenNode *p;

    p = curr_tok;
    while (--i /*&& p->token!=TOK_EOF*/)
        p = p->next;
    return p->lexeme;
}

static void match(Token token)
{
    if (curr_tok->token == token)
        curr_tok = curr_tok->next;
    else
        ERROR("expecting `%s'; found `%s'", token_table[token*2+1], curr_tok->lexeme);
}


/*
 * Functions that test lookahead(1).
 */

static int in_first_storage_class_specifier(void)
{
    switch (lookahead(1)) {
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

static int in_first_type_specifier(void)
{
    switch (lookahead(1)) {
    case TOK_VOID:
    case TOK_CHAR:
    case TOK_SHORT:
    case TOK_INT:
    case TOK_LONG:
    case TOK_SIGNED:
    case TOK_UNSIGNED:
    case TOK_STRUCT:
    case TOK_UNION:
    case TOK_ENUM:
        return TRUE;
    case TOK_ID:
        if (lookahead(2) == TOK_COLON)
            return FALSE; /* label definition */
        return is_typedef_name(get_lexeme(1));
    default:
        return FALSE;
    }
}

static int in_first_type_qualifier(void)
{
    return (lookahead(1)==TOK_CONST||lookahead(1)==TOK_VOLATILE);
}

static int in_first_specifier_qualifier_list(void)
{
    return (in_first_type_specifier()||in_first_type_qualifier());
}

static int in_first_declaration_specifiers(void)
{
    return (in_first_storage_class_specifier()||in_first_type_specifier()||in_first_type_qualifier());
}

/*
 * Main function of the parser.
 */
ExternDecl *parser(TokenNode *tokens)
{
    curr_tok = tokens;
    init_symbol_tables(); /* initialize decl.c's tables */
    return translation_unit();
}

// =============================================================================
// External definitions
// =============================================================================

/*
 * translation_unit = external_declaration { external_declaration } EOF
 */
ExternDecl *translation_unit(void)
{
    ExternDecl *n, *temp;

    n = temp = external_declaration();
    while (lookahead(1) != TOK_EOF) {
        temp->sibling = external_declaration();
        temp = temp->sibling;
    }
    match(TOK_EOF);

    return n;
}

/*
 * external_declaration = function_definition |
 *                        declaration
 */
ExternDecl *external_declaration(void)
{
    ExternDecl *e;
    TypeExp *p, *q;

    q = NULL;
    e = malloc(sizeof(ExternDecl));
    e->sibling = NULL;

    p = declaration_specifiers(FALSE);
    analyze_decl_specs(p);
    if (lookahead(1) != TOK_SEMICOLON)
        q = concrete_declarator();

    if (lookahead(1) == TOK_LBRACE) {
        e->kind = FUNCTION_DEFINITION;
        // e->ed.f = function_definition(p, q);
        e->d = function_definition(p, q);
    } else {
        e->kind = DECLARATION;
        // e->ed.d = declaration(p, q);
        e->d = declaration(p, q);
    }

    return e;
}

/*
 * function_definition = declaration_specifiers declarator compound_statement
 */
Declaration *function_definition(TypeExp *decl_specs, TypeExp *header)
{
    ExternId *ed;
    Declaration *f;

    f = malloc(sizeof(Declaration));
    f->decl_specs = decl_specs;
    f->idl = header;
    restore_scope(); /* restore parameters' scope */
    analyze_function_definition(f);
    f->idl->attr.e = compound_statement(FALSE, FALSE, FALSE);

    if ((ed=lookup_external_id(f->idl->str)) != NULL)
        ed->declarator->attr.e = f->idl->attr.e;

    pop_scope();
    resolve_gotos();
    empty_label_table();

    return f;
}

// =============================================================================
// Declarations
// =============================================================================

/*
 * declaration = declaration_specifiers [ init_declarator_list ] ";"
 */
Declaration *declaration(TypeExp *decl_specs, TypeExp *first_declarator)
{
    Declaration *d;

    d = malloc(sizeof(Declaration));
    d->idl = NULL;
    if (decl_specs != NULL) { /* declaration() was called from external_declaration() */
        d->decl_specs = decl_specs;
    } else {
        d->decl_specs = declaration_specifiers(FALSE);
        analyze_decl_specs(d->decl_specs);
    }

    if (first_declarator!=NULL || lookahead(1)!=TOK_SEMICOLON)
        d->idl = init_declarator_list(d->decl_specs, first_declarator);
    /* else
        >>TODO: check for empty declaration here<<
    */

    match(TOK_SEMICOLON);

    return d;
}

/*
 * declaration_specifiers = storage_class_specifier [ declaration_specifiers ] |
 *                          type_specifier [ declaration_specifiers ] |
 *                          type_qualifier [ declaration_specifiers ]
 */
TypeExp *declaration_specifiers(int type_spec_seen)
{
    TypeExp *n;

    if (in_first_storage_class_specifier())
        n = storage_class_specifier();
    else if (in_first_type_specifier())
        n = type_specifier(), type_spec_seen = TRUE;
    else if (in_first_type_qualifier())
        n = type_qualifier();
    else
        ERROR("expecting declaration-specifier");

    /*
     * static const type_t; // null declaration
     *                 ^
     *             typedef name
     *
     * static const int type_t; // `int' disables typedef names
     *                    ^
     *                identifier
     *
     * If a typedef name is redeclared the declaration-specifiers
     * of the declaration must contain a type-specifier.
     */
    if (in_first_declaration_specifiers() && (lookahead(1)!=TOK_ID || !type_spec_seen))
        n->child = declaration_specifiers(type_spec_seen);

    return n;
}

/*
 * init_declarator_list =  init_declarator { "," init_declarator }
 */
TypeExp *init_declarator_list(TypeExp *decl_specs, TypeExp *first_declarator)
{
    TypeExp *n, *temp;

    n = temp = init_declarator(decl_specs, first_declarator);
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        temp->sibling = init_declarator(decl_specs, NULL);
        temp = temp->sibling;
    }

    return n;
}

/*
 * init_declarator = declarator [ "=" initializer ]
 */
TypeExp *init_declarator(TypeExp *decl_specs, TypeExp *first_declarator)
{
    int good;
    TypeExp *n;

    if (first_declarator == NULL)
        n = concrete_declarator();
    else
        n = first_declarator;

    good = analyze_declarator(decl_specs, n, TRUE);

    if (lookahead(1) == TOK_ASSIGN) {
        match(TOK_ASSIGN);
        n->attr.e = initializer();
    }

    if (good)
        analyze_init_declarator(decl_specs, n, FALSE);

    return n;
}

static TypeExp *new_type_exp_node(void)
{
    TypeExp *new_node;

    new_node = calloc(1, sizeof(TypeExp));
    new_node->info = curr_tok;
    ++number_of_ast_nodes;

    return new_node;
}

/*
 * storage_class_specifier = "typedef" |
 *                           "extern" |
 *                           "static" |
 *                           "auto" |
 *                           "register"
 */
TypeExp *storage_class_specifier(void)
{
    TypeExp *s;

    s = new_type_exp_node();
    s->op = lookahead(1);

    switch (lookahead(1)) {
    case TOK_TYPEDEF:
        match(TOK_TYPEDEF);
        break;
    case TOK_EXTERN:
        match(TOK_EXTERN);
        break;
    case TOK_STATIC:
        match(TOK_STATIC);
        break;
    case TOK_AUTO:
        match(TOK_AUTO);
        break;
    case TOK_REGISTER:
        match(TOK_REGISTER);
        break;
    default:
        my_assert(0, "storage_class_specifier()");
        break;
    }

    return s;
}

/*
 * type_specifier = "void" |
 *                  "char" |
 *                  "short" |
 *                  "int" |
 *                  "long" |
 *                  "float" |
 *                  "double" |
 *                  "signed" |
 *                  "unsigned" |
 *                  struct_or_union_specifier |
 *                  enum_specifier |
 *                  typedef_name
 */
TypeExp *type_specifier(void)
{
    TypeExp *n;

    n = new_type_exp_node();
    n->op = lookahead(1);

    switch (lookahead(1)) {
    case TOK_VOID:
        match(TOK_VOID);
        break;
    case TOK_CHAR:
        match(TOK_CHAR);
        break;
    case TOK_SHORT:
        match(TOK_SHORT);
        break;
    case TOK_INT:
        match(TOK_INT);
        break;
    case TOK_LONG:
        match(TOK_LONG);
        break;
    case TOK_SIGNED:
        match(TOK_SIGNED);
        break;
    case TOK_UNSIGNED:
        match(TOK_UNSIGNED);
        break;
    case TOK_STRUCT:
    case TOK_UNION:
        free(n);
        n = struct_or_union_specifier();
        break;
    case TOK_ENUM:
        free(n);
        n = enum_specifier();
        break;
    case TOK_ID:
        /*
         * assume in_first_type_specifier() said
         * lookahead(1) is a typedef-name.
         */
        free(n);
        n = typedef_name();
        break;
    default:
        my_assert(0, "type_specifier()");
        break;
    }

    return n;
}

/*
 * struct_or_union_specifier = struct_or_union [ identifier ] "{" struct_declaration_list "}" |
 *                             struct_or_union identifier
 */
TypeExp *struct_or_union_specifier(void)
{
    TypeExp *n;

    n = struct_or_union();
    if (lookahead(1) == TOK_ID) {
        TypeTag *cur, *all;

        n->str = get_lexeme(1);

        cur = lookup_tag(n->str, FALSE);
        all = lookup_tag(n->str, TRUE);
        if (all != NULL) {
            /*
             * A declaration of the identifier as a tag
             * is visible in this or an enclosing scope.
             */
            if (cur==NULL && (lookahead(2)==TOK_SEMICOLON||lookahead(2)==TOK_LBRACE)) {
                /*
                 * A declaration of the identifier as a tag is not visible in the current
                 * scope, and the declaration is of the form
                 *      struct-or-union identifier ";"
                 * or
                 *      struct-or-union identifier "{" struct-declaration-list "}"
                 */
                install_tag(n); /* new incomplete type */
            } else {
                if (n->op != all->type->op)
                    ERROR("use of `%s' with tag type that does not match previous declaration", n->str);
                n->str = all->type->str; /* this allows to check for type compatibility through pointers comparison */
            }
        } else {
            install_tag(n); /* new incomplete type */
        }
        match(TOK_ID);
        if (lookahead(1) == TOK_LBRACE) {
            if (cur!=NULL && cur->type->attr.dl!=NULL)
                ERROR("redefinition of `%s %s'", token_table[n->op*2+1], n->str);
            push_struct_descriptor(n);
            match(TOK_LBRACE);
            n->attr.dl = struct_declaration_list();
            match(TOK_RBRACE); /* the type is complete now */
            if (cur!=NULL && cur->type!=n)
                cur->type = n; /* update the previous incomplete declaration */
            pop_struct_descriptor();
        }
    } else if (lookahead(1) == TOK_LBRACE) {
        n->str = strdup("<anonymous>");
        push_struct_descriptor(n);
        match(TOK_LBRACE);
        n->attr.dl = struct_declaration_list();
        match(TOK_RBRACE);
        pop_struct_descriptor();
    } else {
        ERROR("expecting identifier or struct-declaration-list");
    }

    return n;
}

/*
 * struct_or_union = "struct" | "union"
 */
TypeExp *struct_or_union(void)
{
    TypeExp *n;

    n = new_type_exp_node();
    n->op = lookahead(1);

    if (lookahead(1) == TOK_STRUCT)
        match(TOK_STRUCT);
    else if (lookahead(1) == TOK_UNION)
        match(TOK_UNION);
    else
        my_assert(0, "struct_or_union()");

    return n;
}

/*
 * struct_declaration_list = struct_declaration { struct_declaration }
 */
DeclList *struct_declaration_list(void)
{
    DeclList *n, *temp;

    n = temp = malloc(sizeof(DeclList));
    n->decl = struct_declaration();
    n->next = NULL;
    while (in_first_specifier_qualifier_list()) {
        temp->next = malloc(sizeof(DeclList));
        temp = temp->next;
        temp->decl = struct_declaration();
        temp->next = NULL;
    }

    return n;
}

/*
 * struct_declaration = specifier_qualifier_list struct_declarator_list ";"
 */
Declaration *struct_declaration(void)
{
    Declaration *n;

    n = malloc(sizeof(Declaration));

    n->decl_specs = specifier_qualifier_list(FALSE);
    analyze_decl_specs(n->decl_specs);
    n->idl = struct_declarator_list(n->decl_specs);
    match(TOK_SEMICOLON);

    return n;
}

/*
 * specifier_qualifier_list = ( type_specifier | type_qualifier ) [ specifier_qualifier_list ]
 */
TypeExp *specifier_qualifier_list(int type_spec_seen)
{
    TypeExp *n;

    if (in_first_type_specifier())
        n = type_specifier() , type_spec_seen = TRUE;
    else if (in_first_type_qualifier())
        n = type_qualifier();
    else
        ERROR("expecting type specifier or qualifier");

    if (in_first_specifier_qualifier_list() && (lookahead(1)!=TOK_ID || !type_spec_seen))
        n->child = specifier_qualifier_list(type_spec_seen);

    return n;
}

/*
 * struct_declarator_list = struct_declarator { "," struct_declarator }
 */
TypeExp *struct_declarator_list(TypeExp *sql)
{
    TypeExp *n, *temp;

    n = temp = struct_declarator(sql);
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        temp->sibling = struct_declarator(sql);
        temp = temp->sibling;
    }

    return n;
}

/*
 * struct_declarator = declarator |
 *                     [ declarator ] ":" constant_expression
 */
TypeExp *struct_declarator(TypeExp *sql)
{
    TypeExp *n;

    /*if (lookahead(1) == TOK_COLON) {
        match(TOK_COLON);
        constant_expression();
    } else {*/
        n = concrete_declarator();
        analyze_struct_declarator(sql, n);
        /*if (lookahead(1) == TOK_COLON) {
            match(TOK_COLON);
            constant_expression();
        }
    }*/

    return n;
}

/*
 * enum_specifier = "enum" [ identifier ] "{" enumerator_list "}" |
 *                  "enum" identifier
 */
TypeExp *enum_specifier(void)
{
    /*
     * Note: Incomplete enums are not standard,
     * but they allow to handle enums the same
     * way as struct and unions.
     */
    TypeExp *n;

    n = new_type_exp_node();
    n->op = lookahead(1);

    match(TOK_ENUM);
    if (lookahead(1) == TOK_ID) {
        TypeTag *cur, *all;

        n->str = get_lexeme(1);

        cur = lookup_tag(n->str, FALSE);
        all = lookup_tag(n->str, TRUE);
        if (all != NULL) {
            /*
             * A declaration of the identifier as a tag
             * is visible in this or an enclosing scope.
             */
            if (cur==NULL && (lookahead(2)==TOK_SEMICOLON||lookahead(2)==TOK_LBRACE)) {
                /*
                 * A declaration of the identifier as a tag is not visible in the current
                 * scope, and the declaration is of the form
                 *      enum identifier ";"
                 * or
                 *      enum identifier "{" enumerator-list "}"
                 */
                install_tag(n); /* new incomplete type */
            } else {
                if (n->op != all->type->op)
                    ERROR("use of `%s' with tag type that does not match previous declaration", n->str);
                n->str = all->type->str;
            }
        } else {
            install_tag(n); /* new incomplete type */
        }
        match(TOK_ID);
        if (lookahead(1) == TOK_LBRACE) {
            if (cur!=NULL && cur->type->attr.el!=NULL)
                ERROR("redefinition of `enum %s'", n->str);
            match(TOK_LBRACE);
            n->attr.el = enumerator_list();
            match(TOK_RBRACE);
            if (cur!=NULL && cur->type!=n)
                cur->type = n;

        }
    } else if (lookahead(1) == TOK_LBRACE) {
        n->str = strdup("<anonymous>");
        match(TOK_LBRACE);
        n->attr.el = enumerator_list();
        match(TOK_RBRACE);
    } else {
        ERROR("expecting identifier or enumerator-list");
    }

    return n;
}

/*
 * enumerator_list = enumerator { "," enumerator }
 */
TypeExp *enumerator_list(void)
{
    TypeExp *n, *temp;

    n = temp = enumerator();
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        temp->sibling = enumerator();
        temp = temp->sibling;
    }
    reset_enum_val();

    return n;
}

/*
 * enumerator = enumeration_constant [ "=" constant_expression ]
 */
TypeExp *enumerator(void)
{
    TypeExp *n;

    n = enumeration_constant();
    if (lookahead(1) == TOK_ASSIGN) {
        match(TOK_ASSIGN);
        n->attr.e = constant_expression();
    }
    analyze_enumerator(n);

    return n;
}

/*
 * enumeration_constant = identifier
 */
TypeExp *enumeration_constant(void)
{
    TypeExp *n;

    n = new_type_exp_node();
    n->str = get_lexeme(1);
    n->op = TOK_ENUM_CONST;

    match(TOK_ID);

    return n;
}

/*
 * type_qualifier = "const" | "volatile"
 */
TypeExp *type_qualifier(void)
{
    TypeExp *n;

    n = new_type_exp_node();
    n->op = lookahead(1);

    if (lookahead(1) == TOK_CONST)
        match(TOK_CONST);
    else if (lookahead(1) == TOK_VOLATILE)
        match(TOK_VOLATILE);
    else
        my_assert(0, "type_qualifier()");

    return n;
}

TypeExp *concrete_declarator(void)
{
    return declarator(CONCRETE_DECLARATOR);
}

TypeExp *abstract_declarator(void)
{
    return declarator(ABSTRACT_DECLARATOR);
}

/*
 * declarator = [ pointer ] direct_declarator
 */
TypeExp *declarator(DeclaratorCategory dc)
{
    /*TypeExp *n, *temp;

    if (lookahead(1) == TOK_STAR) {
        n = temp = pointer();
        while (temp->child != NULL)
            temp = temp->child;
        temp->child = direct_declarator(dc, install_id, tok);
    } else {
        n = direct_declarator(dc, install_id, tok);
    }*/
    TypeExp *n;

    if (lookahead(1) == TOK_STAR) {
        TypeExp *p, *temp;

        p = pointer();

        n = temp = direct_declarator(dc);
        if (temp != NULL) {
            while (temp->child != NULL)
                temp = temp->child;
            temp->child = p;
        } else {
            n = p;
        }
    } else {
        n = direct_declarator(dc);
    }

    return n;
}

/*
 * direct_declarator = [ identifier | "(" declarator ")" ] { direct_declarator_postfix }
 */
TypeExp *direct_declarator(DeclaratorCategory dc)
{
    TypeExp *n, *temp;

    n = temp = NULL;

    if (lookahead(1) == TOK_ID) {
        if (dc == ABSTRACT_DECLARATOR)
            ERROR("identifier not allowed in abstract-declarator");
        n = temp = new_type_exp_node();
        n->op = lookahead(1);
        n->str = get_lexeme(1);
        /*if (install_id)
            install(decl_specs, n);*/
        match(TOK_ID);
    } else if (lookahead(1)==TOK_LPAREN
    && (lookahead(2)==TOK_LPAREN
    || lookahead(2)==TOK_LBRACKET
    || lookahead(2)==TOK_STAR
    || lookahead(2)==TOK_ID && (dc==CONCRETE_DECLARATOR||/*dc==ABSTRACT_DECLARATOR&&*/!is_typedef_name(get_lexeme(2))))) {
        match(TOK_LPAREN);
        n = temp = declarator(dc);
        match(TOK_RPAREN);
    }

    if (n==NULL && dc==CONCRETE_DECLARATOR)
        ERROR("missing identifier in non-abstract-declarator");

    /*while (lookahead(1)==TOK_LBRACKET || lookahead(1)==TOK_LPAREN) {
        n = direct_declarator_postfix();
        n->child = temp;
        temp = n;
    }*/
    if (lookahead(1)==TOK_LBRACKET || lookahead(1)==TOK_LPAREN) {
        if (temp == NULL) {
            n = temp = direct_declarator_postfix();
            while (lookahead(1)==TOK_LBRACKET || lookahead(1)==TOK_LPAREN) {
                temp->child = direct_declarator_postfix();
                temp = temp->child;
            }
        } else {
            while (temp->child != NULL)
                temp = temp->child;
            do {
                temp->child = direct_declarator_postfix();
                temp = temp->child;
            } while (lookahead(1)==TOK_LBRACKET || lookahead(1)==TOK_LPAREN);
        }
    }

    return n;
}

/*
 * direct_declarator_postfix = "[" [ constant_expression ] "]" |
 *                             "(" parameter_type_list ")" |
 *                             "(" [ identifier_list ] ")"
 */
TypeExp *direct_declarator_postfix(void)
{
    TypeExp *n;

    n = new_type_exp_node();

    if (lookahead(1) == TOK_LBRACKET) {
        n->op = TOK_SUBSCRIPT;
        match(TOK_LBRACKET);
        if (lookahead(1) != TOK_RBRACKET)
            n->attr.e = constant_expression();
        match(TOK_RBRACKET);
    } else if (lookahead(1) == TOK_LPAREN) {
        n->op = TOK_FUNCTION;
        match(TOK_LPAREN);
        /*if (lookahead(1) != TOK_RPAREN) {
            if (lookahead(1) == TOK_ID)
                identifier_list(); // old-style declarator
            else // parameter_type_list or error*/
                n->attr.dl = parameter_type_list();
        /*}*/
        match(TOK_RPAREN);
    } else {
        my_assert(0, "direct_declarator_postfix()");
    }

    return n;
}

/*
 * pointer = "*" [ type_qualifier_list ] [ pointer ]
 */
TypeExp *pointer(void)
{
    /*TypeExp *n, *temp;

    n = temp = new_type_exp_node();
    n->op = lookahead(1);

    match(TOK_STAR);
    if (in_first_type_qualifier())
        n->child = type_qualifier_list();

    if (lookahead(1) == TOK_STAR) {
        while (temp->child != NULL)
            temp = temp->child;
        temp->child = pointer();
    }*/

    /*
     * Example: the declaration
     *      int *const*volatile*z;
     * has the following syntax tree
     * TOK_ID (z)
     *    |
     * TOK_STAR
     *    |
     * TOK_STAR (volatile)
     *    |
     * TOK_STAR (const)
     */

    TypeExp *n;

    n = new_type_exp_node();
    n->op = lookahead(1);

    match(TOK_STAR);
    if (in_first_type_qualifier())
        n->attr.el = type_qualifier_list();

    if (lookahead(1) == TOK_STAR) {
        TypeExp *p, *temp;

        p = temp = pointer();
        while (temp->child != NULL)
            temp = temp->child;
        temp->child = n;
        n = p;
    }

    return n;
}

/*
 * type_qualifier_list = type_qualifier { type_qualifier }
 */
TypeExp *type_qualifier_list(void)
{
    TypeExp *n, *temp;

    n = type_qualifier();
    while (in_first_type_qualifier()) {
        /* condense all qualifiers in a single node */
        temp = type_qualifier();
        if (temp->op != n->op)
            n->op = TOK_CONST_VOLATILE;
        free(temp);
    }

    return n;
}

/*
 * parameter_type_list = parameter_list [ "," "..." ]
 */
DeclList *parameter_type_list(void)
{
    DeclList *n, *temp;

    push_scope();
    n = temp = parameter_list();
    pop_scope(); /* restored if function definition */
    if (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        match(TOK_ELLIPSIS);
        /*
         * Add a node at the end of the parameter list
         * that has TOK_ELLIPSIS as its operator.
         */
        while (temp->next != NULL)
            temp = temp->next;
        temp->next = malloc(sizeof(DeclList));
        temp = temp->next;
        temp->decl = malloc(sizeof(Declaration));
        temp->decl->idl = new_type_exp_node();
        temp->decl->idl->op = TOK_ELLIPSIS;
        temp->decl->decl_specs = NULL;
    }

    return n;
}

/*
 * parameter_list = parameter_declaration { "," parameter_declaration }
 */
DeclList *parameter_list(void)
{
    DeclList *n, *temp;

    n = temp = malloc(sizeof(DeclList));
    n->next = NULL;

    n->decl = parameter_declaration();
    while (lookahead(1)==TOK_COMMA && lookahead(2)!=TOK_ELLIPSIS) {
        match(TOK_COMMA);
        temp->next = malloc(sizeof(DeclList));
        temp = temp->next;
        temp->next = NULL;
        temp->decl = parameter_declaration();
    }

    return n;
}

/*
 * parameter_declaration = declaration_specifiers declarator |
 *                         declaration_specifiers [ abstract_declarator ]
 */
Declaration *parameter_declaration(void)
{
    Declaration *n;

    n = malloc(sizeof(Declaration));
    n->idl = NULL;

    n->decl_specs = declaration_specifiers(FALSE);
    if (lookahead(1)!=TOK_COMMA && lookahead(1)!=TOK_RPAREN) /* FOLLOW(parameter_declaration) = { ",", ")" } */
        n->idl = declarator(EITHER_DECLARATOR);
    analyze_parameter_declaration(n);

    return n;
}

/*
 * identifier_list = identifier { "," identifier }
 */
/*void identifier_list(void)
{
    match(TOK_ID);
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        match(TOK_ID);
    }
}*/

/*
 * type_name = specifier_qualifier_list [ abstract_declarator ]
 */
Declaration *type_name(void)
{
    Declaration *n;

    n = malloc(sizeof(Declaration));
    n->idl = NULL;

    n->decl_specs = specifier_qualifier_list(FALSE);
    if (lookahead(1) != TOK_RPAREN) /* FOLLOW(type_name) = { ")" } */
        n->idl = abstract_declarator();
    analyze_type_name(n);

    return n;
}

/*
 * typedef_name = identifier
 */
TypeExp *typedef_name(void)
{
    TypeExp *n;

    /*
     * Don't check if the identifier is indeed a typedef-name
     * because it's assumed that this fuction is only called
     * if in_first_type_specifier() returned TRUE.
     */
    n = new_type_exp_node();
    n->op = TOK_TYPEDEFNAME;
    n->str = get_lexeme(1);
    DEBUG_PRINTF("typedef-name found: `%s'\n", get_lexeme(1));

    match(TOK_ID);

    return n;
}

static ExecNode *new_op_node(Token op);

/*
 * initializer = assignment_expression |
 *               "{" initializer_list [ "," ] "}"
 */
ExecNode *initializer(void)
{
    ExecNode *n;

    if (lookahead(1) == TOK_LBRACE) {
        match(TOK_LBRACE);
        // n = initializer_list();
        n = new_op_node(TOK_INIT_LIST);
        n->child[0] = initializer_list();
        if (lookahead(1) == TOK_COMMA)
            match(TOK_COMMA);
        match(TOK_RBRACE);
    } else {
        n = assignment_expression();
    }

    return n;
}

/*
 * initializer_list = initializer { "," initializer }
 */
ExecNode *initializer_list(void)
{
    ExecNode *n, *temp;

    n = temp = initializer();
    while (lookahead(1)==TOK_COMMA && lookahead(2)!=TOK_RBRACE) {
        match(TOK_COMMA);
        temp->sibling = initializer();
        temp = temp->sibling;
    }

    return n;
}

// =============================================================================
// Statements
// =============================================================================

static ExecNode *new_stmt_node(StmtKind kind)
{
    ExecNode *new_node;

    new_node = calloc(1, sizeof(ExecNode));
    new_node->node_kind = StmtNode;
    new_node->kind.exp = kind;
    new_node->info = curr_tok;
    ++number_of_ast_nodes;

    return new_node;
}

/*
 * statement = labeled_statement |
 *             compound_statement |
 *             expression_statement |
 *             selection_statement |
 *             iteration_statement |
 *             jump_statement
 */
ExecNode *statement(int in_loop, int in_switch)
{
    switch (lookahead(1)) {
    case TOK_LBRACE:
        return compound_statement(TRUE, in_loop, in_switch);
    case TOK_IF:
    case TOK_SWITCH:
        return selection_statement(in_loop, in_switch);
    case TOK_WHILE:
    case TOK_DO:
    case TOK_FOR:
        return iteration_statement(in_switch);
    case TOK_GOTO:
    case TOK_CONTINUE:
    case TOK_BREAK:
    case TOK_RETURN:
        return jump_statement(in_loop, in_switch);
    case TOK_CASE:
    case TOK_DEFAULT:
        return labeled_statement(in_loop, in_switch);
    case TOK_ID:
        if (lookahead(2) == TOK_COLON)
            return labeled_statement(in_loop, in_switch);
        /* fall through */
    default: /* expression statement or error */
        return expression_statement();
    }
}

/*
 * labeled_statement = identifier ":" statement |
 *                     "case" constant_expression ":" statement |
 *                     "default" ":" statement
 */
ExecNode *labeled_statement(int in_loop, int in_switch)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_ID:
        n = new_stmt_node(LabelStmt);
        n->attr.str = get_lexeme(1);
        match(TOK_ID);
        match(TOK_COLON);
        n->child[0] = statement(in_loop, in_switch);
        break;
    case TOK_CASE:
        n = new_stmt_node(CaseStmt);
        match(TOK_CASE);
        n->child[0] = constant_expression();
        match(TOK_COLON);
        n->child[1] = statement(in_loop, in_switch);
        break;
    case TOK_DEFAULT:
        n = new_stmt_node(DefaultStmt);
        match(TOK_DEFAULT);
        match(TOK_COLON);
        n->child[0] = statement(in_loop, in_switch);
        break;
    }
    analyze_labeled_statement(n, in_switch);

    return n;
}

/*
 * compound_statement = "{" [ declaration_list ] [ statement_list ] "}"
 */
ExecNode *compound_statement(int new_scope, int in_loop, int in_switch)
{
    ExecNode *n;

    n = new_stmt_node(CmpndStmt);

    match(TOK_LBRACE);
    if (in_first_declaration_specifiers()) {
        if (new_scope)
            push_scope();
        n->locals = declaration_list();
    }
    if (lookahead(1) != TOK_RBRACE)
        n->child[0] = statement_list(in_loop, in_switch);
    match(TOK_RBRACE);

    if (new_scope && n->locals!=NULL)
        pop_scope();

    return n;
}

/*
 * declaration_list = declaration { declaration }
 */
DeclList *declaration_list(void)
{
    DeclList *n, *temp;

    n = temp = malloc(sizeof(DeclList));
    n->next = NULL;

    n->decl = declaration(NULL, NULL);
    while (in_first_declaration_specifiers()) {
        temp->next = malloc(sizeof(DeclList));
        temp->next->next = NULL;
        temp->next->decl = declaration(NULL, NULL);
        temp = temp->next;
    }

    return n;
}

/*
 * statement_list = statement { statement }
 */
ExecNode *statement_list(int in_loop, int in_switch)
{
    ExecNode *n, *temp;

    n = temp = statement(in_loop, in_switch);
    while (lookahead(1) != TOK_RBRACE) { /* FOLLOW(statement_list) = { "}" } */
        temp->sibling = statement(in_loop, in_switch);
        temp = temp->sibling;
    }

    return n;
}

/*
 * expression_statement = [ expression ] ";"
 */
ExecNode *expression_statement(void)
{
    ExecNode *n;

    n = new_stmt_node(ExpStmt);
    if (lookahead(1) != TOK_SEMICOLON)
        n->child[0] = expression();
    match(TOK_SEMICOLON);

    return n;
}

/*
 * selection_statement = "if" "(" expression ")" statement [ "else" statement ] |
 *                       "switch" "(" expression ")" statement
 */
ExecNode *selection_statement(int in_loop, int in_switch)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_IF:
        n = new_stmt_node(IfStmt);
        match(TOK_IF);
        match(TOK_LPAREN);
        n->child[0] = expression();
        match(TOK_RPAREN);
        n->child[1] = statement(in_loop, in_switch);
        if (lookahead(1) == TOK_ELSE) {
            match(TOK_ELSE);
            n->child[2] = statement(in_loop, in_switch);
        }
        break;
    case TOK_SWITCH:
        n = new_stmt_node(SwitchStmt);
        match(TOK_SWITCH);
        match(TOK_LPAREN);
        n->child[0] = expression();
        match(TOK_RPAREN);
        increase_switch_nesting_level();
        n->child[1] = statement(in_loop, TRUE);
        decrease_switch_nesting_level();
        break;
    }
    analyze_selection_statement(n);

    return n;
}

/*
 * iteration_statement = "while" "(" expression ")" statement |
 *                       "do" statement "while" "(" expression ")" ";" |
 *                       "for" "(" [ expression ] ";" [ expression ] ";" [ expression ] ")" statement
 */
ExecNode *iteration_statement(int in_switch)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_WHILE:
        n = new_stmt_node(WhileStmt);
        match(TOK_WHILE);
        match(TOK_LPAREN);
        n->child[0] = expression();
        match(TOK_RPAREN);
        n->child[1] = statement(TRUE, in_switch);
        break;
    case TOK_DO:
        n = new_stmt_node(DoStmt);
        match(TOK_DO);
        n->child[1] = statement(TRUE, in_switch);
        match(TOK_WHILE);
        match(TOK_LPAREN);
        n->child[0] = expression();
        match(TOK_RPAREN);
        match(TOK_SEMICOLON);
        break;
    case TOK_FOR:
        n = new_stmt_node(ForStmt);
        match(TOK_FOR);
        match(TOK_LPAREN);
        if (lookahead(1) != TOK_SEMICOLON)
            n->child[1] = expression();
        match(TOK_SEMICOLON);
        if (lookahead(1) != TOK_SEMICOLON)
            n->child[0] = expression();
        match(TOK_SEMICOLON);
        if (lookahead(1) != TOK_RPAREN)
            n->child[2] = expression();
        match(TOK_RPAREN);
        n->child[3] = statement(TRUE, in_switch);
        break;
    }
    analyze_iteration_statement(n);

    return n;
}

/*
 * jump_statement = "goto" identifier ";" |
 *                  "continue" ";" |
 *                  "break" ";" |
 *                  "return" [ expression ] ";"
 */
ExecNode *jump_statement(int in_loop, int in_switch)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_GOTO:
        n = new_stmt_node(GotoStmt);
        match(TOK_GOTO);
        n->attr.str = get_lexeme(1);
        match(TOK_ID);
        break;
    case TOK_CONTINUE:
        n = new_stmt_node(ContinueStmt);
        match(TOK_CONTINUE);
        break;
    case TOK_BREAK:
        n = new_stmt_node(BreakStmt);
        match(TOK_BREAK);
        break;
    case TOK_RETURN:
        n = new_stmt_node(ReturnStmt);
        match(TOK_RETURN);
        if (lookahead(1) != TOK_SEMICOLON)
            n->child[0] = expression();
        break;
    }
    match(TOK_SEMICOLON);

    analyze_jump_statement(n, in_loop, in_switch);

    return n;
}

// =============================================================================
// Expressions
// =============================================================================

static ExecNode *new_op_node(Token op)
{
    ExecNode *new_node;

    new_node = calloc(1, sizeof(ExecNode));
    new_node->node_kind = ExpNode;
    new_node->kind.exp = OpExp;
    new_node->attr.op = op;
    new_node->info = curr_tok;
    ++number_of_ast_nodes;

    return new_node;
}

static ExecNode *new_pri_exp_node(ExpKind kind)
{
    ExecNode *new_node;

    new_node = calloc(1, sizeof(ExecNode));
    new_node->node_kind = ExpNode;
    new_node->kind.exp = kind;
    new_node->info = curr_tok;
    ++number_of_ast_nodes;

    return new_node;
}

/*
 * constant_expression = conditional_expression
 */
ExecNode *constant_expression(void)
{
    return conditional_expression();
}

/*
 * expression = assignment_expression { "," assignment_expression }
 */
ExecNode *expression(void)
{
    ExecNode *n, *temp;

    n = assignment_expression();
    while (lookahead(1) == TOK_COMMA) {
        temp = new_op_node(TOK_COMMA);
        match(TOK_COMMA);
        temp->child[0] = n;
        temp->child[1] = assignment_expression();
        n = temp;
        analyze_expression(n);
    }

    return n;
}

/*
 * assignment_operator = "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="
 */
#define IS_ASSIGNMENT_OP(tok) (tok>=TOK_ASSIGN && tok<=TOK_BW_OR_ASSIGN)

/*
 * assignment_expression = conditional_expression [ assignment_operator assignment_expression ]
 */
ExecNode *assignment_expression(void)
{
    ExecNode *n, *temp;

    n = conditional_expression();
    if (IS_ASSIGNMENT_OP(lookahead(1))) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = assignment_expression();
        n = temp;
        analyze_assignment_expression(n);
    }

    return n;
}

/*
 * conditional_expression = logical_OR_expression [ "?" expression ":" conditional_expression ]
 */
ExecNode *conditional_expression(void)
{
    ExecNode *n, *temp;

    n = logical_OR_expression();
    if (lookahead(1) == TOK_CONDITIONAL) {
        temp = new_op_node(TOK_CONDITIONAL);
        match(TOK_CONDITIONAL);
        temp->child[0] = n;
        temp->child[1] = expression();
        match(TOK_COLON);
        temp->child[2] = conditional_expression();
        n = temp;
        analyze_conditional_expression(n);
    }

    return n;
}

/*
 * logical_OR_expression = logical_AND_expression { "||" logical_AND_expression }
 */
ExecNode *logical_OR_expression(void)
{
    ExecNode *n, *temp;

    n = logical_AND_expression();
    while (lookahead(1) == TOK_OR) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = logical_AND_expression();
        n = temp;
        analyze_logical_operator(n);
    }

    return n;
}

/*
 * logical_AND_expression = inclusive_OR_expression { "&&" inclusive_OR_expression }
 */
ExecNode *logical_AND_expression(void)
{
    ExecNode *n, *temp;

    n = inclusive_OR_expression();
    while (lookahead(1) == TOK_AND) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = inclusive_OR_expression();
        n = temp;
        analyze_logical_operator(n);
    }

    return n;
}

/*
 * inclusive_OR_expression = exclusive_OR_expression { "|" exclusive_OR_expression }
 */
ExecNode *inclusive_OR_expression(void)
{
    ExecNode *n, *temp;

    n = exclusive_OR_expression();
    while (lookahead(1) == TOK_BW_OR) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = exclusive_OR_expression();
        n = temp;
        analyze_bitwise_operator(n);
    }

    return n;
}

/*
 * exclusive_OR_expression = AND_expression { "^" AND_expression }
 */
ExecNode *exclusive_OR_expression(void)
{
    ExecNode *n, *temp;

    n = AND_expression();
    while (lookahead(1) == TOK_BW_XOR) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = AND_expression();
        n = temp;
        analyze_bitwise_operator(n);
    }

    return n;
}

/*
 * AND_expression = equality_expression { "&" equality_expression }
 */
ExecNode *AND_expression(void)
{
    ExecNode *n, *temp;

    n = equality_expression();
    while (lookahead(1) == TOK_AMPERSAND) {
        temp = new_op_node(TOK_BW_AND);
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = equality_expression();
        n = temp;
        analyze_bitwise_operator(n);
    }

    return n;
}

/*
 * equality_expression = relational_expression { equop relational_expression }
 * equop = "==" | "!="
 */
ExecNode *equality_expression(void)
{
    ExecNode *n, *temp;

    n = relational_expression();
    while (lookahead(1)==TOK_EQ || lookahead(1)==TOK_NEQ) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = relational_expression();
        n = temp;
        analyze_relational_equality_expression(n);
    }

    return n;
}

/*
 * relop = "<" | ">" | "<=" | ">=
 */
#define IS_RELOP(tok) (tok>=TOK_LT && tok<=TOK_GET)

/*
 * relational_expression = shift_expression { relop shift_expression }
 */
ExecNode *relational_expression(void)
{
    ExecNode *n, *temp;

    n = shift_expression();
    while (IS_RELOP(lookahead(1))) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = shift_expression();
        n = temp;
        analyze_relational_equality_expression(n);
    }

    return n;
}

/*
 * shift_expression = additive_expression { shiftop additive_expression }
 * shiftop = "<<" | ">>"
 */
ExecNode *shift_expression(void)
{
    ExecNode *n, *temp;

    n = additive_expression();
    while (lookahead(1)==TOK_LSHIFT || lookahead(1)==TOK_RSHIFT) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = additive_expression();
        n = temp;
        analyze_bitwise_operator(n);
    }

    return n;
}

/*
 * additive_expression = multiplicative_expression { addop multiplicative_expression }
 * addop = "+" | "-" ;
 */
ExecNode *additive_expression(void)
{
    ExecNode *n, *temp;

    n = multiplicative_expression();
    while (lookahead(1)==TOK_PLUS || lookahead(1)==TOK_MINUS) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = multiplicative_expression();
        n = temp;
        analyze_additive_expression(n);
    }

    return n;
}

/*
 * multiplicative_expression = cast_expression { mulop cast_expression }
 * mulop = "*" | "/" | "%"
 */
ExecNode *multiplicative_expression(void)
{
    ExecNode *n, *temp;

    n = cast_expression();
    while (lookahead(1)==TOK_STAR || lookahead(1)==TOK_DIV || lookahead(1)==TOK_MOD) {
        temp = new_op_node((lookahead(1)!=TOK_STAR)?lookahead(1):TOK_MUL);
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = cast_expression();
        n = temp;
        analyze_multiplicative_expression(n);
    }

    return n;
}

/*
 * cast_expression = unary_expression |
 *                  "(" type_name ")" cast_expression
 */
ExecNode *cast_expression(void)
{
    ExecNode *n;

    if (lookahead(1) == TOK_LPAREN) {
        TokenNode *temp;

        temp = curr_tok; /* save */
        match(TOK_LPAREN);
        if (in_first_specifier_qualifier_list()) {
            n = new_op_node(TOK_CAST);
            n->child[1] = (ExecNode *)type_name();
            match(TOK_RPAREN);
            n->child[0] = cast_expression();
            analyze_cast_expression(n);
        } else {
            curr_tok = temp; /* restore */
            n = unary_expression();
        }
    } else {
        n = unary_expression();
    }

    return n;
}

/*
 * unary_operator = "&" | "*" | "+" | "-" | "~" | "!"
 */
#define IS_UNARY_OP(tok) (tok>=TOK_AMPERSAND && tok<=TOK_NEGATION)

/*
 * unary_expression = postfix_expression |
 *                    "++" unary_expression |
 *                    "--" unary_expression |
 *                    unary_operator cast_expression |
 *                    "sizeof" unary_expression |
 *                    "sizeof" "(" type_name ")"
 */
ExecNode *unary_expression(void)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_INC:
        n = new_op_node(TOK_PRE_INC);
        match(TOK_INC);
        n->child[0] = unary_expression();
        break;
    case TOK_DEC:
        n = new_op_node(TOK_PRE_DEC);
        match(TOK_DEC);
        n->child[0] = unary_expression();
        break;
    case TOK_SIZEOF: {
        n = new_op_node(TOK_SIZEOF);
        match(TOK_SIZEOF);
        if (lookahead(1) == TOK_LPAREN) {
            TokenNode *temp;

            temp = curr_tok; /* save */
            match(TOK_LPAREN);
            if (in_first_specifier_qualifier_list()) {
                n->child[1] = (ExecNode *)type_name();
                match(TOK_RPAREN);
            } else { /* sizeof applied to a parenthesized expression */
                curr_tok = temp; /* restore */
                n->child[0] = unary_expression();
            }
        } else {
            n->child[0] = unary_expression();
        }
        break;
    }
    case TOK_AMPERSAND:
        n = new_op_node(TOK_ADDRESS_OF);
        match(TOK_AMPERSAND);
        n->child[0] = cast_expression();
        break;
    case TOK_STAR:
        n = new_op_node(TOK_INDIRECTION);
        match(TOK_STAR);
        n->child[0] = cast_expression();
        break;
    case TOK_PLUS:
        n = new_op_node(TOK_UNARY_PLUS);
        match(TOK_PLUS);
        n->child[0] = cast_expression();
        break;
    case TOK_MINUS:
        n = new_op_node(TOK_UNARY_MINUS);
        match(TOK_MINUS);
        n->child[0] = cast_expression();
        break;
    case TOK_COMPLEMENT:
    case TOK_NEGATION:
        n = new_op_node(lookahead(1));
        match(lookahead(1));
        n->child[0] = cast_expression();
        break;
    default:
        n = postfix_expression();
        return n;
    }

    analyze_unary_expression(n);

    return n;
}

#define IS_POSTFIX_OP(t) (t==TOK_LBRACKET||t==TOK_LPAREN||t==TOK_DOT||t==TOK_ARROW||t==TOK_INC||t==TOK_DEC)

/*
 * postfix_expression = primary_expression { postfix }
 */
ExecNode *postfix_expression(void)
{
    ExecNode *n, *temp;

    n = temp = primary_expression();
    while (IS_POSTFIX_OP(lookahead(1))) {
        n = postfix();
        n->child[0] = temp;
        analyze_postfix_expression(n);
        temp = n;
    }

    return n;
}

/*
 * postfix = "[" expression "]" |
 *           "(" [ argument_expression_list ] ")" |
 *           "." identifier |
 *           "->" identifier |
 *           "++" |
 *           "--"
 */
ExecNode *postfix(void)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_LBRACKET:
        n = new_op_node(TOK_SUBSCRIPT);
        match(TOK_LBRACKET);
        n->child[1] = expression();
        match(TOK_RBRACKET);
        break;
    case TOK_LPAREN:
        n = new_op_node(TOK_FUNCTION);
        match(TOK_LPAREN);
        if (lookahead(1) != TOK_RPAREN)
            n->child[1] = argument_expression_list();
        match(TOK_RPAREN);
        break;
    case TOK_DOT:
        n = new_op_node(TOK_DOT);
        match(TOK_DOT);
        n->child[1] = new_pri_exp_node(IdExp);
        n->child[1]->attr.str = get_lexeme(1);
        match(TOK_ID);
        break;
    case TOK_ARROW:
        n = new_op_node(TOK_ARROW);
        match(TOK_ARROW);
        n->child[1] = new_pri_exp_node(IdExp);
        n->child[1]->attr.str = get_lexeme(1);
        match(TOK_ID);
        break;
    case TOK_INC:
        n = new_op_node(TOK_POS_INC);
        match(TOK_INC);
        break;
    case TOK_DEC:
        n = new_op_node(TOK_POS_DEC);
        match(TOK_DEC);
        break;
    }

    return n;
}

char *extra_str[] = { "auto", "static", "none", "external", "internal" };

#define NON_FATAL_ERROR(...) emit_error(FALSE, curr_tok->src_file, curr_tok->src_line, curr_tok->src_column, __VA_ARGS__)

/*
 * primary_expression = identifier |
 *                      constant |
 *                      string_literal |
 *                      "(" expression ")"
 */
ExecNode *primary_expression(void)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_ID: {
        /*
         * 6.5.1.2:
         * An identifier is a primary expression, provided it has been declared as designating an
         * object (in which case it is an lvalue) or a function (in which case it is a function
         * designator).79)
         * 79) Thus, an undeclared identifier is a violation of the syntax.
         */
        Symbol *s;

        n = new_pri_exp_node(IdExp);
        n->attr.str = get_lexeme(1);
        match(TOK_ID);

        if ((s=lookup(n->attr.str, TRUE)) != NULL) {
            TypeExp *scs;

            if ((scs=get_sto_class_spec(s->decl_specs))==NULL || scs->op!=TOK_TYPEDEF) {
                set_attributes(n, s);
                /*printf("identifier `%s', scope=%d, linkage=%s, storage=%s, is_param=%d\n", n->attr.str,
                n->extra[ATTR_SCOPE], extra_str[n->extra[ATTR_LINKAGE]], extra_str[n->extra[ATTR_DURATION]],
                n->extra[ATTR_IS_PARAM]);*/
            } else {
                NON_FATAL_ERROR("expecting primary-expression; found typedef-name `%s'", n->attr.str);
                n->type.decl_specs = get_type_node(TOK_ERROR);
            }
        } else {
            NON_FATAL_ERROR("undeclared identifier `%s'", n->attr.str);
            n->type.decl_specs = get_type_node(TOK_ERROR);
        }
        break;
    }
    case TOK_ICONST:
        n = new_pri_exp_node(IConstExp);
        n->attr.str = get_lexeme(1);
        match(TOK_ICONST);
        break;
    case TOK_STRLIT:
        n = new_pri_exp_node(StrLitExp);
        n->attr.str = get_lexeme(1);
        match(TOK_STRLIT);
        break;
    case TOK_LPAREN:
        match(TOK_LPAREN);
        n = expression();
        match(TOK_RPAREN);
        return n;
    default:
        ERROR("expecting primary-expression; found `%s'", get_lexeme(1));
        break;
    }
    analyze_primary_expression(n);

    return n;
}

/*
 * argument_expression_list = assignment_expression { "," assignment_expression }
 */
ExecNode *argument_expression_list(void)
{
    ExecNode *n, *temp;

    n = temp = assignment_expression();
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        temp->sibling = assignment_expression();
        temp = temp->sibling;
    }

    return n;
}
