#include "parser.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "util.h"
#include "sema.h"

#define SRC_FILE    curr_tok->src_file
#define SRC_LINE    curr_tok->src_line
#define SRC_COLUMN  curr_tok->src_column

static TokenNode *curr_tok;
unsigned number_of_ast_nodes;


typedef enum {
    ABSTRACT_DECLARATOR,
    CONCRETE_DECLARATOR,
    EITHER_DECLARATOR
} DeclaratorCategory;

TypeExp *concrete_declarator(int install_id, Token tok);
TypeExp *abstract_declarator(void);
TypeExp *declarator(DeclaratorCategory dc, int install_id, Token tok);
TypeExp *direct_declarator(DeclaratorCategory dc, int install_id, Token tok);


/*
 * Recursive parsing functions.
 */
ExternDecl *translation_unit(void);
ExternDecl *external_declaration(void);
FuncDef *function_definition(TypeExp *decl_specs, TypeExp *header);
Declaration *declaration(TypeExp *decl_specs, TypeExp *first_declarator);
TypeExp *declaration_specifiers(int type_spec_seen);
TypeExp *init_declarator_list(Token tok, TypeExp *first_declarator);
TypeExp *init_declarator(Token tok, TypeExp *first_declarator);
TypeExp *storage_class_specifier(void);
TypeExp *type_specifier(void);
TypeExp *struct_or_union_specifier(void);
TypeExp *struct_or_union(void);
DeclList *struct_declaration_list(void);
Declaration *struct_declaration(void);
TypeExp *specifier_qualifier_list(int type_spec_seen);
TypeExp *struct_declarator_list(void);
TypeExp *struct_declarator(void);
TypeExp *enum_specifier(void);
TypeExp *enumerator_list(void);
TypeExp *enumerator(void);
TypeExp *enumeration_constant(void);
TypeExp *type_qualifier(void);
// TypeExp *declarator(int install_id, Token tok);
// TypeExp *direct_declarator(int install_id, Token tok);
TypeExp *direct_declarator_postfix(void);
TypeExp *pointer(void);
TypeExp *type_qualifier_list(void);
DeclList *parameter_type_list(void);
DeclList *parameter_list(void);
Declaration *parameter_declaration(void);
// void identifier_list(void);
Declaration *type_name(void);
TypeExp *abstract_declarator(void);
TypeExp *direct_abstract_declarator(void);
TypeExp *direct_abstract_declarator_postfix(void);
TypeExp *typedef_name(void);
ExecNode *initializer(void);
ExecNode *initializer_list(void);

DeclList *declaration_list(void);
ExecNode *compound_statement(int new_scope);
ExecNode *conditional_expression(void);
ExecNode *assignment_expression(void);
ExecNode *constant_expression(void);
ExecNode *expression(void);
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
ExecNode *cast_expression(void);
ExecNode *unary_expression(void);
ExecNode *postfix_expression(void);
ExecNode *argument_expression_list(void);
ExecNode *primary_expression(void);
ExecNode *postfix(void);

/*
 * Functions that test lookahead(1).
 */
int in_first_declaration_specifiers(void);
int in_first_storage_class_specifier(void);
int in_first_type_specifier(void);
int in_first_specifier_qualifier_list(void);
int in_first_type_qualifier(void);


Token lookahead(int i)
{
    TokenNode *p;

    p = curr_tok;
    while (--i /*&& p->token!=TOK_EOF*/)
        p = p->next;
    return p->token;
}

char *get_lexeme(int i)
{
    TokenNode *p;

    p = curr_tok;
    while (--i /*&& p->token!=TOK_EOF*/)
        p = p->next;
    return p->lexeme;
}

void match(Token token)
{
    if (curr_tok->token == token)
        curr_tok = curr_tok->next;
    else
        ERROR("expecting `%s'; found `%s'", token_table[token*2+1], curr_tok->lexeme);
}

void parser(TokenNode *tokens)
{
    curr_tok = tokens;
    translation_unit();
}

int in_first_declaration_specifiers(void)
{
    if (in_first_storage_class_specifier()
    ||  in_first_type_specifier()
    ||  in_first_type_qualifier())
        return TRUE;
    return FALSE;
}

int in_first_storage_class_specifier(void)
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

int in_first_type_specifier(void)
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
    case TOK_ID: {
        Symbol *s;

        if ((s=lookup(get_lexeme(1), TRUE)) != NULL)
            if (s->tok == TOK_TYPEDEFNAME)
                return TRUE;
        return FALSE;
    }
    default:
        return FALSE;
    }
}

int in_first_specifier_qualifier_list(void)
{
    if (in_first_type_specifier() || in_first_type_qualifier())
        return TRUE;
    return FALSE;
}

int in_first_type_qualifier(void)
{
    if (lookahead(1)==TOK_CONST || lookahead(1)==TOK_VOLATILE)
        return TRUE;
    return FALSE;
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
 * Search `typedef' between
 * the declaration specifiers.
 */
int search_typedef(TypeExp *t)
{
    for (; t != NULL; t = t->child)
        if (t->op == TOK_TYPEDEF)
            return TRUE;
    return FALSE;
}

/*
 * external_declaration = function_definition |
 *                        declaration
 */
ExternDecl *external_declaration(void)
{
    /*
    if (?)
        function_definition();
    else if (?)
        declaration();
    else
        // error
    */
    // TokenNode *temp;

    // temp = curr_tok; /* save */
    // declaration_specifiers();
    // if (lookahead(1) != TOK_SEMICOLON)
        // declarator();
    // if (lookahead(1) == TOK_LBRACE) {
        // curr_tok = temp; /* restore */
        // function_definition();
    // } else {
        // curr_tok = temp; /* restore */
        // declaration();
    // }
    ExternDecl *e;
    TypeExp *p, *q;

    q = NULL;
    e = malloc(sizeof(ExternDecl));
    e->sibling = NULL;

    p = declaration_specifiers(FALSE);
    if (lookahead(1) != TOK_SEMICOLON)
        q = concrete_declarator(TRUE, (search_typedef(p))?TOK_TYPEDEFNAME:TOK_ID);

    if (lookahead(1) == TOK_LBRACE) {
        e->kind = FUNCTION_DEFINITION;
        e->ed.f = function_definition(p, q);
    } else {
        e->kind = DECLARATION;
        e->ed.d = declaration(p, q);
    }

    return e;
}

/*
 * function_definition = declaration_specifiers declarator compound_statement
 */
FuncDef *function_definition(TypeExp *decl_specs, TypeExp *header)
{
    FuncDef *f;

    f = malloc(sizeof(FuncDef));
    f->decl_specs = decl_specs;
    f->header = header;
    restore_scope(); /* restore parameters' scope */

    f->body = compound_statement(FALSE);
    pop_scope();

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
    if (decl_specs != NULL) /* declaration() was called from external_declaration() */
        d->decl_specs = decl_specs;
    else
        d->decl_specs = declaration_specifiers(FALSE);

    if (first_declarator!=NULL || lookahead(1)!=TOK_SEMICOLON) {
        Token tok;

        tok = (search_typedef(d->decl_specs))?TOK_TYPEDEFNAME:TOK_ID;
        d->idl = init_declarator_list(tok, first_declarator);
    }
    check_decl_specs(d->decl_specs);

    match(TOK_SEMICOLON);

    //
    /*TypeExp *p = d->idl;
    while (p != NULL) {
        if (p->op == TOK_FUNCTION)
            printf("function returning\n");
        else if (p->op == TOK_SUBSCRIPT)
            printf("array of\n");
        else if (p->op == TOK_STAR)
            printf("pointer to\n");
        else if (p->op == TOK_ID)
            printf("id=%s\n", p->str);
        p = p->child;
    }
    p = d->decl_specs;
    while (p != NULL) {
        if (p->op == TOK_INT)
            printf("int\n");
        if (p->op == TOK_STATIC)
            printf("static\n");
        if (p->op == TOK_CONST)
            printf("const\n");
        p = p->child;
    }*/
    //

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
     *   If a typedef name is used as a declarator, the
     * declaration-specifiers of the declaration must contain
     * a type-specifier.
     */
    if (in_first_declaration_specifiers() && (lookahead(1)!=TOK_ID || !type_spec_seen))
        n->child = declaration_specifiers(type_spec_seen);

    return n;
}

/*
 * init_declarator_list =  init_declarator { "," init_declarator }
 */
TypeExp *init_declarator_list(Token tok, TypeExp *first_declarator)
{
    TypeExp *n, *temp;

    n = temp = init_declarator(tok, first_declarator);
    n->sibling = NULL;
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        temp->sibling = init_declarator(tok, NULL);
        temp = temp->sibling;
    }

    return n;
}

/*
 * init_declarator = declarator [ "=" initializer ]
 */
TypeExp *init_declarator(Token tok, TypeExp *first_declarator)
{
    TypeExp *n;

    if (first_declarator == NULL)
        n = concrete_declarator(TRUE, tok);
    else
        n = first_declarator;

    if (lookahead(1) == TOK_ASSIGN) {
        match(TOK_ASSIGN);
        n->attr.e = initializer();
    }

    return n;
}

TypeExp *new_type_exp_node(void)
{
    TypeExp *new_node;

    new_node = calloc(1, sizeof(TypeExp));
    new_node->info = curr_tok;
    ++number_of_ast_nodes;

    return new_node;
}

/*
storage_class_specifier = "typedef" |
                          "extern" |
                          "static" |
                          "auto" |
                          "register"
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
    default: /* shouldn't be reachable */
        ERROR("parser bug: storage_class_specifier()");
        break;
    }

    return s;
}

/*
type_specifier = "void" |
                 "char" |
                 "short" |
                 "int" |
                 "long" |
                 "float" |
                 "double" |
                 "signed" |
                 "unsigned" |
                 struct_or_union_specifier |
                 enum_specifier |
                 typedef_name
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
    default: /* shouldn't be reachable */
        ERROR("parser bug: type_specifier()");
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
        n->str = get_lexeme(1);
        match(TOK_ID);
        if (lookahead(1) == TOK_LBRACE) {
            match(TOK_LBRACE);
            n->attr.dl = struct_declaration_list();
            match(TOK_RBRACE);
        }
    } else if (lookahead(1) == TOK_LBRACE) {
        match(TOK_LBRACE);
        n->attr.dl = struct_declaration_list();
        match(TOK_RBRACE);
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
        ERROR("parser bug: struct_or_union()");

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
        temp->next->decl = struct_declaration();
        temp->next->next = NULL;
        temp = temp->next;
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
    n->idl = struct_declarator_list();
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
TypeExp *struct_declarator_list(void)
{
    TypeExp *n, *temp;

    n = temp = struct_declarator();
    n->sibling = NULL;
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        temp->sibling = struct_declarator();
        temp = temp->sibling;
    }

    return n;
}

/*
 * struct_declarator = declarator |
 *                     [ declarator ] ":" constant_expression
 */
TypeExp *struct_declarator(void)
{
    TypeExp *n;

    /*if (lookahead(1) == TOK_COLON) {
        match(TOK_COLON);
        constant_expression();
    } else {*/
        n = concrete_declarator(FALSE, 0);
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
    TypeExp *n;

    n = new_type_exp_node();
    n->op = lookahead(1);

    match(TOK_ENUM);
    if (lookahead(1) == TOK_ID) {
        n->str = get_lexeme(1);
        match(TOK_ID);
        if (lookahead(1) == TOK_LBRACE) {
            match(TOK_LBRACE);
            n->attr.el = enumerator_list();
            match(TOK_RBRACE);
        }
    } else if (lookahead(1) == TOK_LBRACE) {
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
    install(get_lexeme(1), TOK_ID);

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
        ERROR("parser bug: type_qualifier()");

    return n;
}

TypeExp *concrete_declarator(int install_id, Token tok)
{
    return declarator(CONCRETE_DECLARATOR, install_id, tok);
}

TypeExp *abstract_declarator(void)
{
    return declarator(ABSTRACT_DECLARATOR, 0, 0);
}

/*
 * declarator = [ pointer ] direct_declarator
 */
TypeExp *declarator(DeclaratorCategory dc, int install_id, Token tok)
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

        n = temp = direct_declarator(dc, install_id, tok);
        if (temp != NULL) {
            while (temp->child != NULL)
                temp = temp->child;
            temp->child = p;
        } else {
            n = p;
        }
    } else {
        n = direct_declarator(dc, install_id, tok);
    }

    return n;
}

/*
 * direct_declarator = [ identifier | "(" declarator ")" ] { direct_declarator_postfix }
 *
 * The parameters `install_id' and `tok' are used only
 * when dc != ABSTRACT_DECLARATOR, and their meaning is
 * as follows:
 *  If `install_id' == TRUE, install the identifier in the
 * parser symbol table, and `tok' indicates if install it as
 * an identifier or a typedef name.
 */
TypeExp *direct_declarator(DeclaratorCategory dc, int install_id, Token tok)
{
    TypeExp *n, *temp;

    n = temp = NULL;

    if (lookahead(1) == TOK_ID) {
        if (dc == ABSTRACT_DECLARATOR)
            ERROR("identifier not allowed in abstract-declarator");
        n = temp = new_type_exp_node();
        n->op = lookahead(1);
        n->str = get_lexeme(1);
        if (install_id)
            install(get_lexeme(1), tok);
        match(TOK_ID);
    } else if ((lookahead(1)==TOK_LPAREN) && (lookahead(2)==TOK_LPAREN
    || lookahead(2)==TOK_LBRACKET || lookahead(2)==TOK_STAR)) {
        match(TOK_LPAREN);
        n = temp = declarator(dc, install_id, tok);
        match(TOK_RPAREN);
    }

    if (n==NULL && dc==CONCRETE_DECLARATOR)
        ERROR("missing identifier in concrete-declarator");

    /*while (lookahead(1)==TOK_LBRACKET || lookahead(1)==TOK_LPAREN) {
        n = direct_declarator_postfix();
        n->child = temp;
        temp = n;
    }*/
    if (temp != NULL) {
        while (temp->child != NULL)
            temp = temp->child;
    }
    while (lookahead(1)==TOK_LBRACKET || lookahead(1)==TOK_LPAREN) {
        temp->child = direct_declarator_postfix();
        temp = temp->child;
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
    // n->op = lookahead(1);

    if (lookahead(1) == TOK_LBRACKET) {
        n->op = TOK_SUBSCRIPT;
        match(TOK_LBRACKET);
        if (lookahead(1) != TOK_RBRACKET)
            n->attr.e = constant_expression();
        match(TOK_RBRACKET);
    } else if (lookahead(1) == TOK_LPAREN) {
        n->op = TOK_FUNCTION;
        match(TOK_LPAREN);
        // if (lookahead(1) != TOK_RPAREN) {
            // if (lookahead(1) == TOK_ID)
                // identifier_list(); // old-style declarator
            // else // parameter_type_list or error
                n->attr.dl = parameter_type_list();
        // }
        match(TOK_RPAREN);
    } else {
        ERROR("parser bug: direct_declarator_postfix()");
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

    n = temp = type_qualifier();
    while (in_first_type_qualifier()) {
        temp->child = type_qualifier();
        temp = temp->child;
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
        temp->next->decl = malloc(sizeof(Declaration));
        temp->next->decl->decl_specs = new_type_exp_node();
        temp->next->decl->decl_specs->op = TOK_ELLIPSIS;
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
        temp->next->next = NULL;
        temp->next->decl = parameter_declaration();
        temp = temp->next;
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
    if (lookahead(1)!=TOK_COMMA && lookahead(1)!=TOK_RPAREN) { /* FOLLOW(parameter_declaration) */
        /* do not allow typedef-name declarations here (always pass TOK_ID to declarator()) */
        n->idl = declarator(EITHER_DECLARATOR, TRUE, TOK_ID);
        /*
         * To allow typedef-name declarations one can do:
         * n->idl = declarator(EITHER_DECLARATOR, TRUE, search_typedef(n->decl_specs)?TOK_TYPEDEFNAME:TOK_ID);
         */
    }

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

    return n;
}

/*
 * typedef_name = identifier
 */
TypeExp *typedef_name(void)
{
    TypeExp *n;

    /*
     * Don't check if identifier is indeed a typedef-name
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

/*
 * initializer = assignment_expression |
 *               "{" initializer_list [ "," ] "}"
 */
ExecNode *initializer(void)
{
    ExecNode *n;

    if (lookahead(1) == TOK_LBRACE) {
        match(TOK_LBRACE);
        n = initializer_list();
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

ExecNode *new_stmt_node(StmtKind kind)
{
    ExecNode *new_node;

    new_node = calloc(1, sizeof(ExecNode));
    new_node->node_kind = StmtNode;
    new_node->kind.exp = kind;
    new_node->src_line = curr_tok->src_line;
    ++number_of_ast_nodes;

    return new_node;
}

ExecNode *statement(void);
ExecNode *labeled_statement(void);
ExecNode *compound_statement(int new_scope);
DeclList *declaration_list(void);
ExecNode *statement_list(void);
ExecNode *expression_statement(void);
ExecNode *selection_statement(void);
ExecNode *iteration_statement(void);
ExecNode *jump_statement(void);

/*
 * statement = labeled_statement |
 *             compound_statement |
 *             expression_statement |
 *             selection_statement |
 *             iteration_statement |
 *             jump_statement
 */
ExecNode *statement(void)
{
    switch (lookahead(1)) {
    case TOK_LBRACE:
        return compound_statement(TRUE);
    case TOK_IF:
    case TOK_SWITCH:
        return selection_statement();
    case TOK_WHILE:
    case TOK_DO:
    case TOK_FOR:
        return iteration_statement();
    case TOK_GOTO:
    case TOK_CONTINUE:
    case TOK_BREAK:
    case TOK_RETURN:
        return jump_statement();
    case TOK_CASE:
    case TOK_DEFAULT:
        return labeled_statement();
    case TOK_ID:
        if (lookahead(2) == TOK_COLON)
            return labeled_statement();
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
ExecNode *labeled_statement(void)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_ID:
        n = new_stmt_node(LabelStmt);
        n->attr.str = get_lexeme(1);
        match(TOK_ID);
        match(TOK_COLON);
        n->child[0] = statement();
        break;
    case TOK_CASE:
        n = new_stmt_node(CaseStmt);
        match(TOK_CASE);
        n->child[0] = constant_expression();
        match(TOK_COLON);
        n->child[1] = statement();
        break;
    case TOK_DEFAULT:
        n = new_stmt_node(DefaultStmt);
        match(TOK_DEFAULT);
        match(TOK_COLON);
        n->child[0] = statement();
        break;
    }

    return n;
}

/*
 * compound_statement = "{" [ declaration_list ] [ statement_list ] "}"
 */
ExecNode *compound_statement(int new_scope)
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
        n->child[0] = statement_list();
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
ExecNode *statement_list(void)
{
    ExecNode *n, *temp;

    n = temp = statement();
    while (lookahead(1) != TOK_RBRACE) { /* FOLLOW(statement_list) = { "}" } */
        temp->sibling = statement();
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
ExecNode *selection_statement(void)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_IF:
        n = new_stmt_node(IfStmt);
        match(TOK_IF);
        match(TOK_LPAREN);
        n->child[0] = expression();
        match(TOK_RPAREN);
        n->child[1] = statement();
        if (lookahead(1) == TOK_ELSE) {
            match(TOK_ELSE);
            n->child[2] = statement();
        }
        break;
    case TOK_SWITCH:
        n = new_stmt_node(SwitchStmt);
        match(TOK_SWITCH);
        match(TOK_LPAREN);
        n->child[0] = expression();
        match(TOK_RPAREN);
        n->child[1] = statement();
        break;
    }

    return n;
}

/*
 * iteration_statement = "while" "(" expression ")" statement |
 *                       "do" statement "while" "(" expression ")" ";" |
 *                       "for" "(" [ expression ] ";" [ expression ] ";" [ expression ] ")" statement
 */
ExecNode *iteration_statement(void)
{
    ExecNode *n;

    switch (lookahead(1)) {
    case TOK_WHILE:
        n = new_stmt_node(WhileStmt);
        match(TOK_WHILE);
        match(TOK_LPAREN);
        n->child[0] = expression();
        match(TOK_RPAREN);
        n->child[1] = statement();
        break;
    case TOK_DO:
        n = new_stmt_node(DoStmt);
        match(TOK_DO);
        n->child[0] = statement();
        match(TOK_WHILE);
        match(TOK_LPAREN);
        n->child[1] = expression();
        match(TOK_RPAREN);
        match(TOK_SEMICOLON);
        break;
    case TOK_FOR:
        n = new_stmt_node(ForStmt);
        match(TOK_FOR);
        match(TOK_LPAREN);
        if (lookahead(1) != TOK_SEMICOLON)
            n->child[0] = expression();
        match(TOK_SEMICOLON);
        if (lookahead(1) != TOK_SEMICOLON)
            n->child[1] = expression();
        match(TOK_SEMICOLON);
        if (lookahead(1) != TOK_RPAREN)
            n->child[2] = expression();
        match(TOK_RPAREN);
        n->child[3] = statement();
        break;
    }

    return n;
}

/*
 * jump_statement = "goto" identifier ";" |
 *                  "continue" ";" |
 *                  "break" ";" |
 *                  "return" [ expression ] ";"
 */
ExecNode *jump_statement(void)
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

    return n;
}

// =============================================================================
// Expressions
// =============================================================================

ExecNode *new_op_node(Token op)
{
    ExecNode *new_node;

    new_node = calloc(1, sizeof(ExecNode));
    new_node->node_kind = ExpNode;
    new_node->kind.exp = OpExp;
    new_node->attr.op = op;
    new_node->src_line = curr_tok->src_line;
    ++number_of_ast_nodes;

    return new_node;
}

ExecNode *new_pri_exp_node(ExpKind kind)
{
    ExecNode *new_node;

    new_node = calloc(1, sizeof(ExecNode));
    new_node->node_kind = ExpNode;
    new_node->kind.exp = kind;
    new_node->src_line = curr_tok->src_line;
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

    n = temp = assignment_expression();
    while (lookahead(1) == TOK_COMMA) {
        temp = new_op_node(TOK_COMMA);
        match(TOK_COMMA);
        temp->child[0] = n;
        temp->child[1] = assignment_expression();
        n = temp;
    }

    return n;
}

/*
 * assignment_operator = "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="
 */
#define IS_ASSIGNMENT_OP(tok) (tok>=TOK_ASSIGN && tok<=TOK_BW_OR_ASSIGN)

/*
 * assignment_expression = conditional_expression [ assignment_operator assignment_expression ]
 *
 * Note: the original production that appears on
 * the standard is:
 *
 *      assignment_expression = conditional_expression |
 *                              unary_expression assignment_operator assignment_expression
 *
 * To simplify the parsing conditional_expression
 * is accepted as a left-hand operand of an assignment
 * operator. As a consequence of this, the expression
 * `1+2=3' will be detected as an error during semantic
 * analysis instead of during parsing.
 */
ExecNode *assignment_expression(void)
{
    ExecNode *n, *temp;

    n = temp = conditional_expression();
    if (IS_ASSIGNMENT_OP(lookahead(1))) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = assignment_expression();
        n = temp;
    }

    return n;
}

/*
 * conditional_expression = logical_OR_expression [ "?" expression ":" conditional_expression ]
 */
ExecNode *conditional_expression(void)
{
    ExecNode *n, *temp;

    n = temp = logical_OR_expression();
    if (lookahead(1) == TOK_CONDITIONAL) {
        temp = new_op_node(TOK_CONDITIONAL);
        match(TOK_CONDITIONAL);
        temp->child[0] = n;
        temp->child[1] = expression();
        match(TOK_COLON);
        temp->child[2] = conditional_expression();
        n = temp;
    }

    return n;
}

/*
 * logical_OR_expression = logical_AND_expression { "||" logical_AND_expression }
 */
ExecNode *logical_OR_expression(void)
{
    ExecNode *n, *temp;

    n = temp = logical_AND_expression();
    while (lookahead(1) == TOK_OR) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = logical_AND_expression();
        n = temp;
    }

    return n;
}

/*
 * logical_AND_expression = inclusive_OR_expression { "&&" inclusive_OR_expression }
 */
ExecNode *logical_AND_expression(void)
{
    ExecNode *n, *temp;

    n = temp = inclusive_OR_expression();
    while (lookahead(1) == TOK_AND) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = inclusive_OR_expression();
        n = temp;
    }

    return n;
}

/*
 * inclusive_OR_expression = exclusive_OR_expression { "|" exclusive_OR_expression }
 */
ExecNode *inclusive_OR_expression(void)
{
    ExecNode *n, *temp;

    n = temp = exclusive_OR_expression();
    while (lookahead(1) == TOK_BW_OR) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = exclusive_OR_expression();
        n = temp;
    }

    return n;
}

/*
 * exclusive_OR_expression = AND_expression { "^" AND_expression }
 */
ExecNode *exclusive_OR_expression(void)
{
    ExecNode *n, *temp;

    n = temp = AND_expression();
    while (lookahead(1) == TOK_BW_XOR) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = AND_expression();
        n = temp;
    }

    return n;
}

/*
 * AND_expression = equality_expression { "&" equality_expression }
 */
ExecNode *AND_expression(void)
{
    ExecNode *n, *temp;

    n = temp = equality_expression();
    while (lookahead(1) == TOK_AMPERSAND) {
        temp = new_op_node(TOK_BW_AND);
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = equality_expression();
        n = temp;
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

    n = temp = relational_expression();
    while (lookahead(1)==TOK_EQ || lookahead(1)==TOK_NEQ) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = relational_expression();
        n = temp;
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

    n = temp = shift_expression();
    while (IS_RELOP(lookahead(1))) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = shift_expression();
        n = temp;
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

    n = temp = additive_expression();
    while (lookahead(1)==TOK_LSHIFT || lookahead(1)==TOK_RSHIFT) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = additive_expression();
        n = temp;
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

    n = temp = multiplicative_expression();
    while (lookahead(1)==TOK_PLUS || lookahead(1)==TOK_MINUS) {
        temp = new_op_node(lookahead(1));
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = multiplicative_expression();
        n = temp;
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

    n = temp = cast_expression();
    while (lookahead(1)==TOK_STAR || lookahead(1)==TOK_DIV || lookahead(1)==TOK_MOD) {
        temp = new_op_node((lookahead(1)!=TOK_STAR)?lookahead(1):TOK_MUL);
        match(lookahead(1));
        temp->child[0] = n;
        temp->child[1] = cast_expression();
        n = temp;
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
            n->attr.tn = type_name();
            match(TOK_RPAREN);
            n->child[0] = cast_expression();
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
                n->attr.tn = type_name();
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
        break;
    }

    return n;
}

#define IS_POSTFIX_OP(t) (t==TOK_LBRACKET || t==TOK_LPAREN || t==TOK_DOT || t==TOK_ARROW\
|| t==TOK_INC || t==TOK_DEC)

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
        n->attr.str = get_lexeme(1);
        match(TOK_ID);
        break;
    case TOK_ARROW:
        n = new_op_node(TOK_ARROW);
        match(TOK_ARROW);
        n->attr.str = get_lexeme(1);
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
     * C99 Standard. 6.5.1.2:
     * An identifier is a primary expression, provided it has been declared as designating an
     * object (in which case it is an lvalue) or a function (in which case it is a function
     * designator).
     * Footnote: Thus, an undeclared identifier is a violation of the syntax.
     */
        Symbol *s;

        if ((s=lookup(get_lexeme(1), TRUE)) == NULL)
            ERROR("undeclared identifier `%s'", get_lexeme(1));
        if (s->tok == TOK_ID) {
            n = new_pri_exp_node(IdExp);
            n->attr.str = get_lexeme(1);
            match(TOK_ID);
        } else {
            ERROR("expecting primary-expression; found typedef-name `%s'", get_lexeme(1));
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
        break;
    default:
        ERROR("expecting primary-expression; found `%s'", get_lexeme(1));
        break;
    }

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
