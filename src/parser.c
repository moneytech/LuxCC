#include "parser.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include "util.h"


#define TRUE  1
#define FALSE 0
#define ERROR(...) \
    do {\
    if (!speculating) {\
        fprintf(stderr, "error: %s, ", __VA_ARGS__),\
        fprintf(stderr, "curr_tok->lexeme: %s\n", curr_tok->lexeme),\
        exit(1);\
    } else {\
        longjmp(env, 1);\
    }\
    } while (0)


static jmp_buf env;
static TokenNode *curr_tok;
static int speculating = FALSE;


#define HASH_SIZE 101
#define MAX_NEST  16
typedef struct Symbol Symbol;
struct Symbol {
    char *id;
    Token tok;
    Symbol *next;
};
Symbol *symbols[HASH_SIZE][MAX_NEST];
int nesting_level = 0;


/*
 * Recursive parser functions.
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
TypeExp *declarator(int install_id, Token tok);
TypeExp *direct_declarator(int install_id, Token tok);
TypeExp *direct_declarator_postfix(void);
TypeExp *pointer(void);
TypeExp *type_qualifier_list(void);
DeclList *parameter_type_list(void);
DeclList *parameter_list(void);
Declaration *parameter_declaration(void);
void identifier_list(void);
void type_name(void);
TypeExp *abstract_declarator(void);
TypeExp *direct_abstract_declarator(void);
TypeExp *direct_abstract_declarator_postfix(void);
TypeExp *typedef_name(void);
ExecNode *initializer(void);
ExecNode *initializer_list(void);

DeclList *declaration_list(void);
ExecNode *assignment_expression(void);
ExecNode *constant_expression(void);
ExecNode *compound_statement(int new_scope);

/*
 * Functions that test lookahead(1).
 */
int in_first_declaration_specifiers(void);
int in_first_storage_class_specifier(void);
int in_first_type_specifier(void);
int in_first_specifier_qualifier_list(void);
int in_first_type_qualifier(void);

/*
 * Other functions.
 */
int speculate_declaration(void);


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
        if (speculating)
            longjmp(env, 1);
        else
            ERROR("match");
}

void parser(TokenNode *tokens)
{
    curr_tok = tokens;
    translation_unit();
}

int speculate_declaration(void)
{
    int success;
    TokenNode *temp;

    temp = curr_tok;
    speculating = TRUE;
    if (!setjmp(env)) {
        // declaration();
        success = TRUE;
    } else {
        success = FALSE;
    }
    speculating = FALSE;
    curr_tok = temp;

    return success;
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

void push_scope(void);
void pop_scope(void);
Symbol *lookup(char *id, int all);
void install(char *id, Token tok);
void restore_scope(void);

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
 * See if `typedef' is between
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
        q = declarator(TRUE, (search_typedef(p))?TOK_TYPEDEFNAME:TOK_ID);

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
    f->decl_specs = decl_specs; //declaration_specifiers(FALSE);
    f->header = header; //declarator(TRUE, TOK_ID);
    restore_scope(); /* restore parameters' scope */

    f->body = compound_statement(FALSE);
    pop_scope();

    return f;
}

// =============================================================================
// Declarations
// =============================================================================

int delayed_delete = FALSE;

void delete_scope(void)
{
    if (nesting_level < 0) {
        fprintf(stderr, "Underflow in pop_scope().\n");
        exit(EXIT_FAILURE);
    }
    Symbol *np, *temp;
    int i;

    for (i = 0; i < HASH_SIZE; i++) {
        if (symbols[i][nesting_level] != NULL) {
            for (np = symbols[i][nesting_level]; np != NULL;) { /* delete chain */
                temp = np;
                np = np->next;
                free(temp);
            }
            symbols[i][nesting_level] = NULL;
        }
    }
    --nesting_level;
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

    if (++nesting_level == MAX_NEST) {
        fprintf(stderr, "Too many nested scopes.\n");
        exit(EXIT_FAILURE);
    }
    // DEBUG_PRINTF("Pushing scope, nest_level=%d\n", nest_level);
}

void pop_scope(void)
{
    if (delayed_delete)
        delete_scope();
    else
        delayed_delete = TRUE;
}

Symbol *lookup(char *id, int all)
{
    Symbol *np;
    int n = nesting_level;

    if (delayed_delete)
        delete_scope();

    if (all == TRUE) {
        for (; n >= 0; n--)
            for (np = symbols[hash(id)%HASH_SIZE][n]; np != NULL; np = np->next)
                if (strcmp(id, np->id) == 0)
                    return np;
        return NULL; /* not found */
    } else {
        for (np = symbols[hash(id)%HASH_SIZE][n]; np != NULL; np = np->next)
            if (strcmp(id, np->id) == 0)
                return np;
        return NULL; /* not found */
    }
}

void install(char *id, Token tok)
{
    Symbol *np;
    unsigned hash_val;

    if (delayed_delete)
        delete_scope();

    if ((np=lookup(id, FALSE)) == NULL) { /* not found */
        np = malloc(sizeof(Symbol));
        np->id = id;
        np->tok = tok;
        hash_val = hash(id)%HASH_SIZE;
        np->next = symbols[hash_val][nesting_level];
        symbols[hash_val][nesting_level] = np;
    } else { /* already in this scope */
        np->tok = tok; /* overwrite token type */
    }
}

/*
 * declaration = declaration_specifiers [ init_declarator_list ] ";"
 */
Declaration *declaration(TypeExp *decl_specs, TypeExp *first_declarator)
{
    Token tok;
    Declaration *d;

    d = malloc(sizeof(Declaration));
    d->idl = NULL;
    if (decl_specs != NULL) /* declaration() was called from external_declaration() */
        d->decl_specs = decl_specs;
    else
        d->decl_specs = declaration_specifiers(FALSE);

    tok = search_typedef(d->decl_specs)?TOK_TYPEDEFNAME:TOK_ID;
    if (first_declarator!=NULL || lookahead(1)!=TOK_SEMICOLON)
        d->idl = init_declarator_list(tok, first_declarator);

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
        ERROR("declaration_specifiers");

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
        n = declarator(TRUE, tok);
    else
        n = first_declarator;

    if (lookahead(1) == TOK_ASSIGN) {
        match(TOK_ASSIGN);
        initializer();
    }

    return n;
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

    s = malloc(sizeof(TypeExp));
    s->child = NULL;
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
        ERROR("storage_class_specifier");
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

    n = malloc(sizeof(TypeExp));
    n->child = NULL;
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
    /* assume type_specifier() was called because in_first_type_specifier() returned TRUE */
        free(n);
        n = typedef_name();
        break;
    default:
        ERROR("type_specifier");
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
        ERROR("struct_or_union_specifier");
    }

    return n;
}

/*
 * struct_or_union = "struct" | "union"
 */
TypeExp *struct_or_union(void)
{
    TypeExp *n;

    n = malloc(sizeof(TypeExp));
    n->child = NULL;
    n->op = lookahead(1);

    if (lookahead(1) == TOK_STRUCT)
        match(TOK_STRUCT);
    else if (lookahead(1) == TOK_UNION)
        match(TOK_UNION);
    else
        ERROR("struct_or_union");

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
        n = type_specifier(); //, type_spec_seen = TRUE;
    else if (in_first_type_qualifier())
        n = type_qualifier(), type_spec_seen = TRUE;
    else
        ERROR("specifier_qualifier_list");

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
        n = declarator(FALSE, 0);
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

    n = malloc(sizeof(TypeExp));
    n->op = lookahead(1);
    n->child = NULL;

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
        ERROR("enum_specifier");
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

    n = malloc(sizeof(TypeExp));
    n->sibling = NULL;
    n->str = get_lexeme(1);
    install(get_lexeme(1), TOK_ID);

    match(TOK_ID);
}

/*
 * type_qualifier = "const" | "volatile"
 */
TypeExp *type_qualifier(void)
{
    TypeExp *n;

    n = malloc(sizeof(TypeExp));
    n->op = lookahead(1);
    n->child = NULL;

    if (lookahead(1) == TOK_CONST)
        match(TOK_CONST);
    else if (lookahead(1) == TOK_VOLATILE)
        match(TOK_VOLATILE);
    else
        ERROR("type_qualifier");

    return n;
}

/*
 * declarator = [ pointer ] direct_declarator
 */
TypeExp *declarator(int install_id, Token tok)
{
    TypeExp *n, *temp;

    if (lookahead(1) == TOK_ASTERISK) {
        n = temp = pointer();
        while (temp->child != NULL)
            temp = temp->child;
        temp->child = direct_declarator(install_id, tok);
    } else {
        n = direct_declarator(install_id, tok);
    }

    return n;
}

/*
 * direct_declarator = ( identifier | "(" declarator ")" ) { direct_declarator_postfix }
 *
 * Install the identifier in the parser symbol
 * table if `install_id' == TRUE. `tok' indicates
 * if it's an identifier or a typedef name.
 */
TypeExp *direct_declarator(int install_id, Token tok)
{
    TypeExp *n, *temp;

    if (lookahead(1) == TOK_ID) {
        n = temp = malloc(sizeof(TypeExp));
        n->child = NULL;
        n->op = lookahead(1);
        n->str = get_lexeme(1);
        if (install_id)
            install(get_lexeme(1), tok);
        match(TOK_ID);
    } else if (lookahead(1) == TOK_LPAREN) {
        match(TOK_LPAREN);
        n = temp = declarator(install_id, tok);
        match(TOK_RPAREN);
    } else {
        ERROR("direct_declarator");
    }

    while (lookahead(1)==TOK_LBRACKET || lookahead(1)==TOK_LPAREN) {
        n = direct_declarator_postfix();
        n->child = temp;
        temp = n;
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

    n = malloc(sizeof(TypeExp));
    n->child = NULL;
    n->op = lookahead(1);

    if (lookahead(1) == TOK_LBRACKET) {
        match(TOK_LBRACKET);
        if (lookahead(1) != TOK_RBRACKET)
            n->attr.e = constant_expression();
        match(TOK_RBRACKET);
    } else if (lookahead(1) == TOK_LPAREN) {
        match(TOK_LPAREN);
        // if (lookahead(1) != TOK_RPAREN) {
            // if (lookahead(1) == TOK_ID)
                // identifier_list(); // old-style declarator
            // else // parameter_type_list or error
                n->attr.dl = parameter_type_list();
        // }
        match(TOK_RPAREN);
    } else {
        ERROR("direct_declarator_postfix");
    }

    return n;
}

/*
 * pointer = "*" [ type_qualifier_list ] [ pointer ]
 */
TypeExp *pointer(void)
{
    TypeExp *n, *temp;

    n = temp = malloc(sizeof(TypeExp));
    n->op = lookahead(1);
    n->child = NULL;

    match(TOK_ASTERISK);
    if (in_first_type_qualifier())
        n->child = type_qualifier_list();

    if (lookahead(1) == TOK_ASTERISK) {
        while (temp->child != NULL)
            temp = temp->child;
        temp->child = pointer();
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
        temp->next->decl->decl_specs = malloc(sizeof(TypeExp));
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

int speculate_declarator(void)
{
    // int success;
    // TokenNode *temp;
//
    // temp = curr_tok;
    // speculating = TRUE;
    // if (!setjmp(env)) {
        // declarator();
        // success = TRUE;
    // } else {
        // success = FALSE;
    // }
    // speculating = FALSE;
    // curr_tok = temp;
//
    // return success;
}

/*
 * parameter_declaration = declaration_specifiers declarator |
 *                         declaration_specifiers [ abstract_declarator ]
 */
Declaration *parameter_declaration(void)
{
    Declaration *n;

    n = malloc(sizeof(Declaration));

    n->decl_specs = declaration_specifiers(FALSE);
    if (lookahead(1)!=TOK_COMMA && lookahead(1)!=TOK_RPAREN) { /* FOLLOW(parameter_declaration) */
        /*if (speculate_declarator())
            declarator();
        else
            abstract_declarator();*/
        /*
         * Try to distinguish between a declarator and an abstract_declarator. Search for
         * an identifier `([here],', `,[here],', or `,[here])', depending on where the
         * the parameter is.
         * TO FIX: there are some constructs that will be reported as a declarator, although
         * they are not. For example:
         *                  `int (*)(int x)'
         */
        int i, pn, id_found;

        i = 1;
        pn = 0;
        id_found = FALSE;
        while (pn>0 || (lookahead(i)!=TOK_COMMA && lookahead(i)!=TOK_RPAREN)) {
            if (lookahead(i) == TOK_LPAREN) {
                ++pn;
            } else if (lookahead(i) == TOK_RPAREN) {
                --pn;
            } else if (lookahead(i) == TOK_ID) {
                id_found = TRUE;
                break;
            }
            ++i;
        }
        if (id_found)
            /* `typedef' must not appear in a parameter declaration */
            n->idl = declarator(TRUE, TOK_ID);
        else
            n->idl = abstract_declarator();
    }

    return n;
}

/*
 * identifier_list = identifier { "," identifier }
 */
void identifier_list(void)
{
    match(TOK_ID);
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        match(TOK_ID);
    }
}

/*
 * type_name = specifier_qualifier_list [ abstract_declarator ]
 */
void type_name(void)
{
    specifier_qualifier_list(FALSE);
    if (lookahead(1) != TOK_RPAREN) /* FOLLOW(type_name) = { ")" } */
        abstract_declarator();
}

/*
 * abstract_declarator = pointer |
 *                       [ pointer ] direct_abstract_declarator
 */
TypeExp *abstract_declarator(void)
{
    if (lookahead(1) == TOK_ASTERISK) {
        match(TOK_ASTERISK);
        if (lookahead(1)==TOK_LPAREN || lookahead(1)==TOK_LBRACKET)
            direct_abstract_declarator();
    } else {
        direct_abstract_declarator();
    }
}

/*
 * direct_abstract_declarator = "(" abstract_declarator ")" { direct_abstract_declarator_postfix } |
 *                              direct_abstract_declarator_postfix { direct_abstract_declarator_postfix }
 */
TypeExp *direct_abstract_declarator(void)
{
    if (lookahead(1) == TOK_LPAREN) {
        if (lookahead(2)==TOK_ASTERISK || lookahead(2)==TOK_LPAREN || lookahead(2)==TOK_LBRACKET) { /* FIRST(abstract_declarator) */
            match(TOK_LPAREN);
            abstract_declarator();
            match(TOK_RPAREN);
        } else {
            direct_abstract_declarator_postfix();
        }
    } else { /* "[" [ constant_expression ] "]" or error */
        direct_abstract_declarator_postfix();
    }

    while (lookahead(1)==TOK_LBRACKET || lookahead(1)==TOK_LPAREN)
        direct_abstract_declarator_postfix();
}

/*
 * direct_abstract_declarator_postfix = "[" [ constant_expression ] "]" |
 *                                      "(" [ parameter_type_list ] ")"
 */
TypeExp *direct_abstract_declarator_postfix(void)
{
    if (lookahead(1) == TOK_LBRACKET) {
        match(TOK_LBRACKET);
        if (lookahead(1) != TOK_RBRACKET)
            constant_expression();
        match(TOK_RBRACKET);
    } else if (lookahead(1) == TOK_LPAREN) {
        match(TOK_LPAREN);
        if (lookahead(1) != TOK_RPAREN)
            parameter_type_list();
        match(TOK_RPAREN);
    } else {
        ERROR("direct_abstract_declarator_postfix");
    }
}

/*
 * typedef_name = identifier
 */
TypeExp *typedef_name(void)
{
    TypeExp *n;

    n = malloc(sizeof(TypeExp));
    n->op = TOK_TYPEDEFNAME;
    n->str = get_lexeme(1);
    n->child = NULL;
    printf("typedef-name found:%s\n", get_lexeme(1));

    match(TOK_ID);

    return n;
}

/*
 * initializer = assignment_expression |
 *               "{" initializer_list [ "," ] "}"
 */
ExecNode *initializer(void)
{
    if (lookahead(1) == TOK_LBRACE) {
        match(TOK_LBRACE);
        initializer_list();
        if (lookahead(1) == TOK_COMMA)
            match(TOK_COMMA);
        match(TOK_RBRACE);
    } else {
        assignment_expression();
    }
}

/*
 * initializer_list = initializer { "," initializer }
 */
ExecNode *initializer_list(void)
{
    initializer();
    while (lookahead(1)==TOK_COMMA && lookahead(2)!=TOK_RBRACE) {
        match(TOK_COMMA);
        initializer();
    }
}

#if 0
// =============================================================================
// Statements
// =============================================================================

statement = labeled_statement |
            compound_statement |
            expression_statement |
            selection_statement |
            iteration_statement |
            jump_statement ;

labeled_statement = identifier ":" statement |
                    "case" constant_expression ":" statement |
                    "default" ":" statement ;
#endif
/*
 * compound_statement = "{" [ declaration_list ] [ statement_list ] "}"
 */
ExecNode *compound_statement(int new_scope)
{
    ExecNode *n;

    n = malloc(sizeof(ExecNode));
    n->locals = NULL;

    match(TOK_LBRACE);
    if (in_first_declaration_specifiers()) {
        if (new_scope)
            push_scope();
        n->locals = declaration_list();
    }
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

#if 0
statement_list = statement { statement } ;

expression_statement = [ expression ] ";" ;

selection_statement = "if" "(" expression ")" statement [ "else" statement ] |
                      "switch" "(" expression ")" statement ;

iteration_statement = "while" "(" expression ")" statement |
                      "do" statement "while" "(" expression ")" ";" |
                      "for" "(" [ expression ] ";" [ expression ] ";" [ expression ] ")" statement ;

jump_statement = "goto" identifier ";" |
                 "continue" ";" |
                 "break" ";" |
                 "return" [ expression ] ";" ;


// =============================================================================
// Expressions
// =============================================================================

primary_expression = identifier |
                     constant |
                     string_literal |
                     "(" expression ")" ;

postfix_expression = primary_expression { postfix } ;

postfix = "[" expression "]" |
          "(" [ argument_expression_list ] ")" |
          "." identifier |
          "->" identifier |
          "++" |
          "--" ;

argument_expression_list = assignment_expression { "," assignment_expression } ;

unary_expression = postfix_expression |
                   "++" unary_expression |
                   "--" unary_expression |
                   unary_operator cast_expression |
                   "sizeof" unary_expression |
                   "sizeof" "(" type_name ")" ;

unary_operator = "&" | "*" | "+" | "-" | "~" | "!" ;

cast_expression = unary_expression |
                  "(" type_name ")" cast_expression ;

expression = assignment_expression { "," assignment_expression } ;
#endif

/*
 * assignment_expression = conditional_expression |
 *                         unary_expression assignment_operator assignment_expression
 */
ExecNode *assignment_expression(void)
{
    if (lookahead(1) == TOK_ICONST)
        match(TOK_ICONST);
    else if (lookahead(1) == TOK_ID) {
        Symbol *s;
        if ((s=lookup(get_lexeme(1), TRUE)) != NULL)
            if (s->tok == TOK_TYPEDEFNAME)
                ERROR("assignment_expression, identifier expected");
        match(TOK_ID);
    }
}

#if 0
assignment_operator = "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|=" ;

conditional_expression = logical_OR_expression [ "?" expression ":" conditional_expression ] ;
#endif

/*
 * constant_expression = conditional_expression
 */
ExecNode *constant_expression(void)
{
    match(TOK_ICONST);
}

#if 0
logical_OR_expression = logical_AND_expression { "||" logical_AND_expression } ;

logical_AND_expression = inclusive_OR_expression { "&&" inclusive_OR_expression } ;

inclusive_OR_expression = exclusive_OR_expression { "|" exclusive_OR_expression } ;

exclusive_OR_expression = AND_expression { "^" AND_expression } ;

AND_expression = equality_expression { "&" equality_expression } ;

equality_expression = relational_expression { equop relational_expression } ;

equop = "==" | "!=" ;

relational_expression = shift_expression { relop shift_expression };

relop = "<" | ">" | "<=" | ">=" ;

shift_expression = additive_expression { shiftop additive_expression } ;

shiftop = "<<" | ">>" ;

additive_expression = multiplicative_expression { addop multiplicative_expression } ;

addop = "+" | "-" ;

multiplicative_expression = cast_expression { mulop cast_expression } ;

mulop = "*" | "/" | "%" ;


// =============================================================================
// Preprocessing directives
// =============================================================================
????
#endif
