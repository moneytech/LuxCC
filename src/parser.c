#include "parser.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>


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


/*
 * Recursive parser functions.
 */
ExternDecl *translation_unit(void);
ExternDecl *external_declaration(void);
FuncDef *function_definition(void);
Declaration *declaration(void);
TypeExp *declaration_specifiers(void);
TypeExp *init_declarator_list(void);
TypeExp *init_declarator(void);
TypeExp *storage_class_specifier(void);
TypeExp *type_specifier(void);
TypeExp *struct_or_union_specifier(void);
TypeExp *struct_or_union(void);
DeclList *struct_declaration_list(void);
Declaration *struct_declaration(void);
TypeExp *specifier_qualifier_list(void);
TypeExp *struct_declarator_list(void);
TypeExp *struct_declarator(void);
TypeExp *enum_specifier(void);
TypeExp *enumerator_list(void);
TypeExp *enumerator(void);
TypeExp *enumeration_constant(void);
TypeExp *type_qualifier(void);
TypeExp *declarator(void);
TypeExp *direct_declarator(void);
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
void typedef_name(void);
ExecNode *initializer(void);
ExecNode *initializer_list(void);

void declaration_list(void);
ExecNode *assignment_expression(void);
ExecNode *constant_expression(void);
ExecNode *compound_statement(void);

/*
 * Other functions.
 */
int speculate_declaration(void);
int in_first_declaration_specifiers(Token t);
int in_first_storage_class_specifier(Token t);
int in_first_type_specifier(Token t);
int in_first_specifier_qualifier_list(Token t);
int in_first_type_qualifier(Token t);


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
        declaration();
        success = TRUE;
    } else {
        success = FALSE;
    }
    speculating = FALSE;
    curr_tok = temp;

    return success;
}

int in_first_declaration_specifiers(Token t)
{
    if (in_first_storage_class_specifier(t)
    ||  in_first_type_specifier(t)
    ||  in_first_type_qualifier(t))
        return TRUE;
    return FALSE;
}

int in_first_storage_class_specifier(Token t)
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

int in_first_type_specifier(Token t)
{
    switch (t) {
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
    // case TOK_ID:
        return TRUE;
    default:
        return FALSE;
    }
}

int in_first_specifier_qualifier_list(Token t)
{
    if (in_first_type_specifier(t) || in_first_type_qualifier(t))
        return TRUE;
    return FALSE;
}

int in_first_type_qualifier(Token t)
{
    if (t==TOK_CONST || t==TOK_VOLATILE)
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
    TokenNode *temp;

    q = NULL;
    e = malloc(sizeof(ExternDecl));
    e->sibling = NULL;

    temp = curr_tok; /* save */
    p = declaration_specifiers();
    if (lookahead(1) != TOK_SEMICOLON)
        q = declarator();
    /* --- delete p and q --- */
    if (lookahead(1) == TOK_LBRACE) {
        curr_tok = temp; /* restore */
        e->kind = FUNCTION_DEFINITION;
        e->ed.f = function_definition();
    } else {
        curr_tok = temp; /* restore */
        e->kind = DECLARATION;
        e->ed.d = declaration();
    }

    return e;
}

/*
 * function_definition = declaration_specifiers declarator compound_statement
 */
FuncDef *function_definition(void)
{
    FuncDef *f;

    f = malloc(sizeof(FuncDef));
    f->decl_specs = declaration_specifiers();
    f->header = declarator();
    f->body = compound_statement();

    return f;
}

// =============================================================================
// Declarations
// =============================================================================

/*
 * declaration = declaration_specifiers [ init_declarator_list ] ";"
 */
Declaration *declaration(void)
{
    Declaration *d;

    d = malloc(sizeof(Declaration));
    d->decl_specs = declaration_specifiers();
    if (lookahead(1) != TOK_SEMICOLON)
        d->idl = init_declarator_list();
    match(TOK_SEMICOLON);

    return d;
}

/*
 * declaration_specifiers = storage_class_specifier [ declaration_specifiers ] |
 *                          type_specifier [ declaration_specifiers ] |
 *                          type_qualifier [ declaration_specifiers ]
 */
TypeExp *declaration_specifiers(void)
{
    TypeExp *n;

    if (in_first_storage_class_specifier(lookahead(1)))
        n = storage_class_specifier();
    else if (in_first_type_specifier(lookahead(1)))
        n = type_specifier();
    else if (in_first_type_qualifier(lookahead(1)))
        n = type_qualifier();
    else
        ERROR("declaration_specifiers");

    if (in_first_declaration_specifiers(lookahead(1)))
        n->child = declaration_specifiers();

    return n;
}

/*
 * init_declarator_list =  init_declarator { "," init_declarator }
 */
TypeExp *init_declarator_list(void)
{
    TypeExp *n, *temp;

    n = temp = init_declarator();
    n->sibling = NULL;
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        temp->sibling = init_declarator();
        temp = temp->sibling;
    }

    return n;
}

/*
 * init_declarator = declarator [ "=" initializer ]
 */
TypeExp *init_declarator(void)
{
    TypeExp *n;

    n = declarator();
    // if (lookahead(1) == TOK_ASSIGN) {
        // match(TOK_ASSIGN);
        // initializer();
    // }

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
    // case TOK_ID:
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
    while (in_first_specifier_qualifier_list(lookahead(1))) {
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

    n->decl_specs = specifier_qualifier_list();
    n->idl = struct_declarator_list();
    match(TOK_SEMICOLON);

    return n;
}

/*
 * specifier_qualifier_list = ( type_specifier | type_qualifier ) [ specifier_qualifier_list ]
 */
TypeExp *specifier_qualifier_list(void)
{
    TypeExp *n;

    if (in_first_type_specifier(lookahead(1)))
        n = type_specifier();
    else if (in_first_type_qualifier(lookahead(1)))
        n = type_qualifier();
    else
        ERROR("specifier_qualifier_list");

    if (in_first_specifier_qualifier_list(lookahead(1)))
        n->child = specifier_qualifier_list();

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

    if (lookahead(1) == TOK_COLON) {
        match(TOK_COLON);
        constant_expression();
    } else {
        n = declarator();
        if (lookahead(1) == TOK_COLON) {
            match(TOK_COLON);
            constant_expression();
        }
    }

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
TypeExp *declarator(void)
{
    TypeExp *n, *temp;

    if (lookahead(1) == TOK_ASTERISK) {
        n = temp = pointer();
        while (temp->child != NULL)
            temp = temp->child;
        temp->child = direct_declarator();
    } else {
        n = direct_declarator();
    }

    return n;
}

/*
 * direct_declarator = ( identifier | "(" declarator ")" ) { direct_declarator_postfix }
 */
TypeExp *direct_declarator(void)
{
    TypeExp *n, *temp;

    if (lookahead(1) == TOK_ID) {
        n = temp = malloc(sizeof(TypeExp));
        n->child = NULL;
        n->op = lookahead(1);
        n->str = get_lexeme(1);
        match(TOK_ID);
    } else if (lookahead(1) == TOK_LPAREN) {
        match(TOK_LPAREN);
        n = temp = declarator();
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
        if (lookahead(1) != TOK_RPAREN) {
            if (lookahead(1) == TOK_ID)
                identifier_list(); /* old-style declarator */
            else /* parameter_type_list or error */
                n->attr.dl = parameter_type_list();
        }
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
    if (in_first_type_qualifier(lookahead(1)))
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
    while (in_first_type_qualifier(lookahead(1))) {
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

    n = temp = parameter_list();
    if (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        match(TOK_ELLIPSIS);
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
    int success;
    TokenNode *temp;

    temp = curr_tok;
    speculating = TRUE;
    if (!setjmp(env)) {
        declarator();
        success = TRUE;
    } else {
        success = FALSE;
    }
    speculating = FALSE;
    curr_tok = temp;

    return success;
}

/*
 * parameter_declaration = declaration_specifiers declarator |
 *                         declaration_specifiers [ abstract_declarator ]
 */
Declaration *parameter_declaration(void)
{
    Declaration *n;

    n = malloc(sizeof(Declaration));

    n->decl_specs = declaration_specifiers();
    if (lookahead(1)!=TOK_COMMA && lookahead(1)!=TOK_RPAREN) { /* FOLLOW(parameter_declaration) */
        /*if (speculate_declarator())
            declarator();
        else
            abstract_declarator();*/
        /*
         * Try to distinguish between a declarator and an abstract_declarator. Search for
         * an identifier `([here],', `,[here],', or `,[here])', depending on where the
         * the parameter is.
         * Note: there are some constructs that will be reported as a declarator, although
         * they are not. For example: `int (*)(int x)'.
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
            n->idl = declarator();
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
    specifier_qualifier_list();
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
void typedef_name(void)
{
    match(TOK_ID);
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
ExecNode *compound_statement(void)
{
    match(TOK_LBRACE);
    match(TOK_RBRACE);
}


/*
 * declaration_list = declaration { declaration }
 */
void declaration_list(void)
{
    declaration();
    while (in_first_declaration_specifiers(lookahead(1)))
        declaration();
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
    match(TOK_ICONST);
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
