#include "parser.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>


#define TRUE  1
#define FALSE 0
#define ERROR(...) \
    fprintf(stderr, "error: %s, ", __VA_ARGS__), \
    fprintf(stderr, "curr_tok->lexeme: %s\n", curr_tok->lexeme), \
    exit(1)


static jmp_buf env;
static TokenNode *curr_tok;
static int speculating = FALSE;


/*
 * Recursive parser functions.
 */
void translation_unit(void);
void external_declaration(void);
void function_definition(void);
void declaration(void);
void declaration_specifiers(void);
void init_declarator_list(void);
void init_declarator(void);
void storage_class_specifier(void);
void type_specifier(void);
void struct_or_union_specifier(void);
void struct_or_union(void);
void struct_declaration_list(void);
void struct_declaration(void);
void specifier_qualifier_list(void);
void struct_declarator_list(void);
void struct_declarator(void);
void enum_specifier(void);
void enumerator_list(void);
void enumerator(void);
void enumeration_constant(void);
void type_qualifier(void);
void declarator(void);
void direct_declarator(void);
void direct_declarator_postfix(void);
void pointer(void);
void type_qualifier_list(void);
void parameter_type_list(void);
void parameter_list(void);
void parameter_declaration(void);
void identifier_list(void);
void type_name(void);
void abstract_declarator(void);
void direct_abstract_declarator(void);
void direct_abstract_declarator_postfix(void);
void typedef_name(void);
void initializer(void);
void initializer_list(void);

void declaration_list(void);
void assignment_expression(void);
void constant_expression(void);

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
void translation_unit(void)
{
    external_declaration();
    while (lookahead(1) != TOK_EOF)
        external_declaration();
}

/*
 * external_declaration = function_definition |
 *                        declaration
 */
void external_declaration(void)
{
    /*
    if (?)
        function_definition();
    else if (?)
        declaration();
    else
        // error
    */
    TokenNode *temp;

    temp = curr_tok; /* save */
    declaration_specifiers();
    declarator();
    if (lookahead(1) == TOK_LBRACE) {
        curr_tok = temp; /* restore */
        function_definition();
    } else {
        curr_tok = temp; /* restore */
        declaration();
    }
}

/*
 * function_definition = declaration_specifiers declarator compound_statement
 */
void function_definition(void)
{
    declaration_specifiers();
    declarator();
    // compound_statement();
}

// =============================================================================
// Declarations
// =============================================================================

/*
 * declaration = declaration_specifiers init_declarator_list ";"
 */
void declaration(void)
{
    declaration_specifiers();
    init_declarator_list();
    match(TOK_SEMICOLON);
}

/*
 * declaration_specifiers = storage_class_specifier [ declaration_specifiers ] |
 *                          type_specifier [ declaration_specifiers ] |
 *                          type_qualifier [ declaration_specifiers ]
 */
void declaration_specifiers(void)
{
    if (in_first_storage_class_specifier(lookahead(1)))
        storage_class_specifier();
    else if (in_first_type_specifier(lookahead(1)))
        type_specifier();
    else if (in_first_type_qualifier(lookahead(1)))
        type_qualifier();
    else
        ERROR("declaration_specifiers");

    if (in_first_declaration_specifiers(lookahead(1)))
        declaration_specifiers();
}

/*
 * init_declarator_list =  init_declarator { "," init_declarator }
 */
void init_declarator_list(void)
{
    init_declarator();
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        init_declarator();
    }
}

/*
 * init_declarator = declarator [ "=" initializer ]
 */
void init_declarator(void)
{
    declarator();
    if (lookahead(1) == TOK_ASSIGN) {
        match(TOK_ASSIGN);
        initializer();
    }
}

/*
storage_class_specifier = "typedef" |
                          "extern" |
                          "static" |
                          "auto" |
                          "register"
*/
void storage_class_specifier(void)
{
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
void type_specifier(void)
{
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
        struct_or_union_specifier();
        break;
    case TOK_ENUM:
        enum_specifier();
        break;
    // case TOK_ID:
    default:
        ERROR("type_specifier");
        break;
    }
}

/*
 * struct_or_union_specifier = struct_or_union [ identifier ] "{" struct_declaration_list "}" |
 *                             struct_or_union identifier
 */
void struct_or_union_specifier(void)
{
    struct_or_union();
    if (lookahead(1) == TOK_ID) {
        match(TOK_ID);
        if (lookahead(1) == TOK_LBRACE) {
            match(TOK_LBRACE);
            struct_declaration_list();
            match(TOK_RBRACE);
        }
    } else if (lookahead(1) == TOK_LBRACE) {
        match(TOK_LBRACE);
        struct_declaration_list();
        match(TOK_RBRACE);
    } else {
        ERROR("struct_or_union_specifier");
    }
}

/*
 * struct_or_union = "struct" | "union"
 */
void struct_or_union(void)
{
    if (lookahead(1) == TOK_STRUCT)
        match(TOK_STRUCT);
    else if (lookahead(1) == TOK_UNION)
        match(TOK_UNION);
    else
        ERROR("struct_or_union");
}

/*
 * struct_declaration_list = struct_declaration { struct_declaration }
 */
void struct_declaration_list(void)
{
    struct_declaration();
    while (in_first_specifier_qualifier_list(lookahead(1)))
        struct_declaration();
}

/*
 * struct_declaration = specifier_qualifier_list struct_declarator_list ";"
 */
void struct_declaration(void)
{
    specifier_qualifier_list();
    struct_declarator_list();
    match(TOK_COMMA);
}

/*
 * specifier_qualifier_list = ( type_specifier | type_qualifier ) [ specifier_qualifier_list ]
 */
void specifier_qualifier_list(void)
{
    if (in_first_type_specifier(lookahead(1)))
        type_specifier();
    else if (in_first_type_qualifier(lookahead(1)))
        type_qualifier();
    else
        ERROR("specifier_qualifier_list");

    if (in_first_specifier_qualifier_list(lookahead(1)))
        specifier_qualifier_list();
}

/*
 * struct_declarator_list = struct_declarator { "," struct_declarator }
 */
void struct_declarator_list(void)
{
    struct_declarator();
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        struct_declarator();
    }
}

/*
 * struct_declarator = declarator |
 *                     [ declarator ] ":" constant_expression
 */
void struct_declarator(void)
{
    if (lookahead(1) == TOK_COLON) {
        match(TOK_COLON);
        constant_expression();
    } else {
        declarator();
        if (lookahead(1) == TOK_COLON) {
            match(TOK_COLON);
            constant_expression();
        }
    }
}

/*
 * enum_specifier = "enum" [ identifier ] "{" enumerator_list "}" |
 *                  "enum" identifier
 */
void enum_specifier(void)
{
    match(TOK_ENUM);
    if (lookahead(1) == TOK_ID) {
        match(TOK_ID);
        if (lookahead(1) == TOK_LBRACE) {
            match(TOK_LBRACE);
            enumerator_list();
            match(TOK_RBRACE);
        }
    } else if (lookahead(1) == TOK_LBRACE) {
        match(TOK_LBRACE);
        enumerator_list();
        match(TOK_RBRACE);
    } else {
        ERROR("enum_specifier");
    }
}

/*
 * enumerator_list = enumerator { "," enumerator }
 */
void enumerator_list(void)
{
    enumerator();
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        enumerator();
    }
}

/*
 * enumerator = enumeration_constant [ "=" constant_expression ]
 */
void enumerator(void)
{
    enumeration_constant();
    if (lookahead(1) == TOK_ASSIGN) {
        match(TOK_ASSIGN);
        constant_expression();
    }
}

/*
 * enumeration_constant = identifier
 */
void enumeration_constant(void)
{
    match(TOK_ID);
}

/*
 * type_qualifier = "const" | "volatile"
 */
void type_qualifier(void)
{
    if (lookahead(1) == TOK_CONST)
        match(TOK_CONST);
    else if (lookahead(1) == TOK_VOLATILE)
        match(TOK_VOLATILE);
    else
        ERROR("type_qualifier");
}

/*
 * declarator = [ pointer ] direct_declarator
 */
void declarator(void)
{
    if (lookahead(1) == TOK_ASTERISK)
        pointer();
    direct_declarator();
}

/*
 * direct_declarator = ( identifier | "(" declarator ")" ) { direct_declarator_postfix }
 */
void direct_declarator(void)
{
    if (lookahead(1) == TOK_ID) {
        match(TOK_ID);
    } else if (lookahead(1) == TOK_LPAREN) {
        match(TOK_LPAREN);
        declarator();
        match(TOK_RPAREN);
    } else {
        ERROR("direct_declarator");
    }

    while (lookahead(1)==TOK_LBRACKET || lookahead(1)==TOK_LPAREN)
        direct_declarator_postfix();
}

/*
 * direct_declarator_postfix = "[" [ constant_expression ] "]" |
 *                             "(" parameter_type_list ")" |
 *                             "(" [ identifier_list ] ")"
 */
void direct_declarator_postfix(void)
{
    if (lookahead(1) == TOK_LBRACKET) {
        match(TOK_LBRACKET);
        if (lookahead(1) != TOK_RBRACKET)
            constant_expression();
        match(TOK_RBRACKET);
    } else if (lookahead(1) == TOK_LPAREN) {
        match(TOK_LPAREN);
        if (lookahead(1) != TOK_RPAREN) {
            if (lookahead(1) == TOK_ID)
                identifier_list();
            else /* parameter_type_list or error */
                parameter_type_list();
        }
        match(TOK_RPAREN);
    } else {
        ERROR("direct_declarator_postfix");
    }
}

/*
 * pointer = "*" [ type_qualifier_list ] [ pointer ]
 */
void pointer(void)
{
    match(TOK_ASTERISK);
    if (in_first_type_qualifier(lookahead(1)))
        type_qualifier_list();
    if (lookahead(1) == TOK_ASTERISK)
        pointer();
}

/*
 * type_qualifier_list = type_qualifier { type_qualifier }
 */
void type_qualifier_list(void)
{
    type_qualifier();
    while (in_first_type_qualifier(lookahead(1)))
        type_qualifier();
}

/*
 * parameter_type_list = parameter_list [ "," "..." ]
 */
void parameter_type_list(void)
{
    parameter_list();
    if (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        match(TOK_ELLIPSIS);
    }
}

/*
 * parameter_list = parameter_declaration { "," parameter_declaration }
 */
void parameter_list(void)
{
    parameter_declaration();
    while (lookahead(1) == TOK_COMMA) {
        match(TOK_COMMA);
        parameter_declaration();
    }
}

/*
 * parameter_declaration = declaration_specifiers declarator |
 *                         declaration_specifiers [ abstract_declarator ]
 */
void parameter_declaration(void)
{
    declaration_specifiers();
    if (lookahead(1)!=TOK_COMMA && lookahead(1)!=TOK_RPAREN) { /* FOLLOW(parameter_declaration) */
        // ???
    }
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
void abstract_declarator(void)
{
    if (lookahead(1) == TOK_ASTERISK) {
        match(TOK_ASTERISK);
        if (lookahead(1)==TOK_LPAREN || lookahead(1)==TOK_LBRACKET)
            direct_abstract_declarator();
    } else {
        direct_abstract_declarator();
    }
}

// direct_abstract_declarator:
  // "(" abstract_declarator ")" |
  // direct_abstract_declarator? "[" constant_expression? "]" |
  // direct_abstract_declarator? "(" parameter_type_list? ")" ;
/*
 * direct_abstract_declarator = "(" abstract_declarator ")" { direct_abstract_declarator_postfix } |
 *                              direct_abstract_declarator_postfix { direct_abstract_declarator_postfix }
 */
void direct_abstract_declarator(void)
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
void direct_abstract_declarator_postfix(void)
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
void initializer(void)
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
void initializer_list(void)
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

compound_statement = "{" [ declaration_list ] [ statement_list ] "}" ;
#endif

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
void assignment_expression(void)
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
void constant_expression(void)
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
