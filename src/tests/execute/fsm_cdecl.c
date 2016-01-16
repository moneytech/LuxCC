/*
    Example program from 'Expert C Programming: Deep C Secrets', Chapter 8.
    Cdecl written as a finite state machine.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAXTOKENS 100
#define MAXTOKENLEN 64

enum type_tag { IDENTIFIER, QUALIFIER, TYPE };

struct token {
    char type;
    char string[MAXTOKENLEN];
};

int top = -1;

/* holds all the tokens before first identifier */
struct token stack[MAXTOKENS];

/* holds the token just read */
struct token this;

#define pop stack[top--]
#define push(s) stack[++top]=s

/* figure out the identifier type */
enum type_tag classify_string(void)
{
    char *s = this.string;
    if (!strcmp(s, "const")) {
        strcpy(s, "read-only");
        return QUALIFIER;
    }
    if (!strcmp(s, "volatile")) return QUALIFIER;
    if (!strcmp(s, "void")) return TYPE;
    if (!strcmp(s, "char")) return TYPE;
	if (!strcmp(s, "signed")) return TYPE;
    if (!strcmp(s, "unsigned")) return TYPE;
    if (!strcmp(s, "short")) return TYPE;
    if (!strcmp(s, "int")) return TYPE;
    if (!strcmp(s, "long")) return TYPE;
    if (!strcmp(s, "float")) return TYPE;
    if (!strcmp(s, "double")) return TYPE;
    if (!strcmp(s, "struct")) return TYPE;
    if (!strcmp(s, "union")) return TYPE;
    if (!strcmp(s, "enum")) return TYPE;
    return IDENTIFIER;
}

char *curr = "char *(*(fun[16]))()";

int get_char()
{
    return *curr++;
}

void unget_char()
{
    --curr;
}

void gettoken(void)
{
	/* read next token into "this" */
    char *p = this.string;

    /* read past any spaces */
    while ((*p = get_char()) == ' ');

    if (isalnum(*p)) {
        /* it starts with A-Z,1-9 read in identifier */
        while (isalnum(*++p = get_char()));
        unget_char();
        *p = '\0';
        this.type = classify_string();
        return;
    }
    this.string[1] = '\0';
    this.type = *p;
    return;
}

void initialize(),get_array(), get_params(), get_lparen(), get_ptr_part(), get_type();

void (*nextstate)(void) = initialize;

int main()
{
/* Cdecl written as a finite state machine */
/* transition through the states, until the pointer is null */
   while (nextstate != NULL)
       (*nextstate)();
	return 0;
}

void initialize()
{
    gettoken();
    while (this.type != IDENTIFIER) {
        push(this);
        gettoken();
    }
    printf("%s is ", this.string);
    gettoken();
    nextstate = get_array;
}

void get_array()
{
    nextstate = get_params;
    while (this.type == '[') {
        printf("array ");
        gettoken();/* a number or ']' */
        if (isdigit(this.string[0])) {
            printf("0..%d ", atoi(this.string) - 1);
            gettoken();/* read the ']' */
        }
        gettoken();/* read next past the ']' */
        printf("of ");
        nextstate = get_lparen;
    }
}

void get_params()
{
    nextstate = get_lparen;
    if (this.type == '(') {
        while (this.type != ')') {
            gettoken();
        }
        gettoken();
        printf("function returning ");
    }
}

void get_lparen()
{
    nextstate = get_ptr_part;
    if (top >= 0) {
        if (stack[top].type == '(') {
            pop;
            gettoken();/* read past ')' */
            nextstate = get_array;
        }
    }
}

void get_ptr_part()
{
    nextstate = get_type;
    if (stack[top].type == '*') {
        printf("pointer to ");
        pop;
        nextstate = get_lparen;
    } else if (stack[top].type == QUALIFIER) {
        printf("%s ", pop.string);
        nextstate = get_lparen;
    }
}

void get_type()
{
    nextstate = NULL;
    /* process tokens that we stacked while reading to identifier */
    while (top >= 0) {
        printf("%s ", pop.string);
    }
    printf("\n");
}
