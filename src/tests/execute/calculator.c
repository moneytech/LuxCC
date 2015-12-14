/*  Recursive descent calculator for simple integer arithmetic according
    to the EBNF:
        <exp> -> <term> { <addop> <term> }
        <addop> -> + | -
        <term> -> <factor> { <mulop> <factor> }
        <mulop> -> *
        <factor> -> ( <exp> ) | Number
*/
#include <stdio.h>
#include <stdlib.h>

char token, *curr;

void error(char *msg)
{
    printf("Error: %s\n", msg);
    exit(EXIT_FAILURE);
}

void match(char expected)
{
    if (token == expected)
        token = *++curr;
    else
        error("unexpected token");
}

int getnum(void)
{
    int num = token-'0';
    match(token);
    while (token>='0' && token<='9') {
        num = num*10+token-'0';
        match(token);
    }
    return num;
}

int expr(void);
int term(void);
int factor(void);

int main(void/*int argc, char *argv[]*/)
{
    int result;
    /*if (argc < 2) {
        fprintf(stderr, "usage: %s <expression>\n", argv[0]);
        exit(EXIT_FAILURE);
    }
    curr = argv[1];*/
    curr = "1+2*3";
    token = *curr; /* load token with first character for lookahead */
    result = expr();
    if (token == '\0')
        printf("Result = %d\n", result);
    else
        error("extraneous chars on line");

    return 0;
}

int expr(void)
{
    int temp = term();
    while ((token=='+') || (token=='-'))
        switch (token) {
        case '+':
            match('+');
            temp = temp+term();
            break;
        case '-':
            match('-');
            temp = temp-term();
            break;
        }
    return temp;
}

int term(void)
{
    int temp = factor();
    while (token == '*') {
        match('*');
        temp = temp*factor();
    }
    return temp;
}

int factor(void)
{
    int temp;
    if (token == '(') {
        match('(');
        temp = expr();
        match(')');
    } else if (token>='0' && token<='9') {
        temp = getnum();
    } else {
        error("expected number or ( expr )");
    }
    return temp;
}
