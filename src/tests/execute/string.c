#include <stdio.h>

#define A "xyz"

char test_string1[] = A "abc" "pqt";
char test_string2[] = "abc" A "pqt";
char test_string3[] = "abc" "pqt" A;
char test_string4[] = "abc\
pqt\
xyz";
char test_string5[] = "abc"/* NOP */"pqt";

int main(void)
{
    printf("%s\n", test_string1);
    printf("%s\n", test_string2);
    printf("%s\n", test_string3);
    printf("%s\n", test_string4);
    printf("%s\n", test_string5);

    printf("\x41\x42\x43\n");
    printf("\101\102\103\n");
    printf("\"test1\"\n");
    printf("\\test2\\\n");

    printf("'\\377'=%d '\\xff'=%d\n", '\377', '\xff');

    return 0;
}
