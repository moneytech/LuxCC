#include <stdio.h>

#define STRING  char *
#define IF      if (
#define THEN    ) {
#define ELSE    } else {
#define FI      ; }
#define WHILE   while (
#define DO      ) {
#define OD      ; }
#define INT     int
#define BEGIN   {
#define END     }
#define PRINT   printf (
#define PEND    ) ;

INT compare(STRING s1, STRING s2)
BEGIN
    WHILE *s1++ == *s2
    DO IF *s2++ == 0
        THEN return (0);
        FI
    OD
    return (*--s1 - *s2);
END

INT main(void)
BEGIN
    PRINT "%d\n", compare("apple", "apple") PEND
    PRINT "%d\n", compare("hello", "world") PEND
END
