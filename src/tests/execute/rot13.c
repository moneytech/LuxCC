#include <stdio.h>

/*
 * Rot13 encryption
 */
void rot13(char *p)
{
    int i = 0, n;
    while (p[i] != '\0') {
        if (p[i]<'A' || p[i]>'a'+26) {
            ++i;
        } else {
            if (p[i] < 'a')
                n = 'A';
            else
                n = 'a';
            p[i] = (p[i]-n+13)%26+n;
            ++i;
        }
    }
}

int main(void)
{
    // char s[32];
    // scanf("%s", s);
    char s[] = "hello world";
    rot13(s);
    printf("Encrypted string: %s\n", s);
    rot13(s);
    printf("Original string: %s\n", s);
}
