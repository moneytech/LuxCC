int main(void)
{
    char a = 255; /* @ warning "`int' to `char' changes value from 255 to -1" */
    unsigned char b = 511; /* @ warning "`int' to `unsigned char' changes value from 511 to 255" */
    short c = 65535; /* @ warning "`int' to `short' changes value from 65535 to -1" */
    unsigned short d = 131071; /* @ warning "`int' to `unsigned short' changes value from 131071 to 65535" */
    int e = 4294967295; /* @ warning "`long long' to `int' changes value from 4294967295 to -1" */
    unsigned f = 8589934591; /* @ warning "`long long' to `unsigned' changes value from 8589934591 to 4294967295" */
    long g = 4294967295; /* @ warning "`long long' to `long' changes value from 4294967295 to -1" */
    unsigned long h = 8589934591; /* @ warning "`long long' to `unsigned long' changes value from 8589934591 to 4294967295" */
    long long i = 18446744073709551615U; /* @ warning "`unsigned long long' to `long long' changes value from 18446744073709551615 to -1" */
    unsigned long long j = 18446744073709551616U; /* @ warning "integer constant is too large for its type" */

    return 0;
}
