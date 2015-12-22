int main(void)
{
    char a = 255; /* @ warning "255 to -1" */
    unsigned char b = 511; /* @ warning "511 to 255" */
    short c = 65535; /* @ warning "65535 to -1" */
    unsigned short d = 131071; /* @ warning "131071 to 65535" */
    int e = 4294967295; /* @ warning "4294967295 to -1" */
    unsigned f = 8589934591; /* @ warning "8589934591 to 4294967295" */
    long g = 4294967295; /* @ warning "4294967295 to -1" */
    unsigned long h = 8589934591; /* @ warning "8589934591 to 4294967295" */
    unsigned long long j = 18446744073709551616U; /* @ warning "integer constant is too large for its type" */

    return 0;
}
