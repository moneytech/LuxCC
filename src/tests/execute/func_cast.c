int main(void)
{
    /*
     * Example from 'Expert C Programming: Deep C Secrets'.
     */
    extern int printf(const char *, ...);
    void *f = (void *)printf;

    (*(int (*)(const char *, ...))f)("Bite my shorts. Also my chars and ints\n");

    return 0;
}
