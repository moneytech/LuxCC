void __libc_init(void)
{
    extern void __set_std_buffering(void);  /* stdio.c */
    extern void __set_up_heap(void);        /* stdlib.c */

    __set_std_buffering();
    __set_up_heap();
}
