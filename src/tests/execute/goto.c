#include <stdio.h>

void foo(void)
{
    printf("jumping to forth\n");
    goto forth;
back:
    printf("in back; returning\n");
    return;
    printf("unprinted\n");
forth:
    printf("in forth; jumping to back\n");
    goto back;
    printf("unprinted\n");
}

void bar(void)
{
    typedef int typedef_name_and_label;
    typedef_name_and_label i;

    i = 0;
typedef_name_and_label:
loop:
    if (i >= 5)
        goto end;
    printf("%d\n", i++);
    goto loop;
end:;
}

int main(void)
{
    foo();
    bar();

    return 0;
}
