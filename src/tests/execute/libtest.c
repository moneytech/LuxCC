#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int icmp1(const void *p1, const void *p2)
{
    int32_t i1 = *(int32_t *)p1;
    int32_t i2 = *(int32_t *)p2;

    if (i1 > i2)
        return 1;
    else if (i1 < i2)
        return -1;
    return 0;
}

int icmp2(const void *p1, const void *p2)
{
    int64_t l1 = *(int64_t *)p1;
    int64_t l2 = *(int64_t *)p2;

    if (l1 > l2)
        return 1;
    else if (l1 < l2)
        return -1;
    return 0;
}

int main(void)
{
    int32_t i;
    int32_t ia[] = { 3, 1, 5, 2, 4 }, ikey;
    int64_t la[] = { 8, 6, 10, 7, 9 }, lkey;

    qsort(ia, 5, sizeof(ia[0]), icmp1);
    for (i = 0; i < 5; i++)
        printf("%d\n", ia[i]);
    ikey = 5;
    printf(">>>%d\n", *(int32_t *)bsearch(&ikey, ia, 5, sizeof(ia[0]), icmp1));

    qsort(la, 5, sizeof(la[0]), icmp2);
    for (i = 0; i < 5; i++)
        printf("%lld\n", la[i]);
    lkey = 6;
    printf(">>>%lld\n", *(int64_t *)bsearch(&lkey, la, 5, sizeof(la[0]), icmp2));

    return 0;
}
