#include "../../sassert.h"

int main(void)
{
    char *pc;
    int *pi;
    void *pv;

    const char *pcc;
    volatile char *pvc;
    const int *pci;
    const void *pcv;

    // char *const *pcpc;
    // int *const *pcpi;
    // void *const *pcpv;

    __static_assert(_ASSERT_TYPE, 1 ? pc : pc, char *);
    __static_assert(_ASSERT_TYPE, 1 ? pi : pi, int *);
    __static_assert(_ASSERT_TYPE, 1 ? pv : pv, void *);
    __static_assert(_ASSERT_TYPE, 1 ? pc : pi, char *);
    __static_assert(_ASSERT_TYPE, 1 ? pc : pv, void *);
    __static_assert(_ASSERT_TYPE, 1 ? pcc : pc, const char *);
    __static_assert(_ASSERT_TYPE, 1 ? pcc : pvc, const volatile char *);
    __static_assert(_ASSERT_TYPE, 1 ? pci : pcv, const void *);
    __static_assert(_ASSERT_TYPE, 1 ? pvc : pcv, const volatile void *);

    return 0;
}
