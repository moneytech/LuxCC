/*
 * Simple x86 code generator
 *      IC ==> x86 ASM.
 * Of interest:
 *   => System V ABI-i386: http://www.sco.com/developers/devspecs/abi386-4.pdf
 */
#include "x86_cgen.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>
#include "../util.h"
#include "../decl.h"
#include "../expr.h"
#include "../arena.h"
#include "../imp_lim.h"
#include "../error.h"
#include "../ic.h"
#include "../dflow.h"
#include "../str.h"

typedef enum {
    X86_EAX,
    X86_EBX,
    X86_ECX,
    X86_EDX,
    X86_ESI,
    X86_EDI,
    X86_NREG,
} X86_Reg;

static int pinned[X86_NREG];
static int modified[X86_NREG];
#define pin_reg(r)      pinned[r] = TRUE;
#define unpin_reg(r)    pinned[r] = FALSE;

static char *x86_reg_str[] = {
    "eax",
    "ebx",
    "ecx",
    "edx",
    "esi",
    "edi",
};

static char *x86_lwreg_str[] = {
    "ax",
    "bx",
    "cx",
    "dx",
    "si",
    "di",
};

static char *x86_lbreg_str[] = {
    "al",
    "bl",
    "cl",
    "dl",
    "??",
    "??",
};

#define nid(a)  (address(a).cont.com.nid)

extern int size_of_local_area;
static char *curr_func;
static Declaration ret_ty;
static unsigned temp_struct_size;
static int big_return;
static unsigned arg_nb;
static int calls_to_fix_counter;
static char *calls_to_fix[64];

typedef struct Temp Temp;
struct Temp {
    int nid;
    int offs;      /* offset from ebp */
    int used;      /* used or free */
    Temp *next;
} *temp_list;

int get_temp_offs(unsigned a)
{
    Temp *p;

    /* see if it was already allocated */
    for (p = temp_list; p != NULL; p = p->next)
        if (p->used && p->nid==nid(a))
            return p->offs;

    /* try to find an unused temp */
    for (p = temp_list; p != NULL; p = p->next) {
        if (!p->used) {
            p->nid = nid(a);
            p->used = TRUE;
            return p->offs;
        }
    }

    /* allocate a new temp */
    p = malloc(sizeof(Temp));
    p->nid = nid(a);
    size_of_local_area -= 4;
    p->offs = size_of_local_area;
    p->used = TRUE;
    p->next = temp_list;
    temp_list = p;
    return p->offs;
}

void free_temp(unsigned a)
{
    Temp *p;

    for (p = temp_list; p != NULL; p = p->next) {
        if (p->used && p->nid==nid(a)) {
            p->used = FALSE;
            break;
        }
    }
}

typedef struct AddrDescr AddrDescr;
struct AddrDescr {
    int in_reg;
    X86_Reg r;
} *addr_descr_tab;

static
void init_addr_descr_tab(void)
{
    addr_descr_tab = calloc(nid_counter, sizeof(AddrDescr));
}

#define addr_in_reg(a)  (addr_descr_tab[nid(a)].in_reg)
#define addr_reg(a)     (addr_descr_tab[nid(a)].r)

#define MAX_ADDRS_PER_REG 10
typedef struct RegDescr RegDescr;
struct RegDescr {
    int naddrs; /* # of addresses this register is housing */
    unsigned addrs[MAX_ADDRS_PER_REG];
} reg_descr_tab[X86_NREG];

void reg_descr_add_addr(X86_Reg r, unsigned a)
{
    int i;

    assert(reg_descr_tab[r].naddrs < MAX_ADDRS_PER_REG);

    /* [?] search before add a new address */

    for (i = 0; i < MAX_ADDRS_PER_REG; i++) {
        if (!reg_descr_tab[r].addrs[i]) {
            reg_descr_tab[r].addrs[i] = a;
            reg_descr_tab[r].naddrs++;
            break;
        }
    }
}

void reg_descr_rem_addr(X86_Reg r, unsigned a)
{
    int i;
    int naddrs;

    i = 0;
    naddrs = reg_descr_tab[r].naddrs;
    while (naddrs && i<MAX_ADDRS_PER_REG) {
        unsigned a2;

        if ((a2=reg_descr_tab[r].addrs[i]) != 0) {
            --naddrs;
            if (nid(a) == nid(a2)) {
                reg_descr_tab[r].addrs[i] = 0;
                reg_descr_tab[r].naddrs--;
                break;
            }
        }
        ++i;
    }
}

void clear_reg_descr(X86_Reg r)
{
    int i;

    if (reg_descr_tab[r].naddrs == 0)
        return;

    for (i = 0; i < MAX_ADDRS_PER_REG; i++)
        reg_descr_tab[r].addrs[i] = 0;
    reg_descr_tab[r].naddrs = 0;
}

static String *func_body, *func_prolog, *func_epilog;
#define emit(...)   (string_printf(func_body, __VA_ARGS__))
#define emitln(...) (string_printf(func_body, __VA_ARGS__), string_printf(func_body, "\n"))
#define emit_prolog(...)   (string_printf(func_prolog, __VA_ARGS__))
#define emit_prologln(...) (string_printf(func_prolog, __VA_ARGS__), string_printf(func_prolog, "\n"))
#define emit_epilog(...)   (string_printf(func_epilog, __VA_ARGS__))
#define emit_epilogln(...) (string_printf(func_epilog, __VA_ARGS__), string_printf(func_epilog, "\n"))

void x86_function_definition(TypeExp *decl_specs, TypeExp *header);

void x86_cgen(void)
{
    int i;
    ExternId *ed;

    ic_init();
    func_body = string_new(1024);
    func_prolog = string_new(1024);
    func_epilog = string_new(1024);

    for (ed = get_extern_symtab(); ed != NULL; ed = ed->next) {
        if (ed->status == REFERENCED) {
            ;
        } else {
            if (ed->declarator->child!=NULL && ed->declarator->child->op==TOK_FUNCTION)
                x86_function_definition(ed->decl_specs, ed->declarator);
            else
                ;
        }
    }

    printf("\n");
    for (i = 0; i < X86_NREG; i++)
        printf("reg%d = %d\n", i, reg_descr_tab[i].naddrs);
}

static X86_Reg get_empty_reg(void);
static void x86_load(X86_Reg r, unsigned a);
static char *get_operand(unsigned a);
static void x86_store(X86_Reg r, unsigned a);
static void spill_reg(X86_Reg r);
static X86_Reg get_reg(int intr);

X86_Reg get_empty_reg(void)
{
    int i;

    for (i = 0; i < X86_NREG; i++) {
        if (!pinned[i] && reg_descr_tab[i].naddrs==0) {
            modified[i] = TRUE;
            return (X86_Reg)i;
        }
    }
    return -1; /* couldn't find any */
}

X86_Reg get_unpinned_reg(void)
{
    int i;

    for (i = 0; i < X86_NREG; i++) {
        if (!pinned[i]) {
            modified[i] = TRUE;
            return (X86_Reg)i;
        }
    }
    return -1;
}

void spill_reg(X86_Reg r)
{
    int i;

    if (reg_descr_tab[r].naddrs == 0)
        return;

    for (i = 0; i < MAX_ADDRS_PER_REG; i++) {
        unsigned a;

        if (!(a=reg_descr_tab[r].addrs[i]))
            continue;
        x86_store(r, a);
        addr_in_reg(a) = FALSE;
        reg_descr_tab[r].addrs[i] = 0;
        reg_descr_tab[r].naddrs--;
    }
}

void spill_all(void)
{
    int i;

    for (i = 0; i < X86_NREG; i++)
        spill_reg((X86_Reg)i);
}

X86_Reg get_reg(int intr)
{
    X86_Reg r;
    unsigned arg1;

    arg1 = instruction(intr).arg1;
    if (address(arg1).kind==IdKind || address(arg1).kind==TempKind) {
        if (addr_in_reg(arg1) && instruction(intr).liveness[1]==DEAD) {
            /* if the register doesn't hold some other address, we are done */
            r = addr_reg(arg1);
            if (reg_descr_tab[r].naddrs == 1)
                return r;
        }
    }

    /* find an empty register */
    if ((r=get_empty_reg()) != -1)
        return r;

    /* choose an unpinned register and spill its contents */
    r = get_unpinned_reg();
    assert(r != -1);
    spill_reg(r);
    return r;
}

#define offset(a) (address(a).cont.var.offset)

char *get_operand(unsigned a)
{
    static char op[128];

    if (address(a).kind == IConstKind) {
        sprintf(op, "%lu", address(a).cont.uval);
    } else if (address(a).kind == StrLitKind) {
        ;
    } else if (address(a).kind == IdKind) {
        ExecNode *e;

        if (addr_in_reg(a))
            return x86_reg_str[addr_reg(a)];

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
            /*assert(0);*/
        case TOK_SUBSCRIPT:
        case TOK_FUNCTION:
            if (e->attr.var.duration == DURATION_STATIC) {
                if (e->attr.var.linkage == LINKAGE_NONE)
                    sprintf(op, "%s@%s", curr_func, e->attr.str);
                else
                    sprintf(op, "%s", e->attr.str);
                return op;
            } else {
                X86_Reg r;

                if ((r=get_empty_reg()) == -1) {
                    r = get_unpinned_reg();
                    assert(r != -1);
                    spill_reg(r);
                }
                if (e->attr.var.is_param)
                    emitln("lea %s, [ebp+%d]", x86_reg_str[r], offset(a));
                else
                    emitln("lea %s, [ebp-%d]", x86_reg_str[r], -offset(a));
                return x86_reg_str[r];
            }
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR: { /* promote to dword */
            X86_Reg r;

            if ((r=get_empty_reg()) == -1) {
                r = get_unpinned_reg();
                assert(r != -1);
                spill_reg(r);
            }
            x86_load(r, a);
            return x86_reg_str[r];
        }
        default: /* dword sized, OK */
            break;
        }

        /* fall through (dword sized operand) */
        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE)
                sprintf(op, "dword [%s@%s]", curr_func, e->attr.str);
            else
                sprintf(op, "dword [%s]", e->attr.str);
        } else {
            if (e->attr.var.is_param)
                sprintf(op, "dword [ebp+%d]", offset(a));
            else
                sprintf(op, "dword [ebp-%d]", -offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_in_reg(a))
            return x86_reg_str[addr_reg(a)];
        else
            sprintf(op, "dword [ebp-%d]", -get_temp_offs(a));
    }

    return op;
}

void x86_load_addr(X86_Reg r, unsigned a)
{
    ExecNode *e;

    e = address(a).cont.var.e;
    if (e->attr.var.duration == DURATION_STATIC) {
        if (e->attr.var.linkage == LINKAGE_NONE)
            emitln("mov %s, %s@%s", x86_reg_str[r], curr_func, e->attr.str);
        else
            emitln("mov %s, %s", x86_reg_str[r], e->attr.str);
    } else {
        if (e->attr.var.is_param)
            emitln("lea %s, [ebp+%d]", x86_reg_str[r], offset(a));
        else
            emitln("lea %s, [ebp-%d]", x86_reg_str[r], -offset(a));
    }
}

void x86_load(X86_Reg r, unsigned a)
{
    if (address(a).kind == IConstKind) {
        emitln("mov %s, %lu", x86_reg_str[r], address(a).cont.uval);
    } else if (address(a).kind == StrLitKind) {
        ;
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *siz_str, *mov_str;

        if (addr_in_reg(a)) {
            if (addr_reg(a) == r)
                ; /* already in the register */
            else
                emitln("mov %s, %s", x86_reg_str[r], x86_reg_str[addr_reg(a)]);
            return;
        }

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
        case TOK_SUBSCRIPT:
        case TOK_FUNCTION:
            x86_load_addr(r, a);
            return;
        case TOK_SHORT:
            mov_str = "movsx";
            siz_str = "word";
            break;
        case TOK_UNSIGNED_SHORT:
            mov_str = "movzx";
            siz_str = "word";
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
            mov_str = "movsx";
            siz_str = "byte";
            break;
        case TOK_UNSIGNED_CHAR:
            mov_str = "movzx";
            siz_str = "byte";
            break;
        default:
            mov_str = "mov";
            siz_str = "dword";
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) /* static local */
                emitln("%s %s, %s [%s@%s]", mov_str, x86_reg_str[r], siz_str, curr_func, e->attr.str);
            else /* global */
                emitln("%s %s, %s [%s]", mov_str, x86_reg_str[r], siz_str, e->attr.str);
        } else { /* parameter or local */
            if (e->attr.var.is_param)
                emitln("%s %s, %s [ebp+%d]", mov_str, x86_reg_str[r], siz_str, offset(a));
            else
                emitln("%s %s, %s [ebp-%d]", mov_str, x86_reg_str[r], siz_str, -offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_in_reg(a)) {
            if (addr_reg(a) == r)
                return; /* already in the register */
            else
                emitln("mov %s, %s", x86_reg_str[r], x86_reg_str[addr_reg(a)]);
        } else {
            emitln("mov %s, dword [ebp-%d]", x86_reg_str[r], -get_temp_offs(a));
        }
    }
}

void x86_store(X86_Reg r, unsigned a)
{
    if (address(a).kind == IdKind) {
        ExecNode *e;
        char *siz_str, *reg_str;

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION: {
            int cluttered;

            cluttered = 0;
            if (r != X86_ESI) {
                if (reg_descr_tab[X86_ESI].naddrs != 0) {
                    cluttered |= 1;
                    emitln("push esi");
                }
                emitln("mov esi, %s", x86_reg_str[r]);
            }
            if (!addr_in_reg(a) || addr_reg(a)!=X86_EDI) {
                if (reg_descr_tab[X86_EDI].naddrs != 0) {
                    cluttered |= 2;
                    emitln("push edi");
                }
                x86_load_addr(X86_EDI, a);
            }
            if (reg_descr_tab[X86_ECX].naddrs != 0) {
                cluttered |= 4;
                emitln("push ecx");
            }
            emitln("mov ecx, %u", compute_sizeof(&e->type));
            /*emitln("cld");*/
            emitln("rep movsb");
            /* restore all */
            if (cluttered & 4)
                emitln("pop ecx");
            if (cluttered & 2)
                emitln("pop edi");
            if (cluttered & 1)
                emitln("pop esi");
        }
            return;
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            siz_str = "word";
            reg_str = x86_lwreg_str[r];
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR:
            /* TODO: handle the case where the register is esi or edi */
            siz_str = "byte";
            reg_str = x86_lbreg_str[r];
            break;
        default:
            siz_str = "dword";
            reg_str = x86_reg_str[r];
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) /* static local */
                emitln("mov %s [%s@%s], %s", siz_str, curr_func, e->attr.str, reg_str);
            else /* global */
                emitln("mov %s [%s], %s", siz_str, e->attr.str, reg_str);
        } else { /* parameter or local */
            if (e->attr.var.is_param)
                emitln("mov %s [ebp+%d], %s", siz_str, offset(a), reg_str);
            else
                emitln("mov %s [ebp-%d], %s", siz_str, -offset(a), reg_str);
        }
    } else if (address(a).kind == TempKind) {
        emitln("mov dword [ebp-%d], %s", -get_temp_offs(a), x86_reg_str[r]);
    }
}

void update_arg_descriptors(unsigned arg, unsigned char liveness, int next_use)
{
    /* If arg is in a register r and arg has no next use, then
       a) If arg is LIVE, generate spill code to move the value of r to memory
       location of arg.
       b) Mark the register and address descriptor tables to indicate that the
       register r no longer contains the value of arg. */
    if ((address(arg).kind!=IdKind && address(arg).kind!=TempKind) || next_use!=NO_NEXT_USE)
        return;

    if (addr_in_reg(arg)) {
        if (liveness == LIVE) { /* spill */
            x86_store(addr_reg(arg), arg);
            addr_in_reg(arg) = FALSE;
        } else {
            if (address(arg).kind == TempKind)
                free_temp(arg);
        }
        reg_descr_rem_addr(addr_reg(arg), arg);
    } else {
        if (liveness == LIVE) {
            ;
        } else {
            if (address(arg).kind == TempKind)
                free_temp(arg);
        }
    }
}

void update_tar_descriptors(X86_Reg res, unsigned tar, unsigned char liveness, int next_use)
{
    /* update the address descriptor table to indicate
       that the value of tar is stored in res only */
    addr_in_reg(tar) = TRUE;
    addr_reg(tar) = res;

    /* update the register descriptor table to indicate that
       res contains the value of tar only */
    clear_reg_descr(res);
    reg_descr_add_addr(res, tar);

    if (next_use == NO_NEXT_USE) {
        if (liveness == LIVE) { /* spill */
            x86_store(res, tar);
            addr_in_reg(tar) = FALSE;
        }
        clear_reg_descr(res);
    }
}

void compare_against_zero(unsigned a)
{
    if (address(a).kind == IConstKind) {
        assert(0); /* can be folded */
    } else if (address(a).kind == StrLitKind) {
        ;
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *siz_str;

        if (addr_in_reg(a)) {
            emitln("cmp %s, 0", x86_reg_str[addr_reg(a)]);
            return;
        }

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            siz_str = "word";
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR:
            siz_str = "byte";
            break;
        default:
            siz_str = "dword";
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) /* static local */
                emitln("cmp %s [%s@%s], 0", siz_str, curr_func, e->attr.str);
            else /* global */
                emitln("cmp %s [%s], 0", siz_str, e->attr.str);
        } else { /* parameter or local */
            if (e->attr.var.is_param)
                emitln("cmp %s [ebp+%d], 0", siz_str, offset(a));
            else
                emitln("cmp %s [ebp-%d], 0", siz_str, -offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_in_reg(a))
            emitln("cmp %s, 0", x86_reg_str[addr_reg(a)]);
        else
            emitln("cmp dword [ebp-%d], 0", -get_temp_offs(a));
    }
}

void x86_add(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("add %s, %s", x86_reg_str[res], get_operand(arg2));
    unpin_reg(res);
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
    update_arg_descriptors(arg2, instruction(i).liveness[2], instruction(i).next_use[2]);
}
void x86_sub(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_mul(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("imul %s, %s", x86_reg_str[res], get_operand(arg2));
    unpin_reg(res);
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
    update_arg_descriptors(arg2, instruction(i).liveness[2], instruction(i).next_use[2]);
}
void x86_div(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    char *instr, *divop;

    if (get_reg(i) != X86_EAX)
        spill_reg(X86_EAX);
    x86_load(X86_EAX, arg1);
    pin_reg(X86_EAX);
    spill_reg(X86_EDX);
    pin_reg(X86_EDX);
    if (is_unsigned_int(get_type_category(instruction(i).type))) {
        emitln("xor edx, edx");
        instr = "div";
    } else {
        emitln("cdq");
        instr = "idiv";
    }
    if (address(arg2).kind != IConstKind) {
        divop = get_operand(arg2);
    } else {
        X86_Reg r;

        if ((r=get_empty_reg()) == -1) {
            r = get_unpinned_reg();
            assert(r != -1);
            spill_reg(r);
        }
        x86_load(r, arg2);
        divop = x86_reg_str[r];
    }
    emitln("%s %s", instr, divop);
    unpin_reg(X86_EAX);
    unpin_reg(X86_EDX);
    update_tar_descriptors(X86_EAX, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
    update_arg_descriptors(arg2, instruction(i).liveness[2], instruction(i).next_use[2]);
}
void x86_rem(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_shl(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_shr(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_and(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_or(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_xor(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_eq(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_neq(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_lt(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_let(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_gt(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_get(int i, unsigned tar, unsigned arg1, unsigned arg2) {}

void x86_neg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("neg %s", x86_reg_str[res]);
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_cmpl(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_not(int i, unsigned tar, unsigned arg1, unsigned arg2) {}
void x86_ch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    /* TODO: handle the case where the register is esi or edi */

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movsx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_uch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    /* TODO: handle the case where the register is esi or edi */

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_sh(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movsx %s, %s", x86_reg_str[res], x86_lwreg_str[res]);
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_ush(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movzx %s, %s", x86_reg_str[res], x86_lwreg_str[res]);
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_addr_of(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load_addr(res, arg1);
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}

void x86_ind(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    BSet *s;
    X86_Reg res;
    char *reg_str;

    /* spill any target currently in a register */
    if ((s=get_pointer_targets(i, nid(arg1))) != NULL) {
        int i;

        for (i = bset_iterate(s); i != -1; i = bset_iterate(NULL))
            if (addr_in_reg(i))
                spill_reg(addr_reg(i));
    } else {
        spill_all();
    }

    res = get_reg(i);
    x86_load(res, arg1);
    reg_str = x86_reg_str[res];
    switch (get_type_category(instruction(i).type)) {
    case TOK_STRUCT:
    case TOK_UNION:
        break;
    case TOK_SHORT:
        emitln("movsx %s, word [%s]", reg_str, reg_str);
        break;
    case TOK_UNSIGNED_SHORT:
        emitln("movzx %s, word [%s]", reg_str, reg_str);
        break;
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
        emitln("movsx %s, byte [%s]", reg_str, reg_str);
        break;
    case TOK_UNSIGNED_CHAR:
        emitln("movzx %s, byte [%s]", reg_str, reg_str);
        break;
    default:
        emitln("mov %s, dword [%s]", reg_str, reg_str);
        break;
    }
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    update_tar_descriptors(res, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
static
void x86_pre_call(int i)
{
    Token cat;

    spill_all();
    if ((cat=get_type_category(instruction(i).type))==TOK_STRUCT || cat==TOK_UNION) {
        unsigned siz;

        siz = compute_sizeof(instruction(i).type);
        if (siz > temp_struct_size)
            temp_struct_size = siz;
        emit("lea eax, [ebp-");
        calls_to_fix[calls_to_fix_counter++] = string_curr(func_body);
        emitln("XXXXXXXXXXXXXXXX");
        emitln("push eax");
    }
}
void x86_indcall(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_pre_call(i);
    emitln("call %s", get_operand(arg1));
    if (arg_nb)
        emitln("add esp, %u", arg_nb);
    arg_nb = 0;
    if (tar)
        update_tar_descriptors(X86_EAX, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_call(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_pre_call(i);
    emitln("call %s", address(arg1).cont.var.e->attr.str);
    if (arg_nb)
        emitln("add esp, %u", arg_nb);
    arg_nb = 0;
    if (tar)
        update_tar_descriptors(X86_EAX, tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    /*update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);*/
}

#define emit_lab(n)         emitln(".L%lu:", n)
#define emit_jmp(target)    emitln("jmp .L%lu", target)
#define emit_jmpeq(target)  emitln("je .L%lu", target)
#define emit_jmpneq(target) emitln("jne .L%lu", target)

void x86_ind_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    BSet *s;
    Token ty;
    X86_Reg res, pr;
    char *siz_str, *reg_str;

    /* force the reload of any target currently in a register */
    if ((s=get_pointer_targets(i, nid(tar))) != NULL) {
        int i;

        for (i = bset_iterate(s); i != -1; i = bset_iterate(NULL))
            if (addr_in_reg(i))
                spill_reg(addr_reg(i));
    } else {
        spill_all();
    }

    ty = get_type_category(instruction(i).type);
    if (ty==TOK_STRUCT || ty==TOK_UNION) {
        int cluttered;

        cluttered = 0;
        if (!addr_in_reg(arg1) || addr_reg(arg1)!=X86_ESI) {
            if (reg_descr_tab[X86_ESI].naddrs != 0) {
                cluttered |= 1;
                emitln("push esi");
            }
            x86_load(X86_ESI, arg1);
        }
        if (!addr_in_reg(tar) || addr_reg(tar)!=X86_EDI) {
            if (reg_descr_tab[X86_EDI].naddrs != 0) {
                cluttered |= 2;
                emitln("push edi");
            }
            x86_load(X86_EDI, tar);
        }
        if (reg_descr_tab[X86_ECX].naddrs != 0) {
            cluttered |= 4;
            emitln("push ecx");
        }
        emitln("mov ecx, %u", compute_sizeof(instruction(i).type));
        /*emitln("cld");*/
        emitln("rep movsb");
        if (cluttered & 4)
            emitln("pop ecx");
        if (cluttered & 2)
            emitln("pop edi");
        if (cluttered & 1)
            emitln("pop esi");
        goto done;
    }

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    switch (ty) {
    case TOK_SHORT:
    case TOK_UNSIGNED_SHORT:
        siz_str = "word";
        reg_str = x86_lwreg_str[res];
        break;
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
    case TOK_UNSIGNED_CHAR:
        /* TODO: handle the case where the register is esi or edi */
        siz_str = "byte";
        reg_str = x86_lbreg_str[res];
        break;
    default:
        siz_str = "dword";
        reg_str = x86_reg_str[res];
        break;
    }
    if (!addr_in_reg(tar)) {
        if ((pr=get_empty_reg()) == -1) {
            pr = get_unpinned_reg();
            assert(pr != -1);
            spill_reg(pr);
        }
        x86_load(pr, tar);
    } else {
        pr = addr_reg(tar);
    }
    emitln("mov %s [%s], %s", siz_str, x86_reg_str[pr], reg_str);
    unpin_reg(res);
done:
    update_arg_descriptors(tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_lab(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    emit_lab(address(tar).cont.uval);
}
void x86_jmp(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    emit_jmp(address(tar).cont.uval);
}
void x86_arg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    Declaration ty;

    /*
     * Note: the type expression comes from the formal parameter
     * or from the actual argument, so some more care must be taken
     * when using it.
     */

    ty = *instruction(i).type;
    if (ty.idl!=NULL && ty.idl->op==TOK_ID)
        ty.idl = ty.idl->child;

    cat = get_type_category(&ty);
    if (cat!=TOK_STRUCT && cat!=TOK_UNION) {
        emitln("push %s", get_operand(arg1));
        arg_nb += 4;
    } else {
        unsigned siz, asiz;
        int cluttered, savnb;

        siz = compute_sizeof(&ty);
        asiz = round_up(siz, 4);
        emitln("sub esp, %lu", asiz);
        arg_nb += asiz;

        cluttered = savnb = 0;
        if (!addr_in_reg(arg1) || addr_reg(arg1)!=X86_ESI) {
            if (reg_descr_tab[X86_ESI].naddrs != 0) {
                cluttered |= 1;
                emitln("push esi");
                savnb += 4;
            }
            x86_load(X86_ESI, arg1);
        }
        if (reg_descr_tab[X86_EDI].naddrs != 0) {
            cluttered |= 2;
            emitln("push edi");
            savnb += 4;
        }
        if (!savnb)
            emitln("mov edi, esp");
        else
            emitln("lea edi, [esp+%d]", savnb);
        if (reg_descr_tab[X86_ECX].naddrs != 0) {
            cluttered |= 4;
            emitln("push ecx");
        }
        emitln("mov ecx, %u", siz);
        /*emitln("cld");*/
        emitln("rep movsb");
        if (cluttered & 4)
            emitln("pop ecx");
        if (cluttered & 2)
            emitln("pop edi");
        if (cluttered & 1)
            emitln("pop esi");
    }
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_ret(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (!big_return) {
        x86_load(X86_EAX, arg1);
    } else {
        unsigned siz;

        siz = compute_sizeof(&ret_ty);
        /*if (reg_descr_tab[X86_ESI].naddrs != 0)
            spill_reg(X86_ESI);*/
        x86_load(X86_ESI, arg1);
        modified[X86_ESI] = TRUE;
        if (reg_descr_tab[X86_EDI].naddrs != 0)
            spill_reg(X86_EDI);
        emitln("mov edi, dword [ebp-4]");
        modified[X86_EDI] = TRUE;
        if (reg_descr_tab[X86_ECX].naddrs != 0)
            spill_reg(X86_ECX);
        emitln("mov ecx, %u", siz);
        emitln("rep movsb");
        /*if (reg_descr_tab[X86_EAX].naddrs != 0)
            spill_reg(X86_EAX);
        emitln("mov eax, dword [ebp-4]");*/
    }
    update_arg_descriptors(arg1, instruction(i).liveness[1], instruction(i).next_use[1]);
}
void x86_cbr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    compare_against_zero(tar);
    /* do any spilling before the jumps */
    update_arg_descriptors(tar, instruction(i).liveness[0], instruction(i).next_use[0]);
    emit_jmpeq(address(arg2).cont.uval);
    emit_jmp(address(arg1).cont.uval);
}
void x86_nop(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

void (*instruction_handlers[])(int, unsigned, unsigned, unsigned) = {
    x86_add, x86_sub, x86_mul, x86_div,
    x86_rem, x86_shl, x86_shr, x86_and,
    x86_or, x86_xor, x86_eq, x86_neq,
    x86_lt, x86_let, x86_gt, x86_get,

    x86_neg, x86_cmpl, x86_not, x86_ch,
    x86_uch, x86_sh, x86_ush, x86_addr_of,
    x86_ind, x86_asn, x86_call, x86_indcall,

    x86_ind_asn, x86_lab, x86_jmp, x86_arg,
    x86_ret, x86_cbr, x86_nop
};

void x86_function_definition(TypeExp *decl_specs, TypeExp *header)
{
    /*
        cdecl calling convention
            ==> caller save: eax, ecx, edx
            ==> callee save: ebp, ebx, esi, edi

                        [ Stack frame layout ]
     => Low addresses
        +-----------------------------------------------------------+ <- ESP    EBP-?
        |               Calle save registers                        |
        +-----------------------------------------------------------+ <- EBP-?
        |               Space for temp struct/union                 |
        |   (used when calling a struct/union valued function)      |
        +-----------------------------------------------------------+
        |               ... (more vars and temps)                   |
        +-----------------------------------------------------------+ <- EBP-8
        |               First local variable                        |
        +-----------------------------------------------------------+ <- EBP-4
        |               Return Value Address                        |
        |       (used when returning a struct/union)                |
        +-----------------------------------------------------------+ <- EBP
        |               Saved EBP                                   |
        +-----------------------------------------------------------+ <- EBP+4
        |               Ret. Addr                                   |
        +-----------------------------------------------------------+ <- EBP+8
        |               First argument                              |
        +-----------------------------------------------------------+
     => High addresses
    */

    int b;
    Token cat;

    ic_function_definition(decl_specs, header);
    init_addr_descr_tab();
    size_of_local_area = round_up(size_of_local_area, 4);
    curr_func = header->str;
    ret_ty.decl_specs = decl_specs;
    ret_ty.idl = header->child->child;

    big_return = ((cat=get_type_category(&ret_ty))==TOK_STRUCT || cat==TOK_UNION);

    emit_prologln("\n; ==== start of definition of function `%s' ====", curr_func);
    emit_prologln("%s:", curr_func);
    if (big_return) {
        emit_prologln("pop eax");
        emit_prologln("xchg [esp], eax");
    }
    emit_prologln("push ebp");
    emit_prologln("mov ebp, esp");
    emit_prolog("sub esp, ");

    for (b = ENTRY_NODE; b < cfg_nodes_counter; b++) {
        int i;

        for (i = cfg_node(b).leader; i <= cfg_node(b).last; i++) {
            unsigned tar, arg1, arg2;

            tar = instruction(i).tar;
            arg1 = instruction(i).arg1;
            arg2 = instruction(i).arg2;

            instruction_handlers[instruction(i).op](i, tar, arg1, arg2);
        } /* end of basic block */
    }
    size_of_local_area -= temp_struct_size;
    while (--calls_to_fix_counter >= 0) {
        int n;
        char *s;

        s = calls_to_fix[calls_to_fix_counter];
        n = sprintf(s, "%d", -size_of_local_area);
        s[n++] = ']';
        for (; s[n] == 'X'; n++)
            s[n] = ' ';
    }

    emit_prologln("%d", -size_of_local_area);
    if (modified[X86_ESI]) emit_prologln("push esi");
    if (modified[X86_EDI]) emit_prologln("push edi");
    if (modified[X86_EBX]) emit_prologln("push ebx");

    if (big_return) {
        emit_prologln("mov dword [ebp-4], eax");
        emit_epilogln("mov eax, dword [ebp-4]");
    }

    if (modified[X86_EBX]) emit_epilogln("pop ebx");
    if (modified[X86_EDI]) emit_epilogln("pop edi");
    if (modified[X86_ESI]) emit_epilogln("pop esi");
    emit_epilogln("mov esp, ebp");
    emit_epilogln("pop ebp");
    emit_epilogln("ret");

    string_write(func_prolog, stdout);
    string_write(func_body, stdout);
    string_write(func_epilog, stdout);

    /* reset everything */
    string_clear(func_prolog);
    string_clear(func_body);
    string_clear(func_epilog);
    temp_struct_size = 0;
    calls_to_fix_counter = 0;
    memset(modified, 0, sizeof(int)*X86_NREG);
    free(addr_descr_tab);
    ic_reset();
}
