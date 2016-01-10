/*
 * Simple MIPS code generator
 *      IC ==> MIPS ASM (to be assembled with luxasmips)
 */
#define DEBUG 0
#include "mips_cgen.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>
#include <limits.h>
#include "../util.h"
#include "../decl.h"
#include "../expr.h"
#include "../arena.h"
#include "../imp_lim.h"
#include "../error.h"
#include "../ic.h"
#include "../dflow.h"
#include "../str.h"
#include "../luxcc.h"

#define MIPS_NREG 32
typedef int bool;
typedef short MIPS_Reg;
typedef struct { MIPS_Reg r1, r2; } MIPS_Reg2;

static int pinned[MIPS_NREG] = {
    1, 1, 1, 1, /* $zero, $at, $v0, $v1 */
    1, 1, 1, 1, /* $a0, $a1, $a2, $a3 */
    0, 0, 0, 0, /* $t0, $t1, $t2, $t3 */
    0, 0, 0, 0, /* $t4, $t5, $t6, $t7 */
    1, 1, 1, 1, /* $s0, $s1, $s2, $s3 */
    1, 1, 1, 1, /* $s4, $s5, $s6, $s7 */
    0, 1, 1, 1, /* $t8, t9, $k0, $k1 */
    1, 1, 1, 1, /* $gp, $sp, $fp, $ra */
};
/*static int modified[MIPS_NREG];*/
#define pin_reg(r)    (pinned[r] = TRUE)
#define pin_reg2(r)   (pinned[r.r1] = TRUE, pinned[r.r2] = TRUE)
#define unpin_reg(r)  (pinned[r] = FALSE)
#define unpin_reg2(r) (pinned[r.r1] = FALSE, pinned[r.r2] = FALSE)

static int size_of_local_area;
static char *curr_func, *enclosing_function;
static unsigned temp_struct_size;
static int big_return, qword_return;
static int calls_to_fix_counter;
static unsigned calls_to_fix[64];
static int string_literals_counter;
static FILE *mips_output_file;
static int arg_stack[64], arg_stack_top;

#define JMP_TAB_MIN_SIZ   3
#define JMP_TAB_MAX_HOLES 10
static int jump_tables_counter;

#define DATA_SEG 0
#define TEXT_SEG 1
#define BSS_SEG  2
#define ROD_SEG  3
static int curr_segment = -1;
static char *str_segment[] = {
    ".data",
    ".text",
    ".bss",
    ".rodata"
};
#define SET_SEGMENT(seg, f)\
    do {\
        if (curr_segment != seg) {\
            f("%%segment %s", str_segment[seg]);\
            curr_segment = seg;\
        }\
    } while(0)

static int new_string_literal(unsigned a);
static void emit_raw_string(String *q, char *s);
static String *func_body, *func_prolog, *func_epilog, *asm_decls, *str_lits;
#define emit(...)           (string_printf(func_body, __VA_ARGS__))
#define emitln(...)         (string_printf(func_body, __VA_ARGS__), string_printf(func_body, "\n"))
#define emit_prolog(...)    (string_printf(func_prolog, __VA_ARGS__))
#define emit_prologln(...)  (string_printf(func_prolog, __VA_ARGS__), string_printf(func_prolog, "\n"))
#define emit_epilog(...)    (string_printf(func_epilog, __VA_ARGS__))
#define emit_epilogln(...)  (string_printf(func_epilog, __VA_ARGS__), string_printf(func_epilog, "\n"))
#define emit_decl(...)      (string_printf(asm_decls, __VA_ARGS__))
#define emit_declln(...)    (string_printf(asm_decls, __VA_ARGS__), string_printf(asm_decls, "\n"))
#define emit_str(...)       (string_printf(str_lits, __VA_ARGS__))
#define emit_strln(...)     (string_printf(str_lits, __VA_ARGS__), string_printf(str_lits, "\n"))

static MIPS_Reg2 *addr_descr_tab;
static unsigned reg_descr_tab[MIPS_NREG];
#define addr_reg(a)    (addr_descr_tab[address_nid(a)])
#define addr_reg1(a)   (addr_descr_tab[address_nid(a)].r1)
#define addr_reg2(a)   (addr_descr_tab[address_nid(a)].r2)
#define reg_isempty(r) (reg_descr_tab[r] == 0)
static void dump_addr_descr_tab(void);
static void dump_reg_descr_tab(void);

static void spill_reg(MIPS_Reg r);
static void spill_all(void);
static void spill_aliased_objects(void);
static MIPS_Reg get_reg0(void);
static MIPS_Reg get_reg(int i);
static MIPS_Reg2 get_reg2(int i);

static void mips_load(MIPS_Reg r, unsigned a);
static void mips_load2(MIPS_Reg2 r, unsigned a);
static void mips_load_addr(MIPS_Reg r, unsigned a);
static char *mips_get_operand(unsigned a, int imm);
static char **mips_get_operand2(unsigned a);
static void mips_store(MIPS_Reg r, unsigned a);
static void mips_store2(MIPS_Reg2 r, unsigned a);
static void mips_function_definition(TypeExp *decl_specs, TypeExp *header);

static long long mips_static_expr(ExecNode *e);
static void mips_static_init(TypeExp *ds, TypeExp *dct, ExecNode *e);
static void mips_allocate_static_objects(void);

typedef struct Temp Temp;
static struct Temp {
    int nid;
    int offs;   /* offset from $fp */
    int free;
    Temp *next;
} *temp_list;

static int get_temp_offs(unsigned a);
static void free_temp(unsigned a);
static void free_all_temps(void);

void emit_raw_string(String *q, char *s)
{
    unsigned len, i;

    len = strlen(s)+1;
    for (i = len/4; i; i--) {
        string_printf(q, "%%dword 0x%02x%02x%02x%02x\n", s[3], s[2], s[1], s[0]);
        s += 4;
    }
    for (i = len%4; i; i--)
        string_printf(q, "%%byte 0x%02x\n", *s++);
}

int new_string_literal(unsigned a)
{
    emit_strln("_@S%d:", string_literals_counter);
    emit_raw_string(str_lits, address(a).cont.str);
    return string_literals_counter++;
}

void dump_addr_descr_tab(void)
{
    int i;

    for (i = 0; i < nid_counter; i++) {
        MIPS_Reg2 ad;

        ad = addr_descr_tab[i];
        if (ad.r1 != -1) {
            if (ad.r2 != -1)
                fprintf(stderr, "%s => $%d:$%d\n", nid2sid_tab[i], ad.r1, ad.r2);
            else
                fprintf(stderr, "%s => $%d\n", nid2sid_tab[i], ad.r1);
        }
    }
}

void dump_reg_descr_tab(void)
{
    int i;

    for (i = 0; i < MIPS_NREG; i++)
        if (!reg_isempty(i))
            fprintf(stderr, "$%d => %s\n", i, address_sid(reg_descr_tab[i]));
}

void spill_reg(MIPS_Reg r)
{
    unsigned a;

    if (reg_isempty(r))
        return;

    a = reg_descr_tab[r];
    if (addr_reg2(a) != -1) { /* spill the register pair */
        MIPS_Reg2 rr;

        rr = addr_reg(a);
        mips_store2(rr, a);
        addr_reg1(a) = addr_reg2(a) = -1;
        reg_descr_tab[rr.r1] = reg_descr_tab[rr.r2] = 0;
    } else {
        mips_store(r, a);
        addr_reg1(a) = -1;
        reg_descr_tab[r] = 0;
    }
}

void spill_all(void)
{
    int i;

    for (i = 0; i < MIPS_NREG; i++)
        spill_reg((MIPS_Reg)i);
}

void spill_aliased_objects(void)
{
    int i;

    for (i = 0; i < MIPS_NREG; i++) {
        unsigned a;

        if (reg_isempty(i))
            continue;
        a = reg_descr_tab[i];
        if (address(a).kind==IdKind && bset_member(address_taken_variables, address_nid(a)))
            spill_reg((MIPS_Reg)i);
    }
}

MIPS_Reg get_reg0(void)
{
    int i;

    for (i = 0; i < MIPS_NREG; i++)
        if (!pinned[i] && reg_isempty(i))
            return (MIPS_Reg)i;

    for (i = 0; i < MIPS_NREG; i++) {
        if (!pinned[i]) {
            spill_reg((MIPS_Reg)i);
            return (MIPS_Reg)i;
        }
    }

    assert(0);
    return 0;
}

MIPS_Reg get_reg(int i)
{
    unsigned arg1;

    arg1 = instruction(i).arg1;
    if (!const_addr(arg1) && addr_reg1(arg1)!=-1 && !arg1_liveness(i))
        return addr_reg1(arg1);
    return get_reg0();
}

MIPS_Reg2 get_reg2(int i)
{
    MIPS_Reg2 r;
    unsigned arg1;

    arg1 = instruction(i).arg1;
    if (!const_addr(arg1) && !arg1_liveness(i)) {
        r.r1 = (addr_reg1(arg1) != -1) ? addr_reg1(arg1) : get_reg0();
        pin_reg(r.r1);
        r.r2 = (addr_reg2(arg1) != -1) ? addr_reg2(arg1) : get_reg0();
        unpin_reg(r.r1);
    } else {
        r.r1 = get_reg0();
        pin_reg(r.r1);
        r.r2 = get_reg0();
        unpin_reg(r.r1);
    }
    return r;
}

int get_temp_offs(unsigned a)
{
    Temp *p;

    /* see if it was already allocated */
    for (p = temp_list; p != NULL; p = p->next)
        if (!p->free && p->nid==address_nid(a))
            return p->offs;

    /* try to find an unused temp */
    for (p = temp_list; p != NULL; p = p->next) {
        if (p->free) {
            p->nid = address_nid(a);
            p->free = FALSE;
            return p->offs;
        }
    }

    /* allocate a new temp */
    p = malloc(sizeof(Temp));
    p->nid = address_nid(a);
    size_of_local_area -= 8;
    p->offs = size_of_local_area;
    p->free = FALSE;
    p->next = temp_list;
    temp_list = p;
    return p->offs;
}

void free_temp(unsigned a)
{
    Temp *p;

    for (p = temp_list; p != NULL; p = p->next) {
        if (!p->free && p->nid==address_nid(a)) {
            p->free = TRUE;
            break;
        }
    }
}

void free_all_temps(void)
{
    Temp *p, *q;

    p = temp_list;
    while (p != NULL) {
        q = p;
        p = p->next;
        free(q);
    }
    temp_list = NULL;
}

#define local_offset(a) (address(a).cont.var.offset)
#define ISLL(ty) ((cat=get_type_category(ty))==TOK_LONG_LONG || cat==TOK_UNSIGNED_LONG_LONG)

void mips_load(MIPS_Reg r, unsigned a)
{
    if (address(a).kind == IConstKind) {
        emitln("li $%d, %u", r, (unsigned)address(a).cont.uval);
    } else if (address(a).kind == StrLitKind) {
        emitln("la $%d, _@S%d", r, new_string_literal(a));
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *ld_str;

        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) == r)
                ; /* already in the register */
            else
                emitln("move $%d, $%d", r, addr_reg1(a));
            return;
        }

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
        case TOK_SUBSCRIPT:
        case TOK_FUNCTION:
            mips_load_addr(r, a);
            return;
        case TOK_SHORT:
            ld_str = "lh";
            break;
        case TOK_UNSIGNED_SHORT:
            ld_str = "lhu";
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
            ld_str = "lb";
            break;
        case TOK_UNSIGNED_CHAR:
            ld_str = "lbu";
            break;
        default:
            ld_str = "lw";
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) /* static local */
                emitln("%s $%d, %s@%s", ld_str, r, curr_func, e->attr.str);
            else /* global */
                emitln("%s $%d, %s", ld_str, r, e->attr.str);
        } else { /* parameter or local */
            emitln("%s $%d, %d($30)", ld_str, r, local_offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) == r)
                return; /* already in the register */
            else
                emitln("move $%d, $%d", r, addr_reg1(a));
        } else {
            emitln("lw $%d, %d($30)", r, get_temp_offs(a));
        }
    }
}

void mips_load2(MIPS_Reg2 r, unsigned a)
{
    if (address(a).kind == IConstKind) {
        unsigned *p;

        p = (unsigned *)&address(a).cont.uval;
        emitln("li $%d, %u", r.r1, p[0]);
        emitln("li $%d, %u", r.r2, p[1]);
    } else if (address(a).kind == StrLitKind) {
        emitln("la $%d, _@S%d", r.r1, new_string_literal(a));
        emitln("clear $%d", r.r2);
    } else if (address(a).kind == IdKind) {
        Token cat;
        ExecNode *e;

        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) != r.r1) {
                assert(addr_reg2(a) != r.r2);
                emitln("move $%d, $%d", r.r1, addr_reg1(a));
                emitln("move $%d, $%d", r.r2, addr_reg2(a));
            } else {
                assert(addr_reg2(a) == r.r2);
            }
            return;
        }

        e = address(a).cont.var.e;
        cat = get_type_category(&e->type);
        assert(cat==TOK_LONG_LONG || cat==TOK_UNSIGNED_LONG_LONG);

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) {
                emitln("lw $%d, %s@%s", r.r1, curr_func, e->attr.str);
                emitln("lw $%d, %s@%s+4", r.r2, curr_func, e->attr.str);
            } else {
                emitln("lw $%d, %s", r.r1, e->attr.str);
                emitln("lw $%d, %s+4", r.r2, e->attr.str);
            }
        } else {
            emitln("lw $%d, %d($30)", r.r1, local_offset(a));
            emitln("lw $%d, %d($30)", r.r2, local_offset(a)+4);
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) != r.r1)
                emitln("move $%d, $%d", r.r1, addr_reg1(a));
            assert(addr_reg2(a) != -1);
            if (addr_reg2(a) != r.r2)
                emitln("move $%d, $%d", r.r2, addr_reg2(a));
        } else {
            int offs;

            offs = get_temp_offs(a);
            emitln("lw $%d, %d($30)", r.r1, offs);
            emitln("lw $%d, %d($30)", r.r2, offs+4);
        }
    }
}

/*
 * The `imm' parameter is used when `a' is an integer constant.
 * Possible values:
 * - 1: return the integer constant if it can be represented in a 16-bit signed integer.
 * - 2: return the integer constant if it can be represented in a 16-bit unsigned integer.
 * - 0: always return the integer constant loaded into a register.
 */
char *mips_get_operand(unsigned a, int imm)
{
    MIPS_Reg r;
    static char op[256];

    if (imm && address(a).kind==IConstKind) {
        int val;

        val = (int)address(a).cont.val;
        if (imm == 1) {
            if (val>=-32768 && val<=32767) {
                sprintf(op, "%u", (unsigned)val);
                return op;
            }
        } else {
            if (val>=0 && val<=65535) {
                sprintf(op, "%u", (unsigned)val);
                return op;
            }
        }
    } else if (address(a).kind==IdKind || address(a).kind==TempKind) {
        if (addr_reg1(a) != -1) {
            sprintf(op, "$%d", addr_reg1(a));
            return op;
        }
    }
    r = get_reg0();
    mips_load(r, a);
    sprintf(op, "$%d", r);
#if 0
    addr_reg1(a) = r;
    reg_descr_tab[r] = a;
#endif
    return op;
}

char **mips_get_operand2(unsigned a)
{
    MIPS_Reg2 r;
    static char op1[256], op2[256];
    static char *op[2] = { op1, op2 };

    if ((address(a).kind==IdKind || address(a).kind==TempKind) && addr_reg1(a)!=-1) {
        assert(addr_reg2(a) != -1);
        sprintf(op1, "$%d", addr_reg1(a));
        sprintf(op2, "$%d", addr_reg2(a));
        return op;
    }
    r.r1 = get_reg0();
    pin_reg(r.r1);
    r.r2 = get_reg0();
    unpin_reg(r.r1);
    mips_load2(r, a);
    sprintf(op1, "$%d", r.r1);
    sprintf(op2, "$%d", r.r2);
    return op;
}

void mips_load_addr(MIPS_Reg r, unsigned a)
{
    ExecNode *e;

    e = address(a).cont.var.e;
    if (e->attr.var.duration == DURATION_STATIC) {
        if (e->attr.var.linkage == LINKAGE_NONE)
            emitln("la $%d, %s@%s", r, curr_func, e->attr.str);
        else
            emitln("la $%d, %s", r, e->attr.str);
    } else {
        emitln("addu $%d, $30, %d", r, local_offset(a));
    }
}

void mips_store(MIPS_Reg r, unsigned a)
{
    if (address(a).kind == IdKind) {
        ExecNode *e;
        char *st_str;

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
            emitln("move $5, $%d", r);
            mips_load_addr(4, a);
            emitln("li $6, %u", get_sizeof(&e->type));
            emitln("jal __lux_mips_memcpy");
            emitln("nop");
            return;
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            st_str = "sh";
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR:
            st_str = "sb";
            break;
        default:
            st_str = "sw";
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) /* static local */
                emitln("%s $%d, %s@%s", st_str, r, curr_func, e->attr.str);
            else /* global */
                emitln("%s $%d, %s", st_str, r, e->attr.str);
        } else { /* parameter or local */
            emitln("%s $%d, %d($30)", st_str, r, local_offset(a));
        }
    } else if (address(a).kind == TempKind) {
        emitln("sw $%d, %d($30)", r, get_temp_offs(a));
    }
}

void mips_store2(MIPS_Reg2 r, unsigned a)
{
    if (address(a).kind == IdKind) {
        ExecNode *e;

        e = address(a).cont.var.e;
        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) {
                emitln("sw $%d, %s@%s", r.r1, curr_func, e->attr.str);
                emitln("sw $%d, %s@%s+4", r.r2, curr_func, e->attr.str);
            } else {
                emitln("sw $%d, %s", r.r1, e->attr.str);
                emitln("sw $%d, %s+4", r.r2, e->attr.str);
            }
        } else {
            emitln("sw $%d, %d($30)", r.r1, local_offset(a));
            emitln("sw $%d, %d($30)", r.r2, local_offset(a)+4);
        }
    } else if (address(a).kind == TempKind) {
        int offs;

        offs = get_temp_offs(a);
        emitln("sw $%d, %d($30)", r.r1, offs);
        emitln("sw $%d, %d($30)", r.r2, offs+4);
    }
}

static void update_arg_descriptors(unsigned arg, unsigned char liveness, int next_use)
{
    if (const_addr(arg) || next_use)
        return;

    if (addr_reg1(arg) != -1) {
        if (liveness) { /* spill */
            if (addr_reg2(arg) != -1)
                mips_store2(addr_reg(arg), arg);
            else
                mips_store(addr_reg1(arg), arg);
        } else if (address(arg).kind == TempKind) {
            free_temp(arg);
        }
        reg_descr_tab[addr_reg1(arg)] = 0;
        if (addr_reg2(arg) != -1)
            reg_descr_tab[addr_reg2(arg)] = 0;
        addr_reg1(arg) = addr_reg2(arg) = -1;
    } else {
        if (liveness)
            ;
        else if (address(arg).kind == TempKind)
            free_temp(arg);
    }
}

static void update_tar_descriptors(MIPS_Reg res, unsigned tar, unsigned char liveness, int next_use)
{
    /* Note:
        maintain the order of the operations for mips_store()
        to work correctly with struct operands.
    */

    addr_reg1(tar) = res;
    reg_descr_tab[res] = tar;

    if (!next_use) {
        if (liveness) /* spill */
            mips_store(res, tar);
        addr_reg1(tar) = -1;
        reg_descr_tab[res] = 0;
    }
}

static void update_tar_descriptors2(MIPS_Reg2 res, unsigned tar, unsigned char liveness, int next_use)
{
    addr_reg1(tar) = res.r1;
    addr_reg2(tar) = res.r2;
    reg_descr_tab[res.r1] = tar;
    reg_descr_tab[res.r2] = tar;

    if (!next_use) {
        if (liveness) /* spill */
            mips_store2(res, tar);
        addr_reg1(tar) = -1;
        addr_reg2(tar) = -1;
        reg_descr_tab[res.r1] = 0;
        reg_descr_tab[res.r2] = 0;
    }
}

#define UPDATE_ADDRESSES(res_reg)\
    do {\
        update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));\
        update_arg_descriptors(arg2, arg2_liveness(i), arg2_next_use(i));\
        update_tar_descriptors(res_reg, tar, tar_liveness(i), tar_next_use(i));\
    } while (0)
#define UPDATE_ADDRESSES2(res_reg)\
    do {\
        update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));\
        update_arg_descriptors(arg2, arg2_liveness(i), arg2_next_use(i));\
        update_tar_descriptors2(res_reg, tar, tar_liveness(i), tar_next_use(i));\
    } while (0)
#define UPDATE_ADDRESSES_UNARY(res_reg)\
    do {\
        update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));\
        update_tar_descriptors(res_reg, tar, tar_liveness(i), tar_next_use(i));\
    } while (0)
#define UPDATE_ADDRESSES_UNARY2(res_reg)\
    do {\
        update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));\
        update_tar_descriptors2(res_reg, tar, tar_liveness(i), tar_next_use(i));\
    } while (0)

#define emit_lab(n)                 emitln("%s.L%d:", curr_func, (int)n)
#define emit_jmp(target)            emitln("b %s.L%d", curr_func, (int)target)
#define emit_bf(r, target)          emitln("beq $%d, $0, %s.L%d", r, curr_func, (int)target)
#define emit_bt(r, target)          emitln("bne $%d, $0, %s.L%d", r, curr_func, (int)target)
#define emit_blt(r1, r2, target)    emitln("blt $%d, $%d, %s.L%d", r1, r2, curr_func, (int)target)
#define emit_bgt(r1, r2, target)    emitln("bgt $%d, $%d, %s.L%d", r1, r2, curr_func, (int)target)
#define emit_beq(r1, r2, target)    emitln("beq $%d, $%d, %s.L%d", r1, r2, curr_func, (int)target)

enum {
    LibMul,
    LibSDiv,
    LibUDiv,
    LibSMod,
    LibUMod,
    LibShL,
    LibSShR,
    LibUShR,
    LibEq,
    LibNeq,
    LibULT,
    LibUGT,
    LibUGET,
    LibULET,
    LibSLT,
    LibSGT,
    LibSGET,
    LibSLET,
    LibNot,
};

static const char *libfuncs[] = {
    "__lux_mul64",
    "__lux_sdiv64",
    "__lux_udiv64",
    "__lux_smod64",
    "__lux_umod64",
    "__lux_shl64",
    "__lux_sshr64",
    "__lux_ushr64",
    "__lux_ucmp64",
    "__lux_ucmp64",
    "__lux_ucmp64",
    "__lux_ucmp64",
    "__lux_ucmp64",
    "__lux_ucmp64",
    "__lux_scmp64",
    "__lux_scmp64",
    "__lux_scmp64",
    "__lux_scmp64",
};

static void mips_do_libcall(int i, unsigned tar, unsigned arg1, unsigned arg2, int func)
{
    MIPS_Reg2 res = { 2, 3 }, r1 = { 4, 5 }, r2 = { 6, 7 };

    switch (func) {
    case LibMul:
    case LibSDiv:
    case LibUDiv:
    case LibSMod:
    case LibUMod:
        mips_load2(r1, arg1);
        mips_load2(r2, arg2);
        emitln("addu $29, $29, -16");
        spill_all();
        emitln("jal %s", libfuncs[func]);
        emitln("nop");
        emitln("addu $29, $29, 16");
        UPDATE_ADDRESSES2(res);
        break;

    case LibShL:
    case LibSShR:
    case LibUShR:
        mips_load2(r1, arg1);
        mips_load(6, arg2);
        emitln("addu $29, $29, -16");
        spill_all();
        emitln("jal %s", libfuncs[func]);
        emitln("nop");
        emitln("addu $29, $29, 16");
        UPDATE_ADDRESSES2(res);
        break;

    case LibNot:
        mips_load2(r1, arg1);
        emitln("li $6, 0");
        emitln("li $7, 0");
        emitln("addu $29, $29, -16");
        spill_all();
        emitln("jal __lux_ucmp64");
        emitln("nop");
        emitln("addu $29, $29, 16");
        emitln("seq $2, $2, 1");
        UPDATE_ADDRESSES_UNARY(2);
        break;

    case LibEq:
    case LibNeq:
    case LibSLT:
    case LibULT:
    case LibSGT:
    case LibUGT:
    case LibSGET:
    case LibUGET:
    case LibSLET:
    case LibULET:
        mips_load2(r1, arg1);
        mips_load2(r2, arg2);
        emitln("addu $29, $29, -16");
        spill_all();
        emitln("jal %s", libfuncs[func]);
        emitln("nop");
        emitln("addu $29, $29, 16");
        switch (func) {
        case LibEq:
            emitln("seq $2, $2, 1");
            break;
        case LibNeq:
            emitln("sne $2, $2, 1");
            break;
        case LibULT:
        case LibSLT:
            emitln("seq $2, $2, 4");
            break;
        case LibUGT:
        case LibSGT:
            emitln("seq $2, $2, 2");
            break;
        case LibUGET:
        case LibSGET:
            emitln("sne $2, $2, 4");
            break;
        case LibULET:
        case LibSLET:
            emitln("sne $2, $2, 2");
            break;
        }
        UPDATE_ADDRESSES(2);
        break;
    }
}

static void mips_add(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        MIPS_Reg2 res;

        res = get_reg2(i);
        mips_load2(res, arg1);
        pin_reg2(res);
        op = mips_get_operand2(arg2);
        emitln("addu $%d, $%d, %s", res.r1, res.r1, op[0]);
        emitln("sltu $25, $%d, %s", res.r1, op[0]);
        emitln("addu $%d, $%d, %s", res.r2, res.r2, op[1]);
        emitln("addu $%d, $%d, $25", res.r2, res.r2);
        unpin_reg2(res);
        UPDATE_ADDRESSES2(res);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("addu $%d, $%d, %s", res, res, mips_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_sub(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        MIPS_Reg2 res;
        MIPS_Reg br;

        res = get_reg2(i);
        mips_load2(res, arg1);
        pin_reg2(res);
        br = get_reg0();
        pin_reg(br);
        op = mips_get_operand2(arg2);
        emitln("subu $25, $%d, %s", res.r1, op[0]);
        emitln("sltu $%d, $%d, $25", br, res.r1);
        emitln("move $%d, $25", res.r1);
        emitln("subu $%d, $%d, %s", res.r2, res.r2, op[1]);
        emitln("subu $%d, $%d, $%d", res.r2, res.r2, br);
        unpin_reg2(res);
        unpin_reg(br);
        UPDATE_ADDRESSES2(res);
    } else {
        char *op;
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        if ((op=mips_get_operand(arg2, 1))[0] == '$')
            emitln("subu $%d, $%d, %s", res, res, op);
        else
            emitln("addu $%d, $%d, -%s", res, res, op);
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_mul(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        mips_do_libcall(i, tar, arg1, arg2, LibMul);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("mul $%d, $%d, %s", res, res, mips_get_operand(arg2, 0));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_div(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        mips_do_libcall(i, tar, arg1, arg2, is_signed_int(cat)?LibSDiv:LibUDiv);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("%s $%d, $%d, %s", is_unsigned_int(cat)?"divdu":"divd", res, res, mips_get_operand(arg2, 0));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_rem(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        mips_do_libcall(i, tar, arg1, arg2, is_signed_int(cat)?LibSMod:LibUMod);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("%s $%d, $%d, %s", is_unsigned_int(cat)?"remu":"rem", res, res, mips_get_operand(arg2, 0));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_shl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        mips_do_libcall(i, tar, arg1, arg2, LibShL);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("sll $%d, $%d, %s", res, res, mips_get_operand(arg2, 2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_shr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        mips_do_libcall(i, tar, arg1, arg2, is_signed_int(cat)?LibSShR:LibUShR);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("%s $%d, $%d, %s", is_unsigned_int(cat)?"srl":"sra", res, res, mips_get_operand(arg2, 2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_and(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        MIPS_Reg2 res;

        res = get_reg2(i);
        mips_load2(res, arg1);
        pin_reg2(res);
        op = mips_get_operand2(arg2);
        emitln("and $%d, $%d, %s", res.r1, res.r1, op[0]);
        emitln("and $%d, $%d, %s", res.r2, res.r2, op[1]);
        unpin_reg2(res);
        UPDATE_ADDRESSES2(res);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("and $%d, $%d, %s", res, res, mips_get_operand(arg2, 2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_or(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        MIPS_Reg2 res;

        res = get_reg2(i);
        mips_load2(res, arg1);
        pin_reg2(res);
        op = mips_get_operand2(arg2);
        emitln("or $%d, $%d, %s", res.r1, res.r1, op[0]);
        emitln("or $%d, $%d, %s", res.r2, res.r2, op[1]);
        unpin_reg2(res);
        UPDATE_ADDRESSES2(res);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("or $%d, $%d, %s", res, res, mips_get_operand(arg2, 2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_xor(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        MIPS_Reg2 res;

        res = get_reg2(i);
        mips_load2(res, arg1);
        pin_reg2(res);
        op = mips_get_operand2(arg2);
        emitln("xor $%d, $%d, %s", res.r1, res.r1, op[0]);
        emitln("xor $%d, $%d, %s", res.r2, res.r2, op[1]);
        unpin_reg2(res);
        UPDATE_ADDRESSES2(res);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("xor $%d, $%d, %s", res, res, mips_get_operand(arg2, 2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_eq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        mips_do_libcall(i, tar, arg1, arg2, LibEq);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("seq $%d, $%d, %s", res, res, mips_get_operand(arg2, 2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_neq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        mips_do_libcall(i, tar, arg1, arg2, LibNeq);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("sne $%d, $%d, %s", res, res, mips_get_operand(arg2, 2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_lt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        mips_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSLT:LibULT);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("%s $%d, $%d, %s", (flags&IC_SIGNED)?"slt":"sltu", res, res, mips_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_let(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        mips_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSLET:LibULET);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("%s $%d, $%d, %s", (flags&IC_SIGNED)?"sle":"sleu", res, res, mips_get_operand(arg2, 0));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_gt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        mips_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSGT:LibUGT);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("%s $%d, $%d, %s", (flags&IC_SIGNED)?"sgt":"sgtu", res, res, mips_get_operand(arg2, 0));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_get(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        mips_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSGET:LibUGET);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        pin_reg(res);
        emitln("%s $%d, $%d, %s", (flags&IC_SIGNED)?"sge":"sgeu", res, res, mips_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void mips_neg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        MIPS_Reg2 res;

        res = get_reg2(i);
        mips_load2(res, arg1);
        emitln("subu $%d, $0, $%d", res.r1, res.r1);
        emitln("sltu $25, $0, $%d", res.r1);
        emitln("subu $%d, $0, $%d", res.r2, res.r2);
        emitln("subu $%d, $%d, $25", res.r2, res.r2);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        emitln("negu $%d, $%d", res, res);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void mips_cmpl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        MIPS_Reg2 res;

        res = get_reg2(i);
        mips_load2(res, arg1);
        emitln("not $%d, $%d", res.r1, res.r1);
        emitln("not $%d, $%d", res.r2, res.r2);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        emitln("not $%d, $%d", res, res);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void mips_not(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        mips_do_libcall(i, tar, arg1, arg2, LibNot);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        emitln("seq $%d, $%d, $0", res, res);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void mips_ch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    MIPS_Reg res;

    res = get_reg(i);
    mips_load(res, arg1);
    emitln("seb $%d, $%d", res, res);
    UPDATE_ADDRESSES_UNARY(res);
}

static void mips_uch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    MIPS_Reg res;

    res = get_reg(i);
    mips_load(res, arg1);
    emitln("and $%d, $%d, 255", res, res);
    UPDATE_ADDRESSES_UNARY(res);
}

static void mips_sh(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    MIPS_Reg res;

    res = get_reg(i);
    mips_load(res, arg1);
    emitln("seh $%d, $%d", res, res);
    UPDATE_ADDRESSES_UNARY(res);
}

static void mips_ush(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    MIPS_Reg res;

    res = get_reg(i);
    mips_load(res, arg1);
    emitln("and $%d, $%d, 65535", res, res);
    UPDATE_ADDRESSES_UNARY(res);
}

static void mips_llsx(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    MIPS_Reg2 res;

    res = get_reg2(i);
    mips_load(res.r1, arg1);
    emitln("move $%d, $%d", res.r2, res.r1);
    emitln("sra $%d, $%d, 31", res.r2, res.r2);
    UPDATE_ADDRESSES_UNARY2(res);
}

static void mips_llzx(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    MIPS_Reg2 res;

    res = get_reg2(i);
    mips_load(res.r1, arg1);
    emitln("move $%d, $0", res.r2);
    UPDATE_ADDRESSES_UNARY2(res);
}

static void mips_addr_of(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    MIPS_Reg res;

    res = get_reg0();
    mips_load_addr(res, arg1);
    update_tar_descriptors(res, tar, tar_liveness(i), tar_next_use(i));
}

static void mips_ind(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    /* spill any target currently in a register */
    spill_aliased_objects();

    if (ISLL(instruction(i).type)) {
        MIPS_Reg2 res;

        res = get_reg2(i);
        mips_load(res.r1, arg1);
        emitln("lw $%d, 4($%d)", res.r2, res.r1);
        emitln("lw $%d, 0($%d)", res.r1, res.r1);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        switch (cat) {
        case TOK_STRUCT:
        case TOK_UNION:
            break;
        case TOK_SHORT:
            emitln("lh $%d, 0($%d)", res, res);
            break;
        case TOK_UNSIGNED_SHORT:
            emitln("lhu $%d, 0($%d)", res, res);
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
            emitln("lb $%d, 0($%d)", res, res);
            break;
        case TOK_UNSIGNED_CHAR:
            emitln("lbu $%d, 0($%d)", res, res);
            break;
        default:
            emitln("lw $%d, 0($%d)", res, res);
            break;
        }
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void mips_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        MIPS_Reg2 res;

        res = get_reg2(i);
        mips_load2(res, arg1);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        MIPS_Reg res;

        res = get_reg(i);
        mips_load(res, arg1);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void mips_pre_call(int i, unsigned arg2)
{
    Token cat;
    int na, nb, top;

    spill_all();

    if ((cat=get_type_category(instruction(i).type))==TOK_STRUCT || cat==TOK_UNION) {
        unsigned siz;

        siz = get_sizeof(instruction(i).type);
        if (siz > temp_struct_size)
            temp_struct_size = siz;
        calls_to_fix[calls_to_fix_counter++] = string_get_pos(func_body);
        emitln("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"); /* addu $25, $30, <?> */
        emitln("addu $29, $29, -4");
        emitln("sw $25, 0($29)");
        arg_stack[arg_stack_top++] = 4;
        ++address(arg2).cont.val;
    }

    /*
     * Move argument to registers.
     */
    nb = 0;
    top = arg_stack_top;
    for (na = (int)address(arg2).cont.val; na; na--)
        nb += arg_stack[--top];
    switch (nb) {
    case 0:
        emitln("addu $29, $29, -16");
        break;
    case 4:
        emitln("lw $4, 0($29)");
        emitln("addu $29, $29, -12");
        break;
    case 8:
        emitln("lw $4, 0($29)");
        emitln("lw $5, 4($29)");
        emitln("addu $29, $29, -8");
        break;
    case 12:
        emitln("lw $4, 0($29)");
        emitln("lw $5, 4($29)");
        emitln("lw $6, 8($29)");
        emitln("addu $29, $29, -4");
        break;
    default:
        emitln("lw $4, 0($29)");
        emitln("lw $5, 4($29)");
        emitln("lw $6, 8($29)");
        emitln("lw $7, 12($29)");
        break;
    }
}

static void mips_post_call(unsigned arg2)
{
    int na, nb;

    na = (int)address(arg2).cont.val;
    nb = 0;
    while (na--)
        nb += arg_stack[--arg_stack_top];
    assert(arg_stack_top >= 0);
    if (nb < 16)
        nb = 16;
    emitln("addu $29, $29, %d", nb);
}

static void mips_indcall(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    mips_pre_call(i, arg2);
    emitln("jalr %s", mips_get_operand(arg1, 0));
    emitln("nop");
    mips_post_call(arg2);
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    if (tar) {
        Token cat;

        if (ISLL(instruction(i).type)) {
            MIPS_Reg2 r = { 2, 3 };

            update_tar_descriptors2(r, tar, tar_liveness(i), tar_next_use(i));
        } else {
            update_tar_descriptors(2, tar, tar_liveness(i), tar_next_use(i));
        }
    }
}

static void mips_call(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    mips_pre_call(i, arg2);
    emitln("jal %s", address(arg1).cont.var.e->attr.str);
    emitln("nop");
    mips_post_call(arg2);
    if (tar) {
        Token cat;

        if (ISLL(instruction(i).type)) {
            MIPS_Reg2 r = { 2, 3 };

            update_tar_descriptors2(r, tar, tar_liveness(i), tar_next_use(i));
        } else {
            update_tar_descriptors(2, tar, tar_liveness(i), tar_next_use(i));
        }
    }
}

static void mips_ind_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    MIPS_Reg pr;
    char *st_str;

    /* force the reload of any target currently in a register */
    spill_aliased_objects();

    if (ISLL(instruction(i).type)) {
        if (addr_reg1(arg1) == -1) {
            pr = get_reg(i);
            mips_load(pr, arg1);
            pin_reg(pr);
        } else {
            pr = addr_reg1(arg1);
        }

        if (address(arg2).kind == IConstKind) {
            unsigned *p;

            p = (unsigned *)&address(arg2).cont.val;
            emitln("li $25, %u", p[0]);
            emitln("sw $25, 0($%d)", pr);
            emitln("li $25, %u", p[1]);
            emitln("sw $25, 4($%d)", pr);
        } else if (address(arg2).kind == StrLitKind) {
            emitln("la $25, _@S%d", new_string_literal(arg2));
            emitln("sw $25, 0($%d)", pr);
            emitln("sw $0, 4($%d)", pr);
        } else {
            MIPS_Reg2 r;

            if (addr_reg1(arg2) == -1) {
                r.r1 = get_reg0();
                pin_reg(r.r1);
                r.r2 = get_reg0();
                unpin_reg(r.r1);
                mips_load2(r, arg2);
            } else {
                r.r1 = addr_reg1(arg2);
                r.r2 = addr_reg2(arg2);
                assert(r.r2 != -1);
            }
            emitln("sw $%d, 0($%d)", r.r1, pr);
            emitln("sw $%d, 4($%d)", r.r2, pr);
        }
        unpin_reg(pr);
        goto done;
    } else if (cat==TOK_STRUCT || cat==TOK_UNION) {
        mips_load(5, arg2);
        mips_load(4, arg1);
        emitln("li $6, %u", get_sizeof(instruction(i).type));
        emitln("jal __lux_mips_memcpy");
        emitln("nop");
        goto done;
    }

    /*
     * <= 4 bytes scalar indirect assignment.
     */

    if (addr_reg1(arg1) == -1) {
        pr = get_reg(i);
        mips_load(pr, arg1);
        pin_reg(pr);
    } else {
        pr = addr_reg1(arg1);
    }

    switch (cat) {
    case TOK_SHORT:
    case TOK_UNSIGNED_SHORT:
        st_str = "sh";
        break;
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
    case TOK_UNSIGNED_CHAR:
        st_str = "sb";
        break;
    default:
        st_str = "sw";
        break;
    }

    if (address(arg2).kind == IConstKind) {
        emitln("li $25, %u", (unsigned)address(arg2).cont.uval);
        emitln("%s $25, 0($%d)", st_str, pr);
    } else if (address(arg2).kind == StrLitKind) {
        emitln("la $25, _@S%d", new_string_literal(arg2));
        emitln("%s $25, 0($%d)", st_str, pr);
    } else {
        MIPS_Reg r;

        if (addr_reg1(arg2) == -1) {
            r = get_reg0();
            mips_load(r, arg2);
        } else {
            r = addr_reg1(arg2);
        }
        emitln("%s $%d, 0($%d)", st_str, r, pr);
    }
    unpin_reg(pr);
done:
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    update_arg_descriptors(arg2, arg2_liveness(i), arg2_next_use(i));
}

static void mips_lab(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    emit_lab(address(tar).cont.val);
}

static void mips_jmp(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (instruction(i+1).op == OpLab
    && address(instruction(i+1).tar).cont.val == address(tar).cont.val)
        return;
    emit_jmp(address(tar).cont.val);
    emitln("nop");
}

static void mips_begarg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void mips_arg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    Declaration ty;

    ty = *instruction(i).type;
    if (ty.idl!=NULL && ty.idl->op==TOK_ID)
        ty.idl = ty.idl->child;

    if (ISLL(&ty)) {
        char **op;

        op = mips_get_operand2(arg1);
        emitln("addu $29, $29, -8");
        emitln("sw %s, 0($29)", op[0]);
        emitln("sw %s, 4($29)", op[1]);
        arg_stack[arg_stack_top++] = 8;
    } else if (cat==TOK_STRUCT || cat==TOK_UNION) {
        unsigned siz, asiz;

        siz = get_sizeof(&ty);
        asiz = round_up(siz, 4);

        emitln("addu $29, $29, -%u", asiz);
        mips_load(5, arg1);
        emitln("move $4, $29");
        emitln("li $6, %u", siz);
        emitln("jal __lux_mips_memcpy");
        emitln("nop");
        arg_stack[arg_stack_top++] = asiz;
    } else {
        MIPS_Reg r;

        if (const_addr(arg1) || (r=addr_reg1(arg1))==-1) {
            r = (MIPS_Reg)25;
            mips_load(r, arg1);
        }
        emitln("addu $29, $29, -4");
        emitln("sw $%d, 0($29)", r);
        arg_stack[arg_stack_top++] = 4;
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

static void mips_ret(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (qword_return) {
        MIPS_Reg2 r = { 2, 3 };

        mips_load2(r, arg1);
    } else if (big_return) {
        mips_load(5, arg1);
        emitln("lw $4, 8($30)");
        emitln("move $2, $4");
        emitln("li $6, %u", get_sizeof(instruction(i).type));
        emitln("jal __lux_mips_memcpy");
        emitln("nop");
    } else {
        mips_load(2, arg1);
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

static void mips_cbr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    MIPS_Reg r;

    if (ISLL(instruction(i).type)) {
        MIPS_Reg2 r2;

        r2 = get_reg2(i);
        mips_load2(r2, arg1);
        emitln("or $%d, $%d, $%d", r2.r1, r2.r1, r2.r2);
        r = r2.r1;
    } else {
        r = get_reg(i);
        mips_load(r, arg1);
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    if (address(tar).cont.val == address(instruction(i+1).tar).cont.val)
        emit_bf(r, address(arg2).cont.val);
    else
        emit_bt(r, address(tar).cont.val);
    emitln("nop");
}

static void mips_nop(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void mips_do_switch64(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    MIPS_Reg2 res;
    static unsigned nlab;

    if (addr_reg1(arg1) == -1) {
        res = get_reg2(i);
        mips_load2(res, arg1);
    } else {
        res = addr_reg(arg1);
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));

    /* do a simple linear search */
    for (++i; ; i++) {
        unsigned *p;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        if (address(arg2).cont.val) {
            emit_jmp(address(arg1).cont.val);
            break;
        }

        p = (unsigned *)&address(tar).cont.uval;
        emitln("li $25, %d", p[0]);
        emitln("bne $%d, $25, %s.sw64L%u", res.r1, curr_func, nlab);
        emitln("nop");
        emitln("li $25, %d", p[1]);
        emit_beq(res.r2, 25, address(arg1).cont.val);
        emitln("nop");
        emitln("%s.sw64L%u:", curr_func, nlab);
        ++nlab;
    }
}

static void mips_switch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    MIPS_Reg res;
    int def_val;
    Arena *lab_arena;
    char **jmp_tab, def_lab[256];
    int ncase, min, max, interval_size, holes;

    if (ISLL(instruction(i).type)) {
        mips_do_switch64(i, tar, arg1, arg2);
        return;
    }

    if (addr_reg1(arg1) == -1) {
        res = get_reg(i);
        mips_load(res, arg1);
    } else {
        res = addr_reg1(arg1);
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    ncase = address(arg2).cont.val-1;
    ++i;

    if (ncase >= JMP_TAB_MIN_SIZ) {
        int case_start;

        case_start = i;
        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;
        min = max = address(tar).cont.val;
        ++i;
        for (;; i++) {
            tar = instruction(i).tar;
            arg1 = instruction(i).arg1;
            arg2 = instruction(i).arg2;
            if (address(arg2).cont.val) {
                def_val = address(arg1).cont.val;
                break;
            }
            if (address(tar).cont.val < min)
                min = address(tar).cont.val;
            else if (address(tar).cont.val > max)
                max = address(tar).cont.val;
        }
        i = case_start;
        interval_size = max-min+1;
        holes = interval_size-ncase;
        if (holes <= JMP_TAB_MAX_HOLES)
            goto jump_table;
    }
    goto linear_search;

jump_table:
    emitln("li $25, %d", max);
    emit_bgt(res, 25, def_val);
    emitln("nop");
    emitln("li $25, %d", min);
    emit_blt(res, 25, def_val);
    emitln("nop");
    if (min != 0)
        emitln("subu $%d, $%d, $25", res, res);
    emitln("la $25, @jt%d", jump_tables_counter);
    emitln("sll $%d, $%d, 2", res, res);
    emitln("addu $%d, $%d, $25", res, res);
    emitln("lw $%d, 0($%d)", res, res);
    emitln("jalr $%d", res);
    emitln("nop");

    /* build jump table */
    jmp_tab = calloc(interval_size, sizeof(char *));
    lab_arena = arena_new(interval_size*256, FALSE);
    sprintf(def_lab, "%s.L%d", curr_func, def_val);
    for (;; i++) {
        char *s;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;
        if (address(arg2).cont.val)
            break;
        s = arena_alloc(lab_arena, 256);
        sprintf(s, "%s.L%d", curr_func, (int)address(arg1).cont.val);
        jmp_tab[address(tar).cont.val-min] = s;
    }
    /* fill holes with the default label */
    for (i = 0; holes; i++) {
        if (jmp_tab[i] == NULL) {
            jmp_tab[i] = def_lab;
            --holes;
        }
    }
    /* emit jump table */
    SET_SEGMENT(ROD_SEG, emitln);
    emitln("@jt%d:", jump_tables_counter++);
    for (i = 0; i < interval_size; i++)
        emitln("%%dword %s", jmp_tab[i]);
    SET_SEGMENT(TEXT_SEG, emitln);

    free(jmp_tab);
    arena_destroy(lab_arena);
    return;

linear_search:
    for (;; i++) {
        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        if (address(arg2).cont.val) {
            emit_jmp(address(arg1).cont.val);
            emitln("nop");
            break;
        }

        emitln("li $25, %d", (int)address(tar).cont.val);
        emit_beq(res, 25, address(arg1).cont.val);
        emitln("nop");
    }
}

static void mips_case(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void (*instruction_handlers[])(int, unsigned, unsigned, unsigned) = {
    mips_add, mips_sub, mips_mul, mips_div,
    mips_rem, mips_shl, mips_shr, mips_and,
    mips_or, mips_xor, mips_eq, mips_neq,
    mips_lt, mips_let, mips_gt, mips_get,

    mips_neg, mips_cmpl, mips_not, mips_ch,
    mips_uch, mips_sh, mips_ush, mips_llsx,
    mips_llzx, mips_addr_of, mips_ind, mips_asn,
    mips_call, mips_indcall,

    mips_ind_asn, mips_lab, mips_jmp, mips_arg,
    mips_ret, mips_switch, mips_case, mips_cbr,
    mips_begarg, mips_nop
};

static void mips_spill_reg_args(DeclList *p)
{
    int arg_nb;
    DeclList *tmp;

    if (get_type_spec(p->decl->decl_specs)->op==TOK_VOID && p->decl->idl==NULL) {
        if (big_return)
            emit_prologln("sw $4, 8($30)");
        return;
    }

    for (tmp = p; tmp != NULL; tmp = tmp->next)
        if (tmp->decl->idl!=NULL && tmp->decl->idl->op==TOK_ELLIPSIS)
            goto spill_all;

    arg_nb = big_return ? 4 : 0;
    while (p != NULL) {
        Declaration ty;

        ty.decl_specs = p->decl->decl_specs;
        ty.idl = p->decl->idl->child;
        arg_nb += round_up(get_sizeof(&ty), 4);
        if (arg_nb >= 16) {
            arg_nb = 16;
            break;
        }
        p = p->next;
    }
    switch (arg_nb) {
    case 4:
        emit_prologln("sw $4, 8($30)");
        break;
    case 8:
        emit_prologln("sw $4, 8($30)");
        emit_prologln("sw $5, 12($30)");
        break;
    case 12:
        emit_prologln("sw $4, 8($30)");
        emit_prologln("sw $5, 12($30)");
        emit_prologln("sw $6, 16($30)");
        break;
    case 16:
    spill_all:
        emit_prologln("sw $4, 8($30)");
        emit_prologln("sw $5, 12($30)");
        emit_prologln("sw $6, 16($30)");
        emit_prologln("sw $7, 20($30)");
        break;
    }
}

void mips_function_definition(TypeExp *decl_specs, TypeExp *header)
{
    Token cat;
    int i, last_i;
    unsigned fn, pos_tmp;
    TypeExp *scs;
    Declaration ty;
    static int first_func = TRUE;

    curr_func = header->str;
    fn = new_cg_node(curr_func);
    size_of_local_area = round_up(cg_node(fn).size_of_local_area, 4);

    ty.decl_specs = decl_specs;
    ty.idl = header->child->child;
    if ((cat=get_type_category(&ty))==TOK_STRUCT || cat==TOK_UNION)
        big_return = TRUE;
    else if (cat==TOK_LONG_LONG || cat==TOK_UNSIGNED_LONG_LONG)
        qword_return = TRUE;

    if (!first_func)
        emit_prolog("\n");
    emit_prologln("; ==== start of definition of function `%s' ====", curr_func);
    SET_SEGMENT(TEXT_SEG, emit_prologln);
    if ((scs=get_sto_class_spec(decl_specs))==NULL || scs->op!=TOK_STATIC)
        emit_prologln("%%global %s", curr_func);
    emit_prologln("%s:", curr_func);

    i = cfg_node(cg_node(fn).bb_i).leader;
    last_i = cfg_node(cg_node(fn).bb_f).last;
    for (; i <= last_i; i++) {
        unsigned tar, arg1, arg2;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        instruction_handlers[instruction(i).op](i, tar, arg1, arg2);
    }
    size_of_local_area -= round_up(temp_struct_size, 4);
    pos_tmp = string_get_pos(func_body);
    while (--calls_to_fix_counter >= 0) {
        int n;
        char *s;

        string_set_pos(func_body, calls_to_fix[calls_to_fix_counter]);
        s = string_curr(func_body);
        n = sprintf(s, "addu $25, $30, %d", size_of_local_area);
        s[n++] = ' ';
        for (; s[n] == 'X'; n++)
            s[n] = ' ';
    }
    string_set_pos(func_body, pos_tmp);

    size_of_local_area -= 8;
    emit_prologln("addu $29, $29, %d", size_of_local_area);
    emit_prologln("sw $31, %d($29)", -size_of_local_area-4);
    emit_prologln("sw $30, %d($29)", -size_of_local_area-8);
    emit_prologln("addu $30, $29, %d", -size_of_local_area-8);
    mips_spill_reg_args(header->child->attr.dl);

    emit_epilogln("move $29, $30");
    emit_epilogln("lw $30, 0($29)");
    emit_epilogln("lw $31, 4($29)");
    emit_epilogln("addu $29, $29, 8");
    emit_epilogln("jr $31");
    emit_epilogln("nop");

    string_write(func_prolog, mips_output_file);
    string_write(func_body, mips_output_file);
    string_write(func_epilog, mips_output_file);

    /* reset everything */
    string_clear(func_prolog);
    string_clear(func_body);
    string_clear(func_epilog);
    temp_struct_size = 0;
    calls_to_fix_counter = 0;
    /*memset(pinned, 0, sizeof(int)*MIPS_NREG);*/
    free_all_temps();
#if 1
    memset(addr_descr_tab, -1, nid_counter*sizeof(int));
    memset(reg_descr_tab, 0, sizeof(unsigned)*MIPS_NREG);
#endif
    big_return = qword_return = FALSE;
#if 1
    dump_addr_descr_tab();
    dump_reg_descr_tab();
#endif
    first_func = FALSE;
}

/*
 * Emit an expression valid for the assembler to evaluate.
 */
static long long mips_static_expr(ExecNode *e)
{
    switch (e->kind.exp) {
    case OpExp:
        switch (e->attr.op) {
        case TOK_SUBSCRIPT: {
            int pi, ii;
            Declaration ty;

            if (is_integer(get_type_category(&e->child[0]->type)))
                pi = 1, ii = 0;
            else
                pi = 0, ii = 1;
            ty = e->child[pi]->type;
            ty.idl = ty.idl->child;
            return mips_static_expr(e->child[pi])+get_sizeof(&ty)*mips_static_expr(e->child[ii]);
        }
        case TOK_DOT:
        case TOK_ARROW:
            if (get_type_category(&e->child[0]->type) != TOK_UNION) {
                StructMember *m;

                m = get_member_descriptor(get_type_spec(e->child[0]->type.decl_specs), e->child[1]->attr.str);
                return mips_static_expr(e->child[0])+m->offset;
            } else {
                return mips_static_expr(e->child[0]);
            }
        case TOK_ADDRESS_OF:
        case TOK_INDIRECTION:
        case TOK_CAST:
            return mips_static_expr(e->child[0]);

        case TOK_PLUS:
            if (is_integer(get_type_category(&e->type))) {
                return mips_static_expr(e->child[0])+mips_static_expr(e->child[1]);
            } else {
                int pi, ii;
                Declaration ty;

                if (is_integer(get_type_category(&e->child[0]->type)))
                    pi = 1, ii = 0;
                else
                    pi = 0, ii = 1;
                ty = e->child[pi]->type;
                ty.idl = ty.idl->child;
                return mips_static_expr(e->child[pi])+get_sizeof(&ty)*mips_static_expr(e->child[ii]);
            }
        case TOK_MINUS:
            if (is_integer(get_type_category(&e->child[0]->type))) { /* int-int */
                return mips_static_expr(e->child[0])-mips_static_expr(e->child[1]);
            } else { /* ptr-int */
                Declaration ty;

                ty = e->child[0]->type;
                ty.idl = ty.idl->child;
                return mips_static_expr(e->child[0])-get_sizeof(&ty)*mips_static_expr(e->child[1]);
            }
        case TOK_CONDITIONAL:
            if (e->child[0]->attr.val)
                return mips_static_expr(e->child[1]);
            else
                return mips_static_expr(e->child[2]);
        default:
            assert(0);
        }
    case IConstExp:
        return e->attr.val;
    case StrLitExp:
        emit_strln("_@S%d:", string_literals_counter);
        emit_raw_string(str_lits, e->attr.str);
        emit_decl("_@S%d+", string_literals_counter++);
        return 0;
    case IdExp:
        if (e->attr.var.linkage == LINKAGE_NONE)
            emit_decl("%s@%s+", enclosing_function, e->attr.str);
        else
            emit_decl("%s+", e->attr.str);
        return 0;
    }
}

void mips_static_init(TypeExp *ds, TypeExp *dct, ExecNode *e)
{
    TypeExp *ts;

    if (dct != NULL) {
        unsigned nelem;

        if (dct->op != TOK_SUBSCRIPT)
            goto scalar; /* pointer */

        /*
         * Array.
         */
        nelem = (unsigned)dct->attr.e->attr.uval;
        if (e->kind.exp == StrLitExp) { /* character array initialized by string literal */
            unsigned n;

            e->attr.str[nelem-1] = '\0';
            emit_raw_string(asm_decls, e->attr.str);
            if ((n=strlen(e->attr.str)+1) < nelem)
                emit_declln("%%zero %u", nelem-n);
        } else {
            /* handle elements with explicit initializer */
            for (e = e->child[0]; e!=NULL /*&& nelem!=0*/; e=e->sibling, --nelem)
                mips_static_init(ds, dct->child, e);

            /* handle elements without explicit initializer */
            if (nelem != 0) {
                Declaration ty;

                ty.decl_specs = ds;
                ty.idl = dct->child;
                emit_declln("%%align %u", get_alignment(&ty));
                emit_declln("%%zero %u", nelem*get_sizeof(&ty));
            }
        }
    } else if ((ts=get_type_spec(ds))->op == TOK_STRUCT) {
        /*
         * Struct.
         */
        DeclList *d;
        int full_init;

        e = e->child[0];

        /* handle members with explicit initializer */
        d = ts->attr.dl;
        full_init = FALSE;
        for (; d != NULL; d = d->next) {
            dct = d->decl->idl;
            for (; e!=NULL && dct!=NULL; e=e->sibling, dct=dct->sibling)
                mips_static_init(d->decl->decl_specs, dct->child, e);

            if (e == NULL) {
                if (dct==NULL && d->next==NULL)
                    full_init = TRUE;
                break;
            }
        }

        /* handle members without explicit initializer */
        if (!full_init) {
            if (dct == NULL) {
                d = d->next;
                dct = d->decl->idl;
            }
            while (TRUE) {
                while (dct != NULL) {
                    Declaration ty;

                    ty.decl_specs = d->decl->decl_specs;
                    ty.idl = dct->child;
                    emit_declln("%%align %u", get_alignment(&ty));
                    emit_declln("%%zero %u",  get_sizeof(&ty));

                    dct = dct->sibling;
                }
                d = d->next;
                if (d != NULL)
                    dct = d->decl->idl;
                else
                    break;
            }
        }
    } else if (ts->op == TOK_UNION) {
        /*
         * Union.
         */
        e = e->child[0];

        /* initialize the first named member */
        mips_static_init(ts->attr.dl->decl->decl_specs, ts->attr.dl->decl->idl->child, e);
    } else {
        /*
         * Scalar.
         */
        Declaration dest_ty;
scalar:
        if (e->kind.exp==OpExp && e->attr.op==TOK_INIT_LIST)
            e = e->child[0];
        dest_ty.decl_specs = ds;
        dest_ty.idl = dct;
        switch (get_type_category(&dest_ty)) {
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR:
            emit_decl("%%byte ");
            break;
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            emit_declln("%%align 2");
            emit_decl("%%word ");
            break;
        case TOK_LONG_LONG:
        case TOK_UNSIGNED_LONG_LONG: {
            long long v;

            emit_declln("%%align 4");
            emit_decl("%%dword ");
            v = mips_static_expr(e);
            emit_declln("%u", ((unsigned *)&v)[0]);
            emit_decl("%%dword ");
            emit_declln("%u", ((unsigned *)&v)[1]);
        }
            return;
        default:
            emit_declln("%%align 4");
            emit_decl("%%dword ");
            break;
        }
        emit_declln("%u", (unsigned)mips_static_expr(e));
    }
}

void mips_allocate_static_objects(void)
{
    ExternId *np;

    for (np = static_objects_list; np != NULL; np = np->next) {
        unsigned al;
        Declaration ty;
        ExecNode *initzr;

        ty.decl_specs = np->decl_specs;
        ty.idl = np->declarator->child;
        initzr = np->declarator->attr.e;

        al = get_alignment(&ty);
        if (initzr != NULL) {
            SET_SEGMENT(DATA_SEG, emit_declln);
            if (al > 1)
                emit_declln("%%align %u", al);
        } else {
            SET_SEGMENT(BSS_SEG, emit_declln);
            if (al > 1)
                emit_declln("%%alignb %u", al);
        }
        if ((enclosing_function=np->enclosing_function) != NULL) { /* static local */
            emit_declln("%s@%s:", np->enclosing_function, np->declarator->str);
        } else {
            TypeExp *scs;

            if ((scs=get_sto_class_spec(np->decl_specs))==NULL || scs->op!=TOK_STATIC)
                emit_declln("%%global %s", np->declarator->str);
            emit_declln("%s:", np->declarator->str);
        }
        if (initzr != NULL)
            mips_static_init(ty.decl_specs, ty.idl, initzr);
        else
            emit_declln("%%res %u", get_sizeof(&ty));
    }
}

void mips_cgen(FILE *outf)
{
    unsigned i, j;
    ExternId *ed, **func_def_list, **ext_sym_list;

    mips_output_file = outf;

    ic_main(&func_def_list, &ext_sym_list);
    compute_liveness_and_next_use();

    asm_decls = string_new(512);
    str_lits = string_new(512);
    func_body = string_new(1024);
    func_prolog = string_new(1024);
    func_epilog = string_new(1024);
    addr_descr_tab = malloc(nid_counter*sizeof(MIPS_Reg2));
    memset(addr_descr_tab, -1, nid_counter*sizeof(MIPS_Reg2));
    for (i = 0; (ed=func_def_list[i]) != NULL; i++)
        mips_function_definition(ed->decl_specs, ed->declarator);
    string_free(func_body);
    string_free(func_prolog);
    string_free(func_epilog);

    emit_declln("\n; == objects with static duration");
    mips_allocate_static_objects();

    emit_declln("\n; == extern symbols");
    /* emit extern directives only for those symbols that were referenced */
    for (j = 0; (ed=ext_sym_list[j]) != NULL; j++) {
        int tmp;

        /* get_var_nid() will increment nid_counter when it sees a new identifier */
        tmp = nid_counter;
        get_var_nid(ed->declarator->str, 0);
        if (tmp == nid_counter)
            emit_declln("%%extern %s", ed->declarator->str);
    }
    /* the front-end may emit calls to memcpy/memset */
    emit_declln("%%extern memcpy");
    emit_declln("%%extern memset");
    emit_declln("%%extern __lux_mips_memcpy");
    /* liblux functions */
    if (include_liblux) {
        emit_declln("%%extern __lux_mul64");
        emit_declln("%%extern __lux_sdiv64");
        emit_declln("%%extern __lux_udiv64");
        emit_declln("%%extern __lux_smod64");
        emit_declln("%%extern __lux_umod64");
        emit_declln("%%extern __lux_shl64");
        emit_declln("%%extern __lux_sshr64");
        emit_declln("%%extern __lux_ushr64");
        emit_declln("%%extern __lux_ucmp64");
        emit_declln("%%extern __lux_scmp64");
    }

    string_write(asm_decls, mips_output_file);
    string_free(asm_decls);

    if (string_literals_counter) {
        fprintf(mips_output_file, "\n; == string literals\n");
        fprintf(mips_output_file, "%%segment .rodata\n");
        string_write(str_lits, mips_output_file);
    }
    string_free(str_lits);
}
