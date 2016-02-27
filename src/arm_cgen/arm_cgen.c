/*
 * Simple ARM code generator
 *      IC ==> ARM ASM (to be assembled with luxasarm)
 */
#define DEBUG 0
#include "arm_cgen.h"
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

#define ARM_NREG    16
#define SCRATCH_REG 12
typedef int bool;
typedef short ARM_Reg;
typedef struct { ARM_Reg r1, r2; } ARM_Reg2;

static int pinned[ARM_NREG] = {
    1, 1, 1, 1, /* a1,  a2,  a3,  a4 */
    0, 0, 0, 0, /* v1,  v2,  v3,  v4 */
    0, 0, 0, 1, /* v5,  v6,  v7,  fp */
    1, 1, 0, 1, /* r12, sp,  lr,  pc */
};

static int modified[ARM_NREG];
#define pin_reg(r)    (pinned[r] = TRUE)
#define pin_reg2(r)   (pinned[r.r1] = TRUE, pinned[r.r2] = TRUE)
#define unpin_reg(r)  (pinned[r] = FALSE)
#define unpin_reg2(r) (pinned[r.r1] = FALSE, pinned[r.r2] = FALSE)

static int size_of_local_area;
static char *curr_func, *enclosing_function;
static unsigned temp_struct_size;
static int struct_union_return, qword_return;
static int calls_to_fix_counter;
static unsigned calls_to_fix[64];
static int retvals_to_fix_counter;
static unsigned retvals_to_fix[64];
static int string_literals_counter;
static FILE *arm_output_file;
static int func_last_quad;
static int arg_offs, max_arg_offs;

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
            f("%s", str_segment[seg]);\
            curr_segment = seg;\
        }\
    } while(0)

static int new_string_literal(unsigned a);
static void emit_raw_string(String *q, char *s);
static String *func_body, *func_prolog, *func_epilog, *asm_decls, *str_lits;

/*
 * Maximum pc relative offset used to load a literal from the literal pool.
 * Assume that every instruction that is emitted needs to load a literal (the worst case).
 * It is set to this value so we have 2048 bytes to reach the start of the pool
 * and 2048 bytes to address a literal within it.
 * XXX: doesn't have into account that pc is actually pc+8 when referenced.
 */
#define MAX_OFFS_TO_LT 2048
static int offs_to_lit_tab, code_cont_counter;

static void dump_and_link(void)
{
    string_printf(func_body, "b __code_cont@%d\n", code_cont_counter);
    string_printf(func_body, ".ltorg\n");
    string_printf(func_body, "__code_cont@%d:\n", code_cont_counter);
    offs_to_lit_tab = 0;
    ++code_cont_counter;
}

static void emit_code(int nl, char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    string_vprintf(func_body, fmt, ap);
    va_end(ap);
    if (nl)
        string_printf(func_body, "\n");

    offs_to_lit_tab += 4;
    if (offs_to_lit_tab >= MAX_OFFS_TO_LT)
        dump_and_link();
}

#define emit(...)           (emit_code(0, __VA_ARGS__))
#define emitln(...)         (emit_code(1, __VA_ARGS__))
#define emit_prolog(...)    (string_printf(func_prolog, __VA_ARGS__))
#define emit_prologln(...)  (string_printf(func_prolog, __VA_ARGS__), string_printf(func_prolog, "\n"))
#define emit_epilog(...)    (string_printf(func_epilog, __VA_ARGS__))
#define emit_epilogln(...)  (string_printf(func_epilog, __VA_ARGS__), string_printf(func_epilog, "\n"))
#define emit_decl(...)      (string_printf(asm_decls, __VA_ARGS__))
#define emit_declln(...)    (string_printf(asm_decls, __VA_ARGS__), string_printf(asm_decls, "\n"))
#define emit_str(...)       (string_printf(str_lits, __VA_ARGS__))
#define emit_strln(...)     (string_printf(str_lits, __VA_ARGS__), string_printf(str_lits, "\n"))

static ARM_Reg2 *addr_descr_tab;
static unsigned reg_descr_tab[ARM_NREG];
#define addr_reg(a)    (addr_descr_tab[address_nid(a)])
#define addr_reg1(a)   (addr_descr_tab[address_nid(a)].r1)
#define addr_reg2(a)   (addr_descr_tab[address_nid(a)].r2)
#define reg_isempty(r) (reg_descr_tab[r] == 0)
static void dump_addr_descr_tab(void);
static void dump_reg_descr_tab(void);

static void spill_reg(ARM_Reg r);
static void spill_all(void);
static void spill_aliased_objects(void);
static ARM_Reg get_reg0(void);
static ARM_Reg get_reg(int i);
static ARM_Reg2 get_reg2(int i);

static void arm_load(ARM_Reg r, unsigned a);
static void arm_load2(ARM_Reg2 r, unsigned a);
static void arm_load_addr(ARM_Reg r, unsigned a);
static char *arm_get_operand(unsigned a, int imm);
static char **arm_get_operand2(unsigned a);
static void arm_store(ARM_Reg r, unsigned a);
static void arm_store2(ARM_Reg2 r, unsigned a);
static void arm_function_definition(TypeExp *decl_specs, TypeExp *header);

static long long arm_static_expr(ExecNode *e);
static void arm_static_init(TypeExp *ds, TypeExp *dct, ExecNode *e);
static void arm_allocate_static_objects(void);

typedef struct Temp Temp;
static struct Temp {
    int nid;
    int offs;
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
        string_printf(q, ".dword #0x%02x%02x%02x%02x\n", s[3], s[2], s[1], s[0]);
        s += 4;
    }
    for (i = len%4; i; i--)
        string_printf(q, ".byte #0x%02x\n", *s++);
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
        ARM_Reg2 ad;

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

    for (i = 0; i < ARM_NREG; i++)
        if (!reg_isempty(i))
            fprintf(stderr, "r%d => %s\n", i, address_sid(reg_descr_tab[i]));
}

void spill_reg(ARM_Reg r)
{
    unsigned a;

    if (reg_isempty(r))
        return;

    a = reg_descr_tab[r];
    if (addr_reg2(a) != -1) { /* spill the register pair */
        ARM_Reg2 rr;

        rr = addr_reg(a);
        arm_store2(rr, a);
        addr_reg1(a) = addr_reg2(a) = -1;
        reg_descr_tab[rr.r1] = reg_descr_tab[rr.r2] = 0;
    } else {
        arm_store(r, a);
        addr_reg1(a) = -1;
        reg_descr_tab[r] = 0;
    }
}

void spill_all(void)
{
    int i;

    for (i = 0; i < ARM_NREG; i++)
        spill_reg((ARM_Reg)i);
}

void spill_aliased_objects(void)
{
    int i;

    for (i = 0; i < ARM_NREG; i++) {
        unsigned a;

        if (reg_isempty(i))
            continue;
        a = reg_descr_tab[i];
        if (address(a).kind==IdKind && bset_member(address_taken_variables, address_nid(a)))
            spill_reg((ARM_Reg)i);
    }
}

ARM_Reg get_reg0(void)
{
    int i;

    for (i = 0; i < ARM_NREG; i++) {
        if (!pinned[i] && reg_isempty(i)) {
            modified[i] = TRUE;
            assert(i > 3);
            return (ARM_Reg)i;
        }
    }

    for (i = 0; i < ARM_NREG; i++) {
        if (!pinned[i]) {
            assert(modified[i]);
            assert(i > 3);
            spill_reg((ARM_Reg)i);
            return (ARM_Reg)i;
        }
    }

    assert(0);
    return 0;
}

ARM_Reg get_reg(int i)
{
    unsigned arg1;

    arg1 = instruction(i).arg1;
    if (!const_addr(arg1) && addr_reg1(arg1)!=-1 && !arg1_liveness(i))
        return addr_reg1(arg1);
    return get_reg0();
}

ARM_Reg2 get_reg2(int i)
{
    ARM_Reg2 r;
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
    if (r.r1 > r.r2) {
        ARM_Reg tmp;

        tmp = r.r1;
        r.r1 = r.r2;
        r.r2 = tmp;
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

void arm_load(ARM_Reg r, unsigned a)
{
    int offs;

    if (address(a).kind == IConstKind) {
        emitln("ldr r%d, =#%u", r, (unsigned)address(a).cont.uval);
    } else if (address(a).kind == StrLitKind) {
        emitln("ldr r%d, =_@S%d", r, new_string_literal(a));
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *ld_str;
        int max_offs;

        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) == r)
                ; /* already in the register */
            else
                emitln("mov r%d, r%d", r, addr_reg1(a));
            return;
        }

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
        case TOK_SUBSCRIPT:
        case TOK_FUNCTION:
            arm_load_addr(r, a);
            return;
        case TOK_SHORT:
            ld_str = "ldrsh";
            max_offs = 255;
            break;
        case TOK_UNSIGNED_SHORT:
            ld_str = "ldrh";
            max_offs = 255;
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
            ld_str = "ldrsb";
            max_offs = 255;
            break;
        case TOK_UNSIGNED_CHAR:
            ld_str = "ldrb";
            max_offs = 4095;
            break;
        default:
            ld_str = "ldr";
            max_offs = 4095;
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) { /* static local */
                emitln("ldr r%d, =%s@%s", r, curr_func, e->attr.str);
                emitln("%s r%d, [r%d]", ld_str, r, r);
            } else { /* global */
                emitln("ldr r%d, =%s", r, e->attr.str);
                emitln("%s r%d, [r%d]", ld_str, r, r);
            }
        } else { /* parameter or local */
            if ((offs=local_offset(a)) < 0) {
                if (offs >= -max_offs) {
                    emitln("%s r%d, [r11, -#%d]", ld_str, r, -offs);
                    return;
                }
            } else {
                if (offs <= max_offs) {
                    emitln("%s r%d, [r11, #%d]", ld_str, r, offs);
                    return;
                }
            }
            /* offset out of range */
            emitln("ldr r%d, =#%u", SCRATCH_REG, offs);
            emitln("%s r%d, [r11, r%d]", ld_str, r, SCRATCH_REG);
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) == r)
                return; /* already in the register */
            else
                emitln("mov r%d, r%d", r, addr_reg1(a));
        } else {
            if ((offs=get_temp_offs(a)) >= -4095) {
                emitln("ldr r%d, [r11, -#%d]", r, -offs);
            } else {
                emitln("ldr r%d, =#%u", SCRATCH_REG, offs);
                emitln("ldr r%d, [r11, r%d]", r, SCRATCH_REG);
            }
        }
    }
}

void arm_load2(ARM_Reg2 r, unsigned a)
{
    int offs;

    if (address(a).kind == IConstKind) {
        unsigned *p;

        p = (unsigned *)&address(a).cont.uval;
        emitln("ldr r%d, =#%u", r.r1, p[0]);
        emitln("ldr r%d, =#%u", r.r2, p[1]);
    } else if (address(a).kind == StrLitKind) {
        emitln("ldr r%d, =_@S%d", r.r1, new_string_literal(a));
        emitln("mov r%d, #0", r.r2);
    } else if (address(a).kind == IdKind) {
        Token cat;
        ExecNode *e;

        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) != r.r1) {
                assert(addr_reg2(a) != r.r2);
                emitln("mov r%d, r%d", r.r1, addr_reg1(a));
                emitln("mov r%d, r%d", r.r2, addr_reg2(a));
            } else {
                assert(addr_reg2(a) == r.r2);
            }
            return;
        }

        e = address(a).cont.var.e;
        cat = get_type_category(&e->type);
        assert(cat==TOK_LONG_LONG || cat==TOK_UNSIGNED_LONG_LONG);

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE)
                emitln("ldr r%d, =%s@%s", SCRATCH_REG, curr_func, e->attr.str);
            else
                emitln("ldr r%d, =%s", SCRATCH_REG, e->attr.str);
            emitln("ldmia r%d, {r%d, r%d}", SCRATCH_REG, r.r1, r.r2);
        } else {
            if ((offs=local_offset(a)) < 0) {
                if (offs >= -4095) {
                    emitln("ldr r%d, [r11, -#%d]", r.r1, -offs);
                    emitln("ldr r%d, [r11, -#%d]", r.r2, -(offs+4));
                    return;
                }
            } else {
                if (offs+4 <= 4095) {
                    emitln("ldr r%d, [r11, #%d]", r.r1, offs);
                    emitln("ldr r%d, [r11, #%d]", r.r2, offs+4);
                    return;
                }
            }
            goto large_offset;
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) != r.r1)
                emitln("mov r%d, r%d", r.r1, addr_reg1(a));
            assert(addr_reg2(a) != -1);
            if (addr_reg2(a) != r.r2)
                emitln("mov r%d, r%d", r.r2, addr_reg2(a));
        } else {
            int offs;

            if ((offs=get_temp_offs(a)) >= -4095) {
                emitln("ldr r%d, [r11, -#%d]", r.r1, -offs);
                emitln("ldr r%d, [r11, -#%d]", r.r2, -(offs+4));
            } else {
                goto large_offset;
            }
        }
    }
    return;
large_offset:
    emitln("ldr r%d, =#%u", SCRATCH_REG, offs);
    emitln("add r%d, r%d, r11", SCRATCH_REG, SCRATCH_REG);
    emitln("ldmia r%d, {r%d, r%d}", SCRATCH_REG, r.r1, r.r2);
}

static bool is_representable_in_imm12(unsigned n)
{
    static unsigned a[16] = { /* rot 0..15 */
        0x000000FF, 0xC000003F,
        0xF000000F, 0xFC000003,
        0xFF000000, 0x3FC00000,
        0x0FF00000, 0x03FC0000,
        0x00FF0000, 0x003FC000,
        0x000FF000, 0x0003FC00,
        0x0000FF00, 0x00003FC0,
        0x00000FF0, 0x000003F0,
    };
    int rot;

    for (rot = 0; rot < 16; rot++)
        if (!(n & ~a[rot]))
            return TRUE;
    return FALSE;
}

char *arm_get_operand(unsigned a, int imm)
{
    ARM_Reg r;
    static char op[256];

    if (imm && address(a).kind==IConstKind) {
        if (is_representable_in_imm12((unsigned)address(a).cont.uval)) {
            sprintf(op, "#%u", (unsigned)address(a).cont.uval);
            return op;
        }
    } else if (address(a).kind==IdKind || address(a).kind==TempKind) {
        if (addr_reg1(a) != -1) {
            sprintf(op, "r%d", addr_reg1(a));
            return op;
        }
    }
    r = get_reg0();
    arm_load(r, a);
    sprintf(op, "r%d", r);
    return op;
}

char **arm_get_operand2(unsigned a)
{
    ARM_Reg2 r;
    static char op1[256], op2[256];
    static char *op[2] = { op1, op2 };

    if ((address(a).kind==IdKind || address(a).kind==TempKind) && addr_reg1(a)!=-1) {
        assert(addr_reg2(a) != -1);
        sprintf(op1, "r%d", addr_reg1(a));
        sprintf(op2, "r%d", addr_reg2(a));
        return op;
    }
    r.r1 = get_reg0();
    pin_reg(r.r1);
    r.r2 = get_reg0();
    unpin_reg(r.r1);
    arm_load2(r, a);
    sprintf(op1, "r%d", r.r1);
    sprintf(op2, "r%d", r.r2);
    return op;
}

void arm_load_addr(ARM_Reg r, unsigned a)
{
    ExecNode *e;

    e = address(a).cont.var.e;
    if (e->attr.var.duration == DURATION_STATIC) {
        if (e->attr.var.linkage == LINKAGE_NONE)
            emitln("ldr r%d, =%s@%s", r, curr_func, e->attr.str);
        else
            emitln("ldr r%d, =%s", r, e->attr.str);
    } else {
        int offs;

        offs = local_offset(a);
        if (is_representable_in_imm12(offs)) {
            emitln("add r%d, r11, #%u", r, offs);
        } else {
            emitln("ldr r%d, =#%u", SCRATCH_REG, offs);
            emitln("add r%d, r11, r%d", r, SCRATCH_REG);
        }
    }
}

void arm_store(ARM_Reg r, unsigned a)
{
    int offs;

    if (address(a).kind == IdKind) {
        ExecNode *e;
        char *st_str;
        int max_offs;

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
            emitln("sub r13, r13, #12");
            arm_load_addr(SCRATCH_REG, a);
            emitln("str r%d, [r13]", SCRATCH_REG);
            emitln("str r%d, [r13, #4]", r);
            emitln("ldr r%d, =#%u", SCRATCH_REG, get_sizeof(&e->type));
            emitln("str r%d, [r13, #8]", SCRATCH_REG);
            emitln("bl __lux_arm_memcpy");
            return;
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            st_str = "strh";
            max_offs = 255;
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR:
            st_str = "strb";
            max_offs = 4095;
            break;
        default:
            st_str = "str";
            max_offs = 4095;
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) { /* static local */
                emitln("ldr r%d, =%s@%s", SCRATCH_REG, curr_func, e->attr.str);
                emitln("%s r%d, [r%d]", st_str, r, SCRATCH_REG);
            } else { /* global */
                emitln("ldr r%d, =%s", SCRATCH_REG, e->attr.str);
                emitln("%s r%d, [r%d]", st_str, r, SCRATCH_REG);
            }
        } else { /* parameter or local */
            if ((offs=local_offset(a)) < 0) {
                if (offs >= -max_offs) {
                    emitln("%s r%d, [r11, -#%d]", st_str, r, -offs);
                    return;
                }
            } else {
                if (offs <= max_offs) {
                    emitln("%s r%d, [r11, #%d]", st_str, r, offs);
                    return;
                }
            }
            /* offset out of range */
            emitln("ldr r%d, =#%u", SCRATCH_REG, offs);
            emitln("%s r%d, [r11, r%d]", st_str, r, SCRATCH_REG);
        }
    } else if (address(a).kind == TempKind) {
        if ((offs=get_temp_offs(a)) >= -4095) {
            emitln("str r%d, [r11, -#%d]", r, -offs);
        } else {
            emitln("ldr r%d, =#%u", SCRATCH_REG, offs);
            emitln("str r%d, [r11, r%d]", r, SCRATCH_REG);
        }
    }
}

void arm_store2(ARM_Reg2 r, unsigned a)
{
    int offs;

    if (address(a).kind == IdKind) {
        ExecNode *e;

        e = address(a).cont.var.e;
        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE)
                emitln("ldr r%d, =%s@%s", SCRATCH_REG, curr_func, e->attr.str);
            else
                emitln("ldr r%d, =%s", SCRATCH_REG, e->attr.str);
            emitln("stmia r%d, {r%d, r%d}", SCRATCH_REG, r.r1, r.r2);
        } else {
            if ((offs=local_offset(a)) < 0) {
                if (offs >= -4095) {
                    emitln("str r%d, [r11, -#%d]", r.r1, -offs);
                    emitln("str r%d, [r11, -#%d]", r.r2, -(offs+4));
                    return;
                }
            } else {
                if (offs+4 <= 4095) {
                    emitln("str r%d, [r11, #%d]", r.r1, offs);
                    emitln("str r%d, [r11, #%d]", r.r2, offs+4);
                    return;
                }
            }
            goto large_offset;
        }
    } else if (address(a).kind == TempKind) {
        int offs;

        if ((offs=get_temp_offs(a)) >= -4095) {
            emitln("str r%d, [r11, -#%d]", r.r1, -offs);
            emitln("str r%d, [r11, -#%d]", r.r2, -(offs+4));
        } else {
            goto large_offset;
        }
    }
    return;
large_offset:
    emitln("ldr r%d, =#%u", SCRATCH_REG, offs);
    emitln("add r%d, r%d, r11", SCRATCH_REG, SCRATCH_REG);
    emitln("stmia r%d, {r%d, r%d}", SCRATCH_REG, r.r1, r.r2);
}

static void update_arg_descriptors(unsigned arg, unsigned char liveness, int next_use)
{
    if (const_addr(arg) || next_use)
        return;

    if (addr_reg1(arg) != -1) {
        if (liveness) { /* spill */
            if (addr_reg2(arg) != -1)
                arm_store2(addr_reg(arg), arg);
            else
                arm_store(addr_reg1(arg), arg);
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

static void update_tar_descriptors(ARM_Reg res, unsigned tar, unsigned char liveness, int next_use)
{
    /* Note:
        maintain the order of the operations for arm_store()
        to work correctly with struct operands.
    */

    addr_reg1(tar) = res;
    reg_descr_tab[res] = tar;

    if (!next_use) {
        if (liveness) /* spill */
            arm_store(res, tar);
        addr_reg1(tar) = -1;
        reg_descr_tab[res] = 0;
    }
}

static void update_tar_descriptors2(ARM_Reg2 res, unsigned tar, unsigned char liveness, int next_use)
{
    addr_reg1(tar) = res.r1;
    addr_reg2(tar) = res.r2;
    reg_descr_tab[res.r1] = tar;
    reg_descr_tab[res.r2] = tar;

    if (!next_use) {
        if (liveness) /* spill */
            arm_store2(res, tar);
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

#define emit_lab(n)         emitln("%s@L%d:", curr_func, (int)n)
#define emit_jmp(target)    emitln("b %s@L%d", curr_func, (int)target)

#define emit_beq(target)    emitln("beq %s@L%d", curr_func, (int)target)
#define emit_bne(target)    emitln("bne %s@L%d", curr_func, (int)target)

#define emit_blt(target)    emitln("blt %s@L%d", curr_func, (int)target)
#define emit_bgt(target)    emitln("bgt %s@L%d", curr_func, (int)target)
#define emit_blo(target)    emitln("blo %s@L%d", curr_func, (int)target)
#define emit_bhi(target)    emitln("bhi %s@L%d", curr_func, (int)target)

#define emit_ble(target)    emitln("ble %s@L%d", curr_func, (int)target)
#define emit_bge(target)    emitln("bge %s@L%d", curr_func, (int)target)
#define emit_bls(target)    emitln("bls %s@L%d", curr_func, (int)target)
#define emit_bhs(target)    emitln("bhs %s@L%d", curr_func, (int)target)

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

static void arm_save_arg_regs(void)
{
    switch (arg_offs / 4) {
    case 0:
        break;
    case 1:
        emitln("str r0, [r13, -#4]!");
        break;
    case 2:
        emitln("stmdb r13!, {r0, r1}");
        break;
    case 3:
        emitln("stmdb r13!, {r0, r1, r2}");
        break;
    default:
        emitln("stmdb r13!, {r0, r1, r2, r3}");
        break;
    }
}

static void arm_restore_arg_regs(void)
{
    switch (arg_offs / 4) {
    case 0:
        break;
    case 1:
        emitln("ldr r0, [r13], #4");
        break;
    case 2:
        emitln("ldmia r13!, {r0, r1}");
        break;
    case 3:
        emitln("ldmia r13!, {r0, r1, r2}");
        break;
    default:
        emitln("ldmia r13!, {r0, r1, r2, r3}");
        break;
    }
}

static void arm_do_libcall(int i, unsigned tar, unsigned arg1, unsigned arg2, int func)
{
    ARM_Reg2 res = { 4, 5 }, r1 = { 0, 1 }, r2 = { 2, 3 };

    switch (func) {
    case LibMul:
    case LibSDiv:
    case LibUDiv:
    case LibSMod:
    case LibUMod:
        arm_save_arg_regs();
        arm_load2(r1, arg1);
        arm_load2(r2, arg2);
        spill_all();
        emitln("bl %s", libfuncs[func]);
        emitln("mov r4, r0");
        emitln("mov r5, r1");
        arm_restore_arg_regs();
        UPDATE_ADDRESSES2(res);
        break;

    case LibShL:
    case LibSShR:
    case LibUShR:
        arm_save_arg_regs();
        arm_load2(r1, arg1);
        arm_load(2, arg2);
        spill_all();
        emitln("bl %s", libfuncs[func]);
        emitln("mov r4, r0");
        emitln("mov r5, r1");
        arm_restore_arg_regs();
        UPDATE_ADDRESSES2(res);
        break;

    case LibNot:
        arm_save_arg_regs();
        arm_load2(r1, arg1);
        spill_all();
        emitln("mov r2, #0");
        emitln("mov r3, #0");
        emitln("bl __lux_ucmp64");
        emitln("cmp r0, #1");
        emitln("moveq r4, #1");
        emitln("movne r4, #0");
        arm_restore_arg_regs();
        UPDATE_ADDRESSES_UNARY(4);
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
        arm_save_arg_regs();
        arm_load2(r1, arg1);
        arm_load2(r2, arg2);
        spill_all();
        emitln("bl %s", libfuncs[func]);
        switch (func) {
        case LibEq:
            emitln("cmp r0, #1");
            emitln("moveq r4, #1");
            emitln("movne r4, #0");
            break;
        case LibNeq:
            emitln("cmp r0, #1");
            emitln("movne r4, #1");
            emitln("moveq r4, #0");
            break;
        case LibULT:
        case LibSLT:
            emitln("cmp r0, #4");
            emitln("moveq r4, #1");
            emitln("movne r4, #0");
            break;
        case LibUGT:
        case LibSGT:
            emitln("cmp r0, #2");
            emitln("moveq r4, #1");
            emitln("movne r4, #0");
            break;
        case LibUGET:
        case LibSGET:
            emitln("cmp r0, #4");
            emitln("movne r4, #1");
            emitln("moveq r4, #0");
            break;
        case LibULET:
        case LibSLET:
            emitln("cmp r0, #2");
            emitln("movne r4, #1");
            emitln("moveq r4, #0");
            break;
        }
        arm_restore_arg_regs();
        UPDATE_ADDRESSES(4);
        break;
    }
}

static void arm_add(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        ARM_Reg2 res;

        res = get_reg2(i);
        arm_load2(res, arg1);
        pin_reg2(res);
        op = arm_get_operand2(arg2);
        emitln("adds r%d, r%d, %s", res.r1, res.r1, op[0]);
        emitln("adc r%d, r%d, %s", res.r2, res.r2, op[1]);
        unpin_reg2(res);
        UPDATE_ADDRESSES2(res);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        pin_reg(res);
        emitln("add r%d, r%d, %s", res, res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void arm_sub(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        ARM_Reg2 res;

        res = get_reg2(i);
        arm_load2(res, arg1);
        pin_reg2(res);
        op = arm_get_operand2(arg2);
        emitln("subs r%d, r%d, %s", res.r1, res.r1, op[0]);
        emitln("sbc r%d, r%d, %s", res.r2, res.r2, op[1]);
        unpin_reg2(res);
        UPDATE_ADDRESSES2(res);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        pin_reg(res);
        emitln("sub r%d, r%d, %s", res, res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void arm_mul(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        arm_do_libcall(i, tar, arg1, arg2, LibMul);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        pin_reg(res);
        emitln("mul r%d, r%d, %s", res, res, arm_get_operand(arg2, 0));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void arm_div(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        arm_do_libcall(i, tar, arg1, arg2, is_signed_int(cat)?LibSDiv:LibUDiv);
    } else {
        ARM_Reg res;

        res = 4;
        arm_save_arg_regs();
        arm_load(0, arg1);
        arm_load(1, arg2);
        spill_all();
        emitln("bl %s", is_unsigned_int(cat)?"__lux_udiv32":"__lux_sdiv32");
        emitln("mov r%d, r0", res);
        arm_restore_arg_regs();
        UPDATE_ADDRESSES(res);
    }
}

static void arm_rem(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        arm_do_libcall(i, tar, arg1, arg2, is_signed_int(cat)?LibSMod:LibUMod);
    } else {
        ARM_Reg res;

        res = 4;
        arm_save_arg_regs();
        arm_load(0, arg1);
        arm_load(1, arg2);
        spill_all();
        emitln("bl %s", is_unsigned_int(cat)?"__lux_umod32":"__lux_smod32");
        emitln("mov r%d, r0", res);
        arm_restore_arg_regs();
        UPDATE_ADDRESSES(res);
    }
}

static void arm_shl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        arm_do_libcall(i, tar, arg1, arg2, LibShL);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        pin_reg(res);
        emitln("mov r%d, r%d, LSL %s", res, res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void arm_shr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        arm_do_libcall(i, tar, arg1, arg2, is_signed_int(cat)?LibSShR:LibUShR);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        pin_reg(res);
        emitln("mov r%d, r%d, %s %s", res, res, is_unsigned_int(cat)?"LSR":"ASR", arm_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void arm_and(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        ARM_Reg2 res;

        res = get_reg2(i);
        arm_load2(res, arg1);
        pin_reg2(res);
        op = arm_get_operand2(arg2);
        emitln("and r%d, r%d, %s", res.r1, res.r1, op[0]);
        emitln("and r%d, r%d, %s", res.r2, res.r2, op[1]);
        unpin_reg2(res);
        UPDATE_ADDRESSES2(res);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        pin_reg(res);
        emitln("and r%d, r%d, %s", res, res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void arm_or(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        ARM_Reg2 res;

        res = get_reg2(i);
        arm_load2(res, arg1);
        pin_reg2(res);
        op = arm_get_operand2(arg2);
        emitln("orr r%d, r%d, %s", res.r1, res.r1, op[0]);
        emitln("orr r%d, r%d, %s", res.r2, res.r2, op[1]);
        unpin_reg2(res);
        UPDATE_ADDRESSES2(res);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        pin_reg(res);
        emitln("orr r%d, r%d, %s", res, res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void arm_xor(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        ARM_Reg2 res;

        res = get_reg2(i);
        arm_load2(res, arg1);
        pin_reg2(res);
        op = arm_get_operand2(arg2);
        emitln("eor r%d, r%d, %s", res.r1, res.r1, op[0]);
        emitln("eor r%d, r%d, %s", res.r2, res.r2, op[1]);
        unpin_reg2(res);
        UPDATE_ADDRESSES2(res);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        pin_reg(res);
        emitln("eor r%d, r%d, %s", res, res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void do_relop_jump(int i, long flags, unsigned tar, unsigned arg1, unsigned arg2)
{
    int nid, j;

    /* do any spilling before the jump */
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    update_arg_descriptors(arg2, arg2_liveness(i), arg2_next_use(i));

    /*
     * Search the OpCBr that uses the result of the relational operator
     * and replace it for OpNOp. Before that, do the conditional jump
     * that quad is supposed to do.
     */
    nid = address(tar).cont.nid;
    for (j = i+1; ; j++) {
        int lab;
        unsigned tar_2, tar_3, arg2_2;

        assert(j <= func_last_quad);
        if (instruction(j).op!=OpCBr || address_nid(instruction(j).arg1)!=nid)
            continue;

        tar_2 = instruction(j).tar;
        tar_3 = instruction(j+1).tar;
        arg2_2 = instruction(j).arg2;
        if (address(tar_2).cont.val == address(tar_3).cont.val) {
            lab = (int)address(arg2_2).cont.val;
            switch (instruction(i).op) {
            case OpEQ:
                emit_bne(lab);
                break;
            case OpNEQ:
                emit_beq(lab);
                break;
            case OpLT:
                if (flags & IC_SIGNED)
                    emit_bge(lab);
                else
                    emit_bhs(lab);
                break;
            case OpLET:
                if (flags & IC_SIGNED)
                    emit_bgt(lab);
                else
                    emit_bhi(lab);
                break;
            case OpGT:
                if (flags & IC_SIGNED)
                    emit_ble(lab);
                else
                    emit_bls(lab);
                break;
            case OpGET:
                if (flags & IC_SIGNED)
                    emit_blt(lab);
                else
                    emit_blo(lab);
                break;
            }
        } else {
            lab = (int)address(tar_2).cont.val;
            switch (instruction(i).op) {
            case OpEQ:
                emit_beq(lab);
                break;
            case OpNEQ:
                emit_bne(lab);
                break;
            case OpLT:
                if (flags & IC_SIGNED)
                    emit_blt(lab);
                else
                    emit_blo(lab);
                break;
            case OpLET:
                if (flags & IC_SIGNED)
                    emit_ble(lab);
                else
                    emit_bls(lab);
                break;
            case OpGT:
                if (flags & IC_SIGNED)
                    emit_bgt(lab);
                else
                    emit_bhi(lab);
                break;
            case OpGET:
                if (flags & IC_SIGNED)
                    emit_bge(lab);
                else
                    emit_bhs(lab);
                break;
            }
        }
        instruction(j).op = OpNOp;
        break;
    }
}

static void arm_eq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        arm_do_libcall(i, tar, arg1, arg2, LibEq);
    } else {
        ARM_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            arm_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp r%d, %s", res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("moveq r%d, #1", res);
            emitln("movne r%d, #0", res);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void arm_neq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        arm_do_libcall(i, tar, arg1, arg2, LibNeq);
    } else {
        ARM_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            arm_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp r%d, %s", res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("movne r%d, #1", res);
            emitln("moveq r%d, #0", res);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void arm_lt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        arm_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSLT:LibULT);
    } else {
        ARM_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            arm_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp r%d, %s", res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("mov%s r%d, #1", (flags&IC_SIGNED)?"lt":"lo", res);
            emitln("mov%s r%d, #0", (flags&IC_SIGNED)?"ge":"hs", res);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void arm_let(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        arm_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSLET:LibULET);
    } else {
        ARM_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            arm_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp r%d, %s", res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("mov%s r%d, #1", (flags&IC_SIGNED)?"le":"ls", res);
            emitln("mov%s r%d, #0", (flags&IC_SIGNED)?"gt":"hi", res);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void arm_gt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        arm_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSGT:LibUGT);
    } else {
        ARM_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            arm_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp r%d, %s", res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("mov%s r%d, #1", (flags&IC_SIGNED)?"gt":"hi", res);
            emitln("mov%s r%d, #0", (flags&IC_SIGNED)?"le":"ls", res);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void arm_get(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        arm_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSGET:LibUGET);
    } else {
        ARM_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            arm_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp r%d, %s", res, arm_get_operand(arg2, 1));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("mov%s r%d, #1", (flags&IC_SIGNED)?"ge":"hs", res);
            emitln("mov%s r%d, #0", (flags&IC_SIGNED)?"lt":"lo", res);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void arm_neg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        ARM_Reg2 res;

        res = get_reg2(i);
        arm_load2(res, arg1);
        emitln("rsbs r%d, r%d, #0", res.r1, res.r1);
        emitln("rsc r%d, r%d, #0", res.r2, res.r2);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        emitln("rsb r%d, r%d, #0", res, res);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void arm_cmpl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        ARM_Reg2 res;

        res = get_reg2(i);
        arm_load2(res, arg1);
        emitln("mvn r%d, r%d", res.r1, res.r1);
        emitln("mvn r%d, r%d", res.r2, res.r2);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        emitln("mvn r%d, r%d", res, res);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void arm_not(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        arm_do_libcall(i, tar, arg1, arg2, LibNot);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        emitln("cmp r%d, #0", res);
        emitln("moveq r%d, #1", res);
        emitln("movne r%d, #0", res);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void arm_ch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    ARM_Reg res;

    res = get_reg(i);
    arm_load(res, arg1);
    emitln("sxtb r%d, r%d", res, res);
    UPDATE_ADDRESSES_UNARY(res);
}

static void arm_uch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    ARM_Reg res;

    res = get_reg(i);
    arm_load(res, arg1);
    emitln("uxtb r%d, r%d", res, res);
    UPDATE_ADDRESSES_UNARY(res);
}

static void arm_sh(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    ARM_Reg res;

    res = get_reg(i);
    arm_load(res, arg1);
    emitln("sxth r%d, r%d", res, res);
    UPDATE_ADDRESSES_UNARY(res);
}

static void arm_ush(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    ARM_Reg res;

    res = get_reg(i);
    arm_load(res, arg1);
    emitln("uxth r%d, r%d", res, res);
    UPDATE_ADDRESSES_UNARY(res);
}

static void arm_llsx(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    ARM_Reg2 res;

    res = get_reg2(i);
    arm_load(res.r1, arg1);
    emitln("mov r%d, r%d, ASR #31", res.r2, res.r1);
    UPDATE_ADDRESSES_UNARY2(res);
}

static void arm_llzx(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    ARM_Reg2 res;

    res = get_reg2(i);
    arm_load(res.r1, arg1);
    emitln("mov r%d, #0", res.r2);
    UPDATE_ADDRESSES_UNARY2(res);
}

static void arm_addr_of(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    ARM_Reg res;

    res = get_reg0();
    arm_load_addr(res, arg1);
    update_tar_descriptors(res, tar, tar_liveness(i), tar_next_use(i));
}

static void arm_ind(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    /* spill any target currently in a register */
    spill_aliased_objects();

    if (ISLL(instruction(i).type)) {
        ARM_Reg2 res;

        res = get_reg2(i);
        arm_load(res.r1, arg1);
        emitln("ldmia r%d, {r%d, r%d}", res.r1, res.r1, res.r2);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        switch (cat) {
        case TOK_STRUCT:
        case TOK_UNION:
            break;
        case TOK_SHORT:
            emitln("ldrsh r%d, [r%d]", res, res);
            break;
        case TOK_UNSIGNED_SHORT:
            emitln("ldrh r%d, [r%d]", res, res);
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
            emitln("ldrsb r%d, [r%d]", res, res);
            break;
        case TOK_UNSIGNED_CHAR:
            emitln("ldrb r%d, [r%d]", res, res);
            break;
        default:
            emitln("ldr r%d, [r%d]", res, res);
            break;
        }
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void arm_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        ARM_Reg2 res;

        res = get_reg2(i);
        arm_load2(res, arg1);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        ARM_Reg res;

        res = get_reg(i);
        arm_load(res, arg1);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void arm_post_call(int i)
{
    Token cat;

    /* we only maintain struct/union addresses in registers */
    if (((cat=get_type_category(instruction(i).type))==TOK_STRUCT || cat==TOK_UNION)
    && (get_sizeof(instruction(i).type) <= 4)) {
        retvals_to_fix[retvals_to_fix_counter++] = string_get_pos(func_body);
        emitln("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
    }

    if (arg_offs > max_arg_offs)
        max_arg_offs = arg_offs;
    arg_offs = 0;
}

static void arm_indcall(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    spill_all();
    arm_load(SCRATCH_REG, arg1);
    emitln("blx r%d", SCRATCH_REG);
    arm_post_call(i);
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    if (tar) {
        Token cat;

        if (ISLL(instruction(i).type)) {
            ARM_Reg2 r = { 4, 5 };

            emitln("mov r4, r0");
            emitln("mov r5, r1");
            update_tar_descriptors2(r, tar, tar_liveness(i), tar_next_use(i));
        } else {
            emitln("mov r4, r0");
            update_tar_descriptors(4, tar, tar_liveness(i), tar_next_use(i));
        }
    }
}

static void arm_call(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    spill_all();
    emitln("bl %s", address(arg1).cont.var.e->attr.str);
    arm_post_call(i);
    if (tar) {
        Token cat;

        if (ISLL(instruction(i).type)) {
            ARM_Reg2 r = { 4, 5 };

            emitln("mov r4, r0");
            emitln("mov r5, r1");
            update_tar_descriptors2(r, tar, tar_liveness(i), tar_next_use(i));
        } else {
            emitln("mov r4, r0");
            update_tar_descriptors(4, tar, tar_liveness(i), tar_next_use(i));
        }
    }
}

static void arm_ind_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    ARM_Reg pr;
    char *st_str;

    /* force the reload of any target currently in a register */
    spill_aliased_objects();

    if (ISLL(instruction(i).type)) {
        if (addr_reg1(arg1) == -1) {
            pr = get_reg(i);
            arm_load(pr, arg1);
            pin_reg(pr);
        } else {
            pr = addr_reg1(arg1);
        }

        if (address(arg2).kind == IConstKind) {
            unsigned *p;

            p = (unsigned *)&address(arg2).cont.val;
            emitln("ldr r%d, =#%u", SCRATCH_REG, p[0]);
            emitln("str r%d, [r%d]", SCRATCH_REG, pr);
            emitln("ldr r%d, =#%u", SCRATCH_REG, p[1]);
            emitln("str r%d, [r%d, #4]", SCRATCH_REG, pr);
        } else if (address(arg2).kind == StrLitKind) {
            emitln("ldr r%d, =_@S%d", SCRATCH_REG, new_string_literal(arg2));
            emitln("str r%d, [r%d]", SCRATCH_REG, pr);
            emitln("mov r%d, #0");
            emitln("str r%d, [r%d, #4]", SCRATCH_REG, pr);
        } else {
            ARM_Reg2 r;

            if (addr_reg1(arg2) == -1) {
                r.r1 = get_reg0();
                pin_reg(r.r1);
                r.r2 = get_reg0();
                unpin_reg(r.r1);
                arm_load2(r, arg2);
            } else {
                r.r1 = addr_reg1(arg2);
                r.r2 = addr_reg2(arg2);
                assert(r.r2 != -1);
            }
            emitln("stmia r%d, {r%d, r%d}", pr, r.r1, r.r2);
        }
        unpin_reg(pr);
        goto done;
    } else if (cat==TOK_STRUCT || cat==TOK_UNION) {
        emitln("sub r13, r13, #12");
        arm_load(SCRATCH_REG, arg1);
        emitln("str r%d, [r13]", SCRATCH_REG);
        arm_load(SCRATCH_REG, arg2);
        emitln("str r%d, [r13, #4]", SCRATCH_REG);
        emitln("ldr r%d, =#%u", SCRATCH_REG, get_sizeof(instruction(i).type));
        emitln("str r%d, [r13, #8]", SCRATCH_REG);
        emitln("bl __lux_arm_memcpy");
        goto done;
    }

    /*
     * <= 4 bytes scalar indirect assignment.
     */

    if (addr_reg1(arg1) == -1) {
        pr = get_reg(i);
        arm_load(pr, arg1);
        pin_reg(pr);
    } else {
        pr = addr_reg1(arg1);
    }

    switch (cat) {
    case TOK_SHORT:
    case TOK_UNSIGNED_SHORT:
        st_str = "strh";
        break;
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
    case TOK_UNSIGNED_CHAR:
        st_str = "strb";
        break;
    default:
        st_str = "str";
        break;
    }

    if (address(arg2).kind == IConstKind) {
        emitln("ldr r%d, =#%u", SCRATCH_REG, (unsigned)address(arg2).cont.uval);
        emitln("%s r%d, [r%d]", st_str, SCRATCH_REG, pr);
    } else if (address(arg2).kind == StrLitKind) {
        emitln("ldr r%d, =_@S%d", SCRATCH_REG, new_string_literal(arg2));
        emitln("%s r%d, [r%d]", st_str, SCRATCH_REG, pr);
    } else {
        ARM_Reg r;

        if (addr_reg1(arg2) == -1) {
            r = get_reg0();
            arm_load(r, arg2);
        } else {
            r = addr_reg1(arg2);
        }
        emitln("%s r%d, [r%d]", st_str, r, pr);
    }
    unpin_reg(pr);
done:
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    update_arg_descriptors(arg2, arg2_liveness(i), arg2_next_use(i));
}

static void arm_lab(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    emit_lab(address(tar).cont.val);
}

static void arm_jmp(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (instruction(i+1).op == OpLab
    && address(instruction(i+1).tar).cont.val == address(tar).cont.val)
        return;
    emit_jmp(address(tar).cont.val);
}

static void arm_begarg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void arm_arg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    Declaration *ty;

    if (arg2 != 0) {
        unsigned siz;

        spill_reg(0);
        siz = get_sizeof(instruction(i).type);
        if (siz > temp_struct_size)
            temp_struct_size = siz;
        calls_to_fix[calls_to_fix_counter++] = string_get_pos(func_body);
        emitln("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"); /* sub r0, r11, <?> */
        arg_offs += 4;
        return;
    }

    ty = instruction(i).type;
    assert(ty->idl==NULL || ty->idl->op!=TOK_ID);

    if (ISLL(ty)) {
        arg_offs = round_up(arg_offs, 8);
        if (arg_offs < 16) {
            ARM_Reg2 r;

            r.r1 = arg_offs/4;
            r.r2 = r.r1+1;
            spill_reg(r.r1);
            spill_reg(r.r2);
            arm_load2(r, arg1);
        } else {
            char **op;
            int offs;

            op = arm_get_operand2(arg1);
            if ((offs=arg_offs-16+4) > 4095) {
                emitln("ldr r%d, =#%u", SCRATCH_REG, offs-4);
                emitln("str %s, [r13, r%d]", op[0], SCRATCH_REG);
                emitln("add r%d, r%d, #4", SCRATCH_REG, SCRATCH_REG);
                emitln("str %s, [r13, r%d]", op[1], SCRATCH_REG);
            } else {
                emitln("str %s, [r13, #%d]", op[0], offs-4);
                emitln("str %s, [r13, #%d]", op[1], offs);
            }
        }
        arg_offs += 8;
    } else if (cat==TOK_STRUCT || cat==TOK_UNION) {
        unsigned siz, asiz;

        siz = get_sizeof(ty);
        asiz = round_up(siz, 4);

        arg_offs = round_up(arg_offs, get_alignment(ty));
        if (arg_offs < 16) {
            int nr, ar;
            ARM_Reg r;

            nr = asiz/4;
            ar = 4-arg_offs/4;
            r = arg_offs/4;

            if (nr <= ar) { /* full registers */
                /* XXX: may read off limits at most 3 bytes */
                arm_load(SCRATCH_REG, arg1);
                switch (nr) {
                case 1:
                    emitln("ldr r%d, [r%d]", r, SCRATCH_REG);
                    break;
                case 2:
                    emitln("ldmia r%d, {r%d, r%d}", SCRATCH_REG, r, r+1);
                    break;
                case 3:
                    emitln("ldmia r%d, {r%d, r%d, r%d}", SCRATCH_REG, r, r+1, r+2);
                    break;
                case 4:
                    emitln("ldmia r%d, {r%d, r%d, r%d, r%d}", SCRATCH_REG, r, r+1, r+2, r+3);
                    break;
                }
            } else { /* part registers, part stack */
                unsigned siz2;

                arm_load(SCRATCH_REG, arg1);
                switch (ar) {
                case 1:
                    emitln("ldr r%d, [r%d]", r, SCRATCH_REG);
                    siz2 = siz-4;
                    break;
                case 2:
                    emitln("ldmia r%d, {r%d, r%d}", SCRATCH_REG, r, r+1);
                    siz2 = siz-8;
                    break;
                case 3:
                    emitln("ldmia r%d, {r%d, r%d, r%d}", SCRATCH_REG, r, r+1, r+2);
                    siz2 = siz-12;
                    break;
                }
                emitln("sub r13, r13, #12");
                if (is_representable_in_imm12(siz-siz2))
                    emitln("add r%d, r%d, #%d", SCRATCH_REG, SCRATCH_REG, siz-siz2);
                else
                    assert(0); /* TODO */
                emitln("str r%d, [r13, #4]", SCRATCH_REG);
                emitln("add r%d, r13, #%d", SCRATCH_REG, 12);
                emitln("str r%d, [r13]", SCRATCH_REG);
                emitln("ldr r%d, =#%u", SCRATCH_REG, siz2);
                emitln("str r%d, [r13, #8]", SCRATCH_REG);
                emitln("bl __lux_arm_memcpy");
            }
        } else { /* full stack */
            emitln("sub r13, r13, #12");
            if (is_representable_in_imm12(arg_offs-16+12)) {
                emitln("add r%d, r13, #%d", SCRATCH_REG, arg_offs-16+12);
            } else {
                emitln("ldr r%d, =#%u", SCRATCH_REG, arg_offs-16+12);
                emitln("add r%d, r13, r%d", SCRATCH_REG, SCRATCH_REG);
            }
            emitln("str r%d, [r13]", SCRATCH_REG);
            arm_load(SCRATCH_REG, arg1);
            emitln("str r%d, [r13, #4]", SCRATCH_REG);
            emitln("ldr r%d, =#%u", SCRATCH_REG, siz);
            emitln("str r%d, [r13, #8]", SCRATCH_REG);
            emitln("bl __lux_arm_memcpy");
        }
        arg_offs += asiz;
    } else {
        ARM_Reg r;

        if (arg_offs < 16) {
            r = arg_offs/4;
            spill_reg(r);
            arm_load(r, arg1);
        } else {
            if (const_addr(arg1) || (r=addr_reg1(arg1))==-1) {
                r = (ARM_Reg)SCRATCH_REG;
                arm_load(r, arg1);
            }
            if (is_representable_in_imm12(arg_offs-16))
                emitln("str r%d, [r13, #%d]", r, arg_offs-16);
            else
                assert(0); /* TODO */
        }
        arg_offs += 4;
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

static void arm_ret(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (qword_return) {
        ARM_Reg2 r = { 0, 1 };

        arm_load2(r, arg1);
    } else if (struct_union_return) {
        unsigned siz;

        if ((siz=get_sizeof(instruction(i).type)) > 4) {
            emitln("sub r13, r13, #12");
            emitln("ldr r0, [r11, #8]");
            emitln("str r0, [r13]", SCRATCH_REG);
            arm_load(SCRATCH_REG, arg1);
            emitln("str r%d, [r13, #4]", SCRATCH_REG);
            emitln("ldr r%d, =#%u", SCRATCH_REG, siz);
            emitln("str r%d, [r13, #8]", SCRATCH_REG);
            emitln("bl __lux_arm_memcpy");
        } else {
            /* See AAPCS #5.4 */
            arm_load(0, arg1);
            switch (siz) {
            case 1:
                emitln("ldrb r0, [r0]");
                break;
            case 2:
                emitln("ldrh r0, [r0]");
                break;
            default:
                /* XXX: may read off limits 1 byte */
                emitln("ldr r0, [r0]");
                break;
            }
        }
    } else {
        arm_load(0, arg1);
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

static void arm_cbr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    ARM_Reg r;

    if (ISLL(instruction(i).type)) {
        ARM_Reg2 r2;

        r2 = get_reg2(i);
        arm_load2(r2, arg1);
        emitln("orr r%d, r%d, r%d", r2.r1, r2.r1, r2.r2);
        r = r2.r1;
    } else {
        r = get_reg(i);
        arm_load(r, arg1);
    }
    emitln("cmp r%d, #0", r);
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    if (address(tar).cont.val == address(instruction(i+1).tar).cont.val)
        emitln("beq %s@L%d", curr_func, (int)address(arg2).cont.val);
    else
        emitln("bne %s@L%d", curr_func, (int)address(tar).cont.val);
}

static void arm_nop(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void arm_do_switch64(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    ARM_Reg2 res;
    static unsigned nlab;

    if (addr_reg1(arg1) == -1) {
        res = get_reg2(i);
        arm_load2(res, arg1);
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
        emitln("ldr r%d, =#%u", SCRATCH_REG, p[0]);
        emitln("cmp r%d, r%d", SCRATCH_REG, res.r1);
        emitln("bne %s@sw64L%u", curr_func, nlab);
        emitln("ldr r%d, =#%u", SCRATCH_REG, p[1]);
        emitln("cmp r%d, r%d", SCRATCH_REG, res.r2);
        emit_beq(address(arg1).cont.val);
        emitln("%s@sw64L%u:", curr_func, nlab);
        ++nlab;
    }
}

static void arm_switch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    ARM_Reg res;
    int def_val;
    Arena *lab_arena;
    char **jmp_tab, def_lab[256];
    int ncase, min, max, interval_size, holes;

    if (ISLL(instruction(i).type)) {
        arm_do_switch64(i, tar, arg1, arg2);
        return;
    }

    if (addr_reg1(arg1) == -1) {
        res = get_reg(i);
        arm_load(res, arg1);
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
    if (is_representable_in_imm12(max)) {
        emitln("cmp r%d, #%u", res, (unsigned)max);
    } else {
        emitln("ldr r%d, =#%u", SCRATCH_REG, (unsigned)max);
        emitln("cmp r%d, r%d", res, SCRATCH_REG);
    }
    emit_bgt(def_val);
    if (is_representable_in_imm12(min)) {
        emitln("cmp r%d, #%u", res, (unsigned)min);
        emit_blt(def_val);
        if (min != 0)
            emitln("sub r%d, r%d, #%u", res, res, (unsigned)min);
    } else {
        emitln("ldr r%d, =#%u", SCRATCH_REG, (unsigned)min);
        emitln("cmp r%d, r%d", res, SCRATCH_REG);
        emit_blt(def_val);
        if (min != 0)
            emitln("sub r%d, r%d, r%d", res, res, SCRATCH_REG);
    }
    emitln("ldr r%d, =@jt%d", SCRATCH_REG, jump_tables_counter);
    emitln("ldr r15, [r%d, r%d, LSL #2]", SCRATCH_REG, res);

    /* build jump table */
    jmp_tab = calloc(interval_size, sizeof(char *));
    lab_arena = arena_new(interval_size*256, FALSE);
    sprintf(def_lab, "%s@L%d", curr_func, def_val);
    for (;; i++) {
        char *s;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;
        if (address(arg2).cont.val)
            break;
        s = arena_alloc(lab_arena, 256);
        sprintf(s, "%s@L%d", curr_func, (int)address(arg1).cont.val);
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
    emitln(".ltorg");
    offs_to_lit_tab = 0;
    SET_SEGMENT(ROD_SEG, emitln);
    emitln("@jt%d:", jump_tables_counter++);
    for (i = 0; i < interval_size; i++)
        emitln(".dword %s", jmp_tab[i]);
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
            break;
        }

        if (is_representable_in_imm12((unsigned)address(tar).cont.val)) {
            emitln("cmp r%d, #%u", res, (unsigned)address(tar).cont.val);
        } else {
            emitln("ldr r%d, =#%u", SCRATCH_REG, (unsigned)address(tar).cont.val);
            emitln("cmp r%d, r%d", res, SCRATCH_REG);
        }
        emit_beq(address(arg1).cont.val);
    }
}

static void arm_case(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void (*instruction_handlers[])(int, unsigned, unsigned, unsigned) = {
    arm_add, arm_sub, arm_mul, arm_div,
    arm_rem, arm_shl, arm_shr, arm_and,
    arm_or, arm_xor, arm_eq, arm_neq,
    arm_lt, arm_let, arm_gt, arm_get,

    arm_neg, arm_cmpl, arm_not, arm_ch,
    arm_uch, arm_sh, arm_ush, arm_llsx,
    arm_llzx, arm_addr_of, arm_ind, arm_asn,
    arm_call, arm_indcall,

    arm_ind_asn, arm_lab, arm_jmp, arm_arg,
    arm_ret, arm_switch, arm_case, arm_cbr,
    arm_begarg, arm_nop
};

/* save/restore v1-v7 (r4-r10) registers */
static void arm_save_restore_var_regs(int rsa_start)
{
    int i;
    int rep;

    /* registers are allocated in increasing order */
    if (!modified[4])
        return;

    if (verbose_asm)
        emit_prologln("; save registers");
    if (rep = is_representable_in_imm12(-rsa_start)) {
        emit_prologln("sub r%d, r11, #%d", SCRATCH_REG, -rsa_start);
    } else {
        emit_prologln("ldr r%d, =#%u", SCRATCH_REG, -rsa_start);
        emit_prologln("sub r%d, r11, r%d", SCRATCH_REG, SCRATCH_REG);
    }
    emit_prolog("stmia r%d, {r4", SCRATCH_REG);
    if (verbose_asm)
        emit_epilogln("; restore registers");
    if (rep) {
        emit_epilogln("sub r%d, r11, #%d", SCRATCH_REG, -rsa_start);
    } else {
        emit_epilogln("ldr r%d, =#%u", SCRATCH_REG, -rsa_start);
        emit_epilogln("sub r%d, r11, r%d", SCRATCH_REG, SCRATCH_REG);
    }
    emit_epilog("ldmia r%d, {r4", SCRATCH_REG);
    for (i = 5; i < 11; i++) {
        if (modified[i]) {
            emit_prolog(", r%d", i);
            emit_epilog(", r%d", i);
        } else {
            break;
        }
    }
    emit_prologln("}");
    emit_epilogln("}");
}

void arm_function_definition(TypeExp *decl_specs, TypeExp *header)
{
    Token cat;
    int i, last_i;
    unsigned fn, pos_tmp;
    TypeExp *scs;
    Declaration ty;
    int rsa_size, rsa_start;
    static int first_func = TRUE;

    curr_func = header->str;
    fn = new_cg_node(curr_func);
    size_of_local_area = round_up(cg_node(fn).size_of_local_area, 8);

    ty.decl_specs = decl_specs;
    ty.idl = header->child->child;
    if ((cat=get_type_category(&ty))==TOK_STRUCT || cat==TOK_UNION)
        struct_union_return = TRUE;
    else if (cat==TOK_LONG_LONG || cat==TOK_UNSIGNED_LONG_LONG)
        qword_return = TRUE;

    if (!first_func)
        emit_prolog("\n");
    else
        emit_prolog("$$a:");
    emit_prologln("; ==== start of definition of function `%s' ====", curr_func);
    SET_SEGMENT(TEXT_SEG, emit_prologln);
    if ((scs=get_sto_class_spec(decl_specs))==NULL || scs->op!=TOK_STATIC)
        emit_prologln(".global $%s", curr_func);
    emit_prologln("$%s:", curr_func);

    i = cfg_node(cg_node(fn).bb_i).leader;
    last_i = cfg_node(cg_node(fn).bb_f).last;
    func_last_quad = last_i;
    for (; i <= last_i; i++) {
        unsigned tar, arg1, arg2;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        if (verbose_asm && C_source[i]!=NULL)
            emitln("; %s", C_source[i]);
        instruction_handlers[instruction(i).op](i, tar, arg1, arg2);
    }
    size_of_local_area -= round_up(temp_struct_size, 8);
    pos_tmp = string_get_pos(func_body);
    /* fix calls to functions that return a struct/union with size <= 4 */
    while (--retvals_to_fix_counter >= 0) {
        int n;
        char *s;

        string_set_pos(func_body, retvals_to_fix[retvals_to_fix_counter]);
        s = string_curr(func_body);
        if (is_representable_in_imm12(-size_of_local_area))
            n = sprintf(s, "str r0, [r11, -#%u]\n"
                           "sub r0, r11, #%u", -size_of_local_area, -size_of_local_area);
        else
            assert(0); /* TODO */
        s[n++] = ' ';
        for (; s[n] == 'X'; n++)
            s[n] = ' ';
    }
    /* fix calls to functions that return a struct/union with size > 4 */
    while (--calls_to_fix_counter >= 0) {
        int n;
        char *s;

        string_set_pos(func_body, calls_to_fix[calls_to_fix_counter]);
        s = string_curr(func_body);
        if (is_representable_in_imm12(-size_of_local_area)) {
            n = sprintf(s, "sub r0, r11, #%u", -size_of_local_area);
        } else {
            n = sprintf(s, "ldr r%d, =#%u\n"
                           "sub r0, r11, r%d",
                           SCRATCH_REG, -size_of_local_area, SCRATCH_REG);
        }
        s[n++] = ' ';
        for (; s[n] == 'X'; n++)
            s[n] = ' ';
    }
    string_set_pos(func_body, pos_tmp);

    /* reserve space to save registers */
    rsa_size = 0;
    for (i = 4; i < 11; i++) {
        if (!modified[i])
            break;
        rsa_size += 4;
    }
    size_of_local_area -= round_up(rsa_size, 8);
    rsa_start = size_of_local_area;

    if (max_arg_offs > 16)
        size_of_local_area -= round_up(max_arg_offs-16, 8);
    emit_prologln("stmdb r13!, {r0, r1, r2, r3}");
    emit_prologln("stmdb r13!, {r11, r14}");
    emit_prologln("mov r11, r13");
    if (size_of_local_area) {
        if (is_representable_in_imm12(-size_of_local_area)) {
            emit_prologln("sub r13, r13, #%d", -size_of_local_area);
        } else {
            emit_prologln("ldr r%d, =#%u", SCRATCH_REG, -size_of_local_area);
            emit_prologln("sub r13, r13, r%d", SCRATCH_REG);
        }
    }

    arm_save_restore_var_regs(rsa_start);

    emit_epilogln("mov r13, r11");
    emit_epilogln("ldmia r13!, {r11, r14}");
    emit_epilogln("add r13, r13, #16");
    emit_epilogln("mov r15, r14");
    emit_epilogln(".ltorg");

    string_write(func_prolog, arm_output_file);
    string_write(func_body, arm_output_file);
    string_write(func_epilog, arm_output_file);

    /* reset everything */
    string_clear(func_prolog);
    string_clear(func_body);
    string_clear(func_epilog);
    temp_struct_size = 0;
    calls_to_fix_counter = 0;
    retvals_to_fix_counter = 0;
    arg_offs = max_arg_offs = 0;
    offs_to_lit_tab = 0;
    /*memset(pinned, 0, sizeof(int)*ARM_NREG);*/
    memset(modified, 0, sizeof(int)*ARM_NREG);
    free_all_temps();
#if 1
    memset(addr_descr_tab, -1, nid_counter*sizeof(int));
    memset(reg_descr_tab, 0, sizeof(unsigned)*ARM_NREG);
#endif
    struct_union_return = qword_return = FALSE;
#if 1
    dump_addr_descr_tab();
    dump_reg_descr_tab();
#endif
    first_func = FALSE;
}

/*
 * Emit an expression valid for the assembler to evaluate.
 */
static long long arm_static_expr(ExecNode *e)
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
            return arm_static_expr(e->child[pi])+get_sizeof(&ty)*arm_static_expr(e->child[ii]);
        }
        case TOK_DOT:
        case TOK_ARROW:
            if (get_type_category(&e->child[0]->type) != TOK_UNION) {
                StructMember *m;

                m = get_member_descriptor(get_type_spec(e->child[0]->type.decl_specs), e->child[1]->attr.str);
                return arm_static_expr(e->child[0])+m->offset;
            } else {
                return arm_static_expr(e->child[0]);
            }
        case TOK_ADDRESS_OF:
        case TOK_INDIRECTION:
        case TOK_CAST:
            return arm_static_expr(e->child[0]);

        case TOK_PLUS:
            if (is_integer(get_type_category(&e->type))) {
                return arm_static_expr(e->child[0])+arm_static_expr(e->child[1]);
            } else {
                int pi, ii;
                Declaration ty;

                if (is_integer(get_type_category(&e->child[0]->type)))
                    pi = 1, ii = 0;
                else
                    pi = 0, ii = 1;
                ty = e->child[pi]->type;
                ty.idl = ty.idl->child;
                return arm_static_expr(e->child[pi])+get_sizeof(&ty)*arm_static_expr(e->child[ii]);
            }
        case TOK_MINUS:
            if (is_integer(get_type_category(&e->child[0]->type))) { /* int-int */
                return arm_static_expr(e->child[0])-arm_static_expr(e->child[1]);
            } else { /* ptr-int */
                Declaration ty;

                ty = e->child[0]->type;
                ty.idl = ty.idl->child;
                return arm_static_expr(e->child[0])-get_sizeof(&ty)*arm_static_expr(e->child[1]);
            }
        case TOK_CONDITIONAL:
            if (e->child[0]->attr.val)
                return arm_static_expr(e->child[1]);
            else
                return arm_static_expr(e->child[2]);
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
    assert(0);
}

void arm_static_init(TypeExp *ds, TypeExp *dct, ExecNode *e)
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
                emit_declln(".zero #%u", nelem-n);
        } else {
            /* handle elements with explicit initializer */
            for (e = e->child[0]; e!=NULL /*&& nelem!=0*/; e=e->sibling, --nelem)
                arm_static_init(ds, dct->child, e);

            /* handle elements without explicit initializer */
            if (nelem != 0) {
                Declaration ty;

                ty.decl_specs = ds;
                ty.idl = dct->child;
                emit_declln(".align #%u", get_alignment(&ty));
                emit_declln(".zero #%u", nelem*get_sizeof(&ty));
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
                arm_static_init(d->decl->decl_specs, dct->child, e);

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
                    emit_declln(".align #%u", get_alignment(&ty));
                    emit_declln(".zero #%u",  get_sizeof(&ty));

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
        arm_static_init(ts->attr.dl->decl->decl_specs, ts->attr.dl->decl->idl->child, e);
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
            emit_decl(".byte ");
            break;
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            emit_declln(".align #2");
            emit_decl(".word ");
            break;
        case TOK_LONG_LONG:
        case TOK_UNSIGNED_LONG_LONG: {
            long long v;

            emit_declln(".align #8");
            emit_decl(".dword ");
            v = arm_static_expr(e);
            emit_declln("#%u", ((unsigned *)&v)[0]);
            emit_decl(".dword ");
            emit_declln("#%u", ((unsigned *)&v)[1]);
        }
            return;
        default:
            emit_declln(".align #4");
            emit_decl(".dword ");
            break;
        }
        emit_declln("#%u", (unsigned)arm_static_expr(e));
    }
}

void arm_allocate_static_objects(void)
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
                emit_declln(".align #%u", al);
        } else {
            SET_SEGMENT(BSS_SEG, emit_declln);
            if (al > 1)
                emit_declln(".alignb #%u", al);
        }
        if ((enclosing_function=np->enclosing_function) != NULL) { /* static local */
            emit_declln("%s@%s:", np->enclosing_function, np->declarator->str);
        } else {
            TypeExp *scs;

            if ((scs=get_sto_class_spec(np->decl_specs))==NULL || scs->op!=TOK_STATIC)
                emit_declln(".global %s", np->declarator->str);
            emit_declln("%s:", np->declarator->str);
        }
        if (initzr != NULL)
            arm_static_init(ty.decl_specs, ty.idl, initzr);
        else
            emit_declln(".res #%u", get_sizeof(&ty));
    }
}

void arm_cgen(FILE *outf)
{
    unsigned i, j;
    ExternId *ed, **func_def_list, **ext_sym_list;

    arm_output_file = outf;

    ic_main(&func_def_list, &ext_sym_list);
    compute_liveness_and_next_use();

    asm_decls = string_new(512);
    str_lits = string_new(512);
    func_body = string_new(1024);
    func_prolog = string_new(1024);
    func_epilog = string_new(1024);
    addr_descr_tab = malloc(nid_counter*sizeof(ARM_Reg2));
    memset(addr_descr_tab, -1, nid_counter*sizeof(ARM_Reg2));
    for (i = 0; (ed=func_def_list[i]) != NULL; i++)
        arm_function_definition(ed->decl_specs, ed->declarator);
    string_free(func_body);
    string_free(func_prolog);
    string_free(func_epilog);

    emit_declln("\n; == objects with static duration");
    arm_allocate_static_objects();

    emit_declln("\n; == extern symbols");
    /* emit extern directives only for those symbols that were referenced */
    for (j = 0; (ed=ext_sym_list[j]) != NULL; j++) {
        int tmp;

        /* get_var_nid() will increment nid_counter when it sees a new identifier */
        tmp = nid_counter;
        get_var_nid(ed->declarator->str, 0);
        if (tmp == nid_counter)
            emit_declln(".extern %s", ed->declarator->str);
    }
    /* the front-end may emit calls to memcpy/memset */
    emit_declln(".extern memcpy");
    emit_declln(".extern memset");
    emit_declln(".extern __lux_arm_memcpy");
    /* liblux functions */
    if (include_liblux) {
        emit_declln(".extern __lux_mul64");
        emit_declln(".extern __lux_sdiv64");
        emit_declln(".extern __lux_udiv64");
        emit_declln(".extern __lux_smod64");
        emit_declln(".extern __lux_umod64");
        emit_declln(".extern __lux_shl64");
        emit_declln(".extern __lux_sshr64");
        emit_declln(".extern __lux_ushr64");
        emit_declln(".extern __lux_ucmp64");
        emit_declln(".extern __lux_scmp64");
        emit_declln(".extern __lux_udiv32");
        emit_declln(".extern __lux_sdiv32");
        emit_declln(".extern __lux_umod32");
        emit_declln(".extern __lux_smod32");
    }

    string_write(asm_decls, arm_output_file);
    string_free(asm_decls);

    if (string_literals_counter) {
        fprintf(arm_output_file, "\n; == string literals\n");
        fprintf(arm_output_file, ".rodata\n");
        string_write(str_lits, arm_output_file);
    }
    string_free(str_lits);
}
