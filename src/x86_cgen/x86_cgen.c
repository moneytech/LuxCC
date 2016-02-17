/*
 * Simple x86 code generator
 *      IC ==> x86 ASM (NASM syntax).
 * Normative document:
 *   => System V ABI-i386: http://www.sco.com/developers/devspecs/abi386-4.pdf
 * Other documents:
 *   => Calling conventions: http://www.agner.org/optimize/calling_conventions.pdf
 * TOFIX:
 * - There are places where the code ignores the fact that byte versions
 *   of ESI and EDI don't exist.
 */
#define DEBUG 0
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
#include "../luxcc.h"

typedef enum {
    X86_EAX,
    X86_EBX,
    X86_ECX,
    X86_EDX,
    X86_ESI,
    X86_EDI,
    X86_NREG,
} X86_Reg;

typedef struct {
    short r1, r2;
} X86_Reg2;

static int pinned[X86_NREG];
static int modified[X86_NREG];
#define pin_reg(r)      (pinned[r] = TRUE)
#define pin_reg2(r)     (pinned[r.r1] = TRUE, pinned[r.r2] = TRUE)
#define unpin_reg(r)    (pinned[r] = FALSE)
#define unpin_reg2(r)   (pinned[r.r1] = FALSE, pinned[r.r2] = FALSE)
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

static int size_of_local_area;
static char *curr_func, *enclosing_function;
static unsigned temp_struct_size;
static int big_return, qword_return;
static int arg_stack[64], arg_stack_top;
static int calls_to_fix_counter;
static unsigned calls_to_fix[64];
static int string_literals_counter;
static FILE *x86_output_file;
static int func_last_quad;

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
            f("segment %s", str_segment[seg]);\
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

static X86_Reg2 *addr_descr_tab;
static unsigned reg_descr_tab[X86_NREG];
#define addr_reg(a)     (addr_descr_tab[address_nid(a)])
#define addr_reg1(a)    (addr_descr_tab[address_nid(a)].r1)
#define addr_reg2(a)    (addr_descr_tab[address_nid(a)].r2)
#define reg_isempty(r)  (reg_descr_tab[r] == 0)
static void dump_addr_descr_tab(void);
static void dump_reg_descr_tab(void);

static void spill_reg(X86_Reg r);
static void spill_all(void);
static void spill_aliased_objects(void);
static X86_Reg get_reg0(void);
static X86_Reg get_reg(int i);
static X86_Reg2 get_reg2(int i);

static void x86_load(X86_Reg r, unsigned a);
static void x86_load2(X86_Reg2 r, unsigned a);
static void x86_load_addr(X86_Reg r, unsigned a);
static char *x86_get_operand(unsigned a);
static char **x86_get_operand2(unsigned a);
static void x86_store(X86_Reg r, unsigned a);
static void x86_store2(X86_Reg2 r, unsigned a);
static void x86_compare_against_constant(unsigned a, unsigned c);
static void x86_function_definition(TypeExp *decl_specs, TypeExp *header);

static void x86_static_expr(ExecNode *e);
static void x86_static_init(TypeExp *ds, TypeExp *dct, ExecNode *e);
static void x86_allocate_static_objects(void);

typedef struct Temp Temp;
static struct Temp {
    int nid;
    int offs; /* offset from ebp */
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
        string_printf(q, "dd 0x%02x%02x%02x%02x\n", s[3], s[2], s[1], s[0]);
        s += 4;
    }
    for (i = len%4; i; i--)
        string_printf(q, "db 0x%02x\n", *s++);
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
        X86_Reg2 ad;

        ad = addr_descr_tab[i];
        if (ad.r1 != -1) {
            if (ad.r2 != -1)
                fprintf(stderr, "%s => %s:%s\n", nid2sid_tab[i], x86_reg_str[ad.r1], x86_reg_str[ad.r2]);
            else
                fprintf(stderr, "%s => %s\n", nid2sid_tab[i], x86_reg_str[ad.r1]);
        }
    }
}

void dump_reg_descr_tab(void)
{
    int i;

    for (i = 0; i < X86_NREG; i++)
        if (!reg_isempty(i))
            fprintf(stderr, "%s => %s\n", x86_reg_str[i], address_sid(reg_descr_tab[i]));
}

void spill_reg(X86_Reg r)
{
    unsigned a;

    if (reg_isempty(r))
        return;

    a = reg_descr_tab[r];
    if (addr_reg2(a) != -1) { /* spill the register pair */
        X86_Reg2 rr;

        rr = addr_reg(a);
        x86_store2(rr, a);
        addr_reg1(a) = addr_reg2(a) = -1;
        reg_descr_tab[rr.r1] = reg_descr_tab[rr.r2] = 0;
    } else {
        x86_store(r, a);
        addr_reg1(a) = -1;
        reg_descr_tab[r] = 0;
    }
}

void spill_all(void)
{
    int i;

    for (i = 0; i < X86_NREG; i++)
        spill_reg((X86_Reg)i);
}

/*
 * Currently all address-taken variables are considered to be aliased.
 * => Possible improvement: use type information to narrow down the set
 * of objects to spill (see 6.5#7).
 */
void spill_aliased_objects(void)
{
    int i;

    for (i = 0; i < X86_NREG; i++) {
        unsigned a;

        if (reg_isempty(i))
            continue;
        a = reg_descr_tab[i];
        if (address(a).kind==IdKind && bset_member(address_taken_variables, address_nid(a)))
            spill_reg(i);
    }
}

X86_Reg get_reg0(void)
{
    int i;

    /* try to find an empty register */
    for (i = 0; i < X86_NREG; i++) {
        if (!pinned[i] && reg_isempty(i)) {
            modified[i] = TRUE;
            return (X86_Reg)i;
        }
    }

    /* choose an unpinned register and spill its contents */
    for (i = 0; i < X86_NREG; i++) {
        if (!pinned[i]) {
            modified[i] = TRUE;
            spill_reg((X86_Reg)i);
            return (X86_Reg)i;
        }
    }

    assert(0);
    return 0;
}

X86_Reg get_reg(int i)
{
    unsigned arg1;

    arg1 = instruction(i).arg1;
    if (!const_addr(arg1) && addr_reg1(arg1)!=-1 && !arg1_liveness(i))
        return addr_reg1(arg1);
    return get_reg0();
}

X86_Reg2 get_reg2(int i)
{
    X86_Reg2 r;
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

void x86_load(X86_Reg r, unsigned a)
{
    if (address(a).kind == IConstKind) {
        emitln("mov %s, %u", x86_reg_str[r], (unsigned)address(a).cont.uval);
    } else if (address(a).kind == StrLitKind) {
        emitln("mov %s, _@S%d", x86_reg_str[r], new_string_literal(a));
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *siz_str, *mov_str;

        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) == r)
                ; /* already in the register */
            else
                emitln("mov %s, %s", x86_reg_str[r], x86_reg_str[addr_reg1(a)]);
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
                emitln("%s %s, %s [$%s@%s]", mov_str, x86_reg_str[r], siz_str, curr_func, e->attr.str);
            else /* global */
                emitln("%s %s, %s [$%s]", mov_str, x86_reg_str[r], siz_str, e->attr.str);
        } else { /* parameter or local */
            emitln("%s %s, %s [ebp+%d]", mov_str, x86_reg_str[r], siz_str, local_offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) == r)
                return; /* already in the register */
            else
                emitln("mov %s, %s", x86_reg_str[r], x86_reg_str[addr_reg1(a)]);
        } else {
            emitln("mov %s, dword [ebp+%d]", x86_reg_str[r], get_temp_offs(a));
        }
    }
}

void x86_load2(X86_Reg2 r, unsigned a)
{
    if (address(a).kind == IConstKind) {
        unsigned *p;

        p = (unsigned *)&address(a).cont.uval;
        emitln("mov %s, %u", x86_reg_str[r.r1], p[0]);
        emitln("mov %s, %u", x86_reg_str[r.r2], p[1]);
    } else if (address(a).kind == StrLitKind) {
        emitln("mov %s, _@S%d", x86_reg_str[r.r1], new_string_literal(a));
        emitln("xor %s, %s", x86_reg_str[r.r2], x86_reg_str[r.r2]);
    } else if (address(a).kind == IdKind) {
        Token cat;
        ExecNode *e;

        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) != r.r1) {
                assert(addr_reg2(a) != r.r2);
                emitln("mov %s, %s", x86_reg_str[r.r1], x86_reg_str[addr_reg1(a)]);
                emitln("mov %s, %s", x86_reg_str[r.r2], x86_reg_str[addr_reg2(a)]);
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
                emitln("mov %s, dword [$%s@%s]", x86_reg_str[r.r1], curr_func, e->attr.str);
                emitln("mov %s, dword [$%s@%s+4]", x86_reg_str[r.r2], curr_func, e->attr.str);
            } else {
                emitln("mov %s, dword [$%s]", x86_reg_str[r.r1], e->attr.str);
                emitln("mov %s, dword [$%s+4]", x86_reg_str[r.r2], e->attr.str);
            }
        } else {
            emitln("mov %s, dword [ebp+%d]", x86_reg_str[r.r1], local_offset(a));
            emitln("mov %s, dword [ebp+%d]", x86_reg_str[r.r2], local_offset(a)+4);
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg1(a) != -1) {
            if (addr_reg1(a) != r.r1)
                emitln("mov %s, %s", x86_reg_str[r.r1], x86_reg_str[addr_reg1(a)]);
            assert(addr_reg2(a) != -1); /* TBD */
            if (addr_reg2(a) != r.r2)
                emitln("mov %s, %s", x86_reg_str[r.r2], x86_reg_str[addr_reg2(a)]);
        } else {
            int offs;

            offs = get_temp_offs(a);
            emitln("mov %s, dword [ebp+%d]", x86_reg_str[r.r1], offs);
            emitln("mov %s, dword [ebp+%d]", x86_reg_str[r.r2], offs+4);
        }
    }
}

char *x86_get_operand(unsigned a)
{
    static char op[256];

    if (address(a).kind == IConstKind) {
        sprintf(op, "dword %u", (unsigned)address(a).cont.uval);
    } else if (address(a).kind == StrLitKind) {
        sprintf(op, "_@S%d", new_string_literal(a));
    } else if (address(a).kind == IdKind) {
        ExecNode *e;

        if (addr_reg1(a) != -1)
            return x86_reg_str[addr_reg1(a)];

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
            /*assert(0);*/
        case TOK_SUBSCRIPT:
        case TOK_FUNCTION:
            if (e->attr.var.duration == DURATION_STATIC) {
                if (e->attr.var.linkage == LINKAGE_NONE)
                    sprintf(op, "$%s@%s", curr_func, e->attr.str);
                else
                    sprintf(op, "$%s", e->attr.str);
                return op;
            } else {
                X86_Reg r;

                r = get_reg0();
                emitln("lea %s, [ebp+%d]", x86_reg_str[r], local_offset(a));
                return x86_reg_str[r];
            }
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR: { /* promote to dword */
            X86_Reg r;

            r = get_reg0();
            x86_load(r, a);
            return x86_reg_str[r];
        }
        default: /* dword sized, OK */
            break;
        }

        /* fall through (dword sized operand) */
        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE)
                sprintf(op, "dword [$%s@%s]", curr_func, e->attr.str);
            else
                sprintf(op, "dword [$%s]", e->attr.str);
        } else {
            sprintf(op, "dword [ebp+%d]", local_offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg1(a) != -1)
            return x86_reg_str[addr_reg1(a)];
        else
            sprintf(op, "dword [ebp+%d]", get_temp_offs(a));
    }

    return op;
}

char **x86_get_operand2(unsigned a)
{
    static char op1[256], op2[256];
    static char *op[2] = { op1, op2 };

    if (address(a).kind == IConstKind) {
        unsigned *p;

        p = (unsigned *)&address(a).cont.uval;
        sprintf(op1, "dword %u", p[0]);
        sprintf(op2, "dword %u", p[1]);
    } else if (address(a).kind == StrLitKind) {
        sprintf(op1, "_@S%d", new_string_literal(a));
        sprintf(op2, "dword 0");
    } else if (address(a).kind == IdKind) {
        Token cat;
        ExecNode *e;

        if (addr_reg1(a) != -1) {
            assert(addr_reg2(a) != -1);
            sprintf(op1, "%s", x86_reg_str[addr_reg1(a)]);
            sprintf(op2, "%s", x86_reg_str[addr_reg2(a)]);
            return op;
        }

        e = address(a).cont.var.e;
        cat = get_type_category(&e->type);
        assert(cat==TOK_LONG_LONG || cat==TOK_UNSIGNED_LONG_LONG);

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) {
                sprintf(op1, "dword [$%s@%s]", curr_func, e->attr.str);
                sprintf(op2, "dword [$%s@%s+4]", curr_func, e->attr.str);
            } else {
                sprintf(op1, "dword [$%s]", e->attr.str);
                sprintf(op2, "dword [$%s+4]", e->attr.str);
            }
        } else {
            sprintf(op1, "dword [ebp+%d]", local_offset(a));
            sprintf(op2, "dword [ebp+%d]", local_offset(a)+4);
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg1(a) != -1) {
            assert(addr_reg2(a) != -1);
            sprintf(op1, "%s", x86_reg_str[addr_reg1(a)]);
            sprintf(op2, "%s", x86_reg_str[addr_reg2(a)]);
        } else {
            int offs;

            offs = get_temp_offs(a);
            sprintf(op1, "dword [ebp+%d]", offs);
            sprintf(op2, "dword [ebp+%d]", offs+4);
        }
    }

    return op;
}

void x86_load_addr(X86_Reg r, unsigned a)
{
    ExecNode *e;

    e = address(a).cont.var.e;
    if (e->attr.var.duration == DURATION_STATIC) {
        if (e->attr.var.linkage == LINKAGE_NONE)
            emitln("mov %s, $%s@%s", x86_reg_str[r], curr_func, e->attr.str);
        else
            emitln("mov %s, $%s", x86_reg_str[r], e->attr.str);
    } else {
        emitln("lea %s, [ebp+%d]", x86_reg_str[r], local_offset(a));
    }
}

void x86_store(X86_Reg r, unsigned a)
{
    if (address(a).kind == IdKind) {
        ExecNode *e;
        int clutter_eax;
        char *siz_str, *reg_str;

        clutter_eax = FALSE;
        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION: {
            int cluttered;

            cluttered = 0;
            if (r != X86_ESI) {
                if (!reg_isempty(X86_ESI)) {
                    cluttered |= 1;
                    emitln("push esi");
                } else {
                    modified[X86_ESI] = TRUE;
                }
                emitln("mov esi, %s", x86_reg_str[r]);
            }
            if (addr_reg1(a) != X86_EDI) {
                if (!reg_isempty(X86_EDI)) {
                    cluttered |= 2;
                    emitln("push edi");
                } else {
                    modified[X86_EDI] = TRUE;
                }
                x86_load_addr(X86_EDI, a);
            }
            if (!reg_isempty(X86_ECX)) {
                cluttered |= 4;
                emitln("push ecx");
            }
            emitln("mov ecx, %u", get_sizeof(&e->type));
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
            siz_str = "byte";
            if (r==X86_ESI || r==X86_EDI) {
                emitln("push eax");
                emitln("mov eax, %s", x86_reg_str[r]);
                reg_str = "al";
                clutter_eax = TRUE;
            } else {
                reg_str = x86_lbreg_str[r];
            }
            break;
        default:
            siz_str = "dword";
            reg_str = x86_reg_str[r];
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) /* static local */
                emitln("mov %s [$%s@%s], %s", siz_str, curr_func, e->attr.str, reg_str);
            else /* global */
                emitln("mov %s [$%s], %s", siz_str, e->attr.str, reg_str);
        } else { /* parameter or local */
            emitln("mov %s [ebp+%d], %s", siz_str, local_offset(a), reg_str);
        }

        if (clutter_eax)
            emitln("pop eax");
    } else if (address(a).kind == TempKind) {
        emitln("mov dword [ebp+%d], %s", get_temp_offs(a), x86_reg_str[r]);
    }
}

void x86_store2(X86_Reg2 r, unsigned a)
{
    if (address(a).kind == IdKind) {
        ExecNode *e;

        e = address(a).cont.var.e;
        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) {
                emitln("mov dword [$%s@%s], %s", curr_func, e->attr.str, x86_reg_str[r.r1]);
                emitln("mov dword [$%s@%s+4], %s", curr_func, e->attr.str, x86_reg_str[r.r2]);
            } else {
                emitln("mov dword [$%s], %s", e->attr.str, x86_reg_str[r.r1]);
                emitln("mov dword [$%s+4], %s", e->attr.str, x86_reg_str[r.r2]);
            }
        } else {
            emitln("mov dword [ebp+%d], %s", local_offset(a), x86_reg_str[r.r1]);
            emitln("mov dword [ebp+%d], %s", local_offset(a)+4, x86_reg_str[r.r2]);
        }
    } else if (address(a).kind == TempKind) {
        int offs;

        offs = get_temp_offs(a);
        emitln("mov dword [ebp+%d], %s", offs, x86_reg_str[r.r1]);
        emitln("mov dword [ebp+%d], %s", offs+4, x86_reg_str[r.r2]);
    }
}

void x86_compare_against_constant(unsigned a, unsigned c)
{
    if (address(a).kind == IConstKind) {
        assert(0); /* can be folded */
    } else if (address(a).kind == StrLitKind) {
        assert(0); /* can be folded */
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *siz_str;

        if (addr_reg1(a) != -1) {
            emitln("cmp %s, %u", x86_reg_str[addr_reg1(a)], c);
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
            if (e->attr.var.linkage == LINKAGE_NONE)
                emitln("cmp %s [$%s@%s], %u", siz_str, curr_func, e->attr.str, c);
            else
                emitln("cmp %s [$%s], %u", siz_str, e->attr.str, c);
        } else {
            emitln("cmp %s [ebp+%d], %u", siz_str, local_offset(a), c);
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg1(a) != -1)
            emitln("cmp %s, %u", x86_reg_str[addr_reg1(a)], c);
        else
            emitln("cmp dword [ebp+%d], %u", get_temp_offs(a), c);
    }
}

static void update_arg_descriptors(unsigned arg, unsigned char liveness, int next_use)
{
    if (const_addr(arg) || next_use)
        return;

    if (addr_reg1(arg) != -1) {
        if (liveness) { /* spill */
            if (addr_reg2(arg) != -1)
                x86_store2(addr_reg(arg), arg);
            else
                x86_store(addr_reg1(arg), arg);
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

static void update_tar_descriptors(X86_Reg res, unsigned tar, unsigned char liveness, int next_use)
{
    /* Note:
        maintain the order of the operations for x86_store()
        to work correctly with struct operands.
    */

    addr_reg1(tar) = res;
    reg_descr_tab[res] = tar;

    if (!next_use) {
        if (liveness) /* spill */
            x86_store(res, tar);
        addr_reg1(tar) = -1;
        reg_descr_tab[res] = 0;
    }
}

static void update_tar_descriptors2(X86_Reg2 res, unsigned tar, unsigned char liveness, int next_use)
{
    /* Note:
        maintain the order of the operations for x86_store()
        to work correctly with struct operands.
    */

    addr_reg1(tar) = res.r1;
    addr_reg2(tar) = res.r2;
    reg_descr_tab[res.r1] = tar;
    reg_descr_tab[res.r2] = tar;

    if (!next_use) {
        if (liveness) /* spill */
            x86_store2(res, tar);
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

#define emit_lab(n)         emitln(".L%d:", (int)n)
#define emit_jmp(target)    emitln("jmp .L%d", (int)target)
#define emit_jmpeq(target)  emitln("je .L%d", (int)target)
#define emit_jmpneq(target) emitln("jne .L%d", (int)target)
/* signed */
#define emit_jl(target)     emitln("jl .L%d", (int)target)
#define emit_jg(target)     emitln("jg .L%d", (int)target)
#define emit_jle(target)    emitln("jle .L%d", (int)target)
#define emit_jge(target)    emitln("jge .L%d", (int)target)
/* unsigned */
#define emit_jb(target)     emitln("jb .L%d", (int)target)
#define emit_ja(target)     emitln("ja .L%d", (int)target)
#define emit_jbe(target)    emitln("jbe .L%d", (int)target)
#define emit_jae(target)    emitln("jae .L%d", (int)target)

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

static void x86_do_libcall(int i, unsigned tar, unsigned arg1, unsigned arg2, int func)
{
    int nb;

    switch (func) {
    case LibMul:
    case LibSDiv:
    case LibUDiv:
    case LibSMod:
    case LibUMod: {
        char **op;
        X86_Reg2 res = { X86_EAX, X86_EDX };

        nb = 0;
        op = x86_get_operand2(arg2);
        emitln("push %s", op[1]);
        emitln("push %s", op[0]);
        nb += 8;
        op = x86_get_operand2(arg1);
        emitln("push %s", op[1]);
        emitln("push %s", op[0]);
        nb += 8;

        spill_all();
        emitln("call %s", libfuncs[func]);
        emitln("add esp, %d", nb);
        UPDATE_ADDRESSES2(res);
    }
        break;

    case LibShL:
    case LibSShR:
    case LibUShR: {
        char **op;
        X86_Reg2 res = { X86_EAX, X86_EDX };

        nb = 0;
        emitln("push %s", x86_get_operand(arg2));
        nb += 4;
        op = x86_get_operand2(arg1);
        emitln("push %s", op[1]);
        emitln("push %s", op[0]);
        nb += 8;

        spill_all();
        emitln("call %s", libfuncs[func]);
        emitln("add esp, %d", nb);
        UPDATE_ADDRESSES2(res);
    }
        break;

    case LibNot: {
        char **op;
        X86_Reg res = X86_EAX;

        nb = 0;
        emitln("push dword 0");
        emitln("push dword 0");
        nb += 8;
        op = x86_get_operand2(arg1);
        emitln("push %s", op[1]);
        emitln("push %s", op[0]);
        nb += 8;

        spill_all();
        emitln("call __lux_ucmp64");
        emitln("add esp, %d", nb);

        emitln("cmp eax, 1");
        emitln("sete al");
        emitln("movzx eax, al");
        UPDATE_ADDRESSES_UNARY(res);
    }
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
    case LibULET: {
        char **op;
        X86_Reg res = X86_EAX;

        nb = 0;
        op = x86_get_operand2(arg2);
        emitln("push %s", op[1]);
        emitln("push %s", op[0]);
        nb += 8;
        op = x86_get_operand2(arg1);
        emitln("push %s", op[1]);
        emitln("push %s", op[0]);
        nb += 8;

        spill_all();
        emitln("call %s", libfuncs[func]);
        emitln("add esp, %d", nb);

        switch (func) {
        case LibEq:
            emitln("cmp eax, 1");
            emitln("sete al");
            emitln("movzx eax, al");
            break;
        case LibNeq:
            emitln("cmp eax, 1");
            emitln("setne al");
            emitln("movzx eax, al");
            break;
        case LibULT:
        case LibSLT:
            emitln("cmp eax, 4");
            emitln("sete al");
            emitln("movzx eax, al");
            break;
        case LibUGT:
        case LibSGT:
            emitln("cmp eax, 2");
            emitln("sete al");
            emitln("movzx eax, al");
            break;
        case LibUGET:
        case LibSGET:
            emitln("cmp eax, 4");
            emitln("setne al");
            emitln("movzx eax, al");
            break;
        case LibULET:
        case LibSLET:
            emitln("cmp eax, 2");
            emitln("setne al");
            emitln("movzx eax, al");
            break;
        }
        UPDATE_ADDRESSES(res);
    }
        break;
    }
}

static void x86_add(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        X86_Reg2 res;

        res = get_reg2(i);
        x86_load2(res, arg1);
        op = x86_get_operand2(arg2);
        emitln("add %s, %s", x86_reg_str[res.r1], op[0]);
        emitln("adc %s, %s", x86_reg_str[res.r2], op[1]);
        UPDATE_ADDRESSES2(res);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        pin_reg(res);
        emitln("add %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void x86_sub(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        X86_Reg2 res;

        res = get_reg2(i);
        x86_load2(res, arg1);
        op = x86_get_operand2(arg2);
        emitln("sub %s, %s", x86_reg_str[res.r1], op[0]);
        emitln("sbb %s, %s", x86_reg_str[res.r2], op[1]);
        UPDATE_ADDRESSES2(res);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        pin_reg(res);
        emitln("sub %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void x86_mul(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        x86_do_libcall(i, tar, arg1, arg2, LibMul);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        pin_reg(res);
        emitln("imul %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void x86_div_rem(X86_Reg res, int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        int f;

        if (is_signed_int(cat))
            f = (res == X86_EAX) ? LibSDiv : LibSMod;
        else
            f = (res == X86_EAX) ? LibUDiv : LibUMod;
        x86_do_libcall(i, tar, arg1, arg2, f);
    } else {
        char *instr, *divop;

        if (get_reg(i) != X86_EAX)
            spill_reg(X86_EAX);
        x86_load(X86_EAX, arg1);
        pin_reg(X86_EAX);
        spill_reg(X86_EDX);
        pin_reg(X86_EDX);
        if (is_unsigned_int(cat)) {
            emitln("xor edx, edx");
            instr = "div";
        } else {
            emitln("cdq");
            instr = "idiv";
        }
        if (address(arg2).kind != IConstKind) {
            divop = x86_get_operand(arg2);
        } else {
            X86_Reg r;

            r = get_reg0();
            x86_load(r, arg2);
            divop = x86_reg_str[r];
        }
        emitln("%s %s", instr, divop);
        unpin_reg(X86_EAX);
        unpin_reg(X86_EDX);
        UPDATE_ADDRESSES(res);
    }
}

static void x86_div(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_div_rem(X86_EAX, i, tar, arg1, arg2);
}

static void x86_rem(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_div_rem(X86_EDX, i, tar, arg1, arg2);
}

static void x86_shl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        x86_do_libcall(i, tar, arg1, arg2, LibShL);
    } else {
        X86_Reg res;

        if (address(arg2).kind == IConstKind) {
            res = get_reg(i);
            x86_load(res, arg1);
            emitln("sal %s, %u", x86_reg_str[res], (unsigned)address(arg2).cont.uval);
        } else {
            if (addr_reg1(arg2) != X86_ECX) {
                spill_reg(X86_ECX);
                x86_load(X86_ECX, arg2);
            }
            pin_reg(X86_ECX);
            res = get_reg(i);
            x86_load(res, arg1);
            emitln("sal %s, cl", x86_reg_str[res]);
            unpin_reg(X86_ECX);
        }
        UPDATE_ADDRESSES(res);
    }
}

static void x86_shr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        x86_do_libcall(i, tar, arg1, arg2, is_signed_int(cat)?LibSShR:LibUShR);
    } else {
        char *instr;
        X86_Reg res;

        instr = (is_unsigned_int(cat)) ? "shr" : "sar";

        if (address(arg2).kind == IConstKind) {
            res = get_reg(i);
            x86_load(res, arg1);
            emitln("%s %s, %u", instr, x86_reg_str[res], (unsigned)address(arg2).cont.uval);
        } else {
            if (addr_reg1(arg2) != X86_ECX) {
                spill_reg(X86_ECX);
                x86_load(X86_ECX, arg2);
            }
            pin_reg(X86_ECX);
            res = get_reg(i);
            x86_load(res, arg1);
            emitln("%s %s, cl", instr, x86_reg_str[res]);
            unpin_reg(X86_ECX);
        }
        UPDATE_ADDRESSES(res);
    }
}

static void x86_and(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        X86_Reg2 res;

        res = get_reg2(i);
        x86_load2(res, arg1);
        op = x86_get_operand2(arg2);
        emitln("and %s, %s", x86_reg_str[res.r1], op[0]);
        emitln("and %s, %s", x86_reg_str[res.r2], op[1]);
        UPDATE_ADDRESSES2(res);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        pin_reg(res);
        emitln("and %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void x86_or(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        X86_Reg2 res;

        res = get_reg2(i);
        x86_load2(res, arg1);
        op = x86_get_operand2(arg2);
        emitln("or %s, %s", x86_reg_str[res.r1], op[0]);
        emitln("or %s, %s", x86_reg_str[res.r2], op[1]);
        UPDATE_ADDRESSES2(res);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        pin_reg(res);
        emitln("or %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        UPDATE_ADDRESSES(res);
    }
}

static void x86_xor(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        char **op;
        X86_Reg2 res;

        res = get_reg2(i);
        x86_load2(res, arg1);
        op = x86_get_operand2(arg2);
        emitln("xor %s, %s", x86_reg_str[res.r1], op[0]);
        emitln("xor %s, %s", x86_reg_str[res.r2], op[1]);
        UPDATE_ADDRESSES2(res);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        pin_reg(res);
        emitln("xor %s, %s", x86_reg_str[res], x86_get_operand(arg2));
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

        assert(j <= func_last_quad); /* this is overkill, the BB's last quad should be enough */
        if (instruction(j).op!=OpCBr || address_nid(instruction(j).arg1)!=nid)
            continue;

        tar_2 = instruction(j).tar;
        tar_3 = instruction(j+1).tar;
        arg2_2 = instruction(j).arg2;
        if (address(tar_2).cont.val == address(tar_3).cont.val) {
            lab = (int)address(arg2_2).cont.val;
            switch (instruction(i).op) {
            case OpEQ:
                emit_jmpneq(lab);
                break;
            case OpNEQ:
                emit_jmpeq(lab);
                break;
            case OpLT:
                if (flags & IC_SIGNED)
                    emit_jge(lab);
                else
                    emit_jae(lab);
                break;
            case OpLET:
                if (flags & IC_SIGNED)
                    emit_jg(lab);
                else
                    emit_ja(lab);
                break;
            case OpGT:
                if (flags & IC_SIGNED)
                    emit_jle(lab);
                else
                    emit_jbe(lab);
                break;
            case OpGET:
                if (flags & IC_SIGNED)
                    emit_jl(lab);
                else
                    emit_jb(lab);
                break;
            }
        } else {
            lab = (int)address(tar_2).cont.val;
            switch (instruction(i).op) {
            case OpEQ:
                emit_jmpeq(lab);
                break;
            case OpNEQ:
                emit_jmpneq(lab);
                break;
            case OpLT:
                if (flags & IC_SIGNED)
                    emit_jl(lab);
                else
                    emit_jb(lab);
                break;
            case OpLET:
                if (flags & IC_SIGNED)
                    emit_jle(lab);
                else
                    emit_jbe(lab);
                break;
            case OpGT:
                if (flags & IC_SIGNED)
                    emit_jg(lab);
                else
                    emit_ja(lab);
                break;
            case OpGET:
                if (flags & IC_SIGNED)
                    emit_jge(lab);
                else
                    emit_jae(lab);
                break;
            }
        }
        instruction(j).op = OpNOp;
        break;
    }
}

static void x86_eq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        x86_do_libcall(i, tar, arg1, arg2, LibEq);
    } else {
        X86_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            x86_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("sete %s", x86_lbreg_str[res]);
            emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void x86_neq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        x86_do_libcall(i, tar, arg1, arg2, LibNeq);
    } else {
        X86_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            x86_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("setne %s", x86_lbreg_str[res]);
            emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void x86_lt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        x86_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSLT:LibULT);
    } else {
        X86_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            x86_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("set%s %s", (flags&IC_SIGNED)?"l":"b", x86_lbreg_str[res]);
            emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void x86_let(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        x86_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSLET:LibULET);
    } else {
        X86_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            x86_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("set%s %s", (flags&IC_SIGNED)?"le":"be", x86_lbreg_str[res]);
            emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void x86_gt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        x86_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSGT:LibUGT);
    } else {
        X86_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            x86_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("set%s %s", (flags&IC_SIGNED)?"g":"a", x86_lbreg_str[res]);
            emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void x86_get(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;

    flags = (long)instruction(i).type;
    if (flags & IC_WIDE) {
        x86_do_libcall(i, tar, arg1, arg2, (flags&IC_SIGNED)?LibSGET:LibUGET);
    } else {
        X86_Reg res;

        if (flags&IC_STORE || const_addr(arg1) || addr_reg1(arg1)==-1) {
            res = get_reg(i);
            x86_load(res, arg1);
        } else {
            res = addr_reg1(arg1);
        }
        pin_reg(res);
        emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
        unpin_reg(res);
        if (flags & IC_STORE) {
            emitln("set%s %s", (flags&IC_SIGNED)?"ge":"ae", x86_lbreg_str[res]);
            emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
            UPDATE_ADDRESSES(res);
        } else {
            do_relop_jump(i, flags, tar, arg1, arg2);
        }
    }
}

static void x86_neg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        X86_Reg2 res;

        res = get_reg2(i);
        x86_load2(res, arg1);
        emitln("neg %s", x86_reg_str[res.r1]);
        emitln("adc %s, 0", x86_reg_str[res.r2]);
        emitln("neg %s", x86_reg_str[res.r2]);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        emitln("neg %s", x86_reg_str[res]);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void x86_cmpl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        X86_Reg2 res;

        res = get_reg2(i);
        x86_load2(res, arg1);
        emitln("not %s", x86_reg_str[res.r1]);
        emitln("not %s", x86_reg_str[res.r2]);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        emitln("not %s", x86_reg_str[res]);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void x86_not(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        x86_do_libcall(i, tar, arg1, arg2, LibNot);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        emitln("cmp %s, 0", x86_reg_str[res]);
        emitln("sete %s", x86_lbreg_str[res]);
        emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void x86_ch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    if (res==X86_ESI || res==X86_EDI) {
        emitln("push eax");
        emitln("mov eax, %s", x86_reg_str[res]);
        emitln("movsx %s, al", x86_reg_str[res]);
        emitln("pop eax");
    } else {
        emitln("movsx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
    }
    UPDATE_ADDRESSES_UNARY(res);
}

static void x86_uch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    if (res==X86_ESI || res==X86_EDI) {
        emitln("push eax");
        emitln("mov eax, %s", x86_reg_str[res]);
        emitln("movzx %s, al", x86_reg_str[res]);
        emitln("pop eax");
    } else {
        emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
    }
    UPDATE_ADDRESSES_UNARY(res);
}

static void x86_sh(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movsx %s, %s", x86_reg_str[res], x86_lwreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x86_ush(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movzx %s, %s", x86_reg_str[res], x86_lwreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x86_llsx(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg2 res;
    char *r1, *r2;

    res = get_reg2(i);
    x86_load(res.r1, arg1);
    r1 = x86_reg_str[res.r1];
    r2 = x86_reg_str[res.r2];
    emitln("mov %s, %s", r2, r1);
    emitln("sar %s, 31", r2);
    UPDATE_ADDRESSES_UNARY2(res);
}

static void x86_llzx(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg2 res;

    res = get_reg2(i);
    x86_load(res.r1, arg1);
    emitln("xor %s, %s", x86_reg_str[res.r2], x86_reg_str[res.r2]);
    UPDATE_ADDRESSES_UNARY2(res);
}

static void x86_addr_of(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg0();
    x86_load_addr(res, arg1);
    update_tar_descriptors(res, tar, tar_liveness(i), tar_next_use(i));
}

static void x86_ind(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    /* spill any target currently in a register */
    spill_aliased_objects();

    if (ISLL(instruction(i).type)) {
        X86_Reg2 res;

        res = get_reg2(i);
        x86_load(res.r1, arg1);
        emitln("mov %s, dword [%s+4]", x86_reg_str[res.r2], x86_reg_str[res.r1]);
        emitln("mov %s, dword [%s]", x86_reg_str[res.r1], x86_reg_str[res.r1]);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        X86_Reg res;
        char *reg_str;

        res = get_reg(i);
        x86_load(res, arg1);
        reg_str = x86_reg_str[res];
        switch (cat) {
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
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void x86_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        X86_Reg2 res;

        res = get_reg2(i);
        x86_load2(res, arg1);
        UPDATE_ADDRESSES_UNARY2(res);
    } else {
        X86_Reg res;

        res = get_reg(i);
        x86_load(res, arg1);
        UPDATE_ADDRESSES_UNARY(res);
    }
}

static void x86_pre_call(int i)
{
    Token cat;

    spill_all();
    if ((cat=get_type_category(instruction(i).type))==TOK_STRUCT || cat==TOK_UNION) {
        unsigned siz;

        siz = get_sizeof(instruction(i).type);
        if (siz > temp_struct_size)
            temp_struct_size = siz;
        emit("lea eax, [ebp+");
        calls_to_fix[calls_to_fix_counter++] = string_get_pos(func_body);
        emitln("XXXXXXXXXXXXXXXX");
        emitln("push eax");
    }
}

static void x86_post_call(unsigned arg2)
{
    int na, nb;

    na = (int)address(arg2).cont.val;
    nb = 0;
    while (na--)
        nb += arg_stack[--arg_stack_top];
    assert(arg_stack_top >= 0);
    if (nb)
        emitln("add esp, %d", nb);
}

static void x86_indcall(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_pre_call(i);
    emitln("call %s", x86_get_operand(arg1));
    x86_post_call(arg2);
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    if (tar) {
        Token cat;

        if (ISLL(instruction(i).type)) {
            X86_Reg2 r = { X86_EAX, X86_EDX };

            update_tar_descriptors2(r, tar, tar_liveness(i), tar_next_use(i));
        } else {
            update_tar_descriptors(X86_EAX, tar, tar_liveness(i), tar_next_use(i));
        }
    }
}

static void x86_call(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_pre_call(i);
    emitln("call $%s", address(arg1).cont.var.e->attr.str);
    x86_post_call(arg2);
    if (tar) {
        Token cat;

        if (ISLL(instruction(i).type)) {
            X86_Reg2 r = { X86_EAX, X86_EDX };

            update_tar_descriptors2(r, tar, tar_liveness(i), tar_next_use(i));
        } else {
            update_tar_descriptors(X86_EAX, tar, tar_liveness(i), tar_next_use(i));
        }
    }
}

static void x86_ind_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X86_Reg pr;
    char *siz_str;

    /* force the reload of any target currently in a register */
    spill_aliased_objects();

    if (ISLL(instruction(i).type)) {
        if (addr_reg1(arg1) == -1) {
            pr = get_reg(i);
            x86_load(pr, arg1);
            pin_reg(pr);
        } else {
            pr = addr_reg1(arg1);
        }

        if (address(arg2).kind == IConstKind) {
            unsigned *p;

            p = (unsigned *)&address(arg2).cont.val;
            emitln("mov dword [%s], %u", x86_reg_str[pr], p[0]);
            emitln("mov dword [%s+4], %u", x86_reg_str[pr], p[1]);
        } else if (address(arg2).kind == StrLitKind) {
            emitln("mov dword [%s], _@S%d", x86_reg_str[pr], new_string_literal(arg2));
            emitln("mov dword [%s+4], 0", x86_reg_str[pr]);
        } else {
            X86_Reg2 r;

            if (addr_reg1(arg2) == -1) {
                r.r1 = get_reg0();
                pin_reg(r.r1);
                r.r2 = get_reg0();
                unpin_reg(r.r1);
                x86_load2(r, arg2);
            } else {
                r.r1 = addr_reg1(arg2);
                r.r2 = addr_reg2(arg2);
                assert(r.r2 != -1);
            }
            emitln("mov dword [%s], %s", x86_reg_str[pr], x86_reg_str[r.r1]);
            emitln("mov dword [%s+4], %s", x86_reg_str[pr], x86_reg_str[r.r2]);
        }
        unpin_reg(pr);
        goto done;
    } else if (cat==TOK_STRUCT || cat==TOK_UNION) {
        int cluttered;

        cluttered = 0;
        if (addr_reg1(arg2) != X86_ESI) {
            if (!reg_isempty(X86_ESI)) {
                cluttered |= 1;
                emitln("push esi");
            } else {
                modified[X86_ESI] = TRUE;
            }
            x86_load(X86_ESI, arg2);
        }
        if (addr_reg1(arg1) != X86_EDI) {
            if (!reg_isempty(X86_EDI)) {
                cluttered |= 2;
                emitln("push edi");
            } else {
                modified[X86_EDI] = TRUE;
            }
            x86_load(X86_EDI, arg1);
        }
        if (!reg_isempty(X86_ECX)) {
            cluttered |= 4;
            emitln("push ecx");
        }
        emitln("mov ecx, %u", get_sizeof(instruction(i).type));
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

    /*
     * <= 4 bytes scalar indirect assignment.
     */

    if (addr_reg1(arg1) == -1) {
        pr = get_reg(i);
        x86_load(pr, arg1);
        pin_reg(pr);
    } else {
        pr = addr_reg1(arg1);
    }

    switch (cat) {
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

    if (address(arg2).kind == IConstKind) {
        emitln("mov %s [%s], %u", siz_str, x86_reg_str[pr], (unsigned)address(arg2).cont.uval);
    } else if (address(arg2).kind == StrLitKind) {
        emitln("mov %s [%s], _@S%d", siz_str, x86_reg_str[pr], new_string_literal(arg2));
    } else {
        X86_Reg r;
        char *reg_str;
        int clutter_eax;

        clutter_eax = FALSE;

        if (addr_reg1(arg2) == -1) {
            r = get_reg0();
            x86_load(r, arg2);
        } else {
            r = addr_reg1(arg2);
        }
        switch (cat) {
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            reg_str = x86_lwreg_str[r];
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR:
            if (r==X86_ESI || r==X86_EDI) {
                emitln("push eax");
                emitln("mov eax, %s", x86_reg_str[r]);
                reg_str = "al";
                clutter_eax = TRUE;
            } else {
                reg_str = x86_lbreg_str[r];
            }
            break;
        default:
            reg_str = x86_reg_str[r];
            break;
        }
        emitln("mov %s [%s], %s", siz_str, x86_reg_str[pr], reg_str);
        if (clutter_eax)
            emitln("pop eax");
    }
    unpin_reg(pr);
done:
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    update_arg_descriptors(arg2, arg2_liveness(i), arg2_next_use(i));
}

static void x86_lab(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    emit_lab(address(tar).cont.val);
}

static void x86_jmp(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (instruction(i+1).op == OpLab
    && address(instruction(i+1).tar).cont.val == address(tar).cont.val)
        return;
    emit_jmp(address(tar).cont.val);
}

static void x86_arg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    Declaration *ty;

    ty = instruction(i).type;
    assert(ty->idl==NULL || ty->idl->op!=TOK_ID);

    if (ISLL(ty)) {
        char **op;

        op = x86_get_operand2(arg1);
        emitln("push %s", op[1]);
        emitln("push %s", op[0]);
        arg_stack[arg_stack_top++] = 8;
    } else if (cat==TOK_STRUCT || cat==TOK_UNION) {
        unsigned siz, asiz;
        int cluttered, savnb;

        siz = get_sizeof(ty);
        asiz = round_up(siz, 4);
        emitln("sub esp, %u", asiz);
        arg_stack[arg_stack_top++] = asiz;

        cluttered = savnb = 0;
        if (addr_reg1(arg1) != X86_ESI) {
            if (!reg_isempty(X86_ESI)) {
                cluttered |= 1;
                emitln("push esi");
                savnb += 4;
            } else {
                modified[X86_ESI] = TRUE;
            }
            x86_load(X86_ESI, arg1);
        }
        if (!reg_isempty(X86_EDI)) {
            cluttered |= 2;
            emitln("push edi");
            savnb += 4;
        } else {
            modified[X86_EDI] = TRUE;
        }
        if (!savnb)
            emitln("mov edi, esp");
        else
            emitln("lea edi, [esp+%d]", savnb);
        if (!reg_isempty(X86_ECX)) {
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
    } else {
        emitln("push %s", x86_get_operand(arg1));
        arg_stack[arg_stack_top++] = 4;
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

static void x86_ret(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (qword_return) {
        X86_Reg2 r = { X86_EAX, X86_EDX };

        x86_load2(r, arg1);
    } else if (big_return) {
        unsigned siz;

        siz = get_sizeof(instruction(i).type);
        /*if (!reg_isempty(X86_ESI))
            spill_reg(X86_ESI);*/
        x86_load(X86_ESI, arg1);
        modified[X86_ESI] = TRUE;
        if (!reg_isempty(X86_EDI))
            spill_reg(X86_EDI);
        emitln("mov edi, dword [ebp+-4]");
        modified[X86_EDI] = TRUE;
        if (!reg_isempty(X86_ECX))
            spill_reg(X86_ECX);
        emitln("mov ecx, %u", siz);
        emitln("rep movsb");
        /*if (!reg_isempty(X86_EAX))
            spill_reg(X86_EAX);
        emitln("mov eax, dword [ebp-4]");*/
    } else {
        x86_load(X86_EAX, arg1);
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

static void x86_cbr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;

    if (ISLL(instruction(i).type)) {
        X86_Reg2 r;

        r = get_reg2(i);
        x86_load2(r, arg1);
        emitln("or %s, %s", x86_reg_str[r.r1], x86_reg_str[r.r2]);
        emitln("cmp %s, 0", x86_reg_str[r.r1]);
    } else {
        x86_compare_against_constant(arg1, 0);
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i)); /* do any spilling before the jumps */
    if (address(tar).cont.val == address(instruction(i+1).tar).cont.val)
        emit_jmpeq(address(arg2).cont.val);
    else
        emit_jmpneq(address(tar).cont.val);
}

static void x86_nop(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void x86_do_switch64(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg2 res;
    static unsigned nlab;

    if (addr_reg1(arg1) == -1) {
        res = get_reg2(i);
        x86_load2(res, arg1);
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
        emitln("cmp %s, %u", x86_reg_str[res.r1], p[0]);
        emitln("jne .sw64L%u", nlab);
        emitln("cmp %s, %u", x86_reg_str[res.r2], p[1]);
        emit_jmpeq(address(arg1).cont.val);
        emitln(".sw64L%u:", nlab);
        ++nlab;
    }
}

static void x86_switch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /*
     * Note:
     *  - The default case is the last case after the 'OpSwitch' instruction.
     */
    Token cat;
    X86_Reg res;
    long def_val;
    Arena *lab_arena;
    char **jmp_tab, def_lab[16];
    long ncase, min, max, interval_size, holes;

    if (ISLL(instruction(i).type)) {
        x86_do_switch64(i, tar, arg1, arg2);
        return;
    }

    if (addr_reg1(arg1) == -1) {
        res = get_reg(i);
        x86_load(res, arg1);
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
#if 0
        fprintf(stderr, "min=%d\n", min);
        fprintf(stderr, "max=%d\n", max);
        fprintf(stderr, "interval=%d\n", interval_size);
        fprintf(stderr, "holes=%d\n", holes);
#endif
        if (holes <= JMP_TAB_MAX_HOLES)
            goto jump_table;
    }
    goto linear_search;

jump_table:
    emitln("cmp %s, %ld", x86_reg_str[res], min);
    emit_jl(def_val);
    emitln("cmp %s, %ld", x86_reg_str[res], max);
    emit_jg(def_val);
    if (min != 0)
        emitln("sub %s, %ld", x86_reg_str[res], min);
    emitln("jmp [%s*4+.jt%d]", x86_reg_str[res], jump_tables_counter);

    /* build jump table */
    jmp_tab = calloc(interval_size, sizeof(char *));
    lab_arena = arena_new(interval_size*16, FALSE);
    sprintf(def_lab, ".L%ld", def_val);
    for (;; i++) {
        char *s;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;
        if (address(arg2).cont.val)
            break;
        s = arena_alloc(lab_arena, 16);
        sprintf(s, ".L%d", (int)address(arg1).cont.val);
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
    emitln(".jt%d:", jump_tables_counter++);
    for (i = 0; i < interval_size; i++)
        emitln("dd %s", jmp_tab[i]);
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

        emitln("cmp %s, %u", x86_reg_str[res], (unsigned)address(tar).cont.uval);
        emit_jmpeq(address(arg1).cont.val);
    }
}

static void x86_case(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void x86_begarg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void (*instruction_handlers[])(int, unsigned, unsigned, unsigned) = {
    x86_add, x86_sub, x86_mul, x86_div,
    x86_rem, x86_shl, x86_shr, x86_and,
    x86_or, x86_xor, x86_eq, x86_neq,
    x86_lt, x86_let, x86_gt, x86_get,

    x86_neg, x86_cmpl, x86_not, x86_ch,
    x86_uch, x86_sh, x86_ush, x86_llsx,
    x86_llzx, x86_addr_of, x86_ind, x86_asn,
    x86_call, x86_indcall,

    x86_ind_asn, x86_lab, x86_jmp, x86_arg,
    x86_ret, x86_switch, x86_case, x86_cbr,
    x86_begarg, x86_nop
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
        |               Callee save registers                       |
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
        emit_prologln("global $%s", curr_func);
    emit_prologln("$%s:", curr_func);
    if (big_return) {
        emit_prologln("pop eax");
        emit_prologln("xchg [esp], eax");
    }
    emit_prologln("push ebp");
    emit_prologln("mov ebp, esp");

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
    size_of_local_area -= round_up(temp_struct_size, 4);
    pos_tmp = string_get_pos(func_body);
    while (--calls_to_fix_counter >= 0) {
        int n;
        char *s;

        string_set_pos(func_body, calls_to_fix[calls_to_fix_counter]);
        s = string_curr(func_body);
        n = sprintf(s, "%d", size_of_local_area);
        s[n++] = ']';
        for (; s[n] == 'X'; n++)
            s[n] = ' ';
    }
    string_set_pos(func_body, pos_tmp);

    if (size_of_local_area)
        emit_prologln("sub esp, %d", -size_of_local_area);
    if (modified[X86_ESI]) emit_prologln("push esi");
    if (modified[X86_EDI]) emit_prologln("push edi");
    if (modified[X86_EBX]) emit_prologln("push ebx");

    if (big_return) {
        emit_prologln("mov dword [ebp+-4], eax");
        emit_epilogln("mov eax, dword [ebp+-4]");
    }

    if (modified[X86_EBX]) emit_epilogln("pop ebx");
    if (modified[X86_EDI]) emit_epilogln("pop edi");
    if (modified[X86_ESI]) emit_epilogln("pop esi");
    emit_epilogln("mov esp, ebp");
    emit_epilogln("pop ebp");
    emit_epilogln("ret");

    string_write(func_prolog, x86_output_file);
    string_write(func_body, x86_output_file);
    string_write(func_epilog, x86_output_file);

    /* reset everything */
    string_clear(func_prolog);
    string_clear(func_body);
    string_clear(func_epilog);
    temp_struct_size = 0;
    calls_to_fix_counter = 0;
    memset(modified, 0, sizeof(int)*X86_NREG);
    memset(pinned, 0, sizeof(int)*X86_NREG);
    free_all_temps();
#if 1
    memset(addr_descr_tab, -1, nid_counter*sizeof(int));
    memset(reg_descr_tab, 0, sizeof(unsigned)*X86_NREG);
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
void x86_static_expr(ExecNode *e)
{
    switch (e->kind.exp) {
    case OpExp:
        switch (e->attr.op) {
        case TOK_SUBSCRIPT: {
            int pi, ii;

            if (is_integer(get_type_category(&e->child[0]->type)))
                pi = 1, ii = 0;
            else
                pi = 0, ii = 1;
            x86_static_expr(e->child[pi]);
            if (e->child[ii]->attr.val != 0) {
                Declaration ty;

                ty = e->child[pi]->type;
                ty.idl = ty.idl->child;
                emit_decl("+%u*", get_sizeof(&ty));
                x86_static_expr(e->child[ii]);
            }
        }
            break;
        case TOK_DOT:
        case TOK_ARROW:
            if (get_type_category(&e->child[0]->type) != TOK_UNION) {
                StructMember *m;

                m = get_member_descriptor(get_type_spec(e->child[0]->type.decl_specs), e->child[1]->attr.str);
                x86_static_expr(e->child[0]);
                if (m->offset)
                    emit_decl("+%u", m->offset);
            } else {
                x86_static_expr(e->child[0]);
            }
            break;
        case TOK_ADDRESS_OF:
        case TOK_INDIRECTION:
        case TOK_CAST:
            x86_static_expr(e->child[0]);
            break;

        case TOK_PLUS:
            if (is_integer(get_type_category(&e->type))) {
                x86_static_expr(e->child[0]);
                emit_decl("+");
                x86_static_expr(e->child[1]);
            } else {
                int pi, ii;

                if (is_integer(get_type_category(&e->child[0]->type)))
                    pi = 1, ii = 0;
                else
                    pi = 0, ii = 1;
                x86_static_expr(e->child[pi]);
                if (e->child[ii]->attr.val != 0) {
                    Declaration ty;

                    ty = e->child[pi]->type;
                    ty.idl = ty.idl->child;
                    emit_decl("+%u*", get_sizeof(&ty));
                    x86_static_expr(e->child[ii]);
                }
            }
            break;
        case TOK_MINUS:
            if (is_integer(get_type_category(&e->child[0]->type))) { /* int-int */
                x86_static_expr(e->child[0]);
                emit_decl("-");
                x86_static_expr(e->child[1]);
            } else { /* ptr-int */
                x86_static_expr(e->child[0]);
                if (e->child[1]->attr.val != 0) {
                    Declaration ty;

                    ty = e->child[0]->type;
                    ty.idl = ty.idl->child;
                    emit_decl("-%u*", get_sizeof(&ty));
                    x86_static_expr(e->child[1]);
                }
            }
            break;
        case TOK_CONDITIONAL:
            if (e->child[0]->attr.val)
                x86_static_expr(e->child[1]);
            else
                x86_static_expr(e->child[2]);
            break;
        default:
            assert(0);
        }
        break;
    case IConstExp:
        emit_decl("%llu", e->attr.uval);
        break;
    case StrLitExp:
        emit_strln("_@S%d:", string_literals_counter);
        emit_raw_string(str_lits, e->attr.str);
        emit_decl("_@S%d", string_literals_counter++);
        break;
    case IdExp:
        if (e->attr.var.linkage == LINKAGE_NONE)
            emit_decl("$%s@%s", enclosing_function, e->attr.str);
        else
            emit_decl("$%s", e->attr.str);
        break;
    }
}

void x86_static_init(TypeExp *ds, TypeExp *dct, ExecNode *e)
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
                emit_declln("times %u db 0", nelem-n);
        } else {
            /* handle elements with explicit initializer */
            for (e = e->child[0]; e!=NULL /*&& nelem!=0*/; e=e->sibling, --nelem)
                x86_static_init(ds, dct->child, e);

            /* handle elements without explicit initializer */
            if (nelem != 0) {
                Declaration ty;

                ty.decl_specs = ds;
                ty.idl = dct->child;
                emit_declln("align %u", get_alignment(&ty));
                emit_declln("times %u db 0", nelem*get_sizeof(&ty));
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
                x86_static_init(d->decl->decl_specs, dct->child, e);

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
                    emit_declln("align %u", get_alignment(&ty));
                    emit_declln("times %u db 0",  get_sizeof(&ty));

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
        x86_static_init(ts->attr.dl->decl->decl_specs, ts->attr.dl->decl->idl->child, e);
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
            emit_decl("db ");
            break;
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            emit_declln("align 2");
            emit_decl("dw ");
            break;
        case TOK_LONG_LONG:
        case TOK_UNSIGNED_LONG_LONG:
            emit_declln("align 4");
            emit_decl("dq ");
            break;
        default:
            emit_declln("align 4");
            emit_decl("dd ");
            break;
        }
        x86_static_expr(e);
        emit_decl("\n");
    }
}

void x86_allocate_static_objects(void)
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
                emit_declln("align %u", al);
        } else {
            SET_SEGMENT(BSS_SEG, emit_declln);
            if (al > 1)
                emit_declln("alignb %u", al);
        }
        if ((enclosing_function=np->enclosing_function) != NULL) { /* static local */
            emit_declln("$%s@%s:", np->enclosing_function, np->declarator->str);
        } else {
            TypeExp *scs;

            if ((scs=get_sto_class_spec(np->decl_specs))==NULL || scs->op!=TOK_STATIC)
                emit_declln("global %s", np->declarator->str);
            emit_declln("$%s:", np->declarator->str);
        }
        if (initzr != NULL)
            x86_static_init(ty.decl_specs, ty.idl, initzr);
        else
            emit_declln("resb %u", get_sizeof(&ty));
    }
}

void x86_cgen(FILE *outf)
{
    unsigned i, j;
    ExternId *ed, **func_def_list, **ext_sym_list;

    x86_output_file = outf;

    /* generate intermediate code and do some analysis */
    ic_main(&func_def_list, &ext_sym_list); //exit(0);
    compute_liveness_and_next_use();

    /* generate assembly */
    asm_decls = string_new(512);
    str_lits = string_new(512);
    func_body = string_new(1024);
    func_prolog = string_new(1024);
    func_epilog = string_new(1024);
    addr_descr_tab = malloc(nid_counter*sizeof(X86_Reg2));
    memset(addr_descr_tab, -1, nid_counter*sizeof(X86_Reg2));
    for (i = 0; (ed=func_def_list[i]) != NULL; i++)
        x86_function_definition(ed->decl_specs, ed->declarator);
    string_free(func_body);
    string_free(func_prolog);
    string_free(func_epilog);

    emit_declln("\n; == objects with static duration");
    x86_allocate_static_objects();

    emit_declln("\n; == extern symbols");
    /* emit extern directives only for those symbols that were referenced */
    for (j = 0; (ed=ext_sym_list[j]) != NULL; j++) {
        int tmp;

        /* get_var_nid() will increment nid_counter when it sees a new identifier */
        tmp = nid_counter;
        get_var_nid(ed->declarator->str, 0);
        if (tmp == nid_counter)
            emit_declln("extern %s", ed->declarator->str);
    }
    /* the front-end may emit calls to memcpy/memset */
    emit_declln("extern memcpy");
    emit_declln("extern memset");
    /* liblux functions */
    if (include_liblux) {
        emit_declln("extern __lux_mul64");
        emit_declln("extern __lux_sdiv64");
        emit_declln("extern __lux_udiv64");
        emit_declln("extern __lux_smod64");
        emit_declln("extern __lux_umod64");
        emit_declln("extern __lux_shl64");
        emit_declln("extern __lux_sshr64");
        emit_declln("extern __lux_ushr64");
        emit_declln("extern __lux_ucmp64");
        emit_declln("extern __lux_scmp64");
    }

    string_write(asm_decls, x86_output_file);
    string_free(asm_decls);

    if (string_literals_counter) {
        fprintf(x86_output_file, "\n; == string literals\n");
        fprintf(x86_output_file, "segment .rodata\n");
        string_write(str_lits, x86_output_file);
    }
    string_free(str_lits);
}
