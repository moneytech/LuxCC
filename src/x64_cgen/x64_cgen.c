/*
 * Simple x64 code generator
 *      IC ==> x64 ASM (NASM syntax).
 *
 * Normative document:
 *  => System V ABI-AMD64: http://www.x86-64.org/documentation/abi.pdf
 * Other documents:
 *  => Calling conventions: http://www.agner.org/optimize/calling_conventions.pdf
 *
 * TOFIX:
 *  - The ABI requires that the stack be aligned to a 16-byte boundary before execute
 *    a call instruction. Currently, this is not enforced and the stack is only guaranteed
 *    to be aligned to a 8-byte boundary (seems like GLIBC has no problem with this).
 */
#define DEBUG 0
#include "x64_cgen.h"
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

typedef enum {
    X64_RAX,
    X64_RBX,
    X64_RCX,
    X64_RDX,
    X64_RSI,
    X64_RDI,
    X64_R8,
    X64_R9,
    X64_R10,
    X64_R11,
    X64_R12,
    X64_R13,
    X64_R14,
    X64_R15,
    X64_NREG,
} X64_Reg;

static int pinned[X64_NREG];
static int modified[X64_NREG];
#define pin_reg(r)      (pinned[r] = TRUE)
#define unpin_reg(r)    (pinned[r] = FALSE)
static char *x64_reg_str[] = {
    "rax",
    "rbx",
    "rcx",
    "rdx",
    "rsi",
    "rdi",
    "r8",
    "r9",
    "r10",
    "r11",
    "r12",
    "r13",
    "r14",
    "r15",
};
static char *x64_ldreg_str[] = {
    "eax",
    "ebx",
    "ecx",
    "edx",
    "esi",
    "edi",
    "r8d",
    "r9d",
    "r10d",
    "r11d",
    "r12d",
    "r13d",
    "r14d",
    "r15d",
};
static char *x64_lwreg_str[] = {
    "ax",
    "bx",
    "cx",
    "dx",
    "si",
    "di",
    "r8w",
    "r9w",
    "r10w",
    "r11w",
    "r12w",
    "r13w",
    "r14w",
    "r15w",
};
static char *x64_lbreg_str[] = {
    "al",
    "bl",
    "cl",
    "dl",
    "sil",
    "dil",
    "r8b",
    "r9b",
    "r10b",
    "r11b",
    "r12b",
    "r13b",
    "r14b",
    "r15b",
};
static X64_Reg x64_arg_reg[] = {
    X64_RDI,
    X64_RSI,
    X64_RDX,
    X64_RCX,
    X64_R8,
    X64_R9,
};

static int size_of_local_area;
static char *curr_func, *enclosing_function;
static unsigned temp_struct_size;
static int big_return;
static int arg_stack[64], arg_stack_top;
static int arg_reg_avail = 6;
static unsigned calls_to_fix[64];
static int calls_to_fix_counter;
static unsigned qrets_to_fix[64];
static int qrets_to_fix_counter;
static unsigned orets_to_fix[64];
static int orets_to_fix_counter;
static int string_literals_counter;
static FILE *x64_output_file;
static int func_last_quad;
static int need_to_extend;

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

static X64_Reg *addr_descr_tab;
static unsigned reg_descr_tab[X64_NREG];
#define addr_reg(a)     (addr_descr_tab[address_nid(a)])
#define reg_isempty(r)  (reg_descr_tab[r] == 0)
static void dump_addr_descr_tab(void);
static void dump_reg_descr_tab(void);

static void spill_reg(X64_Reg r);
static void spill_all(void);
static void spill_aliased_objects(void);
static X64_Reg get_reg(int intr);
static X64_Reg get_reg0(void);

static void x64_load(X64_Reg r, unsigned a);
static void x64_load_addr(X64_Reg r, unsigned a);
static char *x64_get_operand32(unsigned a);
static char *x64_get_operand64(unsigned a);
static void x64_store(X64_Reg r, unsigned a);
static void x64_compare_against_constant(unsigned a, int c);
static void x64_function_definition(TypeExp *decl_specs, TypeExp *header);

static void x64_static_expr(ExecNode *e);
static void x64_static_init(TypeExp *ds, TypeExp *dct, ExecNode *e);
static void x64_allocate_static_objects(void);

typedef struct Temp Temp;
static struct Temp {
    int nid;
    int offs; /* offset from rbp */
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
        X64_Reg ad;

        if ((ad=addr_descr_tab[i]) != -1)
            fprintf(stderr, "%s => %s\n", nid2sid_tab[i], x64_reg_str[ad]);
    }
}

void dump_reg_descr_tab(void)
{
    int i;

    for (i = 0; i < X64_NREG; i++)
        if (!reg_isempty(i))
            fprintf(stderr, "%s => %s\n", x64_reg_str[i], address_sid(reg_descr_tab[i]));
}

void spill_reg(X64_Reg r)
{
    unsigned a;

    if (reg_isempty(r))
        return;

    a = reg_descr_tab[r];
    x64_store(r, a);
    addr_reg(a) = -1;
    reg_descr_tab[r] = 0;
}

void spill_all(void)
{
    int i;

    for (i = 0; i < X64_NREG; i++)
        spill_reg((X64_Reg)i);
}

/*
 * Currently all address-taken variables are considered to be aliased.
 * => Possible improvement: use type information to narrow down the set
 * of objects to spill (see 6.5#7).
 */
void spill_aliased_objects(void)
{
    int i;

    for (i = 0; i < X64_NREG; i++) {
        unsigned a;

        if (reg_isempty(i))
            continue;
        a = reg_descr_tab[i];
        if (address(a).kind==IdKind && bset_member(address_taken_variables, address_nid(a)))
            spill_reg(i);
    }
}

X64_Reg get_reg0(void)
{
    int i;

    /* try to find an empty register */
    for (i = 0; i < X64_NREG; i++) {
        if (!pinned[i] && reg_isempty(i)) {
            modified[i] = TRUE;
            return (X64_Reg)i;
        }
    }

    /* choose an unpinned register and spill its contents */
    for (i = 0; i < X64_NREG; i++) {
        if (!pinned[i]) {
            modified[i] = TRUE;
            spill_reg((X64_Reg)i);
            return (X64_Reg)i;
        }
    }

    assert(0);
    return 0;
}

X64_Reg get_reg(int i)
{
    unsigned arg1;

    arg1 = instruction(i).arg1;
    if (!const_addr(arg1) && addr_reg(arg1)!=-1 && !arg1_liveness(i))
        return addr_reg(arg1);
    return get_reg0();
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

static int x64_islong(Token cat)
{
    switch (cat) {
        case TOK_STAR: case TOK_SUBSCRIPT: case TOK_FUNCTION:
        case TOK_LONG: case TOK_UNSIGNED_LONG:
        case TOK_LONG_LONG: case TOK_UNSIGNED_LONG_LONG:
        return TRUE;
    }
    return FALSE;
}
#define ISLONG(ty) (x64_islong(cat = get_type_category(ty)))
#define local_offset(a) (address(a).cont.var.offset)

void x64_load(X64_Reg r, unsigned a)
{
    need_to_extend = FALSE;

    if (address(a).kind == IConstKind) {
        emitln("mov %s, %lld", x64_reg_str[r], address(a).cont.val);
    } else if (address(a).kind == StrLitKind) {
        emitln("mov %s, _@S%d", x64_reg_str[r], new_string_literal(a));
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *siz_str, *mov_str, *reg_str;

        reg_str = x64_reg_str[r];

        if (addr_reg(a) != -1) {
            if (addr_reg(a) == r)
                ; /* already in the register */
            else
                emitln("mov %s, %s", reg_str, x64_reg_str[addr_reg(a)]);
            need_to_extend = TRUE;
            return;
        }

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
        case TOK_SUBSCRIPT:
        case TOK_FUNCTION:
            x64_load_addr(r, a);
            return;
        case TOK_INT:
        case TOK_ENUM:
            mov_str = "movsx";
            siz_str = "dword";
            break;
        case TOK_UNSIGNED:
            mov_str = "mov";
            siz_str = "dword";
            reg_str = x64_ldreg_str[r];
            break;
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
            siz_str = "qword";
            need_to_extend = TRUE;
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) /* static local */
                emitln("%s %s, %s [$%s@%s]", mov_str, reg_str, siz_str, curr_func, e->attr.str);
            else /* global */
                emitln("%s %s, %s [$%s]", mov_str, reg_str, siz_str, e->attr.str);
        } else { /* parameter or local */
            emitln("%s %s, %s [rbp+%d]", mov_str, reg_str, siz_str, local_offset(a));
        }
    } else if (address(a).kind == TempKind) {
        need_to_extend = TRUE;
        if (addr_reg(a) != -1) {
            if (addr_reg(a) == r)
                return; /* already in the register */
            else
                emitln("mov %s, %s", x64_reg_str[r], x64_reg_str[addr_reg(a)]);
        } else {
            emitln("mov %s, qword [rbp+%d]", x64_reg_str[r], get_temp_offs(a));
        }
    }
}

char *x64_get_operand32(unsigned a)
{
    static char op[256];

    if (address(a).kind == IConstKind) {
        sprintf(op, "%d", (int)address(a).cont.val);
    } else if (address(a).kind == StrLitKind) {
        sprintf(op, "_@S%d", new_string_literal(a)); /* generate R_X86_64_32 reloc */
    } else if (address(a).kind == IdKind) {
        ExecNode *e;

        if (addr_reg(a) != -1)
            return x64_ldreg_str[addr_reg(a)];

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
        case TOK_SUBSCRIPT:
        case TOK_FUNCTION:
            if (e->attr.var.duration == DURATION_STATIC) { /* generate R_X86_64_32 reloc */
                if (e->attr.var.linkage == LINKAGE_NONE)
                    sprintf(op, "$%s@%s", curr_func, e->attr.str);
                else
                    sprintf(op, "$%s", e->attr.str);
                return op;
            } else {
                X64_Reg r;

                r = get_reg0();
                emitln("lea %s, [rbp+%d]", x64_reg_str[r], local_offset(a));
                return x64_ldreg_str[r];
            }
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR: { /* promote to dword */
            X64_Reg r;

            r = get_reg0();
            x64_load(r, a);
            return x64_ldreg_str[r];
        }
        default: /* dword or qword sized, OK */
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE)
                sprintf(op, "dword [$%s@%s]", curr_func, e->attr.str);
            else
                sprintf(op, "dword [$%s]", e->attr.str);
        } else {
            sprintf(op, "dword [rbp+%d]", local_offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg(a) != -1)
            return x64_ldreg_str[addr_reg(a)];
        else
            sprintf(op, "dword [rbp+%d]", get_temp_offs(a));
    }

    return op;
}

char *x64_get_operand64(unsigned a)
{
    static char op[256];

    if (address(a).kind == IConstKind) {
        long long val;

        val = address(a).cont.val;
        if (val>=INT_MIN && val<=INT_MAX) {
            sprintf(op, "%d", (int)val);
        } else {
            X64_Reg r;

            r = get_reg0();
            x64_load(r, a);
            return x64_reg_str[r];
        }
    } else if (address(a).kind == StrLitKind) {
        X64_Reg r;

        r = get_reg0();
        x64_load(r, a);
        return x64_reg_str[r];
    } else if (address(a).kind == IdKind) {
        ExecNode *e;

        if (addr_reg(a) != -1)
            return x64_reg_str[addr_reg(a)];

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION:
        case TOK_SUBSCRIPT:
        case TOK_FUNCTION: {
            X64_Reg r;

            r = get_reg0();
            if (e->attr.var.duration == DURATION_STATIC) {
                if (e->attr.var.linkage == LINKAGE_NONE)
                    sprintf(op, "mov %s, $%s@%s", x64_reg_str[r], curr_func, e->attr.str);
                else
                    sprintf(op, "mov %s, $%s", x64_reg_str[r], e->attr.str);
                return op;
            } else {
                emitln("lea %s, [rbp+%d]", x64_reg_str[r], local_offset(a));
                return x64_reg_str[r];
            }
        }
        case TOK_INT:
        case TOK_ENUM:
        case TOK_UNSIGNED:
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR: { /* promote to qword */
            X64_Reg r;

            r = get_reg0();
            x64_load(r, a);
            return x64_reg_str[r];
        }
        default: /* qword sized, OK */
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE)
                sprintf(op, "qword [$%s@%s]", curr_func, e->attr.str);
            else
                sprintf(op, "qword [$%s]", e->attr.str);
        } else {
            sprintf(op, "qword [rbp+%d]", local_offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg(a) != -1)
            return x64_reg_str[addr_reg(a)];
        else
            sprintf(op, "qword [rbp+%d]", get_temp_offs(a));
    }

    return op;
}

void x64_load_addr(X64_Reg r, unsigned a)
{
    ExecNode *e;

    e = address(a).cont.var.e;
    if (e->attr.var.duration == DURATION_STATIC) {
        if (e->attr.var.linkage == LINKAGE_NONE)
            emitln("mov %s, $%s@%s", x64_reg_str[r], curr_func, e->attr.str);
        else
            emitln("mov %s, $%s", x64_reg_str[r], e->attr.str);
    } else {
        emitln("lea %s, [rbp+%d]", x64_reg_str[r], local_offset(a));
    }
}

void x64_store(X64_Reg r, unsigned a)
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
            if (r != X64_RSI) {
                if (!reg_isempty(X64_RSI)) {
                    cluttered |= 1;
                    emitln("push rsi");
                }
                emitln("mov rsi, %s", x64_reg_str[r]);
            }
            if (addr_reg(a) != X64_RDI) {
                if (!reg_isempty(X64_RDI)) {
                    cluttered |= 2;
                    emitln("push rdi");
                }
                x64_load_addr(X64_RDI, a);
            }
            if (!reg_isempty(X64_RCX)) {
                cluttered |= 4;
                emitln("push rcx");
            }
            emitln("mov ecx, %u", get_sizeof(&e->type));
            emitln("rep movsb");
            /* restore all */
            if (cluttered & 4)
                emitln("pop rcx");
            if (cluttered & 2)
                emitln("pop rdi");
            if (cluttered & 1)
                emitln("pop rsi");
        }
            return;
        case TOK_INT:
        case TOK_ENUM:
        case TOK_UNSIGNED:
            siz_str = "dword";
            reg_str = x64_ldreg_str[r];
            break;
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            siz_str = "word";
            reg_str = x64_lwreg_str[r];
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR:
            siz_str = "byte";
            reg_str = x64_lbreg_str[r];
            break;
        default:
            siz_str = "qword";
            reg_str = x64_reg_str[r];
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE) /* static local */
                emitln("mov %s [$%s@%s], %s", siz_str, curr_func, e->attr.str, reg_str);
            else /* global */
                emitln("mov %s [$%s], %s", siz_str, e->attr.str, reg_str);
        } else { /* parameter or local */
            emitln("mov %s [rbp+%d], %s", siz_str, local_offset(a), reg_str);
        }
    } else if (address(a).kind == TempKind) {
        emitln("mov qword [rbp+%d], %s", get_temp_offs(a), x64_reg_str[r]);
    }
}

void x64_compare_against_constant(unsigned a, int c)
{
    if (address(a).kind == IConstKind) {
        assert(0); /* can be folded */
    } else if (address(a).kind == StrLitKind) {
        assert(0); /* can be folded */
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *siz_str;

        if (addr_reg(a) != -1) {
            emitln("cmp %s, %d", x64_reg_str[addr_reg(a)], c);
            return;
        }

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_INT:
        case TOK_ENUM:
        case TOK_UNSIGNED:
            siz_str = "dword";
            break;
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
            siz_str = "qword";
            break;
        }

        if (e->attr.var.duration == DURATION_STATIC) {
            if (e->attr.var.linkage == LINKAGE_NONE)
                emitln("cmp %s [$%s@%s], %d", siz_str, curr_func, e->attr.str, c);
            else
                emitln("cmp %s [$%s], %d", siz_str, e->attr.str, c);
        } else {
            emitln("cmp %s [rbp+%d], %d", siz_str, local_offset(a), c);
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg(a) != -1)
            emitln("cmp %s, %d", x64_reg_str[addr_reg(a)], c);
        else
            emitln("cmp qword [rbp+%d], %d", get_temp_offs(a), c);
    }
}

static void update_arg_descriptors(unsigned arg, unsigned char liveness, int next_use)
{
    if (const_addr(arg) || next_use)
        return;

    if (addr_reg(arg) != -1) {
        if (liveness) /* spill */
            x64_store(addr_reg(arg), arg);
        else if (address(arg).kind == TempKind)
            free_temp(arg);
        reg_descr_tab[addr_reg(arg)] = 0;
        addr_reg(arg) = -1;
    } else {
        if (liveness)
            ;
        else if (address(arg).kind == TempKind)
            free_temp(arg);
    }
}

static void update_tar_descriptors(X64_Reg res, unsigned tar, unsigned char liveness, int next_use)
{
    /* Note:
        maintain the order of the operations for x64_store()
        to work correctly with struct operands.
    */

    addr_reg(tar) = res;
    reg_descr_tab[res] = tar;

    if (!next_use) {
        if (liveness) /* spill */
            x64_store(res, tar);
        addr_reg(tar) = -1;
        reg_descr_tab[res] = 0;
    }
}

#define UPDATE_ADDRESSES(res_reg)\
    do {\
        update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));\
        update_arg_descriptors(arg2, arg2_liveness(i), arg2_next_use(i));\
        update_tar_descriptors(res_reg, tar, tar_liveness(i), tar_next_use(i));\
    } while (0)
#define UPDATE_ADDRESSES_UNARY(res_reg)\
    do {\
        update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));\
        update_tar_descriptors(res_reg, tar, tar_liveness(i), tar_next_use(i));\
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

static void x64_add(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    pin_reg(res);
    if (ISLONG(instruction(i).type))
        emitln("add %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("add %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

static void x64_sub(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    pin_reg(res);
    if (ISLONG(instruction(i).type))
        emitln("sub %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("sub %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

static void x64_mul(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    pin_reg(res);
    if (ISLONG(instruction(i).type))
        emitln("imul %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("imul %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

static void x64_div_rem(X64_Reg res, int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    int islong;
    char *instr, *divop;

    islong = ISLONG(instruction(i).type);

    if (get_reg(i) != X64_RAX)
        spill_reg(X64_RAX);
    x64_load(X64_RAX, arg1);
    pin_reg(X64_RAX);
    spill_reg(X64_RDX);
    pin_reg(X64_RDX);
    if (is_unsigned_int(cat)) {
        emitln("xor edx, edx");
        instr = "div";
    } else {
        emitln("%s", islong?"cqo":"cdq");
        instr = "idiv";
    }
    if (address(arg2).kind != IConstKind) {
        divop = islong?x64_get_operand64(arg2):x64_get_operand32(arg2);
    } else {
        X64_Reg r;

        r = get_reg0();
        x64_load(r, arg2);
        divop = islong?x64_reg_str[r]:x64_ldreg_str[r];
    }
    emitln("%s %s", instr, divop);
    unpin_reg(X64_RAX);
    unpin_reg(X64_RDX);
    UPDATE_ADDRESSES(res);
}

static void x64_div(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x64_div_rem(X64_RAX, i, tar, arg1, arg2);
}

static void x64_rem(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x64_div_rem(X64_RDX, i, tar, arg1, arg2);
}

static void x64_shl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;
    int islong;
    char *reg_str;

    islong = ISLONG(instruction(i).type);

    if (address(arg2).kind == IConstKind) {
        res = get_reg(i);
        reg_str = islong?x64_reg_str[res]:x64_ldreg_str[res];
        x64_load(res, arg1);
        emitln("sal %s, %u", reg_str, (unsigned)address(arg2).cont.uval);
    } else {
        if (addr_reg(arg2) != X64_RCX) {
            spill_reg(X64_RCX);
            x64_load(X64_RCX, arg2);
        }
        pin_reg(X64_RCX);
        res = get_reg(i);
        reg_str = islong?x64_reg_str[res]:x64_ldreg_str[res];
        x64_load(res, arg1);
        emitln("sal %s, cl", reg_str);
        unpin_reg(X64_RCX);
    }
    UPDATE_ADDRESSES(res);
}

static void x64_shr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    char *instr;
    X64_Reg res;
    int islong;
    char *reg_str;

    islong = ISLONG(instruction(i).type);
    instr = is_unsigned_int(cat)?"shr":"sar";

    if (address(arg2).kind == IConstKind) {
        res = get_reg(i);
        reg_str = islong?x64_reg_str[res]:x64_ldreg_str[res];
        x64_load(res, arg1);
        emitln("%s %s, %u", instr, reg_str, (unsigned)address(arg2).cont.uval);
    } else {
        if (addr_reg(arg2) != X64_RCX) {
            spill_reg(X64_RCX);
            x64_load(X64_RCX, arg2);
        }
        pin_reg(X64_RCX);
        res = get_reg(i);
        reg_str = islong?x64_reg_str[res]:x64_ldreg_str[res];
        x64_load(res, arg1);
        emitln("%s %s, cl", instr, reg_str);
        unpin_reg(X64_RCX);
    }
    UPDATE_ADDRESSES(res);
}

static void x64_and(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    pin_reg(res);
    if (ISLONG(instruction(i).type))
        emitln("and %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("and %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

static void x64_or(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    pin_reg(res);
    if (ISLONG(instruction(i).type))
        emitln("or %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("or %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

static void x64_xor(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    pin_reg(res);
    if (ISLONG(instruction(i).type))
        emitln("xor %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("xor %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
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

static void x64_eq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;
    X64_Reg res;

    flags = (long)instruction(i).type;
    if (flags&IC_STORE || const_addr(arg1) || addr_reg(arg1)==-1) {
        res = get_reg(i);
        x64_load(res, arg1);
    } else {
        res = addr_reg(arg1);
    }
    pin_reg(res);
    if (flags & IC_WIDE)
        emitln("cmp %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("cmp %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    if (flags & IC_STORE) {
        emitln("sete %s", x64_lbreg_str[res]);
        emitln("movzx %s, %s", x64_ldreg_str[res], x64_lbreg_str[res]);
        UPDATE_ADDRESSES(res);
    } else {
        do_relop_jump(i, flags, tar, arg1, arg2);
    }
}

static void x64_neq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;
    X64_Reg res;

    flags = (long)instruction(i).type;
    if (flags&IC_STORE || const_addr(arg1) || addr_reg(arg1)==-1) {
        res = get_reg(i);
        x64_load(res, arg1);
    } else {
        res = addr_reg(arg1);
    }
    pin_reg(res);
    if (flags & IC_WIDE)
        emitln("cmp %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("cmp %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    if (flags & IC_STORE) {
        emitln("setne %s", x64_lbreg_str[res]);
        emitln("movzx %s, %s", x64_ldreg_str[res], x64_lbreg_str[res]);
        UPDATE_ADDRESSES(res);
    } else {
        do_relop_jump(i, flags, tar, arg1, arg2);
    }
}

static void x64_lt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;
    X64_Reg res;

    flags = (long)instruction(i).type;
    if (flags&IC_STORE || const_addr(arg1) || addr_reg(arg1)==-1) {
        res = get_reg(i);
        x64_load(res, arg1);
    } else {
        res = addr_reg(arg1);
    }
    pin_reg(res);
    if (flags & IC_WIDE)
        emitln("cmp %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("cmp %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    if (flags & IC_STORE) {
        emitln("set%s %s", (flags&IC_SIGNED)?"l":"b", x64_lbreg_str[res]);
        emitln("movzx %s, %s", x64_ldreg_str[res], x64_lbreg_str[res]);
        UPDATE_ADDRESSES(res);
    } else {
        do_relop_jump(i, flags, tar, arg1, arg2);
    }
}

static void x64_let(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;
    X64_Reg res;

    flags = (long)instruction(i).type;
    if (flags&IC_STORE || const_addr(arg1) || addr_reg(arg1)==-1) {
        res = get_reg(i);
        x64_load(res, arg1);
    } else {
        res = addr_reg(arg1);
    }
    pin_reg(res);
    if (flags & IC_WIDE)
        emitln("cmp %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("cmp %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    if (flags & IC_STORE) {
        emitln("set%s %s", (flags&IC_SIGNED)?"le":"be", x64_lbreg_str[res]);
        emitln("movzx %s, %s", x64_ldreg_str[res], x64_lbreg_str[res]);
        UPDATE_ADDRESSES(res);
    } else {
        do_relop_jump(i, flags, tar, arg1, arg2);
    }
}

static void x64_gt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;
    X64_Reg res;

    flags = (long)instruction(i).type;
    if (flags&IC_STORE || const_addr(arg1) || addr_reg(arg1)==-1) {
        res = get_reg(i);
        x64_load(res, arg1);
    } else {
        res = addr_reg(arg1);
    }
    pin_reg(res);
    if (flags & IC_WIDE)
        emitln("cmp %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("cmp %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    if (flags & IC_STORE) {
        emitln("set%s %s", (flags&IC_SIGNED)?"g":"a", x64_lbreg_str[res]);
        emitln("movzx %s, %s", x64_ldreg_str[res], x64_lbreg_str[res]);
        UPDATE_ADDRESSES(res);
    } else {
        do_relop_jump(i, flags, tar, arg1, arg2);
    }
}

static void x64_get(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    long flags;
    X64_Reg res;

    flags = (long)instruction(i).type;
    if (flags&IC_STORE || const_addr(arg1) || addr_reg(arg1)==-1) {
        res = get_reg(i);
        x64_load(res, arg1);
    } else {
        res = addr_reg(arg1);
    }
    pin_reg(res);
    if (flags & IC_WIDE)
        emitln("cmp %s, %s", x64_reg_str[res], x64_get_operand64(arg2));
    else
        emitln("cmp %s, %s", x64_ldreg_str[res], x64_get_operand32(arg2));
    unpin_reg(res);
    if (flags & IC_STORE) {
        emitln("set%s %s", (flags&IC_SIGNED)?"ge":"ae", x64_lbreg_str[res]);
        emitln("movzx %s, %s", x64_ldreg_str[res], x64_lbreg_str[res]);
        UPDATE_ADDRESSES(res);
    } else {
        do_relop_jump(i, flags, tar, arg1, arg2);
    }
}

static void x64_neg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    emitln("neg %s", ISLONG(instruction(i).type)?x64_reg_str[res]:x64_ldreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_cmpl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    emitln("not %s", ISLONG(instruction(i).type)?x64_reg_str[res]:x64_ldreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_not(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    if (ISLONG(instruction(i).type))
        emitln("cmp %s, 0", x64_reg_str[res]);
    else
        emitln("cmp %s, 0", x64_ldreg_str[res]);
    emitln("sete %s", x64_lbreg_str[res]);
    emitln("movzx %s, %s", x64_ldreg_str[res], x64_lbreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_ch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    emitln("movsx %s, %s", x64_reg_str[res], x64_lbreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_uch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    emitln("movzx %s, %s", x64_ldreg_str[res], x64_lbreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_sh(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    emitln("movsx %s, %s", x64_reg_str[res], x64_lwreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_ush(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    emitln("movzx %s, %s", x64_ldreg_str[res], x64_lwreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_llsx(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X64_Reg res;

    /*
     * Propagate L.O. dword's sign bit into H.O. dword.
     */
    res = get_reg(i);
    x64_load(res, arg1);
    if (need_to_extend)
        emitln("movsx %s, %s", x64_reg_str[res], x64_ldreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_llzx(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X64_Reg res;

    /*
     * Set H.O. dword to zero.
     */
    res = get_reg(i);
    x64_load(res, arg1);
    if (need_to_extend)
        emitln("mov %s, %s", x64_ldreg_str[res], x64_ldreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_addr_of(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X64_Reg res;

    res = get_reg0();
    x64_load_addr(res, arg1);
    update_tar_descriptors(res, tar, tar_liveness(i), tar_next_use(i));
}

static void x64_ind(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X64_Reg res;
    char *reg_str;

    /* spill any target currently in a register */
    spill_aliased_objects();

    res = get_reg(i);
    x64_load(res, arg1);
    reg_str = x64_reg_str[res];
    switch (get_type_category(instruction(i).type)) {
    case TOK_STRUCT:
    case TOK_UNION:
        break;
    case TOK_INT:
    case TOK_ENUM:
        emitln("movsx %s, dword [%s]", reg_str, reg_str);
        break;
    case TOK_UNSIGNED:
        emitln("mov %s, dword [%s]", x64_ldreg_str[res], reg_str);
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
        emitln("mov %s, qword [%s]", reg_str, reg_str);
        break;
    }
    UPDATE_ADDRESSES_UNARY(res);
}

static void x64_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X64_Reg res;

    res = get_reg(i);
    x64_load(res, arg1);
    UPDATE_ADDRESSES_UNARY(res);
}

static int x64_pre_call(int i, unsigned arg2)
{
    Token cat;
    int na, nb, offs;
    int top;
    unsigned siz;

    spill_all();

    if ((cat=get_type_category(instruction(i).type))==TOK_STRUCT || cat==TOK_UNION) {
        siz = get_sizeof(instruction(i).type);
        if (siz > temp_struct_size)
            temp_struct_size = siz;
        if (siz > 16) {
            emit("lea rdi, [rbp+");
            calls_to_fix[calls_to_fix_counter++] = string_get_pos(func_body);
            emitln("XXXXXXXXXXXXXXXX");
            --arg_reg_avail;
        }
    }

    offs = 0;
    top = arg_stack_top;

    /* pass register arguments (left-to-right) */
    for (na = (int)address(arg2).cont.val; na != 0; na--) {
        siz = arg_stack[--top];
        if (siz>16 || siz>arg_reg_avail*8) {
            ;
        } else {
            X64_Reg r;

            arg_stack[top] = 1;
            r = x64_arg_reg[6-arg_reg_avail];
            --arg_reg_avail;
            emitln("mov %s, [rsp+%d]", x64_reg_str[r], offs);
            if (siz > 8) {
                arg_stack[top] = 2;
                r = x64_arg_reg[6-arg_reg_avail];
                --arg_reg_avail;
                emitln("mov %s, [rsp+%d]", x64_reg_str[r], offs+8);
            }
        }
        offs += siz;
    }
    arg_stack_top = top;
    nb = offs;

    /* pass stack arguments (right-to-left) */
    for (na = (int)address(arg2).cont.val; na != 0; na--) {
        unsigned siz;

        siz = arg_stack[top++];
        if (siz == 1) {
            offs -= 8;
        } else if (siz == 2) {
            offs -= 16;
        } else {
            nb += siz;
            switch (siz) {
            case 8:
                emitln("push qword [rsp+%d]", offs-siz);
                break;
            case 16:
                emitln("push qword [rsp+%d]", offs-siz+8);
                emitln("push qword [rsp+%d]", offs-siz+8);
                break;
            default:
				/* save */
				emitln("mov r11, rdi");
				emitln("mov r12, rsi");
				emitln("mov r13, rcx");
				/* copy */
                emitln("sub rsp, %u", siz);
                emitln("lea rsi, [rsp+%d]", offs);
                emitln("mov rdi, rsp");
                emitln("mov rcx, %u", siz);
                emitln("rep movsb");
                /* restore */
                emitln("mov rdi, r11");
                emitln("mov rsi, r12");
                emitln("mov rcx, r13");
                break;
            }
        }
    }

    if (instruction(i-1).op == OpNOp)
        emitln("xor eax, eax"); /* zero vector registers used */

    return nb;
}

static void x64_post_call(int i, int nb)
{
    Token cat;
    unsigned siz;

    assert(arg_stack_top >= 0);
    if (nb)
        emitln("add rsp, %d", nb);
    /*
     * We only maintain the address of structs in registers (and not the structs
     * themselves), so spill immediately into the space reserved for the temporary
     * struct after the return of a struct (size <= 16) in a register.
     */
    if ((cat=get_type_category(instruction(i).type))==TOK_STRUCT || cat==TOK_UNION) {
        siz = get_sizeof(instruction(i).type);
        if (siz > 16) {
            ;
        } else if (siz > 8) {
            orets_to_fix[orets_to_fix_counter++] = string_get_pos(func_body);
            emitln("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
        } else {
            qrets_to_fix[qrets_to_fix_counter++] = string_get_pos(func_body);
            emitln("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
        }
    }
    arg_reg_avail = 6;
}

static void x64_indcall(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    int nb;

    nb = x64_pre_call(i, arg2);
    emitln("call %s", x64_get_operand64(arg1));
    x64_post_call(i, nb);
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    if (tar)
        update_tar_descriptors(X64_RAX, tar, tar_liveness(i), tar_next_use(i));
}

static void x64_call(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    int nb;

    nb = x64_pre_call(i, arg2);
    emitln("call $%s", address(arg1).cont.var.e->attr.str);
    x64_post_call(i, nb);
    if (tar)
        update_tar_descriptors(X64_RAX, tar, tar_liveness(i), tar_next_use(i));
}

static void x64_ind_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X64_Reg pr;
    char *siz_str;

    /* force the reload of any target currently in a register */
    spill_aliased_objects();

    if ((cat=get_type_category(instruction(i).type))==TOK_STRUCT || cat==TOK_UNION) {
        int cluttered;

        cluttered = 0;
        if (addr_reg(arg2) != X64_RSI) {
            if (!reg_isempty(X64_RSI)) {
                cluttered |= 1;
                emitln("push rsi");
            }
            x64_load(X64_RSI, arg2);
        }
        if (addr_reg(arg1) != X64_RDI) {
            if (!reg_isempty(X64_RDI)) {
                cluttered |= 2;
                emitln("push rdi");
            }
            x64_load(X64_RDI, arg1);
        }
        if (!reg_isempty(X64_RCX)) {
            cluttered |= 4;
            emitln("push rcx");
        }
        emitln("mov ecx, %u", get_sizeof(instruction(i).type));
        emitln("rep movsb");
        if (cluttered & 4)
            emitln("pop rcx");
        if (cluttered & 2)
            emitln("pop rdi");
        if (cluttered & 1)
            emitln("pop rsi");
        goto done;
    }

    /*
     * <= 8 bytes scalar indirect assignment.
     */

    if (addr_reg(arg1) == -1) {
        pr = get_reg(i);
        x64_load(pr, arg1);
        pin_reg(pr);
    } else {
        pr = addr_reg(arg1);
    }

    switch (cat) {
    case TOK_INT:
    case TOK_ENUM:
    case TOK_UNSIGNED:
        siz_str = "dword";
        break;
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
        siz_str = "qword";
        break;
    }

    if (address(arg2).kind == IConstKind) {
        long long val;

        val = address(arg2).cont.uval;
        if (!equal("qword", siz_str) || val>=INT_MIN && val<=INT_MIN) {
            emitln("mov %s [%s], %d", siz_str, x64_reg_str[pr], (int)val);
        } else {
            X64_Reg r;

            r = get_reg0();
            x64_load(r, arg2);
            emitln("mov qword [%s], %s", x64_reg_str[pr], x64_reg_str[r]);
        }
    } else if (address(arg2).kind == StrLitKind) {
        if (equal(siz_str, "qword")) {
            X64_Reg r;

            r = get_reg0();
            x64_load(r, arg2);
            emitln("mov qword [%s], %s", x64_reg_str[pr], x64_reg_str[r]);
        } else {
            emitln("mov %s [%s], _@S%d", siz_str, x64_reg_str[pr], new_string_literal(arg2));
        }
    } else {
        X64_Reg r;
        char *reg_str;

        if (addr_reg(arg2) == -1) {
            r = get_reg0();
            x64_load(r, arg2);
        } else {
            r = addr_reg(arg2);
        }
        switch (cat) {
        case TOK_INT:
        case TOK_ENUM:
        case TOK_UNSIGNED:
            reg_str = x64_ldreg_str[r];
            break;
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            reg_str = x64_lwreg_str[r];
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR:
            reg_str = x64_lbreg_str[r];
            break;
        default:
            reg_str = x64_reg_str[r];
            break;
        }
        emitln("mov %s [%s], %s", siz_str, x64_reg_str[pr], reg_str);
    }
    unpin_reg(pr);
done:
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    update_arg_descriptors(arg2, arg2_liveness(i), arg2_next_use(i));
}

static void x64_lab(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    emit_lab(address(tar).cont.val);
}

static void x64_jmp(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (instruction(i+1).op == OpLab
    && address(instruction(i+1).tar).cont.val == address(tar).cont.val)
        return;
    emit_jmp(address(tar).cont.val);
}

static void x64_arg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    Declaration ty;

    /*
     * Note:
     * The argument type expressions comes from the formal parameter
     * if the argument matches a non-optional parameter, or from the
     * argument expression itself if the argument matches the `...'.
     * If it comes from the formal parameter, an identifier node may
     * have to be skipped.
     */
    ty = *instruction(i).type;
    if (ty.idl!=NULL && ty.idl->op==TOK_ID)
        ty.idl = ty.idl->child;

    if ((cat=get_type_category(&ty))==TOK_STRUCT || cat==TOK_UNION) {
        unsigned siz, asiz;
        int cluttered, savnb;

        siz = get_sizeof(&ty);
        asiz = round_up(siz, 8);
        emitln("sub rsp, %u", asiz);
        arg_stack[arg_stack_top++] = asiz;

        cluttered = savnb = 0;
        if (addr_reg(arg1) != X64_RSI) {
            if (!reg_isempty(X64_RSI)) {
                cluttered |= 1;
                emitln("push rsi");
                savnb += 8;
            }
            x64_load(X64_RSI, arg1);
        }
        if (!reg_isempty(X64_RDI)) {
            cluttered |= 2;
            emitln("push rdi");
            savnb += 8;
        }
        if (!savnb)
            emitln("mov rdi, rsp");
        else
            emitln("lea rdi, [rsp+%d]", savnb);
        if (!reg_isempty(X64_RCX)) {
            cluttered |= 4;
            emitln("push rcx");
        }
        emitln("mov ecx, %u", siz);
        emitln("rep movsb");
        if (cluttered & 4)
            emitln("pop rcx");
        if (cluttered & 2)
            emitln("pop rdi");
        if (cluttered & 1)
            emitln("pop rsi");
    } else {
        emitln("push %s", x64_get_operand64(arg1));
        arg_stack[arg_stack_top++] = 8;
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

static void x64_ret(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    unsigned siz;

    siz = get_sizeof(instruction(i).type);
    if (siz > 16) {
        x64_load(X64_RSI, arg1);
        if (!reg_isempty(X64_RDI))
            spill_reg(X64_RDI);
        emitln("mov rdi, qword [rbp+-8]");
        if (!reg_isempty(X64_RCX))
            spill_reg(X64_RCX);
        emitln("mov ecx, %u", siz);
        emitln("rep movsb");
    } else if (siz > 8) {
        x64_load(X64_RAX, arg1);
        switch (siz) {
        case 9:
            emitln("mov dl, byte [rax+8]");
            break;
        case 10:
            emitln("mov dx, word [rax+8]");
            break;
        case 12:
            emitln("mov edx, dword [rax+8]");
            break;
        case 16:
            emitln("mov rdx, qword [rax+8]");
            break;
        default:
            spill_reg(X64_RSI);
            spill_reg(X64_RDI);
            spill_reg(X64_RCX);
            emitln("sub rsp, 8");
            emitln("lea rsi, [rax+8]");
            emitln("mov rdi, rsp");
            emitln("mov ecx, %u", siz-8);
            emitln("rep movsb");
            emitln("pop rdx");
            break;
        }
        emitln("mov rax, qword [rax]");
    } else {
        Token cat;

        x64_load(X64_RAX, arg1);
        if ((cat=get_type_category(instruction(i).type))==TOK_STRUCT || cat==TOK_UNION) {
            switch (siz) {
            case 1:
                emitln("mov al, byte [rax]");
                break;
            case 2:
                emitln("mov ax, word [rax]");
                break;
            case 4:
                emitln("mov eax, dword [rax]");
                break;
            case 8:
                emitln("mov rax, qword [rax]");
                break;
            default:
                spill_reg(X64_RSI);
                spill_reg(X64_RDI);
                spill_reg(X64_RCX);
                emitln("sub rsp, 8");
                emitln("mov rsi, rax");
                emitln("mov rdi, rsp");
                emitln("mov ecx, %u", siz);
                emitln("rep movsb");
                emitln("pop rax");
                break;
            }
        }
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

static void x64_cbr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x64_compare_against_constant(arg1, 0);
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i)); /* do any spilling before the jumps */
    if (address(tar).cont.val == address(instruction(i+1).tar).cont.val)
        emit_jmpeq(address(arg2).cont.val);
    else
        emit_jmpneq(address(tar).cont.val);
}

static void x64_nop(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void x64_switch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /*
     * Note:
     *  - The default case is the last case after the 'OpSwitch' instruction.
     */
    X64_Reg res, tr1, tr2;
    Arena *lab_arena;
    char **jmp_tab, def_lab[16];
    int def_val;
    long long min, max;
    long long ncase, interval_size, holes;

    if (addr_reg(arg1) == -1) {
        res = get_reg(i);
        x64_load(res, arg1);
    } else {
        res = addr_reg(arg1);
    }
    pin_reg(res);
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
                def_val = (int)address(arg1).cont.val;
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
    tr1 = get_reg0(); pin_reg(tr1);
    tr2 = get_reg0(); unpin_reg(tr1);
    emitln("mov %s, %lld", x64_reg_str[tr1], min);
    emitln("mov %s, %lld", x64_reg_str[tr2], max);
    emitln("cmp %s, %s", x64_reg_str[res], x64_reg_str[tr1]);
    emit_jl(def_val);
    emitln("cmp %s, %s", x64_reg_str[res], x64_reg_str[tr2]);
    emit_jg(def_val);
    if (min != 0)
        emitln("sub %s, %s", x64_reg_str[res], x64_reg_str[tr1]);
    emitln("jmp [%s*8+.jt%d]", x64_reg_str[res], jump_tables_counter);

    /* build jump table */
    jmp_tab = calloc(interval_size, sizeof(char *));
    lab_arena = arena_new(interval_size*16, FALSE);
    sprintf(def_lab, ".L%d", def_val);
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
        emitln("dq %s", jmp_tab[i]);
    SET_SEGMENT(TEXT_SEG, emitln);

    free(jmp_tab);
    arena_destroy(lab_arena);
    unpin_reg(res);
    return;

linear_search:
    for (tr1 = -1;; i++) {
        long long val;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        if (address(arg2).cont.val) {
            emit_jmp(address(arg1).cont.val);
            break;
        }

        val = address(tar).cont.uval;
        if (val>=INT_MIN && val<=INT_MAX) {
            emitln("cmp %s, %d", x64_ldreg_str[res], (int)val);
        } else {
            if (tr1 == -1)
                tr1 = get_reg0();
            emitln("mov %s, %lld", x64_reg_str[tr1], val);
            emitln("cmp %s, %s", x64_reg_str[res], x64_reg_str[tr1]);
        }
        emit_jmpeq(address(arg1).cont.val);
    }
    unpin_reg(res);
}

static void x64_case(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void (*instruction_handlers[])(int, unsigned, unsigned, unsigned) = {
    x64_add, x64_sub, x64_mul, x64_div,
    x64_rem, x64_shl, x64_shr, x64_and,
    x64_or, x64_xor, x64_eq, x64_neq,
    x64_lt, x64_let, x64_gt, x64_get,

    x64_neg, x64_cmpl, x64_not, x64_ch,
    x64_uch, x64_sh, x64_ush, x64_llsx,
    x64_llzx, x64_addr_of, x64_ind, x64_asn,
    x64_call, x64_indcall,

    x64_ind_asn, x64_lab, x64_jmp, x64_arg,
    x64_ret, x64_switch, x64_case, x64_cbr,
    x64_nop
};

static void x64_spill_reg_args(DeclList *p, int offs)
{
    X64_Reg r;
    int nfree_reg;
    DeclList *tmp;

    if (get_type_spec(p->decl->decl_specs)->op==TOK_VOID && p->decl->idl==NULL)
        return; /* function with no parameters */

    /* if vararg function, just spill all argument registers in reverse order */
    for (tmp = p; tmp != NULL; tmp = tmp->next) {
        if (tmp->decl->idl!=NULL && tmp->decl->idl->op==TOK_ELLIPSIS) {
            emit_prologln("mov [rbp+%d], r9", offs);
            emit_prologln("mov [rbp+%d], r8", offs-8);
            emit_prologln("mov [rbp+%d], rcx", offs-16);
            emit_prologln("mov [rbp+%d], rdx", offs-24);
            emit_prologln("mov [rbp+%d], rsi", offs-32);
            emit_prologln("mov [rbp+%d], rdi", offs-40);
            return;
        }
    }

    nfree_reg = big_return ? 5 : 6;
    while (p != NULL) {
        unsigned siz;
        Declaration ty;

        ty.decl_specs = p->decl->decl_specs;
        ty.idl = p->decl->idl->child;
        if ((siz=get_sizeof(&ty))>16 || siz>nfree_reg*8) { /* passed on the stack */
            p = p->next;
            continue;
        }
        if (siz > 8) {
            r = x64_arg_reg[6-nfree_reg+1];
            emit_prologln("mov [rbp+%d], %s", offs, x64_reg_str[r]);
            offs -= 8;
            r = x64_arg_reg[6-nfree_reg];
            emit_prologln("mov [rbp+%d], %s", offs, x64_reg_str[r]);
            nfree_reg -= 2;
            offs -= 8;
        } else {
            r = x64_arg_reg[6-nfree_reg];
            emit_prologln("mov [rbp+%d], %s", offs, x64_reg_str[r]);
            --nfree_reg;
            offs -= 8;
        }

        p = p->next;
    }
}

void x64_function_definition(TypeExp *decl_specs, TypeExp *header)
{
    Token cat;
    TypeExp *scs;
    int i, last_i;
    Declaration ty;
    unsigned fn, pos_tmp;
    static int first_func = TRUE;

    curr_func = header->str;
    fn = new_cg_node(curr_func);
    size_of_local_area = round_up(cg_node(fn).size_of_local_area, 8);

    ty.decl_specs = decl_specs;
    ty.idl = header->child->child;
    if (((cat=get_type_category(&ty))==TOK_STRUCT || cat==TOK_UNION) && get_sizeof(&ty)>16)
        big_return = TRUE;

    if (!first_func)
        emit_prolog("\n");
    emit_prologln("; ==== start of definition of function `%s' ====", curr_func);
    SET_SEGMENT(TEXT_SEG, emit_prologln);
    if ((scs=get_sto_class_spec(decl_specs))==NULL || scs->op!=TOK_STATIC)
        emit_prologln("global $%s", curr_func);
    emit_prologln("$%s:", curr_func);
    emit_prologln("push rbp");
    emit_prologln("mov rbp, rsp");

    i = cfg_node(cg_node(fn).bb_i).leader;
    last_i = cfg_node(cg_node(fn).bb_f).last;
    func_last_quad = last_i;
    for (; i <= last_i; i++) {
        unsigned tar, arg1, arg2;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        instruction_handlers[instruction(i).op](i, tar, arg1, arg2);
    }

    /*
     * Fix calls to struct/union valued functions (with size >16).
     */
    size_of_local_area -= round_up(temp_struct_size, 8);
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

    /*
     * Fix calls to struct/union valued functions (with 8 < size <= 16)
     */
    while (--orets_to_fix_counter >= 0) {
        int n;
        char *s;

        string_set_pos(func_body, orets_to_fix[orets_to_fix_counter]);
        s = string_curr(func_body);
        n = sprintf(s, "mov [rbp+%d], rax\n"
                       "mov [rbp+%d], rdx\n"
                       "lea rax, [rbp+%d]",
                       size_of_local_area, size_of_local_area+8, size_of_local_area);
        s[n++] = ' ';
        for (; s[n] == 'X'; n++)
            s[n] = ' ';
    }

    /*
     * Fix calls to struct/union valued functions (with size <= 8)
     */
    while (--qrets_to_fix_counter >= 0) {
        int n;
        char *s;

        string_set_pos(func_body, qrets_to_fix[qrets_to_fix_counter]);
        s = string_curr(func_body);
        n = sprintf(s, "mov [rbp+%d], rax\n"
                       "lea rax, [rbp+%d]",
                       size_of_local_area, size_of_local_area);
        s[n++] = ' ';
        for (; s[n] == 'X'; n++)
            s[n] = ' ';
    }
    string_set_pos(func_body, pos_tmp);

    if (size_of_local_area)
        emit_prologln("sub rsp, %d", -size_of_local_area);
    x64_spill_reg_args(header->child->attr.dl, big_return?-16:-8);
    if (modified[X64_RBX]) emit_prologln("push rbx");
    if (modified[X64_R12]) emit_prologln("push r12");
    if (modified[X64_R13]) emit_prologln("push r13");
    if (modified[X64_R14]) emit_prologln("push r14");
    if (modified[X64_R15]) emit_prologln("push r15");

    if (big_return) {
        emit_prologln("mov qword [rbp+-8], rdi");
        emit_epilogln("mov rax, qword [rbp+-8]");
    }

    if (modified[X64_R15]) emit_epilogln("pop r15");
    if (modified[X64_R14]) emit_epilogln("pop r14");
    if (modified[X64_R13]) emit_epilogln("pop r13");
    if (modified[X64_R12]) emit_epilogln("pop r12");
    if (modified[X64_RBX]) emit_epilogln("pop rbx");
    emit_epilogln("mov rsp, rbp");
    emit_epilogln("pop rbp");
    emit_epilogln("ret");

    string_write(func_prolog, x64_output_file);
    string_write(func_body, x64_output_file);
    string_write(func_epilog, x64_output_file);

    /* reset everything */
    string_clear(func_prolog);
    string_clear(func_body);
    string_clear(func_epilog);
    temp_struct_size = 0;
    calls_to_fix_counter = 0;
    qrets_to_fix_counter = 0;
    orets_to_fix_counter = 0;
    memset(modified, 0, sizeof(int)*X64_NREG);
    memset(pinned, 0, sizeof(int)*X64_NREG);
    free_all_temps();
#if 1
    memset(addr_descr_tab, -1, nid_counter*sizeof(int));
    memset(reg_descr_tab, 0, sizeof(unsigned)*X64_NREG);
#endif
    big_return = FALSE;
#if 0
    dump_addr_descr_tab();
    dump_reg_descr_tab();
#endif
    first_func = FALSE;
}

/*
 * Emit an expression valid for the assembler to evaluate.
 */
void x64_static_expr(ExecNode *e)
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
            x64_static_expr(e->child[pi]);
            if (e->child[ii]->attr.val != 0) {
                Declaration ty;

                ty = e->child[pi]->type;
                ty.idl = ty.idl->child;
                emit_decl("+%u*", get_sizeof(&ty));
                x64_static_expr(e->child[ii]);
            }
        }
            break;
        case TOK_DOT:
        case TOK_ARROW:
            if (get_type_category(&e->child[0]->type) != TOK_UNION) {
                StructMember *m;

                m = get_member_descriptor(get_type_spec(e->child[0]->type.decl_specs), e->child[1]->attr.str);
                x64_static_expr(e->child[0]);
                if (m->offset)
                    emit_decl("+%u", m->offset);
            } else {
                x64_static_expr(e->child[0]);
            }
            break;
        case TOK_ADDRESS_OF:
        case TOK_INDIRECTION:
        case TOK_CAST:
            x64_static_expr(e->child[0]);
            break;

        case TOK_PLUS:
            if (is_integer(get_type_category(&e->type))) {
                x64_static_expr(e->child[0]);
                emit_decl("+");
                x64_static_expr(e->child[1]);
            } else {
                int pi, ii;

                if (is_integer(get_type_category(&e->child[0]->type)))
                    pi = 1, ii = 0;
                else
                    pi = 0, ii = 1;
                x64_static_expr(e->child[pi]);
                if (e->child[ii]->attr.val != 0) {
                    Declaration ty;

                    ty = e->child[pi]->type;
                    ty.idl = ty.idl->child;
                    emit_decl("+%u*", get_sizeof(&ty));
                    x64_static_expr(e->child[ii]);
                }
            }
            break;
        case TOK_MINUS:
            if (is_integer(get_type_category(&e->child[0]->type))) { /* int-int */
                x64_static_expr(e->child[0]);
                emit_decl("-");
                x64_static_expr(e->child[1]);
            } else { /* ptr-int */
                x64_static_expr(e->child[0]);
                if (e->child[1]->attr.val != 0) {
                    Declaration ty;

                    ty = e->child[0]->type;
                    ty.idl = ty.idl->child;
                    emit_decl("-%u*", get_sizeof(&ty));
                    x64_static_expr(e->child[1]);
                }
            }
            break;
        case TOK_CONDITIONAL:
            if (e->child[0]->attr.val)
                x64_static_expr(e->child[1]);
            else
                x64_static_expr(e->child[2]);
            break;
        default:
            assert(0);
        }
        break;
    case IConstExp:
        emit_decl("%lld", e->attr.val);
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

void x64_static_init(TypeExp *ds, TypeExp *dct, ExecNode *e)
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
                x64_static_init(ds, dct->child, e);

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
                x64_static_init(d->decl->decl_specs, dct->child, e);

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
        x64_static_init(ts->attr.dl->decl->decl_specs, ts->attr.dl->decl->idl->child, e);
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
        case TOK_INT:
        case TOK_UNSIGNED:
        case TOK_ENUM:
            emit_declln("align 4");
            emit_decl("dd ");
            break;
        default:
            emit_declln("align 8");
            emit_decl("dq ");
            break;
        }
        x64_static_expr(e);
        emit_decl("\n");
    }
}

void x64_allocate_static_objects(void)
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
            x64_static_init(ty.decl_specs, ty.idl, initzr);
        else
            emit_declln("resb %u", get_sizeof(&ty));
    }
}

void x64_cgen(FILE *outf)
{
    unsigned i, j;
    ExternId *ed, **func_def_list, **ext_sym_list;

    x64_output_file = outf;

    /* generate intermediate code and do some analysis */
    ic_main(&func_def_list, &ext_sym_list); //exit(0);
    compute_liveness_and_next_use();

    /* generate assembly */
    asm_decls = string_new(512);
    str_lits = string_new(512);
    func_body = string_new(1024);
    func_prolog = string_new(1024);
    func_epilog = string_new(1024);
    addr_descr_tab = malloc(nid_counter*sizeof(X64_Reg));
    memset(addr_descr_tab, -1, nid_counter*sizeof(X64_Reg));
    for (i = 0; (ed=func_def_list[i]) != NULL; i++)
        x64_function_definition(ed->decl_specs, ed->declarator);
    string_free(func_body);
    string_free(func_prolog);
    string_free(func_epilog);

    emit_declln("\n; == objects with static duration");
    x64_allocate_static_objects();

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

    string_write(asm_decls, x64_output_file);
    string_free(asm_decls);

    if (string_literals_counter) {
        fprintf(x64_output_file, "\n; == string literals\n");
        fprintf(x64_output_file, "segment .rodata\n");
        string_write(str_lits, x64_output_file);
    }
    string_free(str_lits);
}
