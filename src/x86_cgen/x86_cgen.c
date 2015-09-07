/*
 * Simple x86 code generator
 *      IC ==> x86 ASM (NASM syntax).
 * Of interest:
 *   => System V ABI-i386: http://www.sco.com/developers/devspecs/abi386-4.pdf
 * TOFIX:
 * - The code ignores the fact that byte versions of ESI and EDI don't exist.
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

static unsigned char *liveness_and_next_use;
static BSet *operand_liveness;
static BSet *operand_next_use;
static void init_operand_table(BSet *block_LiveOut);
static void compute_liveness_and_next_use(unsigned fn);
static void print_liveness_and_next_use(unsigned fn);

#define tar_liveness(i)  (liveness_and_next_use[i] & 0x01)
#define arg1_liveness(i) (liveness_and_next_use[i] & 0x02)
#define arg2_liveness(i) (liveness_and_next_use[i] & 0x04)
#define tar_next_use(i)  (liveness_and_next_use[i] & 0x08)
#define arg1_next_use(i) (liveness_and_next_use[i] & 0x10)
#define arg2_next_use(i) (liveness_and_next_use[i] & 0x20)

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
#define pin_reg(r)   pinned[r] = TRUE;
#define unpin_reg(r) pinned[r] = FALSE;
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
static char *curr_func;
static unsigned temp_struct_size;
static int big_return;
static int arg_stack[64], arg_stack_top;
static int calls_to_fix_counter;
static unsigned calls_to_fix[64];
static int string_literals_counter;
static FILE *x86_output_file;

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
static String *func_body;
static String *func_prolog;
static String *func_epilog;
static String *asm_decls;
static String *str_lits;
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

static int *addr_descr_tab;
static unsigned reg_descr_tab[X86_NREG];
#define addr_reg(a)     (addr_descr_tab[address_nid(a)])
#define reg_is_empty(r) (reg_descr_tab[r] == 0)
static void dump_addr_descr_tab(void);
static void dump_reg_descr_tab(void);

static X86_Reg get_empty_reg(void);
static X86_Reg get_unpinned_reg(void);
static void spill_reg(X86_Reg r);
static void spill_all(void);
static void spill_aliased_objects(void);
static X86_Reg get_reg(int intr);

static void x86_load(X86_Reg r, unsigned a);
static void x86_load_addr(X86_Reg r, unsigned a);
static char *x86_get_operand(unsigned a);
static void x86_store(X86_Reg r, unsigned a);
static void x86_compare_against_constant(unsigned a, long c);
static void x86_function_definition(TypeExp *decl_specs, TypeExp *header);

static int x86_static_expr(ExecNode *e);
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

void x86_cgen(FILE *outf)
{
    unsigned i, j;
    ExternId *ed, *func_def_list[512] = { NULL }, *ext_sym_list[512] = { NULL };

    x86_output_file = outf;
    for (ed=get_extern_symtab(), i=j=0; ed != NULL; ed = ed->next) {
        TypeExp *scs;

        if (ed->status == REFERENCED) {
            if ((scs=get_sto_class_spec(ed->decl_specs))==NULL || scs->op!=TOK_STATIC)
                ext_sym_list[j++] = ed;
        } else {
            if (ed->declarator->child!=NULL && ed->declarator->child->op==TOK_FUNCTION) {
                func_def_list[i++] = ed;
            } else {
                ExternId *np;

                np = malloc(sizeof(ExternId));
                np->decl_specs = ed->decl_specs;
                np->declarator = ed->declarator;
                np->status = 0;
                np->next = static_objects_list;
                static_objects_list = np;
            }
        }
    }

    /* generate intermediate code and do some analysis */
    ic_main(func_def_list); //exit(0);

    /* compute liveness and next use */
    liveness_and_next_use = calloc(ic_instructions_counter, sizeof(unsigned char));
    operand_liveness = bset_new(nid_counter);
    operand_next_use = bset_new(nid_counter);
    for (i = 0; i < cg_nodes_counter; i++)
        compute_liveness_and_next_use(i);
    bset_free(operand_liveness);
    bset_free(operand_next_use);

    /* generate assembly */
    asm_decls = string_new(512);
    str_lits = string_new(512);
    func_body = string_new(1024);
    func_prolog = string_new(1024);
    func_epilog = string_new(1024);
    addr_descr_tab = malloc(nid_counter*sizeof(int));
    memset(addr_descr_tab, -1, nid_counter*sizeof(int));
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

        tmp = nid_counter;
        get_var_nid(ed->declarator->str, 0);
        if (tmp == nid_counter)
            emit_declln("extern %s", ed->declarator->str);
    }
    /* the front-end may emit calls to memcpy/memset */
    emit_declln("extern memcpy"); emit_declln("extern memset");
    string_write(asm_decls, x86_output_file);
    string_free(asm_decls);

    if (string_literals_counter) {
        fprintf(x86_output_file, "\n; == string literals\n");
        fprintf(x86_output_file, "segment .rodata\n");
        string_write(str_lits, x86_output_file);
    }
    string_free(str_lits);

    // printf("=> %d\n", nid_counter);
}

/*
 * Initialize the operand table with the liveness
 * and next use as of the end of the block.
 * Liveness is determined using the LiveOut set.
 */
void init_operand_table(BSet *block_LiveOut)
{
    bset_clear(operand_next_use);
    bset_cpy(operand_liveness, block_LiveOut);
}

void compute_liveness_and_next_use(unsigned fn)
{
    int b;
    unsigned entry_bb, last_bb;

    if (cg_node_is_empty(fn))
        return;

    entry_bb = cg_node(fn).bb_i;
    last_bb = cg_node(fn).bb_f;

    /* annotate the quads of every block with liveness and next-use information */
    for (b = entry_bb; b <= last_bb; b++) {
        int i;

        init_operand_table(cfg_node(b).LiveOut);

        /* scan backward through the block */
        for (i = cfg_node(b).last; i >= (int)cfg_node(b).leader; i--) {
            unsigned tar, arg1, arg2;

            tar = instruction(i).tar;
            arg1 = instruction(i).arg1;
            arg2 = instruction(i).arg2;
            switch (instruction(i).op) {
#define update_tar()\
        do {\
            int tar_nid;\
\
            tar_nid = address_nid(tar);\
            if (bset_member(operand_liveness, tar_nid)) {\
                liveness_and_next_use[i] |= 0x01;\
                bset_delete(operand_liveness, tar_nid);\
            } else {\
                /*liveness_and_next_use[i] &= ~0x01;*/\
            }\
            if (bset_member(operand_next_use, tar_nid)) {\
                liveness_and_next_use[i] |= 0x08;\
                bset_delete(operand_next_use, tar_nid);\
            } else {\
                /*liveness_and_next_use[i] &= ~0x08;*/\
            }\
        } while (0)
#define update_arg1()\
        do {\
            int arg1_nid;\
\
            arg1_nid = address_nid(arg1);\
            if (bset_member(operand_liveness, arg1_nid)) {\
                liveness_and_next_use[i] |= 0x02;\
            } else {\
                /*liveness_and_next_use[i] &= ~0x02;*/\
                bset_insert(operand_liveness, arg1_nid);\
            }\
            if (bset_member(operand_next_use, arg1_nid)) {\
                liveness_and_next_use[i] |= 0x10;\
            } else {\
                /*liveness_and_next_use[i] &= ~0x10;*/\
                bset_insert(operand_next_use, arg1_nid);\
            }\
        } while (0)
#define update_arg2()\
        do {\
            int arg2_nid;\
\
            arg2_nid = address_nid(arg2);\
            if (bset_member(operand_liveness, arg2_nid)) {\
                liveness_and_next_use[i] |= 0x04;\
            } else {\
                /*liveness_and_next_use[i] &= ~0x04;*/\
                bset_insert(operand_liveness, arg2_nid);\
            }\
            if (bset_member(operand_next_use, arg2_nid)) {\
                liveness_and_next_use[i] |= 0x20;\
            } else {\
                /*liveness_and_next_use[i] &= ~0x20;*/\
                bset_insert(operand_next_use, arg2_nid);\
            }\
        } while (0)

            case OpAdd: case OpSub: case OpMul: case OpDiv:
            case OpRem: case OpSHL: case OpSHR: case OpAnd:
            case OpOr: case OpXor: case OpEQ: case OpNEQ:
            case OpLT: case OpLET: case OpGT: case OpGET:
                update_tar();
                if (!const_addr(arg1))
                    update_arg1();
                if (!const_addr(arg2))
                    update_arg2();
                continue;

            case OpNeg: case OpCmpl: case OpNot: case OpCh:
            case OpUCh: case OpSh: case OpUSh: case OpAsn:
                update_tar();
                if (!const_addr(arg1))
                    update_arg1();
                continue;

            case OpArg:
            case OpRet:
            case OpSwitch:
            case OpCBr:
                if (!const_addr(arg1))
                    update_arg1();
                continue;

            case OpAddrOf:
                update_tar();
                continue;

            case OpInd:
                update_tar();
                update_arg1();
                bset_union(operand_liveness, address_taken_variables);
                continue;

            case OpIndAsn:
                update_arg1();
                if (!const_addr(arg2))
                    update_arg2();
                continue;

            case OpCall:
            case OpIndCall:
                if (tar)
                    update_tar();
                if (instruction(i).op==OpIndCall && !const_addr(arg1))
                    update_arg1();
                bset_union(operand_liveness, cg_node(fn).modified_static_objects);
                bset_union(operand_liveness, address_taken_variables);
                continue;

            default: /* other */
                continue;
            } /* switch (instruction(i).op) */
        } /* instructions */
    } /* basic blocks */

#if DEBUG
    print_liveness_and_next_use(fn);
#endif
}

void print_liveness_and_next_use(unsigned fn)
{
    int b;
    unsigned entry_bb, last_bb;

    entry_bb = cg_node(fn).bb_i;
    last_bb = cg_node(fn).bb_f;

    for (b = entry_bb; b <= last_bb; b++) {
        int i;

        printf("Block %d\n", b);

        for (i = cfg_node(b).leader; i <= cfg_node(b).last; i++) {
            unsigned tar, arg1, arg2;

            tar = instruction(i).tar;
            arg1 = instruction(i).arg1;
            arg2 = instruction(i).arg2;
            switch (instruction(i).op) {
#define print_tar()\
        printf("name=%s, ", address_sid(tar)),\
        printf("status=%s, ", tar_liveness(i)?"LIVE":"DEAD"),\
        printf("next use=%d", !!tar_next_use(i))
#define print_arg1()\
        printf("name=%s, ", address_sid(arg1)),\
        printf("status=%s, ", arg1_liveness(i)?"LIVE":"DEAD"),\
        printf("next use=%d", !!arg1_next_use(i))
#define print_arg2()\
        printf("name=%s, ", address_sid(arg2)),\
        printf("status=%s, ", arg2_liveness(i)?"LIVE":"DEAD"),\
        printf("next use=%d", !!arg2_next_use(i))

            case OpAdd: case OpSub: case OpMul: case OpDiv:
            case OpRem: case OpSHL: case OpSHR: case OpAnd:
            case OpOr: case OpXor: case OpEQ: case OpNEQ:
            case OpLT: case OpLET: case OpGT: case OpGET:
                print_tar();
                if (!const_addr(arg1)) {
                    printf(" | ");
                    print_arg1();
                }
                if (!const_addr(arg2)) {
                    printf(" | ");
                    print_arg2();
                }
                break;

            case OpNeg: case OpCmpl: case OpNot: case OpCh:
            case OpUCh: case OpSh: case OpUSh: case OpAsn:
                print_tar();
                if (!const_addr(arg1)) {
                    printf(" | ");
                    print_arg1();
                }
                break;

            case OpArg:
            case OpRet:
            case OpSwitch:
            case OpCBr:
                if (!const_addr(arg1)) {
                    print_arg1();
                }
                break;

            case OpAddrOf:
                print_tar();
                break;

            case OpInd:
                print_tar();
                printf(" | ");
                print_arg1();
                break;

            case OpIndAsn:
                print_arg1();
                if (!const_addr(arg2)) {
                    printf(" | ");
                    print_arg2();
                }
                break;

            case OpCall:
            case OpIndCall:
                if (tar)
                    print_tar();
                if (!const_addr(arg1)) {
                    printf(" | ");
                    print_arg1();
                }
                break;

            default:
                continue;
            } /* instructions */
            printf("\n--------------\n");
        } /* basic blocks */
        printf("\n\n");
    }
}

void dump_addr_descr_tab(void)
{
    int i;

    for (i = 0; i < nid_counter; i++)
        if (addr_descr_tab[i] != -1)
            printf("%s => %s\n", nid2sid_tab[i], x86_reg_str[addr_descr_tab[i]]);
}

void dump_reg_descr_tab(void)
{
    int i;

    for (i = 0; i < X86_NREG; i++)
        if (!reg_is_empty(i))
            printf("%s => %s\n", x86_reg_str[i], address_sid(reg_descr_tab[i]));
}

// =============================================================================

X86_Reg get_empty_reg(void)
{
    int i;

    for (i = 0; i < X86_NREG; i++) {
        if (!pinned[i] && reg_is_empty(i)) {
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
    unsigned a;

    if (reg_is_empty(r))
        return;

    a = reg_descr_tab[r];
    x86_store(r, a);
    addr_reg(a) = -1;
    reg_descr_tab[r] = 0;
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

        if (reg_is_empty(i))
            continue;
        a = reg_descr_tab[i];
        if (address(a).kind==IdKind && bset_member(address_taken_variables, address_nid(a)))
            spill_reg(i);
    }
}

X86_Reg get_reg(int i)
{
    X86_Reg r;
    unsigned arg1;

    arg1 = instruction(i).arg1;
    if (!const_addr(arg1) && addr_reg(arg1)!=-1 && !arg1_liveness(i))
        return addr_reg(arg1);

    /* find an empty register */
    if ((r=get_empty_reg()) != -1)
        return r;

    /* choose an unpinned register and spill its contents */
    r = get_unpinned_reg();
    assert(r != -1);
    spill_reg(r);
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
    size_of_local_area -= 4;
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

void x86_load(X86_Reg r, unsigned a)
{
    if (address(a).kind == IConstKind) {
        emitln("mov %s, %lu", x86_reg_str[r], address(a).cont.uval);
    } else if (address(a).kind == StrLitKind) {
        emitln("mov %s, _@S%d", x86_reg_str[r], new_string_literal(a));
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *siz_str, *mov_str;

        if (addr_reg(a) != -1) {
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
                emitln("%s %s, %s [$%s@%s]", mov_str, x86_reg_str[r], siz_str, curr_func, e->attr.str);
            else /* global */
                emitln("%s %s, %s [$%s]", mov_str, x86_reg_str[r], siz_str, e->attr.str);
        } else { /* parameter or local */
            emitln("%s %s, %s [ebp+%d]", mov_str, x86_reg_str[r], siz_str, local_offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg(a) != -1) {
            if (addr_reg(a) == r)
                return; /* already in the register */
            else
                emitln("mov %s, %s", x86_reg_str[r], x86_reg_str[addr_reg(a)]);
        } else {
            emitln("mov %s, dword [ebp+%d]", x86_reg_str[r], get_temp_offs(a));
        }
    }
}

char *x86_get_operand(unsigned a)
{
    static char op[256];

    if (address(a).kind == IConstKind) {
        sprintf(op, "dword %lu", address(a).cont.uval);
    } else if (address(a).kind == StrLitKind) {
        sprintf(op, "_@S%d", new_string_literal(a));
    } else if (address(a).kind == IdKind) {
        ExecNode *e;

        if (addr_reg(a) != -1)
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
                    sprintf(op, "$%s@%s", curr_func, e->attr.str);
                else
                    sprintf(op, "$%s", e->attr.str);
                return op;
            } else {
                X86_Reg r;

                if ((r=get_empty_reg()) == -1) {
                    r = get_unpinned_reg();
                    assert(r != -1);
                    spill_reg(r);
                }
                emitln("lea %s, [ebp+%d]", x86_reg_str[r], local_offset(a));
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
                sprintf(op, "dword [$%s@%s]", curr_func, e->attr.str);
            else
                sprintf(op, "dword [$%s]", e->attr.str);
        } else {
            sprintf(op, "dword [ebp+%d]", local_offset(a));
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg(a) != -1)
            return x86_reg_str[addr_reg(a)];
        else
            sprintf(op, "dword [ebp+%d]", get_temp_offs(a));
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
        char *siz_str, *reg_str;

        e = address(a).cont.var.e;
        switch (get_type_category(&e->type)) {
        case TOK_STRUCT:
        case TOK_UNION: {
            int cluttered;

            cluttered = 0;
            if (r != X86_ESI) {
                if (!reg_is_empty(X86_ESI)) {
                    cluttered |= 1;
                    emitln("push esi");
                }
                emitln("mov esi, %s", x86_reg_str[r]);
            }
            if (addr_reg(a) != X86_EDI) {
                if (!reg_is_empty(X86_EDI)) {
                    cluttered |= 2;
                    emitln("push edi");
                }
                x86_load_addr(X86_EDI, a);
            }
            if (!reg_is_empty(X86_ECX)) {
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
                emitln("mov %s [$%s@%s], %s", siz_str, curr_func, e->attr.str, reg_str);
            else /* global */
                emitln("mov %s [$%s], %s", siz_str, e->attr.str, reg_str);
        } else { /* parameter or local */
            emitln("mov %s [ebp+%d], %s", siz_str, local_offset(a), reg_str);
        }
    } else if (address(a).kind == TempKind) {
        emitln("mov dword [ebp+%d], %s", get_temp_offs(a), x86_reg_str[r]);
    }
}

void x86_compare_against_constant(unsigned a, long c)
{
    if (address(a).kind == IConstKind) {
        assert(0); /* can be folded */
    } else if (address(a).kind == StrLitKind) {
        assert(0); /* can be folded */
    } else if (address(a).kind == IdKind) {
        ExecNode *e;
        char *siz_str;

        if (addr_reg(a) != -1) {
            emitln("cmp %s, %lu", x86_reg_str[addr_reg(a)], c);
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
                emitln("cmp %s [$%s@%s], %lu", siz_str, curr_func, e->attr.str, c);
            else
                emitln("cmp %s [$%s], %lu", siz_str, e->attr.str, c);
        } else {
            emitln("cmp %s [ebp+%d], %lu", siz_str, local_offset(a), c);
        }
    } else if (address(a).kind == TempKind) {
        if (addr_reg(a) != -1)
            emitln("cmp %s, %lu", x86_reg_str[addr_reg(a)], c);
        else
            emitln("cmp dword [ebp+%d], %lu", get_temp_offs(a), c);
    }
}

void update_arg_descriptors(unsigned arg, unsigned char liveness, int next_use)
{
    if (const_addr(arg) || next_use)
        return;

    if (addr_reg(arg) != -1) {
        if (liveness) /* spill */
            x86_store(addr_reg(arg), arg);
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

void update_tar_descriptors(X86_Reg res, unsigned tar, unsigned char liveness, int next_use)
{
    /* Note: maintain the order of the operations for x86_store() to work correctly */

    addr_reg(tar) = res;
    reg_descr_tab[res] = tar;

    if (!next_use) {
        if (liveness) /* spill */
            x86_store(res, tar);
        addr_reg(tar) = -1;
        reg_descr_tab[res] = 0;
    }
}

// =======================================================================================

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

#define emit_lab(n)         emitln(".L%ld:", n)
#define emit_jmp(target)    emitln("jmp .L%ld", target)
#define emit_jmpeq(target)  emitln("je .L%ld", target)
#define emit_jmpneq(target) emitln("jne .L%ld", target)
#define emit_jl(target)     emitln("jl .L%ld", target)
#define emit_jg(target)     emitln("jg .L%ld", target)

static void x86_add(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_sub(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_mul(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_div_rem(X86_Reg res, int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_div(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_rem(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_shl(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_shr(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_and(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_or(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_xor(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_eq(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_neq(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_lt(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_let(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_gt(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_get(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_neg(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_cmpl(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_not(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_ch(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_uch(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_sh(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_ush(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_addr_of(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_ind(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_asn(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_pre_call(int i);
static void x86_post_call(unsigned arg2);
static void x86_indcall(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_call(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_ind_asn(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_lab(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_ret(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_cbr(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_nop(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_switch(int i, unsigned tar, unsigned arg1, unsigned arg2);
static void x86_case(int i, unsigned tar, unsigned arg1, unsigned arg2);

void x86_add(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("add %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

void x86_sub(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("sub %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

void x86_mul(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("imul %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

void x86_div_rem(X86_Reg res, int i, unsigned tar, unsigned arg1, unsigned arg2)
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
        divop = x86_get_operand(arg2);
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
    UPDATE_ADDRESSES(res);
}

void x86_div(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_div_rem(X86_EAX, i, tar, arg1, arg2);
}

void x86_rem(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_div_rem(X86_EDX, i, tar, arg1, arg2);
}

void x86_shl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    if (address(arg2).kind == IConstKind) {
        res = get_reg(i);
        x86_load(res, arg1);
        emitln("sal %s, %lu", x86_reg_str[res], address(arg2).cont.uval);
    } else {
        if (addr_reg(arg2) != X86_ECX) {
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

void x86_shr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    char *instr;
    X86_Reg res;

    instr = (is_unsigned_int(get_type_category(instruction(i).type))) ? "shr" : "sar";

    if (address(arg2).kind == IConstKind) {
        res = get_reg(i);
        x86_load(res, arg1);
        emitln("%s %s, %lu", instr, x86_reg_str[res], address(arg2).cont.uval);
    } else {
        if (addr_reg(arg2) != X86_ECX) {
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

void x86_and(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("and %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

void x86_or(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("or %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

void x86_xor(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("xor %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    UPDATE_ADDRESSES(res);
}

void x86_eq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    emitln("mov %s, 0", x86_reg_str[res]);
    emitln("sete %s", x86_lbreg_str[res]);
    UPDATE_ADDRESSES(res);
}

void x86_neq(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    emitln("mov %s, 0", x86_reg_str[res]);
    emitln("setne %s", x86_lbreg_str[res]);
    UPDATE_ADDRESSES(res);
}

void x86_lt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    emitln("mov %s, 0", x86_reg_str[res]);
    emitln("set%s %s", ((int)instruction(i).type==IC_SIGNED)?"l":"b", x86_lbreg_str[res]);
    UPDATE_ADDRESSES(res);
}

void x86_let(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    emitln("mov %s, 0", x86_reg_str[res]);
    emitln("set%s %s", ((int)instruction(i).type==IC_SIGNED)?"le":"be", x86_lbreg_str[res]);
    UPDATE_ADDRESSES(res);
}

void x86_gt(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    emitln("mov %s, 0", x86_reg_str[res]);
    emitln("set%s %s", ((int)instruction(i).type==IC_SIGNED)?"g":"a", x86_lbreg_str[res]);
    UPDATE_ADDRESSES(res);
}

void x86_get(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    pin_reg(res);
    emitln("cmp %s, %s", x86_reg_str[res], x86_get_operand(arg2));
    unpin_reg(res);
    emitln("mov %s, 0", x86_reg_str[res]);
    emitln("set%s %s", ((int)instruction(i).type==IC_SIGNED)?"ge":"ae", x86_lbreg_str[res]);
    UPDATE_ADDRESSES(res);
}

void x86_neg(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("neg %s", x86_reg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_cmpl(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("not %s", x86_reg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_not(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("cmp %s, 0", x86_reg_str[res]);
    emitln("mov %s, 0", x86_reg_str[res]);
    emitln("sete %s", x86_lbreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_ch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    /* TODO: handle the case where the register is esi or edi */

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movsx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_uch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    /* TODO: handle the case where the register is esi or edi */

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movzx %s, %s", x86_reg_str[res], x86_lbreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_sh(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movsx %s, %s", x86_reg_str[res], x86_lwreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_ush(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    emitln("movzx %s, %s", x86_reg_str[res], x86_lwreg_str[res]);
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_addr_of(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load_addr(res, arg1);
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_ind(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;
    char *reg_str;

    /* spill any target currently in a register */
    spill_aliased_objects();

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
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    X86_Reg res;

    res = get_reg(i);
    x86_load(res, arg1);
    UPDATE_ADDRESSES_UNARY(res);
}

void x86_pre_call(int i)
{
    Token cat;

    spill_all();
    if ((cat=get_type_category(instruction(i).type))==TOK_STRUCT || cat==TOK_UNION) {
        unsigned siz;

        siz = compute_sizeof(instruction(i).type);
        if (siz > temp_struct_size)
            temp_struct_size = siz;
        emit("lea eax, [ebp+");
        calls_to_fix[calls_to_fix_counter++] = string_get_pos(func_body);
        emitln("XXXXXXXXXXXXXXXX");
        emitln("push eax");
    }
}

void x86_post_call(unsigned arg2)
{
    int na, nb;

    na = address(arg2).cont.val;
    nb = 0;
    while (na--)
        nb += arg_stack[--arg_stack_top];
    assert(arg_stack_top >= 0);
    if (nb)
        emitln("add esp, %d", nb);
}

void x86_indcall(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_pre_call(i);
    emitln("call %s", x86_get_operand(arg1));
    x86_post_call(arg2);
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    if (tar)
        update_tar_descriptors(X86_EAX, tar, tar_liveness(i), tar_next_use(i));
}

void x86_call(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_pre_call(i);
    emitln("call $%s", address(arg1).cont.var.e->attr.str);
    x86_post_call(arg2);
    if (tar)
        update_tar_descriptors(X86_EAX, tar, tar_liveness(i), tar_next_use(i));
}

void x86_ind_asn(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    Token cat;
    X86_Reg pr;
    char *siz_str;

    /* force the reload of any target currently in a register */
    spill_aliased_objects();

    cat = get_type_category(instruction(i).type);
    if (cat==TOK_STRUCT || cat==TOK_UNION) {
        int cluttered;

        cluttered = 0;
        if (addr_reg(arg2) != X86_ESI) {
            if (!reg_is_empty(X86_ESI)) {
                cluttered |= 1;
                emitln("push esi");
            }
            x86_load(X86_ESI, arg2);
        }
        if (addr_reg(arg1) != X86_EDI) {
            if (!reg_is_empty(X86_EDI)) {
                cluttered |= 2;
                emitln("push edi");
            }
            x86_load(X86_EDI, arg1);
        }
        if (!reg_is_empty(X86_ECX)) {
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

    /*
     * <= 4 bytes indirect assignment.
     */

    if (addr_reg(arg1) == -1) {
        pr = get_reg(i);
        x86_load(pr, arg1);
        pin_reg(pr);
    } else {
        pr = addr_reg(arg1);
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
        emitln("mov %s [%s], %lu", siz_str, x86_reg_str[pr], address(arg2).cont.uval);
    } else if (address(arg2).kind == StrLitKind) {
        emitln("mov %s [%s], _@S%d", siz_str, x86_reg_str[pr], new_string_literal(arg2));
    } else {
        X86_Reg r;
        char *reg_str;

        if (addr_reg(arg2) == -1) {
            if ((r=get_empty_reg()) == -1) {
                r = get_unpinned_reg();
                assert(r != -1);
                spill_reg(r);
            }
            x86_load(r, arg2);
        } else {
            r = addr_reg(arg2);
        }
        switch (cat) {
        case TOK_SHORT:
        case TOK_UNSIGNED_SHORT:
            reg_str = x86_lwreg_str[r];
            break;
        case TOK_CHAR:
        case TOK_SIGNED_CHAR:
        case TOK_UNSIGNED_CHAR:
            assert(r != X86_ESI); /* TBD */
            assert(r != X86_EDI); /* TBD */
            reg_str = x86_lbreg_str[r];
            break;
        default:
            reg_str = x86_reg_str[r];
            break;
        }
        emitln("mov %s [%s], %s", siz_str, x86_reg_str[pr], reg_str);
    }
    unpin_reg(pr);
done:
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
    update_arg_descriptors(arg2, arg2_liveness(i), arg2_next_use(i));
}

void x86_lab(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    emit_lab(address(tar).cont.val);
}

void x86_jmp(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (instruction(i+1).op == OpLab
    && address(instruction(i+1).tar).cont.val == address(tar).cont.val)
        return;
    emit_jmp(address(tar).cont.val);
}

void x86_arg(int i, unsigned tar, unsigned arg1, unsigned arg2)
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

    cat = get_type_category(&ty);
    if (cat!=TOK_STRUCT && cat!=TOK_UNION) {
        emitln("push %s", x86_get_operand(arg1));
        arg_stack[arg_stack_top++] = 4;
    } else {
        unsigned siz, asiz;
        int cluttered, savnb;

        siz = compute_sizeof(&ty);
        asiz = round_up(siz, 4);
        emitln("sub esp, %u", asiz);
        arg_stack[arg_stack_top++] = asiz;

        cluttered = savnb = 0;
        if (addr_reg(arg1) != X86_ESI) {
            if (!reg_is_empty(X86_ESI)) {
                cluttered |= 1;
                emitln("push esi");
                savnb += 4;
            }
            x86_load(X86_ESI, arg1);
        }
        if (!reg_is_empty(X86_EDI)) {
            cluttered |= 2;
            emitln("push edi");
            savnb += 4;
        }
        if (!savnb)
            emitln("mov edi, esp");
        else
            emitln("lea edi, [esp+%d]", savnb);
        if (!reg_is_empty(X86_ECX)) {
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
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

void x86_ret(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (!big_return) {
        x86_load(X86_EAX, arg1);
    } else {
        unsigned siz;

        siz = compute_sizeof(instruction(i).type);
        /*if (!reg_is_empty(X86_ESI))
            spill_reg(X86_ESI);*/
        x86_load(X86_ESI, arg1);
        modified[X86_ESI] = TRUE;
        if (!reg_is_empty(X86_EDI))
            spill_reg(X86_EDI);
        emitln("mov edi, dword [ebp+-4]");
        modified[X86_EDI] = TRUE;
        if (!reg_is_empty(X86_ECX))
            spill_reg(X86_ECX);
        emitln("mov ecx, %u", siz);
        emitln("rep movsb");
        /*if (!reg_is_empty(X86_EAX))
            spill_reg(X86_EAX);
        emitln("mov eax, dword [ebp-4]");*/
    }
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i));
}

void x86_cbr(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    x86_compare_against_constant(arg1, 0);
    update_arg_descriptors(arg1, arg1_liveness(i), arg1_next_use(i)); /* do any spilling before the jumps */
    if (address(tar).cont.val == address(instruction(i+1).tar).cont.val)
        emit_jmpeq(address(arg2).cont.val);
    else
        emit_jmpneq(address(tar).cont.val);
}

void x86_nop(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

void x86_switch(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /*
     * Note:
     *  - The default case is the last case after the 'OpSwitch' instruction.
     */
    X86_Reg res;
    long def_val;
    Arena *lab_arena;
    char **jmp_tab, def_lab[16];
    long ncase, min, max, interval_size, holes;

    if (addr_reg(arg1) == -1) {
        res = get_reg(i);
        x86_load(res, arg1);
    } else {
        res = addr_reg(arg1);
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
    lab_arena = arena_new(interval_size*16);
    sprintf(def_lab, ".L%ld", def_val);
    for (;; i++) {
        char *s;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;
        if (address(arg2).cont.val)
            break;
        s = arena_alloc(lab_arena, 16);
        sprintf(s, ".L%ld", address(arg1).cont.val);
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

        emitln("cmp %s, %lu", x86_reg_str[res], address(tar).cont.uval);
        emit_jmpeq(address(arg1).cont.val);
    }
}

void x86_case(int i, unsigned tar, unsigned arg1, unsigned arg2)
{
    /* nothing */
}

static void (*instruction_handlers[])(int, unsigned, unsigned, unsigned) = {
    x86_add, x86_sub, x86_mul, x86_div,
    x86_rem, x86_shl, x86_shr, x86_and,
    x86_or, x86_xor, x86_eq, x86_neq,
    x86_lt, x86_let, x86_gt, x86_get,

    x86_neg, x86_cmpl, x86_not, x86_ch,
    x86_uch, x86_sh, x86_ush, x86_addr_of,
    x86_ind, x86_asn, x86_call, x86_indcall,

    x86_ind_asn, x86_lab, x86_jmp, x86_arg,
    x86_ret, x86_switch, x86_case, x86_cbr,
    x86_nop
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

    curr_func = header->str;
    fn = new_cg_node(curr_func);
    size_of_local_area = round_up(cg_node(fn).size_of_local_area, 4);

    ty.decl_specs = decl_specs;
    ty.idl = header->child->child;
    big_return = ((cat=get_type_category(&ty))==TOK_STRUCT || cat==TOK_UNION);

    emit_prologln("\n; ==== start of definition of function `%s' ====", curr_func);
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
    for (; i <= last_i; i++) {
        unsigned tar, arg1, arg2;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        instruction_handlers[instruction(i).op](i, tar, arg1, arg2);
    }
    size_of_local_area -= temp_struct_size;
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
    free_all_temps();
    /*memset(addr_descr_tab, -1, nid_counter*sizeof(int));*/
    /*memset(reg_descr_tab, 0, sizeof(unsigned)*X86_NREG);*/
    dump_addr_descr_tab();
    dump_reg_descr_tab();
}

/*
 * Try to generate a NASM compatible expression.
 * Return 0 on success, -1 otherwise.
 * Note: it's possible for this to spit out total nonsense that will fail at assemble-time.
 * It has been tested for trivial cases only.
 */
int x86_static_expr(ExecNode *e)
{
    switch (e->kind.exp) {
    case OpExp:
        switch (e->attr.op) {
        case TOK_SIZEOF:
            if (e->child[1] != NULL)
                emit_decl("%u", compute_sizeof((Declaration *)e->child[1]));
            else
                emit_decl("%u", compute_sizeof(&e->child[0]->type));
            break;
#define R x86_static_expr(e->child[0]);
        case TOK_UNARY_PLUS:    emit_decl("+"); return R;
        case TOK_UNARY_MINUS:   emit_decl("-"); return R;
        case TOK_COMPLEMENT:    emit_decl("~"); return R;
        case TOK_NEGATION:      emit_decl("!"); return R;
        case TOK_ADDRESS_OF:    return R;
        case TOK_CAST:          return R; /* TOFIX */
#undef R

#define L x86_static_expr(e->child[0])
#define R x86_static_expr(e->child[1])
        case TOK_MUL:       if (L) return -1; emit_decl("*");  return R;
        case TOK_DIV:       if (L) return -1; emit_decl("/");  return R;
        case TOK_REM:       if (L) return -1; emit_decl("%");  return R;
        case TOK_LSHIFT:    if (L) return -1; emit_decl("<<"); return R;
        case TOK_RSHIFT:    if (L) return -1; emit_decl(">>"); return R; /* NASM's right shift is always unsigned */
        case TOK_BW_AND:    if (L) return -1; emit_decl("&");  return R;
        case TOK_BW_XOR:    if (L) return -1; emit_decl("^");  return R;
        case TOK_BW_OR:     if (L) return -1; emit_decl("|");  return R;
        case TOK_PLUS:
            if (is_integer(get_type_category(&e->type))) {
                if (L) return -1; emit_decl("+"); return R;
            } else {
                int pi;
                Declaration ty;

                pi = 0;
                if (is_integer(get_type_category(&e->child[0]->type)))
                    pi = 1;
                ty = e->child[pi]->type;
                ty.idl = ty.idl->child;
                if (pi == 0) {
                    if (L) return -1; emit_decl("+("); if (R) return -1;
                    emit_decl("*%u)", compute_sizeof(&ty));
                } else {
                    if (R) return -1; emit_decl("+("); if (L) return -1;
                    emit_decl("*%u)", compute_sizeof(&ty));
                }
            }
            break;
        case TOK_MINUS:
            if (is_integer(get_type_category(&e->child[0]->type))) { /* int-int */
                if (L) return -1; emit_decl("-"); return R;
            } else {
                Declaration ty;

                ty = e->child[0]->type;
                ty.idl = ty.idl->child;
                if (is_integer(get_type_category(&e->child[1]->type))) { /* ptr-int */
                    if (L) return -1; emit_decl("-"); emit_decl("("); if (R) return -1;
                    emit_decl("*%u)", compute_sizeof(&ty));
                } else { /* ptr-ptr */
                    /* Note: for this to work both operands must reside in the same segment! */
                    emit_decl("(");
                    if (L) return -1; emit_decl("-"); if (R) return -1;
                    emit_decl(")/%u", compute_sizeof(&ty));
                }
            }
            break;
#undef L
#undef R

        default:
            return -1;
        }
        break;
    case IConstExp:
        emit_decl("%lu", e->attr.uval);
        break;
    case StrLitExp:
        emit_strln("_@S%d:", string_literals_counter);
        emit_raw_string(str_lits, e->attr.str);
        emit_decl("_@S%d", string_literals_counter++);
        break;
    case IdExp:
        emit_decl("%s", e->attr.str);
        break;
    }
    return 0; /* OK */
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
        nelem = dct->attr.e->attr.uval;
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
                emit_declln("times %u db 0", nelem*compute_sizeof(&ty));
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
                    emit_declln("times %u db 0",  compute_sizeof(&ty));

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
        unsigned p;
        Declaration dest_ty;
scalar:
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
        default:
            emit_declln("align 4");
            emit_decl("dd ");
            break;
        }
        p = string_get_pos(asm_decls);
        if (x86_static_expr(e) == -1) {
            string_set_pos(asm_decls, p);
            emit_decl("0");
            emit_warning(e->info->src_file, e->info->src_line, e->info->src_column,
            "initializer form not supported, defaulting to zero");
        }
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
        if (np->status != 0) { /* static local */
            emit_declln("$%s@%s:", (char *)np->status, np->declarator->str);
        } else {
            TypeExp *scs;

            if ((scs=get_sto_class_spec(np->decl_specs))==NULL || scs->op!=TOK_STATIC)
                emit_declln("global %s", np->declarator->str);
            emit_declln("$%s:", np->declarator->str);
        }
        if (initzr != NULL)
            x86_static_init(ty.decl_specs, ty.idl, initzr);
        else
            emit_declln("resb %u", compute_sizeof(&ty));
    }
}

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
