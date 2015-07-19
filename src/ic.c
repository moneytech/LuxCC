/*
 * Intermediate code generator
 *  AST ==> IC
 */
#define DEBUG 0
#include "ic.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "util.h"
#include "decl.h"
#include "expr.h"
#include "arena.h"
#include "imp_lim.h"
#include "loc.h"
#include "dflow.h"
#include "bset.h"

#define IINIT   1024
#define IGROW   2
unsigned ic_instructions_max;
unsigned ic_instructions_counter;
Quad *ic_instructions;

#define AINIT   IINIT*3
#define AGROW   2
unsigned ic_addresses_max;
unsigned ic_addresses_counter;
Address *ic_addresses;

#define NINIT   128
#define NGROW   2
unsigned cfg_nodes_max;
unsigned cfg_nodes_counter;
CFGNode *cfg_nodes;

#define CINIT   32
#define CGROW   2
unsigned cg_nodes_max;
unsigned cg_nodes_counter;
CGNode *cg_nodes;

static int label_counter, label_max;
static unsigned *lab2instr;
static unsigned true_addr, false_addr;
static unsigned memset_addr, memcpy_addr;
static ExecNode memset_node, memcpy_node;
static TypeExp int_expr = { TOK_INT };
static Declaration int_ty = { &int_expr };
static TypeExp unsigned_expr = { TOK_UNSIGNED };
static Declaration unsigned_ty = { &unsigned_expr };
ExternId *static_objects_list;

/*
 * X86 stuff.
 */
#define X86_PARAM_END 8 /* ebp+8 */
/* the amount of space to allocate for the current function's local variables */
int size_of_local_area = 0;
/* used to compute the addresses of local variables */
static int local_offset;
/* ---- */

static Arena *id_table_arena;
static Arena *temp_names_arena;

#define ID_TABLE_SIZE 1009
typedef struct IDNode IDNode;
static struct IDNode {
    char *sid;
    int scope;
    int nid;
    IDNode *next;
} *id_table[ID_TABLE_SIZE];

int nid_counter;
static int nid_max;
char **nid2sid_tab;

static int *atv;
static int atv_counter, atv_max;
BSet *address_taken_variables;

static void ic_reset(void);
static void new_cfg_node(unsigned leader);
static void emit_i(OpKind op, Declaration *type, unsigned tar, unsigned arg1, unsigned arg2);
static unsigned new_address(AddrKind kind);
static unsigned new_temp_addr(void);
static unsigned new_label(void);
static void ic_compound_statement(ExecNode *s, int push_scope);
static void ic_function_definition(TypeExp *decl_specs, TypeExp *header);
static void new_nid(char *sid);
static void new_atv(int vnid);
/*static int get_var_nid(char *sid, int scope);*/
static void edge_init(GraphEdge *p, unsigned max);
static void edge_free(GraphEdge *p);

void new_nid(char *sid)
{
    if (nid_counter >= nid_max) {
        char **p;

        p = realloc(nid2sid_tab, 2*nid_max*sizeof(char *));
        assert(p != NULL);
        nid_max *= 2;
        nid2sid_tab = p;
    }
    nid2sid_tab[nid_counter++] = sid;
}

int get_var_nid(char *sid, int scope)
{
    IDNode *np;
    unsigned h;

    h = hash(sid)%ID_TABLE_SIZE;
    for (np = id_table[h]; np != NULL; np = np->next)
        if (equal(np->sid, sid) && np->scope==scope)
            return np->nid;
    np = arena_alloc(id_table_arena, sizeof(IDNode));
    np->sid = sid;
    np->scope = scope;
    np->nid = nid_counter;
    np->next = id_table[h];
    id_table[h] = np;
    new_nid(sid);
    return np->nid;
}

void edge_init(GraphEdge *p, unsigned max)
{
    p->edges = calloc(max, sizeof(unsigned));
    p->max = max;
    p->n = 0;
}

void edge_add(GraphEdge *p, unsigned e)
{
    unsigned i;

    for (i = 0; i < p->n; i++)
        if (p->edges[i] == e)
            return;

    if (p->n >= p->max) {
        unsigned *new_edges;

        p->max *= 2;
        new_edges = realloc(p->edges, p->max*sizeof(unsigned));
        assert(new_edges != NULL);
        p->edges = new_edges;
    }
    p->edges[p->n++] = e;
}

unsigned edge_iterate(GraphEdge *p)
{
    static unsigned n, *curr;

    if (p != NULL) {
        n = p->n;
        curr = p->edges;
    }
    if (n) {
        --n;
        return *curr++;
    }
    return (unsigned)-1;
}

void edge_free(GraphEdge *p)
{
    free(p->edges);
}

unsigned new_cg_node(char *func_id)
{
    unsigned i;

    for (i = 0; i < cg_nodes_counter; i++)
        if (equal(cg_nodes[i].func_id, func_id))
            return i;
    if (cg_nodes_counter >= cg_nodes_max) {
        CGNode *new_p;

        /* grow */
        new_p = realloc(cg_nodes, CGROW*cg_nodes_max*sizeof(CGNode));
        assert(new_p != NULL);
        cg_nodes_max *= CGROW;
        cg_nodes = new_p;
    }
    memset(&cg_nodes[cg_nodes_counter], 0, sizeof(CGNode));
    cg_nodes[cg_nodes_counter].func_id = func_id;
    edge_init(&cg_nodes[cg_nodes_counter].out, 1);
    return cg_nodes_counter++;
}

void new_cfg_node(unsigned leader)
{
    if (cfg_nodes_counter >= cfg_nodes_max) {
        CFGNode *new_p;

        /* grow */
        new_p = realloc(cfg_nodes, NGROW*cfg_nodes_max*sizeof(CFGNode));
        assert(new_p != NULL);
        cfg_nodes_max *= NGROW;
        cfg_nodes = new_p;
    }
    cfg_nodes[cfg_nodes_counter].leader = leader;
    edge_init(&cfg_nodes[cfg_nodes_counter].out, 2);
    edge_init(&cfg_nodes[cfg_nodes_counter].in, 5);
    ++cfg_nodes_counter;
}

void emit_i(OpKind op, Declaration *type, unsigned tar, unsigned arg1, unsigned arg2)
{
    if (ic_instructions_counter >= ic_instructions_max) {
        Quad *new_p;

        /* grow */
        new_p = realloc(ic_instructions, IGROW*ic_instructions_max*sizeof(Quad));
        assert(new_p != NULL);
        ic_instructions_max *= IGROW;
        ic_instructions = new_p;
    }
    ic_instructions[ic_instructions_counter].op = op;
    ic_instructions[ic_instructions_counter].type = type;
    ic_instructions[ic_instructions_counter].tar = tar;
    ic_instructions[ic_instructions_counter].arg1 = arg1;
    ic_instructions[ic_instructions_counter].arg2 = arg2;
    ++ic_instructions_counter;
}

unsigned new_address(AddrKind kind)
{
    if (ic_addresses_counter >= ic_addresses_max) {
        Address *new_p;

        /* grow */
        new_p = realloc(ic_addresses, AGROW*ic_addresses_max*sizeof(Address));
        assert(new_p != NULL);
        ic_addresses_max *= AGROW;
        ic_addresses = new_p;
    }
    ic_addresses[ic_addresses_counter].kind = kind;

    return ic_addresses_counter++;
}

unsigned new_temp_addr(void)
{
    unsigned n;
    char s[10], *p;
    static unsigned t_counter = 1;

    n = new_address(TempKind);
    p = arena_alloc(temp_names_arena, sprintf(s, "t%u", t_counter++)+1);
    address(n).cont.nid = nid_counter;
    new_nid(strcpy(p, s));

    return n;
}

unsigned new_label(void)
{
    unsigned L;

    if (label_counter >= label_max) {
        unsigned *p;

        p = realloc(lab2instr, label_max*2*sizeof(unsigned));
        assert(p != NULL);
        label_max *= 2;
        lab2instr = p;
    }
    L = new_address(IConstKind);
    address(L).cont.val = label_counter++;

    return L;
}

void emit_label(unsigned L)
{
    lab2instr[address(L).cont.val] = ic_instructions_counter;
    emit_i(OpLab, NULL, L, 0, 0);
}

void new_atv(int vnid)
{
    if (atv_counter >= atv_max) {
        atv_max *= 2;
        atv = realloc(atv, atv_max*sizeof(int));
        assert(atv != NULL);
    }
    atv[atv_counter++] = vnid;
}

void ic_init(void)
{
    location_init();

    /* init instruction buffer */
    ic_instructions = malloc(IINIT*sizeof(Quad));
    assert(ic_instructions != NULL);
    ic_instructions_max = IINIT;
    ic_instructions_counter = 0;

    /* init address buffer */
    ic_addresses = malloc(AINIT*sizeof(Address));
    assert(ic_addresses != NULL);
    ic_addresses_max = AINIT;
    ic_addresses_counter = 1; /* address 0 is reserved for 'empty' */

    /* init CFG buffer */
    cfg_nodes = malloc(NINIT*sizeof(CFGNode));
    assert(cfg_nodes != NULL);
    cfg_nodes_max = NINIT;
    cfg_nodes_counter = 1; /* 0 reserved for null node */

    /* init CG buffer */
    cg_nodes = malloc(CINIT*sizeof(CGNode));
    assert(cg_nodes != NULL);
    cg_nodes_max = CINIT;
    cg_nodes_counter = 0;

    /* init nid -> sid table */
    nid2sid_tab = malloc(128*sizeof(char *));
    assert(nid2sid_tab != NULL);
    nid_max = 128;
    nid_counter = 0;

    id_table_arena = arena_new(256);
    temp_names_arena = arena_new(256);

    /* FALSE/TRUE addresses */
    true_addr = new_address(IConstKind);
    address(true_addr).cont.uval = 1;
    false_addr = new_address(IConstKind);
    address(false_addr).cont.uval = 0;

    memset_node.attr.str = "memset";
    memset_addr = new_address(IdKind);
    address(memset_addr).cont.nid = get_var_nid("memset", 0);
    address(memset_addr).cont.var.e = &memset_node;

    memcpy_node.attr.str = "memcpy";
    memcpy_addr = new_address(IdKind);
    address(memcpy_addr).cont.nid = get_var_nid("memcpy", 0);
    address(memcpy_addr).cont.var.e = &memcpy_node;

    label_max = 64;
    lab2instr = malloc(label_max*sizeof(unsigned));

    atv_max = 32;
    atv = malloc(atv_max*sizeof(int));
    atv_counter = 0;
}

void ic_reset(void)
{
    size_of_local_area = 0;
    local_offset = 0;
    label_counter = 0;
}

void ic_free_all(void)
{
    unsigned i;

    free(ic_instructions);
    free(ic_addresses);
    for (i = 1; i < cfg_nodes_counter; i++) {
        edge_free(&cfg_node(i).out);
        edge_free(&cfg_node(i).in);
        bset_free(cfg_node(i).UEVar);
        bset_free(cfg_node(i).VarKill);
        bset_free(cfg_node(i).LiveOut);
        bset_free(cfg_node(i).Dom);
    }
    free(cfg_nodes);
    arena_destroy(id_table_arena);
    arena_destroy(temp_names_arena);
    /*free_PointOut();*/
    free(lab2instr);
}

static unsigned curr_cg_node;
static void print_CG(void);
static unsigned ic_func_first_instr;
static void dump_ic(unsigned fn);
static void build_CFG(void);
static void print_CFG(unsigned fn);
static void number_CG(void);
static void ic_simplify(void);
static void ic_find_atv_in_expr(ExecNode *e);
static void ic_find_atv_in_init(TypeExp *ds, TypeExp *dct, ExecNode *e);
static void ic_find_atv(void);

/* find address-taken variables in static initializer expression */
void ic_find_atv_in_expr(ExecNode *e)
{
    if (e->kind.exp == OpExp) {
        switch (e->attr.op) {
        case TOK_UNARY_PLUS:
        case TOK_UNARY_MINUS:
        case TOK_COMPLEMENT:
        case TOK_NEGATION:
        case TOK_ADDRESS_OF:
            ic_find_atv_in_expr(e->child[0]);
            break;
        case TOK_MUL:
        case TOK_DIV:
        case TOK_REM:
        case TOK_LSHIFT:
        case TOK_RSHIFT:
        case TOK_BW_AND:
        case TOK_BW_XOR:
        case TOK_BW_OR:
        case TOK_PLUS:
        case TOK_MINUS:
            ic_find_atv_in_expr(e->child[0]);
            ic_find_atv_in_expr(e->child[1]);
            break;
        default:
            break;
        }
    } else if (e->kind.exp == IdExp) {
        new_atv(get_var_nid(e->attr.str, e->attr.var.scope));
    }
}

/* find address-taken variables in static initializer */
void ic_find_atv_in_init(TypeExp *ds, TypeExp *dct, ExecNode *e)
{
    TypeExp *ts;

    if (dct != NULL) {
        if (dct->op != TOK_SUBSCRIPT) {
            ic_find_atv_in_expr(e);
            return;
        }
        if (e->kind.exp == StrLitExp)
            return;
        for (e = e->child[0]; e != NULL; e = e->sibling)
            ic_find_atv_in_init(ds, dct->child, e);
    } else if ((ts=get_type_spec(ds))->op == TOK_STRUCT) {
        DeclList *d;

        e = e->child[0];
        d = ts->attr.dl;
        for (; d != NULL; d = d->next) {
            dct = d->decl->idl;
            for (; e!=NULL && dct!=NULL; e=e->sibling, dct=dct->sibling)
                ic_find_atv_in_init(d->decl->decl_specs, dct->child, e);
            if (e == NULL)
                break;
        }
    } else if (ts->op == TOK_UNION) {
        e = e->child[0];
        ic_find_atv_in_init(ts->attr.dl->decl->decl_specs, ts->attr.dl->decl->idl->child, e);
    } else {
        ic_find_atv_in_expr(e);
    }
}

/* find address-taken variables in static initializers */
void ic_find_atv(void)
{
    ExternId *np;

    for (np = static_objects_list; np != NULL; np = np->next)
        if (np->declarator->attr.e != NULL)
            ic_find_atv_in_init(np->decl_specs, np->declarator->child, np->declarator->attr.e);
}

void ic_simplify(void)
{
    unsigned i;

    for (i = ic_func_first_instr; i < ic_instructions_counter; i++) {
        unsigned tar, arg1, arg2;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        switch (instruction(i).op) {
#define is_iconst(a) (address(a).kind == IConstKind)
#define fold(_i_, _op_) (\
    instruction(_i_).op = OpAsn,\
    address(arg1).kind = IConstKind,\
    address(arg1).cont.val = address(arg1).cont.val _op_ address(arg2).cont.val)
#define fold2(_i_, _op_)\
    do {\
        instruction(_i_).op = OpAsn;\
        address(arg1).kind = IConstKind;\
        if (is_unsigned_int(get_type_category(instruction(i).type)))\
            address(arg1).cont.uval = address(arg1).cont.uval _op_ address(arg2).cont.uval;\
        else\
            address(arg1).cont.val = address(arg1).cont.val _op_ address(arg2).cont.val;\
    } while (0)

        /*
         * x = 0+y => x = y
         * x = y+0 => x = y
         */
        case OpAdd:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2)) {
                    fold(i, +);
                } else if (address(arg1).cont.val == 0) {
                    instruction(i).op = OpAsn;
                    instruction(i).arg1 = arg2;
                }
            } else if (is_iconst(arg2) && address(arg2).cont.val==0) {
                instruction(i).op = OpAsn;
            }
            break;
        /*
         * x = 0-y => x = -y
         * x = y-0 => x = y
         */
        case OpSub:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2)) {
                    fold(i, -);
                } else if (address(arg1).cont.val == 0) {
                    instruction(i).op = OpNeg;
                    instruction(i).arg1 = arg2;
                }
            } else if (is_iconst(arg2) && address(arg2).cont.val==0) {
                instruction(i).op = OpAsn;
            }
            break;
        /*
         * x = 0*y  => x = 0
         * x = 1*y  => x = y
         * x = -1*y => x = -y
         * x = p*y  => x = y<<n (with p == 2^n)
         * x = y*0  => x = 0
         * x = y*1  => x = y
         * x = y*-1 => x = -y
         * x = y*p  => x = y<<n
         */
        case OpMul:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2)) {
                    fold(i, *);
                } else if (address(arg1).cont.val == 0) {
                    instruction(i).op = OpAsn;
                } else if (address(arg1).cont.val == 1) {
                    instruction(i).op = OpAsn;
                    instruction(i).arg1 = arg2;
                } else if (address(arg1).cont.val == -1) {
                    instruction(i).op = OpNeg;
                    instruction(i).arg1 = arg2;
                } else if (is_po2(address(arg1).cont.uval)) {
                    instruction(i).op = OpSHL;
                    address(arg1).cont.uval = ilog2(address(arg1).cont.uval);
                    instruction(i).arg1 = arg2;
                    instruction(i).arg2 = arg1;
                }
            } else if (is_iconst(arg2)) {
                if (address(arg2).cont.val == 0) {
                    instruction(i).op = OpAsn;
                    address(arg1).kind = IConstKind;
                    address(arg1).cont.val = 0;
                } else if (address(arg2).cont.val == 1) {
                    instruction(i).op = OpAsn;
                } else if (address(arg2).cont.val == -1) {
                    instruction(i).op = OpNeg;
                } else if (is_po2(address(arg2).cont.uval)) {
                    instruction(i).op = OpSHL;
                    address(arg2).cont.uval = ilog2(address(arg2).cont.uval);
                }
            }
            break;
        /*
         * x = 0/y => x = 0
         * x = y/1 => x = y
         */
        case OpDiv:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2))
                    fold2(i, /);
                else if (address(arg1).cont.val == 0)
                    instruction(i).op = OpAsn;
            } else if (is_iconst(arg2) && address(arg2).cont.val==1) {
                instruction(i).op = OpAsn;
            }
            break;
        /*
         * x = 0%y => x = 0
         * x = y%1 => x = 0
         */
        case OpRem:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2))
                    fold2(i, %);
                else if (address(arg1).cont.val == 0)
                    instruction(i). op = OpAsn;
            } else if (is_iconst(arg2) && address(arg2).cont.val==1) {
                instruction(i). op = OpAsn;
                address(arg1).kind = IConstKind;
                address(arg1).cont.val = 0;
            }
            break;
        /*
         * x = 0<<y => x = 0
         * x = y<<0 => x = y
         */
        case OpSHL:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2))
                    fold(i, <<);
                else if (address(arg1).cont.val == 0)
                    instruction(i).op = OpAsn;
            } else if (is_iconst(arg2) && address(arg2).cont.val==0) {
                instruction(i).op = OpAsn;
            }
            break;
        /*
         * x = 0>>y => x = 0
         * x = y>>0 => x = y
         */
        case OpSHR:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2))
                    fold2(i, >>);
                else if (address(arg1).cont.val == 0)
                    instruction(i).op = OpAsn;
            } else if (is_iconst(arg2) && address(arg2).cont.val==0) {
                instruction(i).op = OpAsn;
            }
            break;
        /*
         * x = 0&y => x = 0
         * x = y&0 => x = 0
         */
        case OpAnd:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2))
                    fold(i, &);
                else if (address(arg1).cont.val == 0)
                    instruction(i).op = OpAsn;
            } else if (is_iconst(arg2) && address(arg2).cont.val==0) {
                instruction(i).op = OpAsn;
                instruction(i).arg1 = arg2;
            }
            break;
        /*
         * x = 0|y => x = y
         * x = y|0 => x = y
         */
        case OpOr:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2)) {
                    fold(i, |);
                } else if (address(arg1).cont.val == 0) {
                    instruction(i).op = OpAsn;
                    instruction(i).arg1 = arg2;
                }
            } else if (is_iconst(arg2) && address(arg2).cont.val==0) {
                instruction(i).op = OpAsn;
            }
            break;
        /*
         * x = 0^y => x = y
         * x = y^0 => x = y
         */
        case OpXor:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2)) {
                    fold(i, ^);
                } else if (address(arg1).cont.val == 0) {
                    instruction(i).op = OpAsn;
                    instruction(i).arg1 = arg2;
                }
            } else if (is_iconst(arg2) && address(arg2).cont.val==0) {
                instruction(i).op = OpAsn;
            }
            break;
        case OpEQ:
            if (is_iconst(arg1))
                if (is_iconst(arg2))
                    fold(i, ==);
            break;
        case OpNEQ:
            if (is_iconst(arg1))
                if (is_iconst(arg2))
                    fold(i, !=);
            break;
        case OpLT:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2)) {
                    instruction(i).op = OpAsn;
                    address(arg1).kind = IConstKind;
                    if ((int)instruction(i).op == IC_SIGNED)
                        address(arg1).cont.val = address(arg1).cont.val<address(arg2).cont.val;
                    else
                        address(arg1).cont.val = address(arg1).cont.uval<address(arg2).cont.uval;
                }
            }
            break;
        case OpLET:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2)) {
                    instruction(i).op = OpAsn;
                    address(arg1).kind = IConstKind;
                    if ((int)instruction(i).op == IC_SIGNED)
                        address(arg1).cont.val = address(arg1).cont.val<=address(arg2).cont.val;
                    else
                        address(arg1).cont.val = address(arg1).cont.uval<=address(arg2).cont.uval;
                }
            }
            break;
        case OpGT:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2)) {
                    instruction(i).op = OpAsn;
                    address(arg1).kind = IConstKind;
                    if ((int)instruction(i).op == IC_SIGNED)
                        address(arg1).cont.val = address(arg1).cont.val>address(arg2).cont.val;
                    else
                        address(arg1).cont.val = address(arg1).cont.uval>address(arg2).cont.uval;
                }
            }
            break;
        case OpGET:
            if (is_iconst(arg1)) {
                if (is_iconst(arg2)) {
                    instruction(i).op = OpAsn;
                    address(arg1).kind = IConstKind;
                    if ((int)instruction(i).op == IC_SIGNED)
                        address(arg1).cont.val = address(arg1).cont.val>=address(arg2).cont.val;
                    else
                        address(arg1).cont.val = address(arg1).cont.uval>=address(arg2).cont.uval;
                }
            }
            break;

#undef fold
#undef fold2
#define fold(_i_, _op_)\
instruction(i).op = OpAsn,\
address(arg1).kind = IConstKind,\
address(arg1).cont.val = _op_ address(arg1).cont.val

        case OpNeg:
            if (is_iconst(arg1))
                fold(i, -);
            break;
        case OpCmpl:
            if (is_iconst(arg1))
                fold(i, ~);
            break;
        case OpNot:
            if (is_iconst(arg1))
                fold(i, !);
            break;
        case OpCh:
            if (is_iconst(arg1))
                fold(i, (char));
            break;
        case OpUCh:
            if (is_iconst(arg1))
                fold(i, (unsigned char));
            break;
        case OpSh:
            if (is_iconst(arg1))
                fold(i, (short));
            break;
        case OpUSh:
            if (is_iconst(arg1))
                fold(i, (unsigned short));
            break;
        // case OpAddrOf:
        // case OpInd:
        // case OpAsn:
        // case OpCall:
        // case OpIndCall:

        // case OpIndAsn:

        // case OpLab:
        // case OpJmp:

        // case OpArg:
        // case OpRet:

        // case OpSwitch:
        // case OpCase:

        case OpCBr:
            if (is_iconst(arg1)) {
                instruction(i).op = OpJmp;
                if (address(arg1).cont.val)
                    ; //instruction(i).tar = tar;
                else
                    instruction(i).tar = arg2;
            }
            break;
        // case OpNOp:
        } /* switch (instruction(i).op) */
#undef is_iconst
#undef fold
#undef fold2
    }
}

void ic_main(ExternId *func_def_list[])
{
    unsigned i;
    ExternId *ed;

    ic_init();
    for (i = 0, ed = func_def_list[i]; ed != NULL; ++i, ed = func_def_list[i]) {
        ic_func_first_instr = ic_instructions_counter;
        curr_cg_node = new_cg_node(ed->declarator->str);
        ic_function_definition(ed->decl_specs, ed->declarator);
        ic_simplify();
        build_CFG();
        ic_reset();
    }
    if (i == 0)
        return;

    ic_find_atv();
    address_taken_variables = bset_new(nid_counter);
    for (i = 0; i < atv_counter; i++)
        bset_insert(address_taken_variables, atv[i]);
    free(atv);

    /*
     * [!] Important: from now on the nid counter
     * must stand still for the analyses to work
     * correctly.
     */
    number_CG();
    // opt_main();
    for (i = 0; i < cg_nodes_counter; i++) {
        // dump_ic(i);
        // print_CFG(i);
        dflow_Dom(i);
        dflow_LiveOut(i);
        // dflow_ReachIn(i, i == cg_nodes_counter-1);
    }
    // print_CG();
}

static void fix_gotos(void);
static unsigned exit_label;

void ic_function_definition(TypeExp *decl_specs, TypeExp *header)
{
    Token cat;
    DeclList *p;
    Declaration ty;
    int param_offs;
    unsigned entry_label;

    location_push_scope();
    p = header->child->attr.dl;
    if (get_type_spec(p->decl->decl_specs)->op==TOK_VOID && p->decl->idl==NULL)
        p = NULL; /* function with no parameters */

    param_offs = X86_PARAM_END;
    while (p != NULL) {
        if (p->decl->idl!=NULL && p->decl->idl->op==TOK_ELLIPSIS)
            break; /* start of optional parameters (`...') */

        location_new(p->decl->idl->str, param_offs);
        DEBUG_PRINTF("==> param:`%s', offset:%d\n", p->decl->idl->str, param_offs);
        ty.decl_specs = p->decl->decl_specs;
        ty.idl = p->decl->idl->child;
        param_offs += round_up(compute_sizeof(&ty), 4);

        if (cg_node(curr_cg_node).pn == NULL) {
            cg_node(curr_cg_node).pn = malloc(sizeof(ParamNid));
            cg_node(curr_cg_node).pn->sid = p->decl->idl->str;
            cg_node(curr_cg_node).pn->nid = -1;
            cg_node(curr_cg_node).pn->next = NULL;
        } else {
            ParamNid *pn;

            for (pn = cg_node(curr_cg_node).pn; pn->next != NULL; pn = pn->next);
            pn->next = malloc(sizeof(ParamNid));
            pn = pn->next;
            pn->sid = p->decl->idl->str;
            pn->nid = -1;
            pn->next = NULL;
        }

        p = p->next;
    }

    ty.decl_specs = decl_specs;
    ty.idl = header->child->child;
    if ((cat=get_type_category(&ty))==TOK_STRUCT || cat==TOK_UNION)
        local_offset -= 4; /* allocate space for the 'return value address' */

    entry_label = new_label();
    exit_label = new_label();

    /* build ENTRY node */
    emit_i(OpJmp, NULL, entry_label, 0, 0);
    emit_label(entry_label);

    ic_compound_statement(header->attr.e, FALSE);
    emit_label(exit_label); /* return's target */

    /* build EXIT node */
    exit_label = new_label();
    emit_i(OpJmp, NULL, exit_label, 0, 0);
    emit_label(exit_label);

    location_pop_scope();
    fix_gotos();
    cg_node(curr_cg_node).size_of_local_area = size_of_local_area;
}

static int pocount;
static int *visited, nunvisited;
// =============================================================================
// Call-Graph (CG)
// =============================================================================
static void number_subCG(unsigned n);
static void print_CG_ordering(void);

void print_CG_ordering(void)
{
    unsigned i;

    printf("CG PO = [ ");
    for (i = 0; i < cg_nodes_counter; i++)
        printf("%u, ", cg_node(i).PO);
    printf("]\n");
    printf("CG RPO = [ ");
    for (i = 0; i < cg_nodes_counter; i++)
        printf("%u, ", cg_node(i).RPO);
    printf("]\n");
}

void print_CG(void)
{
    unsigned i;

    printf("Program Call-Graph\n");
// #if DEBUG
    print_CG_ordering();
// #endif
    printf("digraph {\n");
    for (i = 0; i < cg_nodes_counter; i++) {
        unsigned j;

        printf("V%u[label=\"F%u %s\\n", i, i, cg_node(i).func_id);
        printf("[%u, %u]\"];\n", cg_node(i).bb_i, cg_node(i).bb_f);
        for (j = edge_iterate(&cg_node(i).out); j != -1; j = edge_iterate(NULL))
            printf("V%u -> V%u;\n", i, j);
    }
    printf("}\n");
}

void number_CG(void)
{
    unsigned n, n2;

    visited = calloc(cg_nodes_counter, sizeof(int));
    nunvisited = cg_nodes_counter;
    pocount = 0;

    while (nunvisited != 0) {
        for (n = 0; n < cg_nodes_counter; n++)
            if (!visited[n])
                break;
        number_subCG(n);
    }
    for (n2=cg_nodes_counter-1, n=0; n < cg_nodes_counter; n2--, n++)
        cg_node(n).RPO = cg_node(n2).PO;

    free(visited);
}

void number_subCG(unsigned n)
{
    int i;

    visited[n] = TRUE;
    --nunvisited;
    if (!cg_node_is_empty(n)) {
        for (i = 0; i < cg_node(n).out.n; i++) {
            unsigned succ;

            succ = cg_node(n).out.edges[i];
            if (!visited[succ])
                number_subCG(succ);
        }
    }
    cg_node(pocount).PO = n;
    ++pocount;
}

// =============================================================================
// Control Flow Graph (CFG)
// =============================================================================
static void number_CFG(void);
static void number_subCFG(unsigned n);
static void print_CFG_ordering(unsigned fn);

void print_CFG_ordering(unsigned fn)
{
    unsigned i;
    unsigned entry_bb, last_bb;

    entry_bb = cg_node(fn).bb_i;
    last_bb = cg_node(fn).bb_f;

    printf("CFG PO = [ ");
    for (i = entry_bb; i <= last_bb; i++)
        printf("%u, ", cfg_node(i).PO);
    printf("]\n");

    printf("CFG RPO = [ ");
    for (i = entry_bb; i <= last_bb; i++)
        printf("%u, ", cfg_node(i).RPO);
    printf("]\n");
}

/* emit a DOT definition of the CFG */
void print_CFG(unsigned fn)
{
    unsigned i;

    if (cg_node_is_empty(fn))
        return;

// #if DEBUG
    print_CFG_ordering(fn);
// #endif

    printf("digraph {\n");
    for (i = cg_node(fn).bb_i; i <= cg_node(fn).bb_f; i++) {
        unsigned j;

        printf("V%u[label=\"B%u ", i, i);
        for (j = cfg_node(i).leader; j <= cfg_node(i).last; j++)
            printf("(%u), ", j);
        printf("\"];\n");

        for (j = 0; j < cfg_node(i).out.n; j++)
            printf("V%u -> V%u;\n", i, cfg_node(i).out.edges[j]);
    }
    printf("}\n");
}

void number_CFG(void)
{
    unsigned n, n2;

    visited = calloc(cg_node_nbb(curr_cg_node), sizeof(int));
    pocount = cg_node(curr_cg_node).bb_i;

    nunvisited = cg_node_nbb(curr_cg_node);
    while (nunvisited != 0) {
        for (n = cg_node(curr_cg_node).bb_i; n <= cg_node(curr_cg_node).bb_f; n++)
            if (!visited[n-cg_node(curr_cg_node).bb_i])
                break;
        number_subCFG(n);
    }
    n = cg_node(curr_cg_node).bb_i;
    n2 = cg_node(curr_cg_node).bb_f;
    while (n <= cg_node(curr_cg_node).bb_f) {
        cfg_node(n).RPO = cfg_node(n2).PO;
        --n2, ++n;
    }

    free(visited);
}

void number_subCFG(unsigned n)
{
    int i;

    visited[n-cg_node(curr_cg_node).bb_i] = TRUE;
    --nunvisited;
    for (i = 0; i < cfg_node(n).out.n; i++) {
        unsigned succ;

        succ = cfg_node(n).out.edges[i];
        if (!visited[succ-cg_node(curr_cg_node).bb_i])
            number_subCFG(succ);
    }
    cfg_node(pocount).PO = n;
    ++pocount;
}

void build_CFG(void)
{
    int i;
    unsigned leader, func_ninstr, *leader2node;

    func_ninstr = ic_instructions_counter-ic_func_first_instr;
    leader2node = calloc(func_ninstr, sizeof(unsigned));

    cg_node(curr_cg_node).bb_i = cfg_nodes_counter;
    leader2node[0] = cfg_nodes_counter;
    new_cfg_node(ic_func_first_instr);
    leader2node[1] = cfg_nodes_counter;
    new_cfg_node(ic_func_first_instr+1);
    for (i = ic_func_first_instr+2; i < ic_instructions_counter; i++) {
        unsigned *p;

        switch (instruction(i).op) {
        case OpJmp:
            leader = lab2instr[address(instruction(i).tar).cont.val];
            if (!*(p=&leader2node[leader-ic_func_first_instr])) {
                *p = cfg_nodes_counter;
                new_cfg_node(leader);
            }
            leader = i+1;
            if (!*(p=&leader2node[leader-ic_func_first_instr])) {
                *p = cfg_nodes_counter;
                new_cfg_node(leader);
            }
            break;
        case OpCBr:
            leader = lab2instr[address(instruction(i).tar).cont.val];
            if (!*(p=&leader2node[leader-ic_func_first_instr])) {
                *p = cfg_nodes_counter;
                new_cfg_node(leader);
            }
            leader = lab2instr[address(instruction(i).arg2).cont.val];
            if (!*(p=&leader2node[leader-ic_func_first_instr])) {
                *p = cfg_nodes_counter;
                new_cfg_node(leader);
            }
            break;
        case OpSwitch: {
            int j;

            for (j = i+1; instruction(j).op == OpCase; j++) {
                leader = lab2instr[address(instruction(j).arg1).cont.val];
                if (!*(p=&leader2node[leader-ic_func_first_instr])) {
                    *p = cfg_nodes_counter;
                    new_cfg_node(leader);
                }
            }
        }
            break;
        }
    }
    cg_node(curr_cg_node).bb_f = cfg_nodes_counter-1;

    for (i = cg_node(curr_cg_node).bb_i; i < cfg_nodes_counter; i++) {
        unsigned last;

        last = cfg_node(i).leader+1;
        while (last<ic_instructions_counter && !leader2node[last-ic_func_first_instr])
             ++last;
        cfg_node(i).last = --last;

        /* add edges */
        switch (instruction(last).op) {
        case OpCBr: {
            unsigned succ1, succ2;

            leader = lab2instr[address(instruction(last).tar).cont.val];
            succ1 = leader2node[leader-ic_func_first_instr];
            leader = lab2instr[address(instruction(last).arg2).cont.val];
            succ2 = leader2node[leader-ic_func_first_instr];

            /* set out edges of current node */
            edge_add(&cfg_node(i).out, succ1);
            edge_add(&cfg_node(i).out, succ2);

            /* set in edges of successors */
            edge_add(&cfg_node(succ1).in, i);
            edge_add(&cfg_node(succ2).in, i);
        }
            break;

        case OpJmp: {
            unsigned succ;

            leader = lab2instr[address(instruction(last).tar).cont.val];
            succ = leader2node[leader-ic_func_first_instr];
            assert(succ != 0);

            edge_add(&cfg_node(i).out, succ);
            edge_add(&cfg_node(succ).in, i);
        }
            break;

        case OpCase: {
            unsigned succ;

            do {
                leader = lab2instr[address(instruction(last).arg1).cont.val];
                succ = leader2node[leader-ic_func_first_instr];
                assert(succ != 0);

                edge_add(&cfg_node(i).out, succ);
                edge_add(&cfg_node(succ).in, i);
                --last;
            } while (instruction(last).op == OpCase);
        }
            break;

        default: /* fall-through */
            if (last != ic_instructions_counter-1) {
                unsigned succ;

                assert(instruction(last+1).op == OpLab);

                leader = lab2instr[address(instruction(last+1).tar).cont.val];
                succ = leader2node[leader-ic_func_first_instr];
                assert(succ != 0);

                edge_add(&cfg_node(i).out, succ);
                edge_add(&cfg_node(succ).in, i);
            }
            break;
        }
    }
    free(leader2node);
    number_CFG();
}

// =============================================================================
// Statements
// =============================================================================
typedef struct SwitchLabel SwitchLabel;
struct SwitchLabel {
    unsigned lab;
    long val;
    SwitchLabel *next;
} *ic_case_labels[MAX_SWITCH_NEST], *ic_default_labels[MAX_SWITCH_NEST];
static int ic_switch_nesting_level = -1;

typedef struct Label Label;
static struct Label {
    char *str;
    unsigned addr;
    Label *next;
} *ic_labels;

static int gotos_to_fix[MAX_GOTOS_PER_FUNC], gotos_to_fix_counter;
static void register_label(char *str, unsigned addr);
static unsigned get_label_address(char *str);

static unsigned btarget_stack[128], ctarget_stack[128];
static int bt_stack_top = -1, ct_stack_top = -1;
static void push_break_target(unsigned lab);
static void pop_break_target(void);
static void push_continue_target(unsigned lab);
static void pop_continue_target(void);

static void ic_if_statement(ExecNode *s);
static void ic_switch_statement(ExecNode *s);
static void ic_while_statement(ExecNode *s);
static void ic_do_statement(ExecNode *s);
static void ic_for_statement(ExecNode *s);
static void ic_goto_statement(ExecNode *s);
static void ic_continue_statement(void);
static void ic_break_statement(void);
static void ic_return_statement(ExecNode *s);
static void ic_case_statement(ExecNode *s);
static void ic_default_statement(ExecNode *s);
static void ic_expression_statement(ExecNode *s);
static void ic_label_statement(ExecNode *s);
static void ic_statement(ExecNode *s);
static unsigned ic_expression2(ExecNode *e);
static unsigned ic_expr_convert(ExecNode *e, Declaration *dest);
static void ic_auto_init(TypeExp *ds, TypeExp *dct, ExecNode *e, unsigned id, unsigned offset);
static void ic_zero(unsigned id, unsigned offset, unsigned nb);

void push_break_target(unsigned lab)
{
    btarget_stack[++bt_stack_top] = lab;
}

void pop_break_target(void)
{
    --bt_stack_top;
}

void push_continue_target(unsigned lab)
{
    ctarget_stack[++ct_stack_top] = lab;
}

void pop_continue_target(void)
{
    --ct_stack_top;
}

void ic_statement(ExecNode *s)
{
    switch (s->kind.stmt) {
    case CmpndStmt:
        ic_compound_statement(s, TRUE);
        break;
    case IfStmt:
        ic_if_statement(s);
        break;
    case SwitchStmt:
        ic_switch_statement(s);
        break;
    case WhileStmt:
        ic_while_statement(s);
        break;
    case DoStmt:
        ic_do_statement(s);
        break;
    case ForStmt:
        ic_for_statement(s);
        break;
    case GotoStmt:
        ic_goto_statement(s);
        break;
    case ContinueStmt:
        ic_continue_statement();
        break;
    case BreakStmt:
        ic_break_statement();
        break;
    case ReturnStmt:
        ic_return_statement(s);
        break;
    case CaseStmt:
        ic_case_statement(s);
        break;
    case DefaultStmt:
        ic_default_statement(s);
        break;
    case ExpStmt:
        ic_expression_statement(s);
        break;
    case LabelStmt:
        ic_label_statement(s);
        break;
    }
}

void ic_if_statement(ExecNode *s)
{
    /*
    ==> if (<e>) <stmt1> else <stmt2>
    CBr L1, <e>, L2
    L1:
    <stmt1>
    Jmp L3
    L2:
    <stmt2>
    L3:
    ...
     */
    int else_part;
    unsigned L1, L2, L3;

    /* does the else part is present? */
    else_part = s->child[2]!=NULL;

    L1 = new_label();
    L2 = new_label();
    if (else_part)
        L3 = new_label();

    emit_i(OpCBr, NULL, L1, ic_expression2(s->child[0]), L2);
    emit_label(L1);
    ic_statement(s->child[1]);
    if (else_part)
        emit_i(OpJmp, NULL, L3, 0, 0);
    emit_label(L2);
    if (else_part) {
        ic_statement(s->child[2]);
        emit_label(L3);
    }
}

void ic_while_statement(ExecNode *s)
{
    /*
    ==> while (<e>) <stmt>
    CBr L1, <e>, L3
    L1:
    <stmt>
    L2:
    CBr L1, <e>, L3
    L3:
    ...
     */
    unsigned L1, L2, L3;

    L1 = new_label();
    L2 = new_label();
    L3 = new_label();

    emit_i(OpCBr, NULL, L1, ic_expression2(s->child[0]), L3);
    emit_label(L1);
    push_break_target(L3), push_continue_target(L2);
    ic_statement(s->child[1]);
    pop_break_target(), pop_continue_target();
    emit_label(L2);
    emit_i(OpCBr, NULL, L1, ic_expression2(s->child[0]), L3);
    emit_label(L3);
}

void ic_do_statement(ExecNode *s)
{
    /*
    ==> do <stmt> while (<e>)
    L1:
    <stmt>
    L2:
    CBr L1, <e>, L3
    L3:
    ...
     */
    int iprev;
    unsigned L1, L2, L3;

    L2 = new_label();
    L3 = new_label();

    iprev = ic_instructions_counter-1;
    if (instruction(iprev).op != OpLab) {
        L1 = new_label();
        emit_label(L1);
    } else {
        L1 = instruction(iprev).tar;
    }
    push_break_target(L3), push_continue_target(L2);
    ic_statement(s->child[1]);
    pop_break_target(), pop_continue_target();
    emit_label(L2);
    emit_i(OpCBr, NULL, L1, ic_expression2(s->child[0]), L3);
    emit_label(L3);
}

void ic_for_statement(ExecNode *s)
{
    /*
    ==> for (<e1>; <e2>; <e3>) <stmt>
    <e1>
    CBr L1, <e2>, L3
    L1:
    <stmt>
    L2:
    <e3>
    CBr L1, <e2>, L3
    L3:
    ...
     */
    unsigned L1, L2, L3;

    L1 = new_label();
    L2 = new_label();
    L3 = new_label();

    if (s->child[1] != NULL)
        ic_expression2(s->child[1]);
    if (s->child[0] != NULL)
        emit_i(OpCBr, NULL, L1, ic_expression2(s->child[0]), L3);
    emit_label(L1);
    push_break_target(L3), push_continue_target(L2);
    ic_statement(s->child[3]);
    pop_break_target(), pop_continue_target();
    emit_label(L2);
    if (s->child[2] != NULL)
        ic_expression2(s->child[2]);
    if (s->child[0] != NULL)
        emit_i(OpCBr, NULL, L1, ic_expression2(s->child[0]), L3);
    else
        emit_i(OpJmp, NULL, L1, 0, 0);
    emit_label(L3);
}

void ic_label_statement(ExecNode *s)
{
    int iprev;

    iprev = ic_instructions_counter-1;
    if (instruction(iprev).op != OpLab) {
        unsigned L;

        L = new_label();
        emit_label(L);
        register_label(s->attr.str, L);
    } else {
        register_label(s->attr.str, instruction(iprev).tar);
    }
    ic_statement(s->child[0]);
}

void ic_goto_statement(ExecNode *s)
{
    gotos_to_fix[gotos_to_fix_counter++] = ic_instructions_counter;
    emit_i(OpJmp, (Declaration *)s->attr.str, 0, 0, 0);
}

void ic_continue_statement(void)
{
    emit_i(OpJmp, NULL, ctarget_stack[ct_stack_top], 0, 0);
}

void ic_break_statement(void)
{
    emit_i(OpJmp, NULL, btarget_stack[bt_stack_top], 0, 0);
}

void ic_return_statement(ExecNode *s)
{
    if (s->child[0] != NULL) {
        Declaration *ty;

        ty = malloc(sizeof(Declaration));
        ty->decl_specs = (TypeExp *)s->child[1];
        ty->idl = (TypeExp *)s->child[2];
        emit_i(OpRet, ty, 0, ic_expr_convert(s->child[0], ty), 0);
    }
    emit_i(OpJmp, NULL, exit_label, 0, 0);
}

void ic_switch_statement(ExecNode *s)
{
    /*
    ==> switch (<e>) <stmt>
    switch <e>
    ...
    EXIT:
     */
    unsigned EXIT, i, a;
    SwitchLabel *np, *temp;

    ++ic_switch_nesting_level;

    EXIT = new_label();
    push_break_target(EXIT);
    a = new_address(IConstKind);
    address(a).cont.val = s->attr.val;
    emit_i(OpSwitch, NULL, 0, ic_expression2(s->child[0]), a);
    i = ic_instructions_counter;
    ic_instructions_counter += s->attr.val;
    ic_statement(s->child[1]);
    pop_break_target();
    emit_label(EXIT);

    for (np = ic_case_labels[ic_switch_nesting_level]; np != NULL; ) {
        a = new_address(IConstKind);
        address(a).cont.val = np->val;

        ic_instructions[i].op = OpCase;
        ic_instructions[i].tar = a;
        ic_instructions[i].arg1 = np->lab;
        ic_instructions[i].arg2 = false_addr;
        ++i;

        temp = np;
        np = np->next;
        free(temp);
    }
    ic_case_labels[ic_switch_nesting_level] = NULL;

    np = ic_default_labels[ic_switch_nesting_level];
    ic_default_labels[ic_switch_nesting_level] = NULL;
    ic_instructions[i].op = OpCase;
    ic_instructions[i].tar = false_addr;
    if (np != NULL) {
        ic_instructions[i].arg1 = np->lab;
        free(np);
    } else {
        ic_instructions[i].arg1 = EXIT;
    }
    ic_instructions[i].arg2 = true_addr;

    --ic_switch_nesting_level;
}

void ic_case_statement(ExecNode *s)
{
    unsigned iprev;
    OpKind prev_op;
    SwitchLabel *np;

    iprev = ic_instructions_counter-1;
    prev_op = instruction(iprev).op;

    np = malloc(sizeof(SwitchLabel));
    np->val = s->child[0]->attr.val;
    if (prev_op == OpLab) {
        np->lab = instruction(iprev).tar;
    } else {
        np->lab = new_label();
        emit_label(np->lab);
    }
    ic_statement(s->child[1]);
    np->next = ic_case_labels[ic_switch_nesting_level];
    ic_case_labels[ic_switch_nesting_level] = np;
}

void ic_default_statement(ExecNode *s)
{
    unsigned iprev;
    OpKind prev_op;
    SwitchLabel *np;

    iprev = ic_instructions_counter-1;
    prev_op = instruction(iprev).op;

    np = malloc(sizeof(SwitchLabel));
    if (prev_op == OpLab) {
        np->lab = instruction(iprev).tar;
    } else {
        np->lab = new_label();
        emit_label(np->lab);
    }
    ic_statement(s->child[0]);
    np->next = ic_default_labels[ic_switch_nesting_level];
    ic_default_labels[ic_switch_nesting_level] = np;
}

void ic_compound_statement(ExecNode *s, int push_scope)
{
    ExecNode *sl;
    int old_local_offset;

    if (s->locals != NULL) {
        DeclList *dl;

        old_local_offset = local_offset;
        if (push_scope)
            location_push_scope();

        /* traverse declaration list */
        for (dl = s->locals; dl != NULL; dl = dl->next) {
            TypeExp *dct, *scs;

            /* check for extern/static local variables */
            if ((scs=get_sto_class_spec(dl->decl->decl_specs)) != NULL) {
                if (scs->op == TOK_STATIC) {
                    for (dct = dl->decl->idl; dct != NULL; dct = dct->sibling) {
                        ExternId *np;

                        np = malloc(sizeof(ExternId));
                        np->decl_specs = dl->decl->decl_specs;
                        np->declarator = dct;
                        np->status = (ExtIdStatus)cg_node(curr_cg_node).func_id;
                        np->next = static_objects_list;
                        static_objects_list = np;
                    }
                    continue;
                } else if (scs->op==TOK_EXTERN || scs->op==TOK_TYPEDEF) {
                    continue;
                }
            }

            /* traverse init declarator list */
            for (dct = dl->decl->idl; dct != NULL; dct = dct->sibling) {
                Declaration lty;

                lty.decl_specs = dl->decl->decl_specs;
                lty.idl = dct->child;
                if (get_type_category(&lty) == TOK_FUNCTION)
                    continue;
                local_offset = round_up(local_offset, get_alignment(&lty));
                local_offset -= compute_sizeof(&lty);
                location_new(dct->str, local_offset);
                DEBUG_PRINTF("==> var: %s, offset: %d\n", dct->str, local_offset);
                if (dct->attr.e != NULL) {
                    unsigned a;
                    ExecNode *id_node;

                    id_node = calloc(1, sizeof(ExecNode));
                    id_node->node_kind = ExpNode;
                    id_node->kind.exp = IdExp;
                    id_node->attr.var.id = dct->str;
                    id_node->attr.var.scope = s->attr.var.scope;
                    id_node->attr.var.linkage = LINKAGE_NONE;
                    id_node->attr.var.duration = DURATION_AUTO;
                    id_node->attr.var.is_param = FALSE;
                    id_node->type = lty;

                    a = new_address(IdKind);
                    address(a).cont.var.e = id_node;
                    address(a).cont.nid = get_var_nid(dct->str, s->attr.var.scope);
                    address(a).cont.var.offset = local_offset;

                    ic_auto_init(lty.decl_specs, lty.idl, dct->attr.e, a, 0);
                }
            }
        }
    }

    for (sl = s->child[0]; sl != NULL; sl = sl->sibling)
        ic_statement(sl);

    if (local_offset < size_of_local_area)
        size_of_local_area = local_offset;

    if (push_scope && s->locals!=NULL) {
        local_offset = old_local_offset;
        location_pop_scope();
    }
}

void ic_expression_statement(ExecNode *s)
{
    if (s->child[0] != NULL)
        ic_expression2(s->child[0]);
}

void register_label(char *str, unsigned addr)
{
    Label *np;

    np = malloc(sizeof(Label));
    np->str = str;
    np->addr = addr;
    np->next = ic_labels;
    ic_labels = np;
}

unsigned get_label_address(char *str)
{
    Label *np;

    for (np = ic_labels; np != NULL; np = np->next)
        if (equal(np->str, str))
            return np->addr;
    assert(0);
}

void fix_gotos(void)
{
    Label *np, *temp;

    while (--gotos_to_fix_counter >= 0) {
        int i;

        i = gotos_to_fix[gotos_to_fix_counter];
        instruction(i).tar = get_label_address((char *)instruction(i).type);
    }
    gotos_to_fix_counter = 0;

    for (np = ic_labels; np != NULL; ) {
        temp = np;
        np = np->next;
        free(temp);
    }
    ic_labels = NULL;
}

void ic_zero(unsigned id, unsigned offset, unsigned nb)
{
    /*
     * Do memset(arg #1, arg #2, arg #3);
     * where:
     *  - arg #1: &id+offset
     *  - arg #2: 0
     *  - arg #3: nb
     * Also assume sizeof(int) == sizeof(void *)
     */
    unsigned a1;

    /* arg #3 */
    a1 = new_address(IConstKind);
    address(a1).cont.uval = nb;
    emit_i(OpArg, &unsigned_ty, 0, a1, 0);
    /* arg #2 */
    a1 = new_address(IConstKind);
    address(a1).cont.uval = 0;
    emit_i(OpArg, &int_ty, 0, a1, 0);
    /* arg #1 */
    a1 = new_temp_addr();
    emit_i(OpAddrOf, NULL, a1, id, 0);
    if (offset > 0) {
        unsigned a2, a3;

        a2 = new_address(IConstKind);
        address(a2).cont.uval = offset;
        a3 = new_temp_addr();
        emit_i(OpAdd, NULL, a3, a1, a2);
        a1 = a3;
    }
    emit_i(OpArg, &int_ty, 0, a1, 0);
    /* do the call */
    emit_i(OpCall, &int_ty, new_temp_addr(), memset_addr, 0);
}

void ic_auto_init(TypeExp *ds, TypeExp *dct, ExecNode *e, unsigned id, unsigned offset)
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
        if (e->kind.exp == StrLitExp) { /* char array initialized by string literal */
            unsigned a1, n, nfill;

            a1 = new_address(IConstKind);
            n = strlen(e->attr.str)+1;
            nfill = 0;
            if (nelem == n) { /* fits nicely */
                address(a1).cont.uval = n;
            } else if (nelem < n) { /* no enough room; just copy the first nelem chars of the string */
                address(a1).cont.uval = nelem;
            } else { /* copy all the string and zero the trailing elements */
                address(a1).cont.uval = n;
                nfill = nelem-n;
            }
            emit_i(OpArg, &int_ty, 0, a1, 0);
            a1 = new_address(StrLitKind);
            address(a1).cont.str = e->attr.str;
            emit_i(OpArg, &int_ty, 0, a1, 0);
            a1 = new_temp_addr();
            emit_i(OpAddrOf, NULL, a1, id, 0);
            if (offset > 0) {
                unsigned a2, a3;

                a2 = new_address(IConstKind);
                address(a2).cont.uval = offset;
                a3 = new_temp_addr();
                emit_i(OpAdd, NULL, a3, a1, a2);
                a1 = a3;
            }
            emit_i(OpArg, &int_ty, 0, a1, 0);
            emit_i(OpCall, &int_ty, new_temp_addr(), memcpy_addr, 0);
            if (nfill > 0)
                ic_zero(id, offset+n, nfill);
        } else {
            unsigned elem_size;
            Declaration ty;

            /* get element size */
            ty.decl_specs = ds;
            ty.idl = dct->child;
            elem_size = compute_sizeof(&ty);

            /* handle elements with explicit initializer */
            for (e = e->child[0]; e!=NULL && nelem!=0; e=e->sibling, --nelem) {
                ic_auto_init(ds, dct->child, e, id, offset);
                offset += elem_size;
            }

            /* handle elements without explicit initializer */
            if (nelem != 0)
                ic_zero(id, offset, nelem*elem_size);
        }
    } else if ((ts=get_type_spec(ds))->op == TOK_STRUCT) {
        /*
         * Struct.
         */
        DeclList *d;
        int full_init;


        if (e->attr.op != TOK_INIT_LIST)
            goto scalar;
        e = e->child[0];

        /* handle members with explicit initializer */
        d = ts->attr.dl;
        full_init = FALSE;
        for (; d != NULL; d = d->next) {
            dct = d->decl->idl;
            for (; e!=NULL && dct!=NULL; e=e->sibling, dct=dct->sibling) {
                unsigned mem_offs;

                mem_offs = get_member_descriptor(ts, dct->str)->offset;
                ic_auto_init(d->decl->decl_specs, dct->child, e, id, offset+mem_offs);
            }

            if (e == NULL) {
                if (dct==NULL && d->next==NULL)
                    full_init = TRUE;
                break;
            }
        }

        /* handle members without explicit initializer */
        if (!full_init) {
            unsigned p1, p2;

            if (dct == NULL) {
                d = d->next;
                dct = d->decl->idl;
            }
            p1 = -1;
            while (TRUE) {
                while (dct != NULL) {
                    StructMember *md;

                    md = get_member_descriptor(ts, dct->str);
                    if (p1 == -1) {
                        p1 = offset+md->offset;
                        p2 = p1+md->size;
                    } else {
                        p2 = offset+md->offset+md->size;
                    }
                    dct = dct->sibling;
                }
                d = d->next;
                if (d != NULL)
                    dct = d->decl->idl;
                else
                    break;
            }
            ic_zero(id, p1, p2-p1);
        }
    } else if (ts->op == TOK_UNION) {
        /*
         * Union.
         */

        if (e->attr.op != TOK_INIT_LIST)
            goto scalar;
        e = e->child[0];

        /* initialize the first named member */
        ic_auto_init(ts->attr.dl->decl->decl_specs, ts->attr.dl->decl->idl->child, e, id, offset);
    } else {
        /*
         * Scalar.
         */
        unsigned a1;
        Declaration *ty;
scalar:
        a1 = new_temp_addr();
        emit_i(OpAddrOf, NULL, a1, id, 0);
        if (offset > 0) {
            unsigned a2, a3;

            a2 = new_address(IConstKind);
            address(a2).cont.uval = offset;
            a3 = new_temp_addr();
            emit_i(OpAdd, NULL, a3, a1, a2);
            a1 = a3;
        }
        ty = malloc(sizeof(Declaration));
        ty->decl_specs = ds;
        ty->idl = dct;
        emit_i(OpIndAsn, ty, 0, a1, ic_expr_convert(e, ty));
    }
}
// =============================================================================
// Expressions
// =============================================================================
static int is_binary(Token op);
static int number_expression_tree(ExecNode *e);
static void print_addr(unsigned addr);
static int function_argument(ExecNode *arg, DeclList *param);
static unsigned ic_dereference(unsigned ptr, Declaration *ty);
static void ic_indirect_assignment(unsigned ptr, unsigned expr, Declaration *ty);
static unsigned get_step_size(ExecNode *e);
static unsigned ic_expression(ExecNode *e, int is_addr);

#define NREG(x) ((x)->nreg)

int is_binary(Token op);

/*
 * Annotate an expression syntax tree with the number
 * of registers needed to evaluate the expressions it
 * represent.
 */
int number_expression_tree(ExecNode *e)
{
    assert(e != NULL);

    switch (e->kind.exp) {
    case OpExp:
        if (is_binary(e->attr.op)) {
            int nl, nr;

            nl = number_expression_tree(e->child[0]);
            nr = number_expression_tree(e->child[1]);
            if (nl == nr)
                e->nreg = nl+1;
            else
                e->nreg = nl>nr?nl:nr;
        } else {
            /* TBD [!] may be not accurate */
            e->nreg = number_expression_tree(e->child[0])+1;
        }
        break;
    case IConstExp:
    case StrLitExp:
    case IdExp:
        e->nreg = 1;
        break;
    }

    return e->nreg;
}

unsigned ic_expression2(ExecNode *e)
{
    number_expression_tree(e);
    return ic_expression(e, FALSE);
}

unsigned ic_dereference(unsigned ptr, Declaration *ty)
{
    unsigned dst;

    switch (get_type_category(ty)) {
    case TOK_SUBSCRIPT:
    case TOK_FUNCTION:
        return ptr;
    }
    if (const_addr(ptr)) {
        unsigned tmp;

        /* to avoid a lot of annoying "is this a constant?" checks later */
        tmp = new_temp_addr();
        emit_i(OpAsn, NULL, tmp, ptr, 0);
        ptr = tmp;
    }
    /* dst = *(ty *)ptr */
    dst = new_temp_addr();
    emit_i(OpInd, ty, dst, ptr, 0);
    return dst;
}

void ic_indirect_assignment(unsigned ptr, unsigned expr, Declaration *ty)
{
    if (const_addr(ptr)) {
        unsigned tmp;

        /* to avoid future checks (analogous to ic_dereference()) */
        tmp = new_temp_addr();
        emit_i(OpAsn, NULL, tmp, ptr, 0);
        ptr = tmp;
    }
    /* *(ty *)ptr = expr */
    emit_i(OpIndAsn, ty, 0, ptr, expr);
}

unsigned get_step_size(ExecNode *e)
{
    unsigned a;

    a = new_address(IConstKind);
    if (is_integer(get_type_category(&e->type))) {
        address(a).cont.uval = 1;
    } else {
        Declaration ty;

        ty = e->type;
        ty.idl = ty.idl->child;
        address(a).cont.uval = compute_sizeof(&ty);
    }
    return a;
}

unsigned ic_expression(ExecNode *e, int is_addr)
{
    switch (e->kind.exp) {
    case OpExp:
        switch (e->attr.op) {
        case TOK_COMMA:
            ic_expression(e->child[0], FALSE);
            return ic_expression(e->child[1], FALSE);

        case TOK_ASSIGN: {
            unsigned a1, a2;

            a2 = ic_expr_convert(e->child[1], &e->type);
            if (e->child[0]->kind.exp == IdExp) {
                a1 = ic_expression(e->child[0], FALSE);
                emit_i(OpAsn, &e->type, a1, a2, 0);
                return a1;
            } else {
                a1 = ic_expression(e->child[0], TRUE);
                ic_indirect_assignment(a1, a2, &e->type);
                return a2;
            }
        }

        case TOK_MUL_ASSIGN:
        case TOK_DIV_ASSIGN:
        case TOK_REM_ASSIGN:
        case TOK_PLUS_ASSIGN:
        case TOK_MINUS_ASSIGN:
        case TOK_LSHIFT_ASSIGN:
        case TOK_RSHIFT_ASSIGN:
        case TOK_BW_AND_ASSIGN:
        case TOK_BW_XOR_ASSIGN:
        case TOK_BW_OR_ASSIGN: {
            ExecNode *new_e;
            unsigned a1, a2;

            /*
             * TOFIX:
             * - This generates code that evaluates the left operand twice.
             *  e.g.
             *      *f() += 123; ==> *f() = *f()+123;
             *  Instead, it should be something like
             *      tmp = f();
             *      *tmp = *tmp+123
             */

            new_e = malloc(sizeof(ExecNode));
            *new_e = *e;
            switch (e->attr.op) {
                case TOK_MUL_ASSIGN:    new_e->attr.op = TOK_MUL;     break;
                case TOK_DIV_ASSIGN:    new_e->attr.op = TOK_DIV;     break;
                case TOK_REM_ASSIGN:    new_e->attr.op = TOK_REM;     break;
                case TOK_PLUS_ASSIGN:   new_e->attr.op = TOK_PLUS;    break;
                case TOK_MINUS_ASSIGN:  new_e->attr.op = TOK_MINUS;   break;
                case TOK_LSHIFT_ASSIGN: new_e->attr.op = TOK_LSHIFT;  break;
                case TOK_RSHIFT_ASSIGN: new_e->attr.op = TOK_RSHIFT;  break;
                case TOK_BW_AND_ASSIGN: new_e->attr.op = TOK_BW_AND;  break;
                case TOK_BW_XOR_ASSIGN: new_e->attr.op = TOK_BW_XOR;  break;
                case TOK_BW_OR_ASSIGN:  new_e->attr.op = TOK_BW_OR;   break;
            }
            new_e->type.decl_specs = (TypeExp *)e->child[2];
            new_e->type.idl = (TypeExp *)e->child[3];
            a2 = ic_expr_convert(new_e, &e->type);
            if (e->child[0]->kind.exp == IdExp) {
                a1 = ic_expression(e->child[0], FALSE);
                emit_i(OpAsn, &e->type, a1, a2, 0);
                return a1;
            } else {
                a1 = ic_expression(e->child[0], TRUE);
                ic_indirect_assignment(a1, a2, &e->type);
                return a2;
            }
        }

        case TOK_CONDITIONAL: {
            unsigned a;
            unsigned L1, L2, L3;

            L1 = new_label();
            L2 = new_label();
            L3 = new_label();

            a = new_temp_addr();
            emit_i(OpCBr, NULL, L1, ic_expression(e->child[0], FALSE), L2);
            emit_label(L1);
            emit_i(OpAsn, NULL, a, ic_expression(e->child[1], FALSE), 0);
            emit_i(OpJmp, NULL, L3, 0, 0);
            emit_label(L2);
            emit_i(OpAsn, NULL, a, ic_expression(e->child[2], FALSE), 0);
            /*emit_i(OpJmp, NULL, L3, 0, 0);*/
            emit_label(L3);
            return a;
        }

        case TOK_OR: {
            unsigned a;
            unsigned L1, L2, L3, L4;

            L1 = new_label();
            L2 = new_label();
            L3 = new_label();
            L4 = new_label();

            a = new_temp_addr();
            emit_i(OpCBr, NULL, L1, ic_expression(e->child[0], FALSE), L2);
            emit_label(L2);
            emit_i(OpCBr, NULL, L1, ic_expression(e->child[1], FALSE), L3);
            emit_label(L1);
            emit_i(OpAsn, NULL, a, true_addr, 0);
            emit_i(OpJmp, NULL, L4, 0, 0);
            emit_label(L3);
            emit_i(OpAsn, NULL, a, false_addr, 0);
            /*emit_i(OpJmp, NULL, L4, 0, 0);*/
            emit_label(L4);
            return a;
        }

        case TOK_AND: {
            unsigned a;
            unsigned L1, L2, L3, L4;

            L1 = new_label();
            L2 = new_label();
            L3 = new_label();
            L4 = new_label();

            a = new_temp_addr();
            emit_i(OpCBr, NULL, L1, ic_expression(e->child[0], FALSE), L3);
            emit_label(L1);
            emit_i(OpCBr, NULL, L2, ic_expression(e->child[1], FALSE), L3);
            emit_label(L2);
            emit_i(OpAsn, NULL, a, true_addr, 0);
            emit_i(OpJmp, NULL, L4, 0, 0);
            emit_label(L3);
            emit_i(OpAsn, NULL, a, false_addr, 0);
            /*emit_i(OpJmp, NULL, L4, 0, 0);*/
            emit_label(L4);
            return a;
        }

        case TOK_LT:
        case TOK_GT:
        case TOK_LET:
        case TOK_GET: {
            OpKind op;
            int signedness;
            Token cat1, cat2;
            unsigned a1, a2, a3;

            if (NREG(e->child[0]) >= NREG(e->child[1])) {
                a1 = ic_expression(e->child[0], FALSE);
                a2 = ic_expression(e->child[1], FALSE);
            } else {
                a2 = ic_expression(e->child[1], FALSE);
                a1 = ic_expression(e->child[0], FALSE);
            }

            cat1 = get_type_category(&e->child[0]->type);
            cat2 = get_type_category(&e->child[1]->type);

            signedness = IC_UNSIGNED;
            if (is_integer(cat1) && is_integer(cat2)
            && is_signed_int(get_promoted_type(cat1)) && is_signed_int(get_promoted_type(cat2)))
                signedness = IC_SIGNED;

            switch (e->attr.op) {
            case TOK_LT: op = OpLT; break;
            case TOK_GT: op = OpGT; break;
            case TOK_LET: op = OpLET; break;
            case TOK_GET: op = OpGET; break;
            }
            a3 = new_temp_addr();
            emit_i(op, (Declaration *)signedness, a3, a1, a2);
            return a3;
        }

        case TOK_PLUS: {
            unsigned a1, a2, a3;

            if (is_integer(get_type_category(&e->type))) {
                if (NREG(e->child[0]) >= NREG(e->child[1])) {
                    a1 = ic_expression(e->child[0], FALSE);
                    a2 = ic_expression(e->child[1], FALSE);
                } else {
                    a2 = ic_expression(e->child[1], FALSE);
                    a1 = ic_expression(e->child[0], FALSE);
                }
                a3 = new_temp_addr();
                emit_i(OpAdd, NULL, a3, a1, a2);
                return a3;
            } else {
                int ii, pi;
                Declaration ty;
                unsigned a4, a5;

                if (is_integer(get_type_category(&e->child[0]->type)))
                    ii = 0, pi = 1;
                else
                    ii = 1, pi = 0;
                if (NREG(e->child[ii]) >= NREG(e->child[pi])) {
                    a1 = ic_expression(e->child[ii], FALSE);
                    a2 = ic_expression(e->child[pi], FALSE);
                } else {
                    a2 = ic_expression(e->child[pi], FALSE);
                    a1 = ic_expression(e->child[ii], FALSE);
                }
                ty = e->child[pi]->type;
                ty.idl = ty.idl->child;
                a3 = new_address(IConstKind);
                address(a3).cont.uval = compute_sizeof(&ty);
                a4 = new_temp_addr();
                emit_i(OpMul, NULL, a4, a1, a3);
                a5 = new_temp_addr();
                emit_i(OpAdd, NULL, a5, a2, a4);
                return a5;
            }
        }

        case TOK_MINUS: {
            unsigned a1, a2, a3;

            if (NREG(e->child[0]) >= NREG(e->child[1])) {
                a1 = ic_expression(e->child[0], FALSE);
                a2 = ic_expression(e->child[1], FALSE);
            } else {
                a2 = ic_expression(e->child[1], FALSE);
                a1 = ic_expression(e->child[0], FALSE);
            }
            if (is_integer(get_type_category(&e->child[0]->type))) { /* int-int */
                a3 = new_temp_addr();
                emit_i(OpSub, NULL, a3, a1, a2);
                return a3;
            } else {
                Declaration ty;
                unsigned a4, a5;

                ty = e->child[0]->type;
                ty.idl = ty.idl->child;
                a3 = new_address(IConstKind);
                address(a3).cont.uval = compute_sizeof(&ty);

                if (is_integer(get_type_category(&e->child[1]->type))) { /* ptr-int */
                    a4 = new_temp_addr();
                    emit_i(OpMul, NULL, a4, a2, a3);
                    a5 = new_temp_addr();
                    emit_i(OpSub, (Declaration *)1, a5, a1, a4); /* the '1' is a mark for later pointer analysis; */
                } else { /* ptr-ptr */                           /* it signals ptr-int */
                    a4 = new_temp_addr();
                    emit_i(OpSub, NULL, a4, a1, a2);
                    a5 = new_temp_addr();
                    emit_i(OpDiv, &int_ty, a5, a4, a3);
                }
                return a5;
            }
        }

        case TOK_EQ:
        case TOK_NEQ:
        case TOK_BW_OR:
        case TOK_BW_XOR:
        case TOK_BW_AND:
        case TOK_LSHIFT:
        case TOK_RSHIFT:
        case TOK_MUL:
        case TOK_DIV:
        case TOK_REM: {
            OpKind op;
            unsigned a1, a2, a3;

            if (NREG(e->child[0]) >= NREG(e->child[1])) {
                a1 = ic_expression(e->child[0], FALSE);
                a2 = ic_expression(e->child[1], FALSE);
            } else {
                a2 = ic_expression(e->child[1], FALSE);
                a1 = ic_expression(e->child[0], FALSE);
            }
            a3 = new_temp_addr();
            switch (e->attr.op) {
            case TOK_EQ:     op = OpEQ;  break;
            case TOK_NEQ:    op = OpNEQ; break;
            case TOK_BW_OR:  op = OpOr;  break;
            case TOK_BW_XOR: op = OpXor; break;
            case TOK_BW_AND: op = OpAnd; break;
            case TOK_LSHIFT: op = OpSHL; break;
            case TOK_RSHIFT: op = OpSHR; break;
            case TOK_MUL:    op = OpMul; break;
            case TOK_DIV:    op = OpDiv; break;
            case TOK_REM:    op = OpRem; break;
            }
            emit_i(op, &e->type, a3, a1, a2);
            return a3;
        }

        case TOK_CAST:
            return ic_expr_convert(e->child[0], (Declaration *)e->child[1]);

        case TOK_PRE_INC:
        case TOK_PRE_DEC: {
            Token cat;
            unsigned a1, a2, a3, a4, a5;

#define CAST()\
    if ((cat=get_type_category(&e->type)) == TOK_UNSIGNED_CHAR) {\
        a4 = new_temp_addr();\
        emit_i(OpUCh, NULL, a4, a3, 0);\
    } else if (cat == TOK_UNSIGNED_SHORT) {\
        a4 = new_temp_addr();\
        emit_i(OpUSh, NULL, a4, a3, 0);\
    } else {\
        a4 = a3;\
    }
            a1 = get_step_size(e);
            if (e->child[0]->kind.exp == IdExp) {
                a2 = ic_expression(e->child[0], FALSE);
                a3 = new_temp_addr();
                emit_i((e->attr.op==TOK_PRE_INC)?OpAdd:OpSub, NULL, a3, a2, a1);
                CAST();
                emit_i(OpAsn, NULL, a2, a4, 0);
                return a2;
            } else {
                a2 = ic_expression(e->child[0], FALSE);
                a3 = new_temp_addr();
                emit_i((e->attr.op==TOK_PRE_INC)?OpAdd:OpSub, NULL, a3, a2, a1);
                CAST();
                a5 = ic_expression(e->child[0], TRUE);
                ic_indirect_assignment(a5, a4, &e->type);
                return a4;
            }
        }
#undef CAST

        case TOK_POS_INC:
        case TOK_POS_DEC: {
            unsigned a1, a2, a3, a4;

            a1 = get_step_size(e);
            if (e->child[0]->kind.exp == IdExp) {
                a2 = ic_expression(e->child[0], FALSE);
                a3 = new_temp_addr();
                emit_i(OpAsn, NULL, a3, a2, 0);
                a4 = new_temp_addr();
                emit_i((e->attr.op==TOK_POS_INC)?OpAdd:OpSub, NULL, a4, a3, a1);
                emit_i(OpAsn, NULL, a2, a4, 0);
                return a3;
            } else {
                a2 = ic_expression(e->child[0], FALSE);
                a3 = new_temp_addr();
                emit_i((e->attr.op==TOK_POS_INC)?OpAdd:OpSub, NULL, a3, a2, a1);
                a4 = ic_expression(e->child[0], TRUE);
                ic_indirect_assignment(a4, a3, &e->type);
                return a2;
            }
        }

        case TOK_ADDRESS_OF:
            return ic_expression(e->child[0], TRUE);
        case TOK_INDIRECTION:
            if (is_addr)
                return ic_expression(e->child[0], FALSE);
            else
                return ic_dereference(ic_expression(e->child[0], FALSE), &e->type);

        case TOK_COMPLEMENT:
        case TOK_NEGATION:
        case TOK_UNARY_MINUS: {
            OpKind op;
            unsigned a1, a2;

            a1 = ic_expression(e->child[0], FALSE);
            a2 = new_temp_addr();
            switch (e->attr.op) {
            case TOK_COMPLEMENT:  op = OpCmpl; break;
            case TOK_NEGATION:    op = OpNot;  break;
            case TOK_UNARY_MINUS: op = OpNeg;  break;
            }
            emit_i(op, &e->type, a2, a1, 0);
            return a2;
        }
        case TOK_UNARY_PLUS:
            return ic_expression(e->child[0], FALSE);

        case TOK_SUBSCRIPT: {
            int ii, pi;
            Declaration ty;
            unsigned a1, a2, a3, a4, a5;

            if (is_integer(get_type_category(&e->child[0]->type)))
                ii = 0, pi = 1;
            else
                ii = 1, pi = 0;
            if (NREG(e->child[ii]) >= NREG(e->child[pi])) {
                a1 = ic_expression(e->child[ii], FALSE);
                a2 = ic_expression(e->child[pi], FALSE);
            } else {
                a2 = ic_expression(e->child[pi], FALSE);
                a1 = ic_expression(e->child[ii], FALSE);
            }
            ty = e->child[pi]->type;
            ty.idl = ty.idl->child;
            a3 = new_address(IConstKind);
            address(a3).cont.uval = compute_sizeof(&ty);
            a4 = new_temp_addr();
            emit_i(OpMul, NULL, a4, a1, a3);
            a5 = new_temp_addr();
            emit_i(OpAdd, NULL, a5, a2, a4);
            if (is_addr)
                return a5;
            else
                return ic_dereference(a5, &e->type);
        }

        case TOK_FUNCTION: {
            int na;
            OpKind op;
            ExecNode *tmp;
            unsigned a1, a2;

            a2 = new_address(IConstKind);
            na = function_argument(e->child[1], e->locals);
            address(a2).cont.val = na;

            tmp = e->child[0];
            while (tmp->kind.exp==OpExp /*&& tmp->attr.op==TOK_STAR*/)
                 tmp = tmp->child[0];
            if (tmp->kind.exp==IdExp && get_type_category(&tmp->type)==TOK_FUNCTION) {
                unsigned tar_fn;

                op = OpCall;
                a1 = new_address(IdKind);
                address(a1).cont.var.e = tmp;
                address(a1).cont.nid = get_var_nid(tmp->attr.str, tmp->attr.var.scope);
                /* add call-graph edge */
                tar_fn = new_cg_node(tmp->attr.str);
                edge_add(&cg_node(curr_cg_node).out, tar_fn);
            } else {
                op = OpIndCall;
                a1 = ic_expression(e->child[0], FALSE);
            }
            if (get_type_category(&e->type) != TOK_VOID) {
                unsigned a3;

                a3 = new_temp_addr();
                emit_i(op, &e->type, a3, a1, a2);
                return a3;
            } else {
                emit_i(op, &e->type, 0, a1, a2);
                return 0;
            }
        }

        case TOK_DOT:
        case TOK_ARROW: {
            int is_union;
            unsigned a1;

            if (e->attr.op == TOK_DOT) {
                a1 = ic_expression(e->child[0], TRUE);
                is_union = get_type_category(&e->child[0]->type) == TOK_UNION;
            } else {
                a1 = ic_expression(e->child[0], FALSE);
                is_union = get_type_spec(e->child[0]->type.decl_specs)->op == TOK_UNION;
            }
            if (!is_union) {
                StructMember *m;
                unsigned a2, a3;

                m = get_member_descriptor(get_type_spec(e->child[0]->type.decl_specs), e->child[1]->attr.str);
                a2 = new_address(IConstKind);
                address(a2).cont.uval = m->offset;
                a3 = new_temp_addr();
                emit_i(OpAdd, NULL, a3, a1, a2);
                a1 = a3;
            }
            if (is_addr)
                return a1;
            else
                return ic_dereference(a1, &e->type);
        }
        } /* switch (e->attr.op) */
        break;
    case IConstExp: {
        unsigned a;

        a = new_address(IConstKind);
        address(a).cont.uval = e->attr.uval;
        return a;
    }
    case StrLitExp: {
        unsigned a;

        a = new_address(StrLitKind);
        address(a).cont.str = e->attr.str;
        return a;
    }
    case IdExp: {
        Token cat;
        unsigned a1;

        a1 = new_address(IdKind);
        address(a1).cont.var.e = e;
        address(a1).cont.nid = get_var_nid(e->attr.str, e->attr.var.scope);
        if (e->attr.var.duration == DURATION_AUTO)
            address(a1).cont.var.offset = location_get_offset(e->attr.str);
        if (e->attr.var.is_param) {
            ParamNid *pn;

            for (pn = cg_node(curr_cg_node).pn; pn != NULL; pn = pn->next) {
                if (equal(pn->sid, e->attr.str)) {
                    pn->nid = address(a1).cont.nid;
                    break;
                }
            }
            assert(pn != NULL);
        }

        if (is_addr || (cat=get_type_category(&e->type))==TOK_SUBSCRIPT || cat==TOK_FUNCTION) {
            unsigned a2;

            a2 = new_temp_addr();
            emit_i(OpAddrOf, NULL, a2, a1, 0);
            new_atv(address(a1).cont.nid);
            return a2;
        } else {
            return a1;
        }
    }
    } /* switch (e->kind.exp) */

    assert(0);
}

/*
 * Evaluate expression `e' and convert the result to type `dest'.
 */
unsigned ic_expr_convert(ExecNode *e, Declaration *dest)
{
    OpKind op;
    unsigned a1, a2;
    Token cat_dest, cat_src;

    a1 = ic_expression(e, FALSE);

    cat_src  = get_type_category(&e->type);
    cat_dest = get_type_category(dest);

    switch (cat_dest) {
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
        if (cat_src!=TOK_CHAR && cat_src!=TOK_SIGNED_CHAR) {
            op = OpCh;
            break;
        }
        return a1; /* no conversion */
    case TOK_UNSIGNED_CHAR:
        if (cat_src != TOK_UNSIGNED_CHAR) {
            op = OpUCh;
            break;
        }
        return a1; /* no conversion */
    case TOK_SHORT:
        if (cat_src != TOK_CHAR
        &&  cat_src != TOK_SIGNED_CHAR
        &&  cat_src != TOK_UNSIGNED_CHAR
        &&  cat_src != TOK_SHORT) {
            op = OpSh;
            break;
        }
        return a1; /* no conversion */
    case TOK_UNSIGNED_SHORT:
        if (cat_src!=TOK_UNSIGNED_CHAR && cat_src!=TOK_UNSIGNED_SHORT) {
            op = OpUSh;
            break;
        }
        return a1; /* no conversion */
    default:
        return a1; /* no conversion */
    }
    /* fall through */
    /* convert */
    a2 = new_temp_addr();
    emit_i(op, NULL, a2, a1, 0);
    return a2;
}

/*
 * Push arguments from right to left recursively.
 * Return the number of arguments pushed.
 */
int function_argument(ExecNode *arg, DeclList *param)
{
    int n;

    if (arg == NULL)
        return 0;

    n = 1;
    if (param->decl->idl==NULL || param->decl->idl->op!=TOK_ELLIPSIS) {
        /* this argument matches a declared (non-optional) parameter */

        Declaration ty;

        n += function_argument(arg->sibling, param->next);
        ty = *param->decl;
        if (ty.idl!=NULL && ty.idl->op==TOK_ID) /* skip any identifier */
            ty.idl = ty.idl->child;
        emit_i(OpArg, param->decl, 0, ic_expr_convert(arg, &ty), 0);
    } else {
        /* this and the arguments that follow match the `...' */

        n += function_argument(arg->sibling, param);
        emit_i(OpArg, &arg->type, 0, ic_expression(arg, FALSE), 0);
    }
    return n;
}

int is_binary(Token op)
{
    switch (op) {
    /*case TOK_COMMA:
    case TOK_ASSIGN:
    case TOK_MUL_ASSIGN:
    case TOK_DIV_ASSIGN:
    case TOK_REM_ASSIGN:
    case TOK_PLUS_ASSIGN:
    case TOK_MINUS_ASSIGN:
    case TOK_LSHIFT_ASSIGN:
    case TOK_RSHIFT_ASSIGN:
    case TOK_BW_AND_ASSIGN:
    case TOK_BW_XOR_ASSIGN:
    case TOK_BW_OR_ASSIGN:
    case TOK_CONDITIONAL:*/
    case TOK_OR:
    case TOK_AND:
    case TOK_BW_OR:
    case TOK_BW_XOR:
    case TOK_BW_AND:
    case TOK_EQ:
    case TOK_NEQ:
    case TOK_LT:
    case TOK_GT:
    case TOK_LET:
    case TOK_GET:
    case TOK_LSHIFT:
    case TOK_RSHIFT:
    case TOK_PLUS:
    case TOK_MINUS:
    case TOK_MUL:
    case TOK_DIV:
    case TOK_REM:
    case TOK_SUBSCRIPT:
        return TRUE;
    default:
        return FALSE;
    }
}

/*static
int is_unary(Token op)
{
    switch (op) {
    case TOK_CAST:
    case TOK_PRE_INC:
    case TOK_PRE_DEC:
    case TOK_SIZEOF:
    case TOK_ADDRESS_OF:
    case TOK_INDIRECTION:
    case TOK_UNARY_PLUS:
    case TOK_UNARY_MINUS:
    case TOK_COMPLEMENT:
    case TOK_NEGATION:
    case TOK_FUNCTION:
    case TOK_DOT:
    case TOK_ARROW:;
    case TOK_INC:
    case TOK_DEC:
        return TRUE;
    default:
        return FALSE;
    }
}*/

void print_addr(unsigned addr)
{
    if (addr == 0)
        return;

    switch (address(addr).kind) {
    case IConstKind:
        printf("%ld", address(addr).cont.val);
        break;
    case TempKind:
    case IdKind:
        printf("%s", address_sid(addr));
        break;
    case StrLitKind:
        printf("\"%s\"", address(addr).cont.str);
        break;
    }
}

static
void print_binop(Quad *i, char *op)
{
    print_addr(i->tar);
    printf(" = ");
    print_addr(i->arg1);
    printf(" %s ", op);
    print_addr(i->arg2);
}

static
void print_unaop(Quad *i, char *op)
{
    print_addr(i->tar);
    printf(" = %s", op);
    print_addr(i->arg1);
}

void dump_ic(unsigned fn)
{
    unsigned i;
    unsigned first, last;

    if (cg_node_is_empty(fn))
        return;

    first = cfg_node(cg_node(fn).bb_i).leader;
    last  = cfg_node(cg_node(fn).bb_f).last;
    for (i = first; i <= last; i++) {
        Quad *p;

        p = &ic_instructions[i];
        printf("(%d) ", i);
        switch (p->op) {
        case OpAdd: print_binop(p, "+");  break;
        case OpSub: print_binop(p, "-");  break;
        case OpMul: print_binop(p, "*");  break;
        case OpDiv: print_binop(p, "/");  break;
        case OpRem: print_binop(p, "%");  break;
        case OpSHL: print_binop(p, "<<"); break;
        case OpSHR: print_binop(p, ">>"); break;
        case OpAnd: print_binop(p, "&");  break;
        case OpOr:  print_binop(p, "|");  break;
        case OpXor: print_binop(p, "^");  break;
        case OpEQ:  print_binop(p, "=="); break;
        case OpNEQ: print_binop(p, "!="); break;
        case OpLT:  print_binop(p, "<");  break;
        case OpLET: print_binop(p, "<="); break;
        case OpGT:  print_binop(p, ">");  break;
        case OpGET: print_binop(p, ">="); break;

        case OpNeg:  print_unaop(p, "-"); break;
        case OpCmpl: print_unaop(p, "~"); break;
        case OpNot:  print_unaop(p, "!"); break;
        case OpCh:   print_unaop(p, "(char)");           break;
        case OpUCh:  print_unaop(p, "(unsigned char)");  break;
        case OpSh:   print_unaop(p, "(short)");          break;
        case OpUSh:  print_unaop(p, "(unsigned short)"); break;
        case OpIndAsn:
            printf("*");
            print_addr(p->arg1);
            printf(" = ");
            print_addr(p->arg2);
            break;
        case OpAsn:    print_unaop(p, "");  break;
        case OpAddrOf: print_unaop(p, "&"); break;
        case OpInd:    print_unaop(p, "*"); break;

        case OpLab:
            printf("L%ld:", address(p->tar).cont.val);
            break;
        case OpJmp:
            printf("jmp L%ld", address(p->tar).cont.val);
            break;
        case OpSwitch:
            printf("switch ");
            print_addr(p->arg1);
            printf(" [");
            print_addr(p->arg2);
            printf(" labels]");
            break;
        case OpCase:
            printf("case ");
            print_addr(p->tar);
            printf(", L%ld, %ld", address(p->arg1).cont.val, address(p->arg2).cont.val);
            break;
        case OpCBr:
            printf("cbr L%ld, ", address(p->tar).cont.val);
            print_addr(p->arg1);
            printf(", L%ld", address(p->arg2).cont.val);
            break;

        case OpArg:
            printf("arg ");
            print_addr(p->arg1);
            break;
        case OpCall:
        case OpIndCall:
            if (p->tar) {
                print_addr(p->tar);
                printf(" = ");
            }
            if (p->op == OpCall)
                print_addr(p->arg1);
            else
                printf("(*"), print_addr(p->arg1), printf(")");
            printf("() ["); print_addr(p->arg2); printf(" arg]");
            break;
        case OpRet:
            printf("ret ");
            print_addr(p->arg1);
            break;
        }
        printf("\n");
    }
}
