/*
 * Intermediate code generator
 *  AST ==> IC
 * Generate code for a single function each time.
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


static int label_counter = 1;
static unsigned true_addr, false_addr;
static TypeExp int_expr = { TOK_INT };
static Declaration int_ty = { &int_expr };

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

void ic_reset(void);
static void disassemble(void);
static void new_cfg_node(unsigned leader);
static void emit_i(OpKind op, Declaration *type, unsigned tar, unsigned arg1, unsigned arg2);
static unsigned new_address(AddrKind kind);
static unsigned new_temp_addr(void);
static unsigned new_label(void);
static void ic_compound_statement(ExecNode *s, int push_scope);
static void new_nid(char *sid);
static int get_var_nid(char *sid, int scope);

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

void new_cfg_node(unsigned leader)
{
    int i;

    if (cfg_nodes_counter >= cfg_nodes_max) {
        CFGNode *new_p;

        /* grow */
        new_p = realloc(cfg_nodes, NGROW*cfg_nodes_max*sizeof(CFGNode));
        assert(new_p != NULL);
        cfg_nodes_max *= NGROW;
        cfg_nodes = new_p;
    }
    cfg_nodes[cfg_nodes_counter].leader = leader;
    cfg_nodes[cfg_nodes_counter].out_edges[0] = 0;
    cfg_nodes[cfg_nodes_counter].out_edges[1] = 0;
    for (i = 0; i < MAX_IN_EDGES; i++)
        cfg_nodes[cfg_nodes_counter].in_edges[i] = 0;
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

    L = new_address(IConstKind);
    address(L).cont.val = label_counter++;

    return L;
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
}

void ic_reset(void)
{
    unsigned i;

    size_of_local_area = 0;
    local_offset = 0;

    memset(ic_instructions, 0, sizeof(Quad)*ic_instructions_counter);
    ic_instructions_counter = 0;

    memset(ic_addresses, 0, sizeof(Address)*ic_addresses_counter);
    ic_addresses_counter = 1;

    for (i = 1; i < cfg_nodes_counter; i++) {
        bset_free(cfg_node(i).UEVar);
        bset_free(cfg_node(i).VarKill);
        bset_free(cfg_node(i).LiveOut);
        bset_free(cfg_node(i).Dom);
    }
    memset(cfg_nodes, 0, sizeof(CFGNode)*cfg_nodes_counter);
    cfg_nodes_counter = 1;

    free_PointOut();
    nid_counter = 0;
    arena_reset(id_table_arena);
    memset(id_table, 0, sizeof(IDNode *)*ID_TABLE_SIZE);
    arena_reset(temp_names_arena);
}

static void build_CFG(void);
static void print_CFG(void);
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

        /*add_potential_target(get_nid(p->decl->idl->str));*/

        p = p->next;
    }

    ty.decl_specs = decl_specs;
    ty.idl = header->child->child;
    if ((cat=get_type_category(&ty))==TOK_STRUCT || cat==TOK_UNION)
        local_offset -= 4; /* allocate space for the 'return value address' */

    entry_label = new_label();
    exit_label = new_label();
    emit_i(OpJmp, NULL, entry_label, 0, 0);
    emit_i(OpLab, NULL, entry_label, 0, 0);
    ic_compound_statement(header->attr.e, FALSE);
    emit_i(OpJmp, NULL, exit_label, 0, 0);
    emit_i(OpLab, NULL, exit_label, 0, 0);
    location_pop_scope();

    fix_gotos();

    disassemble();
    if (ic_instructions_counter > 0) {
        build_CFG();
        print_CFG();
        dflow_dominance();
        dflow_PointOut();
        dflow_LiveOut();
        compute_liveness_and_next_use();
    }
}

// =============================================================================
// Control Flow Graph (CFG)
// =============================================================================
static int pocount;
static int *visited, nunvisited;
/* post-order and reverse post-order of the CFG */
unsigned *CFG_PO;
unsigned *CFG_RPO;
/* post-order and reverse post-order of the reverse CFG */
unsigned *RCFG_PO;
unsigned *RCFG_RPO;

static void number_CFG(void);
static void number_subCFG(unsigned n);
static void number_subRCFG(unsigned n);

void print_CFG_ordering(void)
{
    unsigned i;

    printf("CFG PO = [ ");
    for (i = ENTRY_NODE; i < cfg_nodes_counter; i++)
        printf("%u, ", CFG_PO[i]);
    printf("]\n");
    printf("CFG RPO = [ ");
    for (i = ENTRY_NODE; i < cfg_nodes_counter; i++)
        printf("%u, ", CFG_RPO[i]);
    printf("]\n");
    printf("RCFG PO = [ ");
    for (i = ENTRY_NODE; i < cfg_nodes_counter; i++)
        printf("%u, ", RCFG_PO[i]);
    printf("]\n");
    printf("RCFG RPO = [ ");
    for (i = ENTRY_NODE; i < cfg_nodes_counter; i++)
        printf("%u, ", RCFG_RPO[i]);
    printf("]\n");
}

/* emit a DOT definition of the CFG */
void print_CFG(void)
{
    unsigned i;

    print_CFG_ordering();

    printf("digraph {\n");
    for (i = ENTRY_NODE; i < cfg_nodes_counter; i++) {
        unsigned j;

        printf("V%u[label=\"B%u ", i, i);
        for (j = cfg_node(i).leader; j <= cfg_node(i).last; j++)
            printf("(%u), ", j);
        printf("\"];\n");

        /*for (j = 0; j < MAX_IN_EDGES; j++)
            if (cfg_node(i).in_edges[j])
                printf("in_edges[%u]=%u, ", j, cfg_node(i).in_edges[j]);
        printf("\n");*/

        if (cfg_node(i).out_edges[0]) {
            if (cfg_node(i).out_edges[1]) {
                printf("V%u -> V%u;\n", i, cfg_node(i).out_edges[0]);
                printf("V%u -> V%u;\n", i, cfg_node(i).out_edges[1]);
            } else {
                printf("V%u -> V%u;\n", i, cfg_node(i).out_edges[0]);
            }
        }
    }
    printf("}\n");
}

void number_subCFG(unsigned n)
{
    int i;

    visited[n] = TRUE;
    --nunvisited;
    for (i = 0; i < MAX_OUT_EDGES; i++) {
        unsigned succ;

        if (!(succ=cfg_node(n).out_edges[i]))
            break;
        if (!visited[succ])
            number_subCFG(succ);
    }
    assert(CFG_PO[pocount] == 0);
    CFG_RPO[cfg_nodes_counter-pocount] = n;
    CFG_PO[pocount++] = n;
}

void number_subRCFG(unsigned n)
{
    int i;

    visited[n] = TRUE;
    --nunvisited;
    for (i = 0; i < MAX_IN_EDGES; i++) {
        unsigned pred;

        if (!(pred=cfg_node(n).in_edges[i]))
            break;
        if (!visited[pred])
            number_subRCFG(pred);
    }
    assert(RCFG_PO[pocount] == 0);
    RCFG_RPO[cfg_nodes_counter-pocount] = n;
    RCFG_PO[pocount++] = n;
}

void number_CFG(void)
{
    unsigned n;

    visited = calloc(cfg_nodes_counter, sizeof(int));
    CFG_PO = calloc(cfg_nodes_counter, sizeof(unsigned));
    CFG_RPO = malloc(cfg_nodes_counter*sizeof(unsigned));
    nunvisited = cfg_nodes_counter-1; /* -1 for NULL node */
    pocount = 1;

    while (nunvisited != 0) {
        for (n = ENTRY_NODE; n < cfg_nodes_counter; n++)
            if (!visited[n])
                break;
        number_subCFG(n);
    }

    memset(visited, 0, sizeof(int)*cfg_nodes_counter);
    RCFG_PO = calloc(cfg_nodes_counter, sizeof(unsigned));
    RCFG_RPO = malloc(cfg_nodes_counter*sizeof(unsigned));
    nunvisited = cfg_nodes_counter-1;
    pocount = 1;

    while (nunvisited != 0) {
        for (n = ENTRY_NODE; n < cfg_nodes_counter; n++)
            if (!visited[n])
                break;
        number_subRCFG(n);
    }

    free(visited);
}

void build_CFG(void)
{
    /*
     * Assumptions:
     * - Every basic block ends with a branch or a jump.
     */

    unsigned i;
    unsigned *lab2node;

    /* allocate table used to map labels to CFG nodes */
    lab2node = malloc(sizeof(unsigned)*label_counter);

    /*
     * 1st step: find leaders.
     */
    /* first instruction (always a leader) */
    if (instruction(0).op == OpLab)
        lab2node[address(instruction(0).tar).cont.val] = cfg_nodes_counter;
    new_cfg_node(0);
    /* remaining instructions */
    for (i = 1; i < ic_instructions_counter; i++) {
        if (instruction(i).op == OpLab) {
            lab2node[address(instruction(i).tar).cont.val] = cfg_nodes_counter;
            new_cfg_node(i);
        }
    }

    /*
     * 2nd step: find last and add edges.
     */
    for (i = 1; i < cfg_nodes_counter; i++) {
        unsigned last;

        if (i != cfg_nodes_counter-1)
            /* set last as the instruction immediately preceding the next leader */
            last = cfg_node(i+1).leader-1;
        else /* last node of the CFG */
            /* set last as the last instruction of the function */
            last = ic_instructions_counter-1;
        cfg_node(i).last = last;

        /* add edges */
        if (instruction(last).op == OpCBr) {
            unsigned j;
            unsigned succ1, succ2;

            succ1 = lab2node[address(instruction(last).arg1).cont.val];
            succ2 = lab2node[address(instruction(last).arg2).cont.val];

            /* set out edges of current node */
            cfg_node(i).out_edges[0] = succ1;
            cfg_node(i).out_edges[1] = succ2;

            /* set in edges of successors */
            for (j = 0; j < MAX_IN_EDGES; j++) {
                if (!cfg_node(succ1).in_edges[j]) {
                    cfg_node(succ1).in_edges[j] = i;
                    break;
                }
            }
            assert(j != MAX_IN_EDGES);
            for (j = 0; j < MAX_IN_EDGES; j++) {
                if (!cfg_node(succ2).in_edges[j]) {
                    cfg_node(succ2).in_edges[j] = i;
                    break;
                }
            }
            assert(j != MAX_IN_EDGES);
        } else if (instruction(last).op == OpJmp) {
            unsigned j;
            unsigned succ;

            succ = lab2node[address(instruction(last).tar).cont.val];

            cfg_node(i).out_edges[0] = succ;

            for (j = 0; j < MAX_IN_EDGES; j++) {
                if (!cfg_node(succ).in_edges[j]) {
                    cfg_node(succ).in_edges[j] = i;
                    break;
                }
            }
            assert(j != MAX_IN_EDGES);
        }
    }

    free(lab2node);
    number_CFG();
}

// =============================================================================
// Statements
// =============================================================================
typedef struct Label Label;
static struct Label {
    char *str;
    unsigned addr;
    Label *next;
} *ic_labels;

static int gotos_to_fix[64], gotos_to_fix_counter;
static void register_label(char *str, unsigned addr);
static unsigned get_label_address(char *str);
// static void emit_label(unsigned L);

static unsigned btarget_stack[128], ctarget_stack[128];
static int bt_stack_top = -1, ct_stack_top = -1;
static void push_break_target(unsigned lab);
static void pop_break_target(void);
static void push_continue_target(unsigned lab);
static void pop_continue_target(void);

static void ic_if_statement(ExecNode *s);
// static void ic_switch_statement(ExecNode *s);
static void ic_while_statement(ExecNode *s);
static void ic_do_statement(ExecNode *s);
static void ic_for_statement(ExecNode *s);
static void ic_goto_statement(ExecNode *s);
static void ic_continue_statement(void);
static void ic_break_statement(void);
static void ic_return_statement(ExecNode *s);
// static void ic_case_statement(ExecNode *s);
// static void ic_default_statement(ExecNode *s);
static void ic_expression_statement(ExecNode *s);
static void ic_label_statement(ExecNode *s);
static void ic_statement(ExecNode *s);
static unsigned ic_expression2(ExecNode *e);
static unsigned ic_expr_convert(ExecNode *e, Declaration *dest);
static void split_block(void);

/*void emit_label(unsigned L)
{
    int iprev;

    iprev = ic_instructions_counter-1;
    if (instruction(iprev).op == OpLab)
        address(L).cont.val = address(instruction(iprev).tar).cont.val;
    else
        emit_i(OpLab, NULL, L, 0, 0);
}*/
void split_block(void)
{
    emit_i(OpLab, NULL, new_label(), 0, 0);
}

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
    // case SwitchStmt: break;
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
    // case CaseStmt: break;
    // case DefaultStmt: break;
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
    ==> if <e> <stmt1> else <stmt2>
    CBr <e>, L1, L2
    L1:
    <stmt1>
    Jmp L3
    L2:
    <stmt2>
    Jmp L3
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

    emit_i(OpCBr, NULL, ic_expression2(s->child[0]), L1, L2);
    emit_i(OpLab, NULL, L1, 0, 0);
    ic_statement(s->child[1]);
    emit_i(OpJmp, NULL, else_part?L3:L2, 0, 0);
    emit_i(OpLab, NULL, L2, 0, 0);
    if (else_part) {
        ic_statement(s->child[2]);
        emit_i(OpJmp, NULL, L3, 0, 0);
        emit_i(OpLab, NULL, L3, 0, 0);
    }
}

void ic_while_statement(ExecNode *s)
{
    /*
    ==> while (<e>) <stmt>
    CBr <e>, L1, L3
    L1:
    <stmt>
    Jmp L2
    L2:
    CBr <e>, L1, L3
    L3:
    ...
     */
    unsigned L1, L2, L3;

    L1 = new_label();
    L2 = new_label();
    L3 = new_label();

    emit_i(OpCBr, NULL, ic_expression2(s->child[0]), L1, L3);
    emit_i(OpLab, NULL, L1, 0, 0);
    push_break_target(L3), push_continue_target(L2);
    ic_statement(s->child[1]);
    pop_break_target(), pop_continue_target();
    emit_i(OpJmp, NULL, L2, 0, 0);
    emit_i(OpLab, NULL, L2, 0, 0); /* continue's target */
    emit_i(OpCBr, NULL, ic_expression2(s->child[0]), L1, L3);
    emit_i(OpLab, NULL, L3, 0, 0);
}

void ic_do_statement(ExecNode *s)
{
    /*
    ==> do <stmt> while (<e>)
    L1:
    <stmt>
    Jmp L2
    L2:
    CBr <e>, L1, L3
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
        emit_i(OpJmp, NULL, L1, 0, 0);
        emit_i(OpLab, NULL, L1, 0, 0);
    } else {
        L1 = instruction(iprev).tar;
    }
    push_break_target(L3), push_continue_target(L2);
    ic_statement(s->child[1]);
    pop_break_target(), pop_continue_target();
    emit_i(OpJmp, NULL, L2, 0, 0);
    emit_i(OpLab, NULL, L2, 0, 0);
    emit_i(OpCBr, NULL, ic_expression2(s->child[0]), L1, L3);
    emit_i(OpLab, NULL, L3, 0, 0);
}

void ic_for_statement(ExecNode *s)
{
    /*
    ==> for (<e1>; <e2>; <e3>) <stmt>
    <e1>
    CBr <e2>, L1, L3
    L1:
    <stmt>
    Jmp L2
    L2:
    <e3>
    CBr <e2>, L1, L3
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
        emit_i(OpCBr, NULL, ic_expression2(s->child[0]), L1, L3);
    else
        emit_i(OpJmp, NULL, L1, 0, 0);
    emit_i(OpLab, NULL, L1, 0, 0);
    push_break_target(L3), push_continue_target(L2);
    ic_statement(s->child[3]);
    pop_break_target(), pop_continue_target();
    emit_i(OpJmp, NULL, L2, 0, 0);
    emit_i(OpLab, NULL, L2, 0, 0);
    if (s->child[2] != NULL)
        ic_expression2(s->child[2]);
    if (s->child[0] != NULL)
        emit_i(OpCBr, NULL, ic_expression2(s->child[0]), L1, L3);
    else
        emit_i(OpJmp, NULL, L1, 0, 0);
    emit_i(OpLab, NULL, L3, 0, 0);
}

void ic_label_statement(ExecNode *s)
{
    int iprev;

    iprev = ic_instructions_counter-1;
    if (instruction(iprev).op != OpLab) {
        unsigned L;

        L = new_label();
        emit_i(OpJmp, NULL, L, 0, 0);
        emit_i(OpLab, NULL, L, 0, 0);
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
    split_block();
}

void ic_continue_statement(void)
{
    emit_i(OpJmp, NULL, ctarget_stack[ct_stack_top], 0, 0);
    split_block();
}

void ic_break_statement(void)
{
    emit_i(OpJmp, NULL, btarget_stack[bt_stack_top], 0, 0);
    split_block();
}

void ic_return_statement(ExecNode *s)
{
    if (s->child[0] != NULL) {
        Declaration ret_ty;

        ret_ty.decl_specs = (TypeExp *)s->child[1];
        ret_ty.idl = (TypeExp *)s->child[2];
        emit_i(OpRet, NULL, 0, ic_expr_convert(s->child[0], &ret_ty), 0); /* TBD: type field */
    }
    emit_i(OpJmp, NULL, exit_label, 0, 0);
    split_block();
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
                    for (dct = dl->decl->idl; dct != NULL; dct = dct->sibling)
                        /*static_object_definition(dl->decl->decl_specs, dct, TRUE);*/
                        ;
                    // emit(".text");
                    continue;
                } else if (scs->op == TOK_EXTERN) {
                    // emit(".extern %s", dl->decl->idl->str);
                    continue;
                } else if (scs->op == TOK_TYPEDEF) {
                    continue;
                }
            }

            /* traverse init declarator list */
            for (dct = dl->decl->idl; dct != NULL; dct = dct->sibling) {
                Declaration lty;

                lty.decl_specs = dl->decl->decl_specs;
                lty.idl = dct->child;
                local_offset = round_up(local_offset, get_alignment(&lty));
                local_offset -= compute_sizeof(&lty);
                location_new(dct->str, local_offset);
                DEBUG_PRINTF("==> var: %s, offset: %d\n", dct->str, local_offset);
                /*if (dct->attr.e != NULL)
                    do_auto_init(lty.decl_specs, lty.idl, dct->attr.e, local_offset);*/
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
    if (s->child[0] == NULL)
        return;

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
}
// =============================================================================
// Expressions
// =============================================================================
static int is_binary(Token op);
static int number_expression_tree(ExecNode *e);
static void print_addr(unsigned addr);
static void function_argument(ExecNode *arg, DeclList *param);
static unsigned ic_dereference(unsigned ptr, Declaration *ty);
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
            /* [!] may be not accurate */
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
    /* dst = (ty *)ptr */
    dst = new_temp_addr();
    emit_i(OpInd, ty, dst, ptr, 0);
    return dst;
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
                emit_i(OpIndAsn, &e->type, a1, a2, 0);
                return a2;
            }
        }
#if 0
        case TOK_MUL_ASSIGN:
            return ic_compound_assignment(e, TOK_MUL);
        case TOK_DIV_ASSIGN:
            return ic_compound_assignment(e, TOK_DIV);
        case TOK_REM_ASSIGN:
            return ic_compound_assignment(e, TOK_REM);
        case TOK_PLUS_ASSIGN:
            return ic_compound_assignment(e, TOK_PLUS);
        case TOK_MINUS_ASSIGN:
            return ic_compound_assignment(e, TOK_MINUS);
        case TOK_LSHIFT_ASSIGN:
            return ic_compound_assignment(e, TOK_LSHIFT);
        case TOK_RSHIFT_ASSIGN:
            return ic_compound_assignment(e, TOK_RSHIFT);
        case TOK_BW_AND_ASSIGN:
            return ic_compound_assignment(e, TOK_BW_AND);
        case TOK_BW_XOR_ASSIGN:
            return ic_compound_assignment(e, TOK_BW_XOR);
        case TOK_BW_OR_ASSIGN:
            return ic_compound_assignment(e, TOK_BW_OR);
#endif
        case TOK_CONDITIONAL: {
            /*
            ==> <a> ? <b> : <c>
            CBr <a>, L1, L2
            L1:
            t1 = <b>
            Jmp L3
            L2:
            t1 = <c>
            Jmp L3
            L3:
            */
            unsigned a;
            unsigned L1, L2, L3;

            L1 = new_label();
            L2 = new_label();
            L3 = new_label();

            a = new_temp_addr();
            emit_i(OpCBr, NULL, ic_expression(e->child[0], FALSE), L1, L2);
            emit_i(OpLab, NULL, L1, 0, 0);
            emit_i(OpAsn, NULL, a, ic_expression(e->child[1], FALSE), 0);
            emit_i(OpJmp, NULL, L3, 0, 0);
            emit_i(OpLab, NULL, L2, 0, 0);
            emit_i(OpAsn, NULL, a, ic_expression(e->child[2], FALSE), 0);
            emit_i(OpJmp, NULL, L3, 0, 0);
            emit_i(OpLab, NULL, L3, 0, 0);
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
            emit_i(OpCBr, NULL, ic_expression(e->child[0], FALSE), L1, L2);
            emit_i(OpLab, NULL, L2, 0, 0);
            emit_i(OpCBr, NULL, ic_expression(e->child[1], FALSE), L1, L3);
            emit_i(OpLab, NULL, L1, 0, 0);
            emit_i(OpAsn, NULL, a, true_addr, 0);
            emit_i(OpJmp, NULL, L4, 0, 0);
            emit_i(OpLab, NULL, L3, 0, 0);
            emit_i(OpAsn, NULL, a, false_addr, 0);
            emit_i(OpJmp, NULL, L4, 0, 0);
            emit_i(OpLab, NULL, L4, 0, 0);
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
            emit_i(OpCBr, NULL, ic_expression(e->child[0], FALSE), L1, L3);
            emit_i(OpLab, NULL, L1, 0, 0);
            emit_i(OpCBr, NULL, ic_expression(e->child[1], FALSE), L2, L3);
            emit_i(OpLab, NULL, L2, 0, 0);
            emit_i(OpAsn, NULL, a, true_addr, 0);
            emit_i(OpJmp, NULL, L4, 0, 0);
            emit_i(OpLab, NULL, L3, 0, 0);
            emit_i(OpAsn, NULL, a, false_addr, 0);
            emit_i(OpJmp, NULL, L4, 0, 0);
            emit_i(OpLab, NULL, L4, 0, 0);
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
                    emit_i(OpSub, (Declaration *)1, a5, a1, a4); /* the '1' is a mark for later pointer analysis */
                } else { /* ptr-ptr */
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
                emit_i(OpIndAsn, &e->type, a5, a4, 0);
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
                emit_i(OpIndAsn, &e->type, a4, a3, 0);
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
            OpKind op;
            unsigned a1;

            op = (get_type_category(&e->child[0]->type) == TOK_STAR) ? OpIndCall : OpCall;

            function_argument(e->child[1], e->locals);
            a1 = ic_expression(e->child[0], FALSE);
            if (get_type_category(&e->type) != TOK_VOID) {
                unsigned a2;

                a2 = new_temp_addr();
                emit_i(op, &e->type, a2, a1, 0);
                return a2;
            } else {
                emit_i(op, &e->type, 0, a1, 0);
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
    case StrLitExp:
#if 0
        /* ==> "abc"
         (0) t1 = "abc"
         */
        arg1 = new_address(StrLitKind);
        address(arg1).cont.str = e->attr.str;
        tar = new_temp_addr();
        append_instruction(op, tar, arg1, 0);

        return tar;
#endif
        assert(0);
    case IdExp: {
        Token cat;
        unsigned a1;

        a1 = new_address(IdKind);
        address(a1).cont.var.e = e;
        address(a1).cont.nid = get_var_nid(e->attr.str, e->attr.var.scope);
        if (e->attr.var.duration == DURATION_AUTO)
            address(a1).cont.var.offset = location_get_offset(e->attr.str);

        if (is_addr || (cat=get_type_category(&e->type))==TOK_SUBSCRIPT || cat==TOK_FUNCTION) {
            unsigned a2;

            a2 = new_temp_addr();
            emit_i(OpAddrOf, NULL, a2, a1, 0);
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
    emit_i(op, dest, a2, a1, 0);
    return a2;
}

/*
 * Push arguments from right to left recursively.
 */
void function_argument(ExecNode *arg, DeclList *param)
{
    if (arg == NULL)
        return;

    if (param->decl->idl==NULL || param->decl->idl->op!=TOK_ELLIPSIS) {
        /* this argument match a declared (non-optional) parameter */

        Declaration ty;

        function_argument(arg->sibling, param->next);
        ty = *param->decl;
        if (ty.idl!=NULL && ty.idl->op==TOK_ID) /* skip any identifier */
            ty.idl = ty.idl->child;
        emit_i(OpArg, param->decl, 0, ic_expr_convert(arg, &ty), 0);
    } else {
        /* this and the follow arguments match the `...' */

        function_argument(arg->sibling, param);
        emit_i(OpArg, &arg->type, 0, ic_expression(arg, FALSE), 0);
    }
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
        // printf("%s (nid = %d)", address(addr).cont.id, address(addr).cont.com.nid);
        printf("%s", address_sid(addr));
        break;
    /*case StrLitKind:
        printf("%s", address(addr).cont.str);
        break;
    case TempKind:
        printf("t%u", address(addr).cont.tnum);
        break;*/
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

void disassemble(void)
{
    unsigned i;

    for (i = 0; i < ic_instructions_counter; i++) {
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
        case OpIndAsn: printf("*");
        case OpAsn:    print_unaop(p, "");  break;
        case OpAddrOf: print_unaop(p, "&"); break;
        case OpInd:    print_unaop(p, "*"); break;

        case OpLab:
            printf("L%ld:", address(p->tar).cont.val);
            break;
        case OpJmp:
            printf("jmp L%ld", address(p->tar).cont.val);
            break;
        // case OpIndJ:
        // case OpTbl:
        case OpCBr:
            printf("cbr ");
            print_addr(p->tar);
            printf(", L%ld, L%ld", address(p->arg1).cont.val, address(p->arg2).cont.val);
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
            printf("()");
            break;
        case OpRet:
            printf("ret ");
            print_addr(p->arg1);
            break;
        }
        printf("\n");
    }
}
