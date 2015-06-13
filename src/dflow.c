/*
 * Iterative data-flow analyses.
 */
#define DEBUG 0
#include "dflow.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "util.h"
#include "ic.h"
#include "expr.h"
#include "bset.h"

#define nonconst_addr(a) (address(a).kind!=IConstKind && address(a).kind!=StrLitKind)

// =======================================================================================
// Dominance
// =======================================================================================
static
void dom_print_set(BSet *s)
{
    int i, c;

    c = bset_card(s);
    for (i = bset_iterate(s); i != -1; i = bset_iterate(NULL))
        printf("%d%s", i, (c--!=1)?", ":"");
}

void dflow_dominance(void)
{
    /*
     * TBD:
     * Unreachable code (e.g. the one caused by 'return' statements)
     * makes the exit node be dominated by itself only.
     */

    int i, changed;
    BSet *N, *temp;

    /* Dom(n0) = { n0 } */
    cfg_node(ENTRY_NODE).Dom = bset_new(cfg_nodes_counter);
    bset_insert(cfg_node(ENTRY_NODE).Dom, ENTRY_NODE);

    /* N = all nodes of the CFG */
    N = bset_new(cfg_nodes_counter);
    for (i = ENTRY_NODE; i < cfg_nodes_counter; i++)
        bset_insert(N, i);

    /* for every n != n0, Dom(n) = N */
    for (i = ENTRY_NODE+1; i < cfg_nodes_counter; i++) {
        cfg_node(i).Dom = bset_new(cfg_nodes_counter);
        bset_cpy(cfg_node(i).Dom, N);
    }

    /* working set */
    temp = bset_new(cfg_nodes_counter);

    /* solve equations */
    changed = TRUE;
    while (changed) {
        DEBUG_PRINTF("==> Dom solver iteration\n");
        changed = FALSE;
        for (i = ENTRY_NODE+1; i < cfg_nodes_counter; i++) {
            int j;
            unsigned pred;

            if ((pred = cfg_node(i).in.edges[0])) {
                bset_cpy(temp, cfg_node(pred).Dom);
                for (j = 1; j<cfg_node(i).in.n && (pred=cfg_node(i).in.edges[j]); j++)
                    bset_inters(temp, cfg_node(pred).Dom);
            }
            bset_insert(temp, i);

            if (!bset_eq(temp, cfg_node(i).Dom)) {
                bset_cpy(cfg_node(i).Dom, temp);
                changed = TRUE;
            }
            bset_clear(temp);
        }
    }
    bset_free(N), bset_free(temp);

#if DEBUG
    for (i = ENTRY_NODE; i < cfg_nodes_counter; i++) {
        printf("Dom(n%d) = { ", i);
        dom_print_set(cfg_node(i).Dom);
        printf(" }\n");
    }
#endif
}

// =======================================================================================
// Live analysis.
// =======================================================================================
static BSet *modified_static_objects;

static
void live_print_set(BSet *s)
{
    int i, c;

    c = bset_card(s);
    for (i = bset_iterate(s); i != -1; i = bset_iterate(NULL))
        printf("%s%s", nid2sid_tab[i], (c--!=1)?", ":"");
}

/* compute UEVar(b) and VarKill(b) */
void live_init_block(int b)
{
    int i;
    BSet *UEVar, *VarKill;

    /* sets initially empty */
    UEVar = bset_new(nid_counter);
    VarKill = bset_new(nid_counter);

    if (b == cfg_nodes_counter-1) /* EXIT node */
        bset_cpy(UEVar, modified_static_objects);

    for (i = cfg_node(b).leader; i <= cfg_node(b).last; i++) {
        // TBD:
        // - What to do at function calls?
        // - String literal operands
        unsigned tar, arg1, arg2;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        switch (instruction(i).op) {
#define add_UEVar(e)\
    if (!bset_member(VarKill, address_nid(e)))\
        bset_insert(UEVar, address_nid(e))
#define add_VarKill(e)\
    bset_insert(VarKill, address_nid(e))

        case OpAdd: case OpSub: case OpMul: case OpDiv:
        case OpRem: case OpSHL: case OpSHR: case OpAnd:
        case OpOr: case OpXor: case OpEQ: case OpNEQ:
        case OpLT: case OpLET: case OpGT: case OpGET:
            if (nonconst_addr(arg1))
                add_UEVar(arg1);
            if (nonconst_addr(arg2))
                add_UEVar(arg2);
            add_VarKill(tar);
            continue;

        case OpNeg: case OpCmpl: case OpNot: case OpCh:
        case OpUCh: case OpSh: case OpUSh: case OpAsn:
            if (nonconst_addr(arg1))
                add_UEVar(arg1);
            add_VarKill(tar);

            /* keep track of modified static objects */
            if ((instruction(i).op == OpAsn)
            && (address(tar).kind == IdKind)
            && (address(tar).cont.var.e->attr.var.duration == DURATION_STATIC))
                bset_insert(modified_static_objects, address_nid(tar));
            continue;

        case OpArg:
        case OpRet:
            if (nonconst_addr(arg1))
                add_UEVar(arg1);
            continue;

        case OpAddrOf:
            add_VarKill(tar);
            continue;

        case OpInd: {
            BSet *s;

            /*
             * For safety, overestimate ambiguous indirect
             * references (assume every variable the pointer
             * may point to is referenced).
             */
            if ((s=get_pointer_targets(i, address_nid(arg1))) != NULL) {
                BSet *temp;

                temp = bset_new(nid_counter);
                bset_cpy(temp, s);
                bset_diff(temp, VarKill);
                bset_union(UEVar, temp);
                bset_free(temp);
            } else {
                bset_fill(UEVar, nid_counter);
            }
            add_UEVar(arg1);
            add_VarKill(tar);
        }
            continue;

        case OpIndAsn: {
            /*
             * Must-point-to information is required in order
             * to include pointer targets in the VarKill set.
             * For safety, underestimate ambiguous indirect
             * assignments (assume no variables are modified).
             */

            if (nonconst_addr(arg1))
                add_UEVar(arg1);
            add_UEVar(tar);
        }
            continue;

        /*
         * TBD: fill UEVar set? (overestimate function calls)
         */
        // case OpCall:
        case OpIndCall:
            if (nonconst_addr(arg1))
                add_UEVar(arg1);
            if (tar)
                add_VarKill(tar);
            continue;

        case OpCBr:
            if (nonconst_addr(tar))
                add_UEVar(tar);
            continue;

        default:
            continue;
        }
    }
    cfg_node(b).UEVar = UEVar;
    cfg_node(b).VarKill = VarKill;
}

/* compute LiveOut for all the blocks of the CFG */
void dflow_LiveOut(void)
{
    int i, changed;
    BSet *temp, *new_out;

    modified_static_objects = bset_new(nid_counter);

    /* gather initial information */
    for (i = ENTRY_NODE; i < cfg_nodes_counter; i++) {
        live_init_block(i);

#if DEBUG
        printf("UEVar[%d]=", i);
        live_print_set(cfg_node(i).UEVar);
        printf("\n\n");

        printf("VarKill[%d]=", i);
        live_print_set(cfg_node(i).VarKill);
        printf("\n\n");
#endif

        /* all LiveOut sets are initially empty */
        cfg_node(i).LiveOut = bset_new(nid_counter);
    }
    bset_free(modified_static_objects);

    /* working sets */
    temp = bset_new(nid_counter);
    new_out = bset_new(nid_counter);

    /* solve equations */
    changed = TRUE;
    while (changed) {
        DEBUG_PRINTF("==> LiveOut solver iteration\n");
        changed = FALSE;
        for (i = ENTRY_NODE; i < cfg_nodes_counter; i++) {
            int j, b;
            unsigned succ;

            b = RCFG_RPO[i];
            /*
             * LiveOut(b) = the union of all successors of b, where the contribution
             *              of each successor m is       __________
             *                  UEVar(m) U (LiveOut(m) ∩ VarKill(m))
             */
            for (j = 0; j<cfg_node(b).out.n && (succ=cfg_node(b).out.edges[j]); j++) {
                bset_cpy(temp, cfg_node(succ).LiveOut);
                bset_diff(temp, cfg_node(succ).VarKill);
                bset_union(temp, cfg_node(succ).UEVar);
                bset_union(new_out, temp);
            }
            if (!bset_eq(cfg_node(b).LiveOut, new_out)) {
                bset_cpy(cfg_node(b).LiveOut, new_out);
                changed = TRUE;
            }
            bset_clear(new_out);
        }
    }
    bset_free(temp), bset_free(new_out);

#if DEBUG
    for (i = ENTRY_NODE; i < cfg_nodes_counter; i++) {
        printf("LiveOut[%d]=", i);
        live_print_set(cfg_node(i).LiveOut);
        printf("\n\n");
    }
#endif
}


/*
 * Compute liveness & next-use required for code generation.
 */
typedef struct Operand Operand;
static struct Operand {
    int liveness;
    int next_use;
} *operand_table;

static void print_liveness_and_next_use(void);

/*
 * Initialize the operand table with the liveness
 * and next use as of the end of the block.
 * Liveness is determined using the LiveOut set.
 */
void init_operand_table(BSet *bLO)
{
    int i;

    for (i = 0; i < nid_counter; i++) {
        operand_table[i].liveness = DEAD;
        operand_table[i].next_use = NO_NEXT_USE;
    }

    for (i = bset_iterate(bLO); i != -1; i = bset_iterate(NULL)) {
        operand_table[i].liveness = LIVE;
        operand_table[i].next_use = NO_NEXT_USE;
    }
}

void compute_liveness_and_next_use(void)
{
    int b;

    /* allocate operand table */
    operand_table = malloc(nid_counter*sizeof(Operand));

    /* annotate the quads of every block with liveness and next-use information */
    for (b = ENTRY_NODE; b < cfg_nodes_counter; b++) {
        int i;

        init_operand_table(cfg_node(b).LiveOut);

        /* scan backward through the block */
        for (i = cfg_node(b).last; i >= (int)cfg_node(b).leader; i--) {
            unsigned tar, arg1, arg2;
            unsigned char *liveness;
            int *next_use;

            tar = instruction(i).tar;
            arg1 = instruction(i).arg1;
            arg2 = instruction(i).arg2;
            liveness = instruction(i).liveness;
            next_use = instruction(i).next_use;

            switch (instruction(i).op) {
#define update_tar()\
        liveness[0] = operand_table[address_nid(tar)].liveness,\
        next_use[0] = operand_table[address_nid(tar)].next_use,\
        operand_table[address_nid(tar)].liveness = DEAD,\
        operand_table[address_nid(tar)].next_use = NO_NEXT_USE
#define update_arg1()\
        liveness[1] = operand_table[address_nid(arg1)].liveness,\
        next_use[1] = operand_table[address_nid(arg1)].next_use,\
        operand_table[address_nid(arg1)].liveness = LIVE,\
        operand_table[address_nid(arg1)].next_use = i
#define update_arg2()\
        liveness[2] = operand_table[address_nid(arg2)].liveness,\
        next_use[2] = operand_table[address_nid(arg2)].next_use,\
        operand_table[address_nid(arg2)].liveness = LIVE,\
        operand_table[address_nid(arg2)].next_use = i

            case OpAdd: case OpSub: case OpMul: case OpDiv:
            case OpRem: case OpSHL: case OpSHR: case OpAnd:
            case OpOr: case OpXor: case OpEQ: case OpNEQ:
            case OpLT: case OpLET: case OpGT: case OpGET:
                update_tar();
                if (nonconst_addr(arg1))
                    update_arg1();
                if (nonconst_addr(arg2))
                    update_arg2();
                continue;

            case OpNeg: case OpCmpl: case OpNot: case OpCh:
            case OpUCh: case OpSh: case OpUSh: case OpAsn:
                update_tar();
                if (nonconst_addr(arg1))
                    update_arg1();
                continue;

            case OpArg:
            case OpRet:
                if (nonconst_addr(arg1))
                    update_arg1();
                continue;

            case OpAddrOf:
                update_tar();
                continue;

            case OpInd: {
                int j;
                BSet *s;

                update_tar();
                update_arg1();

                if ((s=get_pointer_targets(i, address_nid(arg1))) != NULL) {
                    for (j = bset_iterate(s); j != -1; j = bset_iterate(NULL))
                        operand_table[j].liveness = LIVE;
                } else {
                    /*
                     * TBD: if temporaries cannot cross basic block boundaries,
                     * only programmer declared variables need to be marked 'LIVE'.
                     */
                    for (j = 0; j < nid_counter; j++)
                        operand_table[j].liveness = LIVE;
                }
            }
                continue;

            case OpIndAsn:
                liveness[0] = operand_table[address_nid(tar)].liveness;
                next_use[0] = operand_table[address_nid(tar)].next_use;
                operand_table[address_nid(tar)].liveness = LIVE;
                operand_table[address_nid(tar)].next_use = i;
                if (nonconst_addr(arg1))
                    update_arg1();
                continue;

            case OpCall:
                if (tar)
                    update_tar();
                continue;

            case OpIndCall:
                if (tar)
                    update_tar();
                if (nonconst_addr(arg1))
                    update_arg1();
                continue;

            case OpCBr:
                if (nonconst_addr(tar)) {
                    liveness[0] = operand_table[address_nid(tar)].liveness;
                    next_use[0] = operand_table[address_nid(tar)].next_use;
                    operand_table[address_nid(tar)].liveness = LIVE;
                    operand_table[address_nid(tar)].next_use = i;
                }
                continue;

            default: /* other */
                continue;
            } /* switch (instruction(i).op) */
        } /* instructions */
    } /* basic blocks */

    free(operand_table);
#if DEBUG
    print_liveness_and_next_use();
#endif
}

void print_liveness_and_next_use(void)
{
    int b;

    for (b = ENTRY_NODE; b < cfg_nodes_counter; b++) {
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
        printf("status=%s, ", instruction(i).liveness[0]?"LIVE":"DEAD"),\
        printf("next use=%d", instruction(i).next_use[0])
#define print_arg1()\
        printf("name=%s, ", address_sid(arg1)),\
        printf("status=%s, ", instruction(i).liveness[1]?"LIVE":"DEAD"),\
        printf("next use=%d", instruction(i).next_use[1])
#define print_arg2()\
        printf("name=%s, ", address_sid(arg2)),\
        printf("status=%s, ", instruction(i).liveness[2]?"LIVE":"DEAD"),\
        printf("next use=%d", instruction(i).next_use[2])

            case OpAdd: case OpSub: case OpMul: case OpDiv:
            case OpRem: case OpSHL: case OpSHR: case OpAnd:
            case OpOr: case OpXor: case OpEQ: case OpNEQ:
            case OpLT: case OpLET: case OpGT: case OpGET:
                print_tar();
                if (nonconst_addr(arg1)) {
                    printf(" | ");
                    print_arg1();
                }
                if (nonconst_addr(arg2)) {
                    printf(" | ");
                    print_arg2();
                }
                break;

            case OpNeg: case OpCmpl: case OpNot: case OpCh:
            case OpUCh: case OpSh: case OpUSh: case OpAsn:
                print_tar();
                if (nonconst_addr(arg1)) {
                    printf(" | ");
                    print_arg1();
                }
                break;

            case OpArg:
            case OpRet:
                if (nonconst_addr(arg1)) {
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
                print_tar();
                if (nonconst_addr(arg1)) {
                    printf(" | ");
                    print_arg1();
                }
                break;

            case OpCall:
                if (tar)
                    print_tar();
                break;

            case OpIndCall:
                if (tar)
                    print_tar();
                if (nonconst_addr(arg1)) {
                    printf(" | ");
                    print_arg1();
                }
                break;

            case OpCBr:
                if (nonconst_addr(tar))
                    print_tar();
                break;
            default:
                continue;
            } /* instructions */
            printf("\n--------------\n");
        } /* basic blocks */
        printf("\n\n");
    }
}

// =======================================================================================
// Pointer analysis (flow-sensitive, may point-to analysis).
// =======================================================================================
typedef struct PointToSet PointToSet;
struct PointToSet {
    int ptr;
    BSet *tl;
    PointToSet *next;
};

static BSet *ptr_tmp;
static PointToSet **point_OUT;
static int ptr_changed;

static PointToSet *new_ptr(int ptr, PointToSet *next);
static void add_point_to(int i, int ptr, int tgt);
static void union_point_to(int i, int ptr, BSet *s2);
static void cpy_point_to(int i, int ptr, BSet *src);
static PointToSet *search_point_to(PointToSet *setp, int ptr);
static void ptr_iteration(void);
static PointToSet *get_ptr(int i, int ptr);

PointToSet *new_ptr(int ptr, PointToSet *next)
{
    PointToSet *n;

    n = malloc(sizeof(PointToSet));
    n->ptr = ptr;
    n->tl = bset_new(nid_counter);
    n->next = next;
    return n;
}

PointToSet *get_ptr(int i, int ptr)
{
    PointToSet *s;

    for (s = point_OUT[i]; s != NULL; s = s->next)
        if (s->ptr == ptr)
            break;
    if (s == NULL) {
        s = new_ptr(ptr, point_OUT[i]);
        point_OUT[i] = s;
    }
    return s;
}

void add_point_to(int i, int ptr, int tgt)
{
    PointToSet *s;

    s = get_ptr(i, ptr);
    bset_cpy(ptr_tmp, s->tl);
    bset_insert(s->tl, tgt);
    if (!bset_eq(s->tl, ptr_tmp))
        ptr_changed = TRUE;
}

void union_point_to(int i, int ptr, BSet *s2)
{
    PointToSet *s;

    s = get_ptr(i, ptr);
    bset_cpy(ptr_tmp, s->tl);
    bset_union(s->tl, s2);
    if (!bset_eq(s->tl, ptr_tmp))
        ptr_changed = TRUE;
}

void cpy_point_to(int i, int ptr, BSet *src)
{
    PointToSet *s;

    s = get_ptr(i, ptr);
    if (!bset_eq(s->tl, src)) {
        bset_cpy(s->tl, src);
        ptr_changed = TRUE;
    }
}

PointToSet *search_point_to(PointToSet *setp, int ptr)
{
    for (; setp != NULL; setp = setp->next)
        if (setp->ptr == ptr)
            break;
    return setp;
}

static
void ptr_print_set(BSet *s)
{
    int i, c;

    c = bset_card(s);
    for (i = bset_iterate(s); i != -1; i = bset_iterate(NULL))
        printf("%s%s", nid2sid_tab[i], (c--!=1)?", ":"");
}

void print_point_to_set(PointToSet *setp)
{
    for (; setp != NULL; setp = setp->next) {
        printf("%s -> {", nid2sid_tab[setp->ptr]);
        ptr_print_set(setp->tl);
        if (setp->next != NULL)
            printf("}, ");
        else
            printf("}");
    }
}

void print_point_OUT(void)
{
    int i;

    for (i = 0; i < ic_instructions_counter; i++) {
        printf("point_OUT[%d] = ", i);
        if (point_OUT[i] != NULL) {
            printf("[ ");
            print_point_to_set(point_OUT[i]);
            printf(" ]");
        } else {
            printf("<empty>");
        }
        printf("\n");
    }
}

void ptr_iteration(void)
{
    int b;

    for (b = ENTRY_NODE; b < cfg_nodes_counter; b++) {
        int i;

        for (i = cfg_node(b).leader; i <= cfg_node(b).last; i++) {
            unsigned tar, arg1/*, arg2*/;

            tar = instruction(i).tar;
            arg1 = instruction(i).arg1;
            /*arg2 = instruction(i).arg2;*/

            switch (instruction(i).op) {
            case OpAddrOf: { /* x = &y */
                PointToSet *s;

                add_point_to(i, address_nid(tar), address_nid(arg1));
                for (s = point_OUT[i-1]; s != NULL; s = s->next) {
                    if (s->ptr == address_nid(tar))
                        continue;
                    cpy_point_to(i, s->ptr, s->tl);
                }
            }
                continue;

            case OpAsn: { /* x = y */
                PointToSet *s;

                if ((address(arg1).kind==TempKind || address(arg1).kind==IdKind)
                && (s=search_point_to(point_OUT[i-1], address_nid(arg1))) != NULL) { /* y -> {...} */
                    cpy_point_to(i, address_nid(tar), s->tl);
                } else if ((s=search_point_to(point_OUT[i-1], address_nid(tar))) != NULL) { /* x -> {...} */
                    ;
                } else {
                    break;
                }
                for (s = point_OUT[i-1]; s != NULL; s = s->next) {
                    if (s->ptr == address_nid(tar))
                        continue;
                    cpy_point_to(i, s->ptr, s->tl);
                }
            }
                continue;

            case OpIndAsn: { /* *x = y */
                PointToSet *s, *s1, *s2;

                s2 = NULL;
                if (address(arg1).kind==TempKind || address(arg1).kind==IdKind)
                    s2 = search_point_to(point_OUT[i-1], address_nid(arg1));
                if ((s1=search_point_to(point_OUT[i-1], address_nid(tar))) != NULL) { /* x -> {...} */
                    if (s2 != NULL) { /* and y -> {...} */
                        int p;

                        for (p = bset_iterate(s1->tl); p != -1; p = bset_iterate(NULL))
                            cpy_point_to(i, p, s2->tl);
                    }
                    for (s = point_OUT[i-1]; s != NULL; s = s->next) {
                        if (bset_member(s1->tl, s->ptr))
                            continue;
                        cpy_point_to(i, s->ptr, s->tl);
                    }
                } else if (s2 != NULL) { /* only y -> {...} */
                    for (s = point_OUT[i-1]; s != NULL; s = s->next)
                        cpy_point_to(i, s->ptr, s2->tl);
                } else { /* x -> {} & y -> {} */
                    point_OUT[i] = NULL;
                }
            }
                continue;

            case OpInd: { /* x = *y */
                PointToSet *s1, *s2;

                if ((s1=search_point_to(point_OUT[i-1], address_nid(arg1))) != NULL) { /* y -> {...} */
                    int p;

#if 1 /* p U ? = p (less safe) */
                    for (p = bset_iterate(s1->tl); p != -1; p = bset_iterate(NULL))
                        if ((s2=search_point_to(point_OUT[i-1], p)) != NULL)
                            union_point_to(i, address_nid(tar), s2->tl);
#else /* p U ? = ? (more safe) */
                    for (p = bset_iterate(s1->tl); p != -1; p = bset_iterate(NULL))
                        if (search_point_to(point_OUT[i-1], p) == NULL)
                            break;
                    if (p == -1)
                        for (p = bset_iterate(s1->tl); p != -1; p = bset_iterate(NULL))
                            union_point_to(i, address_nid(tar), search_point_to(point_OUT[i-1], p)->tl);
#endif
                }
                for (s1 = point_OUT[i-1]; s1 != NULL; s1 = s1->next) {
                    if (s1->ptr == address_nid(tar))
                        continue;
                    cpy_point_to(i, s1->ptr, s1->tl);
                }
            }
                continue;

            case OpAdd: {
                PointToSet *s;

                /* assume front-end normalized the code
                   so only the first arg can be a pointer */
                if ((address(arg1).kind==TempKind || address(arg1).kind==IdKind)
                && (s=search_point_to(point_OUT[i-1], address_nid(arg1))) != NULL)
                    cpy_point_to(i, address_nid(tar), s->tl);
                else
                    break;
                for (s = point_OUT[i-1]; s != NULL; s = s->next) {
                    if (s->ptr == address_nid(tar))
                        continue;
                    cpy_point_to(i, s->ptr, s->tl);
                }
            }
                continue;

            case OpSub: {
                PointToSet *s;

                /* the front-end put an '1' in the type fields to
                   indicate a subtraction between a pointer and
                   an integer */
                if (instruction(i).type == (Declaration *)1
                && (address(arg1).kind==TempKind || address(arg1).kind==IdKind)
                && (s=search_point_to(point_OUT[i-1], address_nid(arg1))) != NULL)
                    cpy_point_to(i, address_nid(tar), s->tl);
                else
                    break;
                for (s = point_OUT[i-1]; s != NULL; s = s->next) {
                    if (s->ptr == address_nid(tar))
                        continue;
                    cpy_point_to(i, s->ptr, s->tl);
                }
            }
                continue;

            case OpCall:
            case OpIndCall:
                /* currently, assume worst-case (that every pointer is modified) */
                point_OUT[i] = NULL;
                continue;

            default:
                break;
            } /* switch (instruction(i).op) */

            /* fall through; some instruction that doesn't contribute to point-to information */
            if (i == cfg_node(b).leader) {
                int npred;
                unsigned pred;

                npred = 0;
                while (cfg_node(b).in.edges[npred])
                    ++npred;
                if (npred == 0) {
                    ;
                } else if (npred == 1) {
                    pred = cfg_node(b).in.edges[0];
                    point_OUT[i] = point_OUT[cfg_node(pred).last];
                } else {
#if 1 /* to be in OUT[i], a pointer (that is, ptr -> {...}) must be in at least one predecessor (less safe) */
                    int j;
                    unsigned pred;
                    PointToSet *s;

                    for (j = 0; j < npred; j++) {
                        pred = cfg_node(b).in.edges[j];
                        for (s = point_OUT[cfg_node(pred).last]; s != NULL; s = s->next)
                            union_point_to(i, s->ptr, s->tl);
                    }
#else /* to be in OUT[i], a pointer must be in all predecessors (more safe) */
                    PointToSet *p[MAX_IN_EDGES], *s;

                    /* take the ptr intersection */
                    pred = cfg_node(b).in.edges[0];
                    for (s = point_OUT[cfg_node(pred).last]; s != NULL; s = s->next) {
                        int j;

                        memset(p, 0, sizeof(PointToSet *)*MAX_IN_EDGES);
                        p[0] = s;
                        for (j = 1; j < npred; j++) {
                            pred = cfg_node(b).in.edges[j];
                            if ((p[j]=search_point_to(point_OUT[cfg_node(pred).last], s->ptr)) == NULL)
                                break;
                        }
                        if (j != npred)
                            continue;

                        for (j = 0; j < npred; j++)
                            union_point_to(i, s->ptr, p[j]->tl);
                    }
#endif
                }
            } else {
                point_OUT[i] = point_OUT[i-1];
            }
        } /* instructions */
    } /* CFG nodes */
}

void dflow_PointOut(void)
{
    ptr_tmp = bset_new(nid_counter);

    point_OUT = calloc(ic_instructions_counter, sizeof(PointToSet *));
    ptr_changed = TRUE;
    while (ptr_changed) {
        ptr_changed = FALSE;
        DEBUG_PRINTF("==> Point-to solver iteration\n");
        ptr_iteration();
    }
    bset_free(ptr_tmp);
#if DEBUG
    print_point_OUT();
#endif
}

BSet *get_pointer_targets(int i, int p)
{
    PointToSet *s;

    for (s = point_OUT[i-1]; s != NULL; s = s->next)
        if (s->ptr == p)
            return s->tl;
    return NULL;
}

void free_PointOut(void)
{
    free(point_OUT);
}
