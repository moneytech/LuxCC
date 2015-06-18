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

// =======================================================================================
// MayMod/MayRef summaries
// =======================================================================================
static void print_summary(BSet *s);
static void init_summary(unsigned fn);

void print_summary(BSet *s)
{
    int i, c;

    c = bset_card(s);
    for (i = bset_iterate(s); i != -1; i = bset_iterate(NULL))
        printf("%s%s", nid2sid_tab[i], (c--!=1)?", ":"");
}

/* compute LocalMod(fn) & LocalRef(fn) */
void init_summary(unsigned fn)
{
    unsigned i;
    BSet *LocalMod, *LocalRef;

    LocalMod = bset_new(nid_counter);
    LocalRef = bset_new(nid_counter);

    if (cg_node_is_empty(fn)) { /* external function */
        /* we don't know what it does, be conservative */
        bset_fill(LocalMod, nid_counter);
        bset_fill(LocalRef, nid_counter);
        goto done;
    }

    for (i = cfg_node(cg_node(fn).bb_i).leader; i <= cfg_node(cg_node(fn).bb_f).last; i++) {
        unsigned tar, arg1, arg2;

        tar = instruction(i).tar;
        arg1 = instruction(i).arg1;
        arg2 = instruction(i).arg2;

        switch (instruction(i).op) {
#define add_LocalRef(addr)\
    if (address(addr).kind==IdKind && address(addr).cont.var.e->attr.var.linkage!=LINKAGE_NONE)\
        bset_insert(LocalRef, address_nid(addr))
#define add_LocalMod(addr)\
    if (address(addr).kind==IdKind && address(addr).cont.var.e->attr.var.linkage!=LINKAGE_NONE)\
        bset_insert(LocalMod, address_nid(addr))

        case OpAdd: case OpSub: case OpMul: case OpDiv:
        case OpRem: case OpSHL: case OpSHR: case OpAnd:
        case OpOr: case OpXor: case OpEQ: case OpNEQ:
        case OpLT: case OpLET: case OpGT: case OpGET:
            if (nonconst_addr(arg1))
                add_LocalRef(arg1);
            if (nonconst_addr(arg2))
                add_LocalRef(arg2);
            continue;

        case OpAsn:
            add_LocalMod(tar);
        case OpNeg: case OpCmpl: case OpNot: case OpCh:
        case OpUCh: case OpSh: case OpUSh:
            if (nonconst_addr(arg1))
                add_LocalRef(arg1);
            continue;

        case OpArg:
        case OpRet:
            if (nonconst_addr(arg1))
                add_LocalRef(arg1);
            continue;

        case OpInd: {
            BSet *s;

            if ((s=get_pointer_targets(i, address_nid(arg1))) != NULL)
                bset_union(LocalRef, s);
            else
                bset_fill(LocalRef, nid_counter);
        }
            continue;

        case OpIndAsn: {
            BSet *s;

            if ((s=get_pointer_targets(i, address_nid(tar))) != NULL)
                bset_union(LocalMod, s);
            else
                bset_fill(LocalMod, nid_counter);
        }
            continue;

        case OpIndCall:
            if (nonconst_addr(arg1))
                add_LocalRef(arg1);
            continue;

        case OpCBr:
            if (nonconst_addr(tar))
                add_LocalRef(tar);
            continue;

        default:
            continue;
        }
    }
done:
    cg_node(fn).LocalMod = LocalMod;
    cg_node(fn).LocalRef = LocalRef;
}

void dflow_summaries(void)
{
    int changed;
    BSet *new_out;
    unsigned i, i2;

    for (i = 0; i < cg_nodes_counter; i++) {
        init_summary(i);
        cg_node(i).MayMod = bset_new(nid_counter);
        cg_node(i).MayRef = bset_new(nid_counter);
    }
    new_out = bset_new(nid_counter);

    /* MayMod */
    changed = TRUE;
    while (changed) {
        DEBUG_PRINTF("==> MayMod solver iteration\n");
        changed = FALSE;
        for (i = 0; i < cg_nodes_counter; i++) {
            unsigned j;

            i2 = CG_PO[i];
            /*
             * MayMod(p) = LocalMod(p) U MayMod(q)
             *                       q ∈ called(p)
             */
            bset_cpy(new_out, cg_node(i2).LocalMod);
            for (j = edge_iterate(&cg_node(i2).out); j != -1; j = edge_iterate(NULL))
                bset_union(new_out, cg_node(j).MayMod);
            if (!bset_eq(cg_node(i2).MayMod, new_out)) {
                bset_cpy(cg_node(i2).MayMod, new_out);
                changed = TRUE;
            }
            bset_clear(new_out);
         }
     }

    /* MayRef */
    changed = TRUE;
    while (changed) {
        DEBUG_PRINTF("==> MayRef solver iteration\n");
        changed = FALSE;
        for (i = 0; i < cg_nodes_counter; i++) {
            unsigned j;

            i2 = CG_PO[i];
            /*
             * MayRef(p) = LocalRef(p) U MayRef(q)
             *                       q ∈ called(p)
             */
            bset_cpy(new_out, cg_node(i2).LocalRef);
            for (j = edge_iterate(&cg_node(i2).out); j != -1; j = edge_iterate(NULL))
                bset_union(new_out, cg_node(j).MayRef);
            if (!bset_eq(cg_node(i2).MayRef, new_out)) {
                bset_cpy(cg_node(i2).MayRef, new_out);
                changed = TRUE;
            }
            bset_clear(new_out);
         }
     }

     bset_free(new_out);
#if DEBUG
    for (i = 0; i < cg_nodes_counter; i++) {
        printf("MayMod(%s) = { ", cg_node(i).func_id);
        print_summary(cg_node(i).MayMod);
        printf(" }\n");
    }
    for (i = 0; i < cg_nodes_counter; i++) {
        printf("MayRef(%s) = { ", cg_node(i).func_id);
        print_summary(cg_node(i).MayRef);
        printf(" }\n");
    }
#endif
}

// =======================================================================================
// Dominance
// =======================================================================================
static void dom_print_set(BSet *s);

void dom_print_set(BSet *s)
{
    int i, c;

    c = bset_card(s);
    for (i = bset_iterate(s); i != -1; i = bset_iterate(NULL))
        printf("%d%s", i, (c--!=1)?", ":"");
}

void dflow_dominance(unsigned fn)
{
    /*
     * TBD:
     * Unreachable code (e.g. the one caused by 'return' statements)
     * makes the exit node be dominated by itself only.
     */

    int i, changed;
    BSet *N, *temp;
    unsigned entry_bb, last_bb;

    if (cg_node_is_empty(fn))
        return;

    entry_bb = cg_node(fn).bb_i;
    last_bb = cg_node(fn).bb_f;

    /* Dom(n0) = { n0 } */
    cfg_node(entry_bb).Dom = bset_new(cfg_nodes_counter);
    bset_insert(cfg_node(entry_bb).Dom, entry_bb);

    /* N = all nodes of the CFG */
    N = bset_new(cfg_nodes_counter);
    for (i = entry_bb; i <= last_bb; i++)
        bset_insert(N, i);

    /* for every n != n0, Dom(n) = N */
    for (i = entry_bb+1; i <= last_bb; i++) {
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
        for (i = entry_bb+1; i <= last_bb; i++) {
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
        printf("Dominance, function: `%s'\n", cg_node(fn).func_id);
        for (i = cg_node(fn).bb_i; i <= cg_node(fn).bb_f; i++) {
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
static void live_print_set(BSet *s);
static void live_init_block(int b, int last_bb);

void live_print_set(BSet *s)
{
    int i, c;

    c = bset_card(s);
    for (i = bset_iterate(s); i != -1; i = bset_iterate(NULL))
        printf("%s%s", nid2sid_tab[i], (c--!=1)?", ":"");
}

/* compute UEVar(b) and VarKill(b) */
void live_init_block(int b, int last_bb)
{
    int i;
    BSet *UEVar, *VarKill;

    /* sets initially empty */
    UEVar = bset_new(nid_counter);
    VarKill = bset_new(nid_counter);

    if (last_bb) /* EXIT node */
        bset_cpy(UEVar, modified_static_objects);

    for (i = cfg_node(b).leader; i <= cfg_node(b).last; i++) {
        // TBD:
        // - What to do at function calls?
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

        case OpAsn: /* keep track of modified static objects */
            if ((address(tar).kind == IdKind)
            && (address(tar).cont.var.e->attr.var.duration == DURATION_STATIC))
                bset_insert(modified_static_objects, address_nid(tar));
        case OpNeg: case OpCmpl: case OpNot: case OpCh:
        case OpUCh: case OpSh: case OpUSh:
            if (nonconst_addr(arg1))
                add_UEVar(arg1);
            add_VarKill(tar);
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

        case OpCall: {
            BSet *temp;
            unsigned fn;

            fn = new_cg_node(address(arg1).cont.var.e->attr.str);
            temp = bset_new(nid_counter);
            bset_cpy(temp, cg_node(fn).MayRef);
            bset_diff(temp, VarKill);
            bset_union(UEVar, temp);
            bset_free(temp);
            if (tar)
                add_VarKill(tar);
        }
            continue;

        case OpIndCall: {
            BSet *s;

            if ((s=get_pointer_targets(i, address_nid(arg1))) != NULL) {
                unsigned j;

                for (j = bset_iterate(s); j != -1; j = bset_iterate(NULL)) {
                    BSet *temp;
                    unsigned fn;

                    fn = new_cg_node(nid2sid_tab[j]);
                    temp = bset_new(nid_counter);
                    bset_cpy(temp, cg_node(fn).MayRef);
                    bset_diff(temp, VarKill);
                    bset_union(UEVar, temp);
                    bset_free(temp);
                }
            } else {
                bset_fill(UEVar, nid_counter);
            }
            if (nonconst_addr(arg1))
                add_UEVar(arg1);
            if (tar)
                add_VarKill(tar);
        }
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
void dflow_LiveOut(unsigned fn)
{
    int i, changed;
    BSet *temp, *new_out;
    unsigned entry_bb, last_bb;

    if (cg_node_is_empty(fn))
        return;

    entry_bb = cg_node(fn).bb_i;
    last_bb = cg_node(fn).bb_f;

    modified_static_objects = bset_new(nid_counter);

    /* gather initial information */
    for (i = entry_bb; i <= last_bb; i++) {
        live_init_block(i, i == last_bb);

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
        for (i = entry_bb; i <= last_bb; i++) {
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
    for (i = entry_bb; i <= last_bb; i++) {
        printf("LiveOut[%d]=", i);
        live_print_set(cfg_node(i).LiveOut);
        printf("\n\n");
    }
#endif
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

static BSet *tl_tmp;
static PointToSet **point_OUT;
static int ptr_changed;

static void add_point_to(int i, int ptr, int tgt);
static void union_point_to(int i, int ptr, BSet *s2);
static void cpy_point_to(int i, int ptr, BSet *src);
static PointToSet *search_point_to(PointToSet *setp, int ptr);
static PointToSet *get_ptr(int i, int ptr);
static PointToSet *new_ptr(int ptr, PointToSet *next);
static void ptr_iteration(unsigned fn);
static void ptr_print_set(BSet *s);
static void print_point_to_set(PointToSet *setp);
static void print_point_OUT(void);

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
    bset_cpy(tl_tmp, s->tl);
    bset_insert(s->tl, tgt);
    if (!bset_eq(s->tl, tl_tmp))
        ptr_changed = TRUE;
}

void union_point_to(int i, int ptr, BSet *s2)
{
    PointToSet *s;

    s = get_ptr(i, ptr);
    bset_cpy(tl_tmp, s->tl);
    bset_union(s->tl, s2);
    if (!bset_eq(s->tl, tl_tmp))
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

static int arg_stack[32], arg_stack_top;

void ptr_iteration(unsigned fn)
{
    int b;

    if (cg_node_is_empty(fn))
        return;

    for (b = cg_node(fn).bb_i; b <= cg_node(fn).bb_f; b++) {
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

            case OpCall: {
                ParamNid *pn;
                PointToSet *s;
                unsigned tar_fn, arg;

                tar_fn = new_cg_node(address(arg1).cont.var.e->attr.str);
                edge_add(&cg_node(fn).out, tar_fn);
                for (pn = cg_node(tar_fn).pn; pn != NULL; pn = pn->next) {
                    if (pn->nid == -1) { /* unreferenced parameter */
                        --arg_stack_top;
                        continue;
                    }
                    arg = instruction(arg_stack[--arg_stack_top]).arg1;
                    if ((address(arg).kind==TempKind || address(arg).kind==IdKind)
                    && (s=search_point_to(point_OUT[i-1], address_nid(arg))) != NULL)
                        union_point_to(cfg_node(cg_node(tar_fn).bb_i).leader, pn->nid, s->tl);
                }
                arg_stack_top = 0;

#if 0 /* OPTION-1 */
                /*
                 * Unviable: we don't (because we can't: no call-graph)
                 * compute MayMod sets yet.
                 */
                for (s = point_OUT[i-1]; s != NULL; s = s->next) {
                    if (bset_member(cg_node(tar_fn).MayMod, s->ptr))
                        continue;
                    cpy_point_to(i, s->ptr, s->tl);
                }
                continue;
#endif

#if 0 /* OPTION-2 */
                /*
                 * Worst case, always correct.
                 */
                point_OUT[i] = NULL;
                continue;
#endif

                /*
                 * OPTION-3: be optimistic and assume that
                 * no pointer is modified [ unsafe! ].
                 */
                if (cg_node(tar_fn).PtrRet != NULL) {
                    cpy_point_to(i, address_nid(tar), cg_node(tar_fn).PtrRet);
                    for (s = point_OUT[i-1]; s != NULL; s = s->next) {
                        if (s->ptr == address_nid(tar))
                            continue;
                        cpy_point_to(i, s->ptr, s->tl);
                    }
                } else {
                    break;
                }
            }
                continue;

            case OpIndCall: {
                int p, tmp;
                ParamNid *pn;
                PointToSet *s;
                BSet *fn_PtrRet; /* cumulative set for all possible callees */
                unsigned tar_fn, arg;

                if ((s=search_point_to(point_OUT[i-1], address_nid(arg1))) == NULL)
                    assert(0); /* TBD */

                fn_PtrRet = NULL;
                for (p = bset_iterate(s->tl); p != -1; p = bset_iterate(NULL)) {
                    tar_fn = new_cg_node(nid2sid_tab[p]);
                    edge_add(&cg_node(fn).out, tar_fn);
                    tmp = arg_stack_top;
                    for (pn = cg_node(tar_fn).pn; pn != NULL; pn = pn->next) {
                        if (pn->nid == -1) { /* unreferenced parameter */
                            --arg_stack_top;
                            continue;
                        }
                        arg = instruction(arg_stack[--arg_stack_top]).arg1;
                        if ((address(arg).kind==TempKind || address(arg).kind==IdKind)
                        && (s=search_point_to(point_OUT[i-1], address_nid(arg))) != NULL)
                            union_point_to(cfg_node(cg_node(tar_fn).bb_i).leader, pn->nid, s->tl);
                    }
                    arg_stack_top = tmp;

                    if (cg_node(tar_fn).PtrRet != NULL) {
                        if (fn_PtrRet == NULL)
                            fn_PtrRet = bset_new(nid_counter);
                        bset_union(fn_PtrRet, cg_node(tar_fn).PtrRet);
                    }
                }
                arg_stack_top = 0;

                if (fn_PtrRet != NULL) {
                    cpy_point_to(i, address_nid(tar), fn_PtrRet);
                    for (s = point_OUT[i-1]; s != NULL; s = s->next) {
                        if (s->ptr == address_nid(tar))
                            continue;
                        cpy_point_to(i, s->ptr, s->tl);
                    }
                    bset_free(fn_PtrRet);
                } else {
                    break;
                }
            }
                continue;

            case OpArg:
                arg_stack[arg_stack_top++] = i;
                break;

            case OpRet: {
                PointToSet *s;

                if ((s=search_point_to(point_OUT[i-1], address_nid(arg1))) != NULL) {
                    if (cg_node(fn).PtrRet == NULL) {
                        cg_node(fn).PtrRet = bset_new(nid_counter);
                        bset_cpy(cg_node(fn).PtrRet, s->tl);
                        ptr_changed = TRUE;
                    } else {
                        bset_cpy(tl_tmp, cg_node(fn).PtrRet);
                        bset_union(cg_node(fn).PtrRet, s->tl);
                        if (!bset_eq(cg_node(fn).PtrRet, tl_tmp))
                            ptr_changed = TRUE;
                    }
                }
            }
                break;

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
    tl_tmp = bset_new(nid_counter);

    point_OUT = calloc(ic_instructions_counter, sizeof(PointToSet *));
    ptr_changed = TRUE;
    while (ptr_changed) {
        unsigned i;

        ptr_changed = FALSE;
        DEBUG_PRINTF("==> Point-to solver iteration\n");
        for (i = 0; i < cg_nodes_counter; i++)
            ptr_iteration(i);
    }
    bset_free(tl_tmp);
// #if DEBUG
    print_point_OUT();
// #endif
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
