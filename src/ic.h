#ifndef IC_H_
#define IC_H_

#include "parser.h"
#include "bset.h"
#include "decl.h" /* for ExternId */

#define IC_UNSIGNED 0
#define IC_SIGNED   1

typedef struct Address Address;
typedef struct Quad Quad;
typedef struct GraphEdge GraphEdge;
typedef struct CFGNode CFGNode;
typedef struct CGNode CGNode;
typedef struct ParamNid ParamNid;

typedef enum {
    /* x = y op z */
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpRem,
    OpSHL,
    OpSHR,
    OpAnd,
    OpOr,
    OpXor,
    OpEQ,
    OpNEQ,
    OpLT,
    OpLET,
    OpGT,
    OpGET,

    /* x = op y */
    OpNeg,
    OpCmpl,
    OpNot,
    OpCh,
    OpUCh,
    OpSh,
    OpUSh,
    OpAddrOf,
    OpInd,
    OpAsn,      /* x = y */
    OpCall,     /* x = y() or, if void-valued, y() */
    OpIndCall,  /* x = (*y)() */

    /* *x = y */
    OpIndAsn,

    OpLab,
    OpJmp,

    OpArg,
    OpRet,

    OpSwitch,
    OpCase,

    OpCBr,
    OpNOp,
} OpKind;

/*
 * Addresses
 */
typedef enum {
    IConstKind,
    StrLitKind,
    IdKind,
    TempKind
} AddrKind;

struct Address {
    AddrKind kind;
    union {
        long val;
        unsigned long uval;
        char *str;
        int nid;
        struct {
            int vnid;
            ExecNode *e;
            int offset;
        } var;
    } cont; /* contents */
};
extern Address *ic_addresses;
extern int nid_counter;
extern char **nid2sid_tab;
#define address(n)       (ic_addresses[n])
#define address_nid(a)   (address(a).cont.nid)
#define address_sid(a)   (nid2sid_tab[address_nid(a)])
#define const_addr(a)    (address(a).kind==IConstKind || address(a).kind==StrLitKind)

/*
 * Instructions
 */
struct Quad {
    OpKind op;
    Declaration *type;
    unsigned tar, arg1, arg2;
};
extern Quad *ic_instructions;
extern unsigned ic_instructions_counter;
#define instruction(n) (ic_instructions[n])

/*
 * Edges
 */
struct GraphEdge {
    unsigned *edges;
    unsigned max, n;
};
void edge_add(GraphEdge *p, unsigned e);
unsigned edge_iterate(GraphEdge *p);

/*
 * Control Flow Graphs
 */
#define ENTRY_NODE      1
struct CFGNode { /* CFG node == basic block */
    unsigned leader, last;
    GraphEdge out;      /* successors */
    GraphEdge in;       /* predecessors */
    BSet *UEVar;        /* upward-exposed variables in the block */
    BSet *VarKill;      /* variables defined/killed in the block */
    BSet *LiveOut;      /* variables live on exit from the block */
    BSet *Dom;          /* blocks that dominate this block */
    unsigned PO, RPO;   /* post-order & reverse post-order numbers */
#if 0
    BSet *DEDef;    /* downward-exposed definitions */
    BSet *DefKill;  /* all definition points obscured by this block */
    BSet *ReachIn;  /* definitions that reach this block */
#endif
};
extern CFGNode *cfg_nodes;
extern unsigned cfg_nodes_counter;
#define cfg_node(n) (cfg_nodes[n])

/*
 * Call Graphs
 */
struct ParamNid {
    char *sid;
    int nid;
    ParamNid *next;
};

struct CGNode { /* CG node == function */
    char *func_id;
    unsigned bb_i, bb_f;
    GraphEdge out;
#if 0
    ParamNid *pn;
#endif
    BSet *modified_static_objects;
    unsigned size_of_local_area;
    unsigned PO, RPO;
};
extern CGNode *cg_nodes;
extern unsigned cg_nodes_counter;
unsigned new_cg_node(char *func_id);
#define cg_node(n)          (cg_nodes[n])
#define cg_node_is_empty(n) (cg_node(n).bb_i == 0)
#define cg_node_nbb(n)      (cg_node(n).bb_f-cg_node(n).bb_i+1)

/*
 * Misc
 */
int get_var_nid(char *sid, int scope);
extern ExternId *static_objects_list;
extern BSet *address_taken_variables;

void ic_main(ExternId *func_def_list[]);

#endif
