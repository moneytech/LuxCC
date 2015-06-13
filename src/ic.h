#ifndef IC_H_
#define IC_H_

#include "parser.h"
#include "bset.h"

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
    OpAsn,      /* x = y   */
    OpCall,     /* x = y() or, if void-valued, y() */
    OpIndCall,  /* x = (*y)() */

    /* *x = y */
    OpIndAsn,

    OpLab,
    OpJmp,

    /*OpIndJ,*/
    OpArg,
    OpRet,

    /*OpTbl,*/

    OpCBr,
    OpNOp,
} OpKind;

#define IC_UNSIGNED 0
#define IC_SIGNED   1

typedef enum {
    IConstKind,
    StrLitKind,
    IdKind,
    TempKind
} AddrKind;

typedef struct Address Address;
typedef struct Quad Quad;
typedef struct GraphEdge GraphEdge;
typedef struct CFGNode CFGNode;
typedef struct CGNode CGNode;

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

#define LIVE         1
#define DEAD         0
#define NO_NEXT_USE -1

struct Quad {
    OpKind op;
    Declaration *type;
    unsigned tar, arg1, arg2;
    unsigned char liveness[3];
    int next_use[3];
};

struct GraphEdge {
    unsigned *edges;
    unsigned max, n;
};

#define ENTRY_NODE      1
struct CFGNode { /* CFG node == basic block */
    unsigned leader, last;
    GraphEdge out;  /* successors */
    GraphEdge in;   /* predecessors */
    BSet *UEVar;    /* upward-exposed variables in the block */
    BSet *VarKill;  /* variables defined/killed in the block */
    BSet *LiveOut;  /* variables live on exit from the block */
    BSet *Dom;      /* blocks that dominate this block */
};

struct CGNode {
    char *func_id;
    unsigned bb_i, bb_f;
    GraphEdge out;
    GraphEdge in;
};

extern unsigned *CFG_PO;
extern unsigned *CFG_RPO;
extern unsigned *RCFG_PO;
extern unsigned *RCFG_RPO;

#define instruction(n)  (ic_instructions[n])
#define address(n)      (ic_addresses[n])
#define cfg_node(n)     (cfg_nodes[n])
#define cg_node(n)      (cg_nodes[n])
extern Quad *ic_instructions;
extern Address *ic_addresses;
extern CFGNode *cfg_nodes;
extern unsigned cfg_nodes_counter;
extern unsigned ic_instructions_counter;

extern int nid_counter;
extern char **nid2sid_tab;
#define address_nid(a)   (address(a).cont.nid)
#define address_sid(a)   (nid2sid_tab[address_nid(a)])

void ic_main(void);
void ic_init(void);
void ic_reset(void);
void ic_function_definition(TypeExp *decl_specs, TypeExp *header);

#endif
