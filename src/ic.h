#ifndef IC_H_
#define IC_H_

#include "parser.h"
#include "bset.h"
#include "decl.h" /* for ExternId */

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
typedef struct ParamNid ParamNid;

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

struct Quad {
    OpKind op;
    Declaration *type;
    unsigned tar, arg1, arg2;
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

struct ParamNid {
    char *sid;
    int nid;
    ParamNid *next;
};

struct CGNode { /* CG node == function */
    char *func_id;
    unsigned bb_i, bb_f;
    GraphEdge out;
    GraphEdge in;
    ParamNid *pn;
    BSet *PtrRet;   /* pointers returned by this function */
    BSet *LocalMod; /* outside-visible names that may be modified by this function */
    BSet *MayMod;   /* LocalMod + LocalMod of all the functions called by this function */
    BSet *LocalRef; /* outside-visible names that may be referenced by this function */
    BSet *MayRef;   /* LocalRef + LocalRef of all the functions called by this function */
    unsigned size_of_local_area;
};

extern unsigned *CFG_PO;
extern unsigned *CFG_RPO;
extern unsigned *RCFG_PO;
extern unsigned *RCFG_RPO;

extern unsigned *CG_PO;
extern unsigned *CG_RPO;

#define instruction(n)      (ic_instructions[n])
#define address(n)          (ic_addresses[n])
#define cfg_node(n)         (cfg_nodes[n])
#define cg_node(n)          (cg_nodes[n])
#define cg_node_is_empty(n) (cg_node(n).bb_i == 0)
#define cg_node_nbb(n)      (cg_node(n).bb_f-cg_node(n).bb_i+1)
extern Quad *ic_instructions;
extern Address *ic_addresses;
extern CFGNode *cfg_nodes;
extern CGNode *cg_nodes;
extern unsigned ic_instructions_counter;
extern unsigned cfg_nodes_counter;
extern unsigned cg_nodes_counter;

extern int nid_counter;
extern char **nid2sid_tab;
#define address_nid(a)   (address(a).cont.nid)
#define address_sid(a)   (nid2sid_tab[address_nid(a)])
#define nonconst_addr(a) (address(a).kind!=IConstKind && address(a).kind!=StrLitKind)
#define const_addr(a)    (address(a).kind==IConstKind || address(a).kind==StrLitKind)

void ic_main(ExternId *func_def_list[]);
void edge_add(GraphEdge *p, unsigned e);
unsigned new_cg_node(char *func_id);
unsigned edge_iterate(GraphEdge *p);

#endif
