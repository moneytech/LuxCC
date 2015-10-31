#include "operations.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "../util.h"
#include "vm.h"

static Operation operations[] = {
    { "halt",   OpHalt,     0 },
    { "ldb",    OpLdB,      0 },
    { "ldub",   OpLdUB,     0 },
    { "ldw",    OpLdW,      0 },
    { "lduw",   OpLdUW,     0 },
    { "lddw",   OpLdDW,     0 },
    { "ldqw",   OpLdQW,     0 },
    { "ldn",    OpLdN,      1 },

    { "stb",    OpStB,      0 },
    { "stw",    OpStW,      0 },
    { "stdw",   OpStDW,     0 },
    { "stqw",   OpStQW,     0 },
    { "stn",    OpStN,      1 },
    { "memcpy", OpMemCpy,   1 },

    { "add",    OpAdd,      0 },
    { "sub",    OpSub,      0 },
    { "mul",    OpMul,      0 },
    { "sdiv",   OpSDiv,     0 },
    { "udiv",   OpUDiv,     0 },
    { "smod",   OpSMod,     0 },
    { "umod",   OpUMod,     0 },

    { "neg",    OpNeg,      0 },
    { "cmpl",   OpCmpl,     0 },
    { "not",    OpNot,      0 },

    { "slt",    OpSLT,      0 },
    { "ult",    OpULT,      0 },
    { "slet",   OpSLET,     0 },
    { "ulet",   OpULET,     0 },
    { "sgt",    OpSGT,      0 },
    { "ugt",    OpUGT,      0 },
    { "sget",   OpSGET,     0 },
    { "uget",   OpUGET,     0 },
    { "eq",     OpEQ,       0 },
    { "neq",    OpNEQ,      0 },

    { "and",    OpAnd,      0 },
    { "or",     OpOr,       0 },
    { "xor",    OpXor,      0 },
    { "sll",    OpSLL,      0 },
    { "srl",    OpSRL,      0 },
    { "sra",    OpSRA,      0 },

    { "dw2b",   OpDW2B,     0 },
    { "dw2ub",  OpDW2UB,    0 },
    { "dw2w",   OpDW2W,     0 },
    { "dw2uw",  OpDW2UW,    0 },
    { "qw2b",   OpQW2B,     0 },
    { "qw2ub",  OpQW2UB,    0 },
    { "qw2w",   OpQW2W,     0 },
    { "qw2uw",  OpQW2UW,    0 },
    { "qw2dw",  OpQW2DW,    0 },
    { "qw2udw", OpQW2UDW,   0 },

    { "ldi",    OpLdI,      1 },
    { "ldbp",   OpLdBP,     1 },

    { "jmpf",   OpJmpF,     1 },
    { "jmpt",   OpJmpT,     1 },
    { "jmp",    OpJmp,      1 },

    { "call",   OpCall,     1 },
    { "ret",    OpRet,      0 },

    { "dup",    OpDup,      0 },
    { "pop",    OpPop,      0 },
    { "addsp",  OpAddSP,    1 },
    { "nop",    OpNop,      0 },
    { "swap",   OpSwap,     0 },

    { "fill",   OpFill,     1 },
    { "switch", OpSwitch,   0 },
    { "switch2",OpSwitch2,  0 },
    { "libcall",OpLibCall,  1 },
    { "pushsp", OpPushSP,   0 },
};

static int cmp_op(const void *p1, const void *p2)
{
    Operation *x1 = (Operation *)p1;
    Operation *x2 = (Operation *)p2;

    return strcmp(x1->str, x2->str);
}

Operation *lookup_operation(char *op_str)
{
    Operation key, *res;
    static int sorted = FALSE;

    if (!sorted) {
        qsort(operations, NELEMS(operations), sizeof(operations[0]), cmp_op);
        sorted = TRUE;
    }

    key.str = op_str;
    res = bsearch(&key, operations, NELEMS(operations), sizeof(operations[0]), cmp_op);

    return res;
}
