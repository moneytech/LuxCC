#ifndef VM_H_
#define VM_H_

/* Types de base de la machine

   b  = signed 8 bits
   ub = unsigned 8 bits
   w  = signed 16 bits
   uw = unsigned 16 bits
   i  = signed 32 bits
   ui = unsigned 32 bits

   p  = pointer (32 bits)
*/

/* nom de l'architecture */
// #define VM_ARCH_NAME "i386"


/* taille des types (sert aussi pour l'alignement) */
// #define VM_CHAR_SIZE    1
// #define VM_SHORT_SIZE   2
// #define VM_INT_SIZE     4
// #define VM_POINTER_SIZE 4

/* taille minimum d'un élément mis sur la pile */
// #define VM_STACK_ALIGN  4

/* alignement des segments */
// #define VM_SEG_ALIGN  16

/* Endianité */
// #define VM_LITTLE_ENDIAN 1

/* modèle de pile de la machine virtuelle */
// #define VM_LOCAL_START        4
// #define VM_LOCAL_PARAM_END    -8

/*

        Possible Extensions

    Note: X = B, UB, W, UW, DW.

    LdAX = LdI addr;
           LdX;

    StAX = LdI addr;
           StX;
*/

/*
 * Highly inspired in Fabrice Bellard's VM for the fbcc compiler.
 */

#define VM_STACK_ALIGN      4
#define VM_LOCAL_START      4
#define VM_LOCAL_PARAM_END -8

#define VM_CHAR_SIZE        1
#define VM_SHORT_SIZE       2
#define VM_INT_SIZE         4
#define VM_POINTER_SIZE     4

/*
 *      Base types
 *
 *   b   = signed 8 bits
 *   ub  = unsigned 8 bits
 *   w   = signed 16 bits
 *   uw  = unsigned 16 bits
 *   dw  = 32 bits (signed or unsigned, it doesn't matter)
 */
enum {
    OpHalt,
    OpLdB,
    OpLdUB,
    OpLdW,
    OpLdUW,
    OpLdDW,
    OpLdN,

    OpStB,
    OpStW,
    OpStDW,
    OpStN,
    OpMemCpy,

    OpAdd,
    OpSub,
    OpMul,
    OpSDiv,
    OpUDiv,
    OpSMod,
    OpUMod,

    OpNeg,
    OpCmpl,
    OpNot,

    OpSLT,
    OpULT,
    OpSLET,
    OpULET,
    OpSGT,
    OpUGT,
    OpSGET,
    OpUGET,
    OpEQ,
    OpNEQ,

    OpAnd,
    OpOr,
    OpXor,
    OpSLL,
    OpSRL,
    OpSRA,

    OpDW2B,
    OpDW2UB,
    OpDW2W,
    OpDW2UW,

    OpLdI,
    OpLdBP,

    OpJmpF, /* jump if TOS == 0 */
    OpJmpT, /* jump if TOS != 0 */
    OpJmp,

     // switch_i,

     /* appel de fonctions */
    OpCall,
    OpRet,

     /* gestion de la pile */
    OpDup,
    OpPop,
    OpAddSP,
    OpSwap,

     /* appel de fonctions externes */
     // libcall,
    OpFill,

    OpNop,
};

#endif
