;
; void __lux_mips_memcpy(void *dest, void *src, unsigned n);
;
; memcpy() function used by the MIPS code generator.
; It doesn't modify any register.
; It neither needs of the argument space that is generally
; allocated by the caller for register arguments.
; It expects arguments on the stack as follows:
;   dest: $sp+0
;   src:  $sp+4
;   n:    $sp+8
;
%global __lux_mips_memcpy
__lux_mips_memcpy:
    ; save registers
    addu $29, $29, -16
    sw $8, 0($29)
    sw $9, 4($29)
    sw $10, 8($29)
    sw $11, 12($29)

    ; load arguments
    lw $8, 16($29)
    lw $9, 20($29)
    lw $10, 24($29)
top:
    ; copy
    blez $10, done
    nop
    lb $11, 0($9)
    sb $11, 0($8)
    addu $8, $8, 1
    addu $9, $9, 1
    addu $10, $10, -1
    b top
    nop

done:
    ; restore registers
    lw $8, 0($29)
    lw $9, 4($29)
    lw $10, 8($29)
    lw $11, 12($29)
    ; remove saved registers + arguments
    addu $29, $29, 28

    jr $31
    nop
