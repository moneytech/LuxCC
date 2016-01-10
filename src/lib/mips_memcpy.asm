;
; void __lux_mips_memcpy(void *dest, void *src, unsigned n);
;
; memcpy() function used by the MIPS code generator.
; It only uses and modifies the registers $4, $5, $6, and $7.
; It doesn't need the argument space that is generally allocated
; by the caller for register arguments.
;
%global __lux_mips_memcpy
__lux_mips_memcpy:
top:
    blez $6, done
    nop
    lb $7, 0($5)
    sb $7, 0($4)
    addu $4, $4, 1
    addu $5, $5, 1
    addu $6, $6, -1
    b top
    nop
done:
    jr $31
    nop
