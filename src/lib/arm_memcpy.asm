;
; memcpy() function used by the ARM code generator.
; It doesn't modify any register.
; It expects arguments on the stack as follows:
;   dest: sp+0
;   src:  sp+4
;   n:    sp+8
;
.global __lux_arm_memcpy
__lux_arm_memcpy:
    ; save registers
    stmdb r13!, {r4, r5, r6, r7}
    ; load arguments
    ldr r4, [r13, #16]
    ldr r5, [r13, #20]
    ldr r6, [r13, #24]
top:
    ; copy
    cmp r6, #0
    ble done
    ldrb r7, [r5], #1
    strb r7, [r4], #1
    sub r6, r6, #1
    b top
done:
    ; restore registers
    ldmia r13!, {r4, r5, r6, r7}
    ; remove arguments
    add r13, r13, #12
    mov r15, r14
