.text

; arm jmp_buf:
;
; typedef struct {
    ; long long _align;
    ; int _sp;
    ; int _v1;
    ; int _v2;
    ; int _v3;
    ; int _v4;
    ; int _v5;
    ; int _v6;
    ; int _v7;
    ; int _v8;
    ; int _lr;
; } jmp_buf[1];

; int setjmp(jmp_buf env);
.global setjmp
setjmp:
    ; fill buf
    str r13, [r0, #8]
    str r4, [r0, #12]
    str r5, [r0, #16]
    str r6, [r0, #20]
    str r7, [r0, #24]
    str r8, [r0, #28]
    str r9, [r0, #32]
    str r10, [r0, #36]
    str r11, [r0, #40]
    str r14, [r0, #44]
    ; a direct invocation to setjmp() returns zero
    eor r0, r0, r0
    mov r15, r14

; void longjmp(jmp_buf env, int val);
.global longjmp
longjmp:
    ; restore registers
    ldr r13, [r0, #8]
    ldr r4, [r0, #12]
    ldr r5, [r0, #16]
    ldr r6, [r0, #20]
    ldr r7, [r0, #24]
    ldr r8, [r0, #28]
    ldr r9, [r0, #32]
    ldr r10, [r0, #36]
    ldr r11, [r0, #40]
    ldr r14, [r0, #44]
    ; we always return != zero
    cmp r1, #0
    bne done
    ldr r1, =#1
done:
    mov r15, r14

