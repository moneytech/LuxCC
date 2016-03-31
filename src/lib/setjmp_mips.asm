%section .text

; mips jmp_buf:
;
; typedef struct {
    ; int _sp;
    ; int _fp;
    ; int _s0;
    ; int _s1;
    ; int _s2;
    ; int _s3;
    ; int _s4;
    ; int _s5;
    ; int _s6;
    ; int _s7;
    ; int _pc;
; } jmp_buf[1];

; int setjmp(jmp_buf env);
%global setjmp:function
setjmp:
    ; fill buf
    sw $29, 0($4)
    sw $30, 4($4)
    sw $16, 8($4)
    sw $17, 12($4)
    sw $18, 16($4)
    sw $19, 20($4)
    sw $20, 24($4)
    sw $21, 28($4)
    sw $22, 32($4)
    sw $23, 36($4)
    sw $31, 40($4)
    ; a direct invocation to setjmp() returns zero
    move $2, $0
    jr $31
    nop

; void longjmp(jmp_buf env, int val);
%global longjmp:function
longjmp:
    ; restore registers
    lw $29, 0($4)
    lw $30, 4($4)
    lw $16, 8($4)
    lw $17, 12($4)
    lw $18, 16($4)
    lw $19, 20($4)
    lw $20, 24($4)
    lw $21, 28($4)
    lw $22, 32($4)
    lw $23, 36($4)
    lw $31, 40($4)
    ; we always return != zero
    bne $5, $0, done
    nop
    li $5, 1
done:
    move $2, $5
    jr $31
    nop

