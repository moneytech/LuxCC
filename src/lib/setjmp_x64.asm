section .text

; x86_64 jmp_buf:
;
; typedef struct {
    ; long _rbx;
    ; long _rbp;
    ; long _rsp;
    ; long _r12;
    ; long _r13;
    ; long _r14;
    ; long _r15;
    ; long _rip;
; } jmp_buf[1];

; int setjmp(jmp_buf env);
global setjmp:function
setjmp:
    mov rdx, rdi        ; rdx = &env
    mov rax, [rsp]      ; rax = return address
    ; fill buf
    mov [rdx+0], rbx
    mov [rdx+8], rbp
    mov [rdx+16], rsp
    mov [rdx+24], r12
    mov [rdx+32], r13
    mov [rdx+40], r14
    mov [rdx+48], r15
    mov [rdx+56], rax
    ; a direct invocation to setjmp() returns zero
    xor eax, eax
    ret

; void longjmp(jmp_buf env, int val);
global longjmp:function
longjmp:
    mov rdx, rdi        ; rdx = &env
    mov rax, rsi        ; rax = val
    ; restore stack pointer
    mov rsp, [rdx+16]
    ; set our return address to be the same as the return
    ; address of the setjmp() call that filled this buffer
    mov rbx, [rdx+56]
    mov [rsp], rbx
    ; restore the other regs
    mov rbx, [rdx+0]
    mov rbp, [rdx+8]
    mov r12, [rdx+24]
    mov r13, [rdx+32]
    mov r14, [rdx+40]
    mov r15, [rdx+48]
    ; we always return != zero
    or eax, eax
    jne .done
    mov eax, 1
.done:
    ret

