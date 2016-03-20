section .text

; i386 jmp_buf:
;
; typedef struct {
    ; int _ebx;
    ; int _ebp;
    ; int _esp;
    ; int _edi;
    ; int _esi;
    ; int _eip;
; } jmp_buf[1];

; int setjmp(jmp_buf env);
global setjmp
setjmp:
    mov edx, [esp+4]    ; edx = &env
    mov eax, [esp]      ; eax = return address
    ; fill buf
    mov [edx+0], ebx
    mov [edx+4], ebp
    mov [edx+8], esp
    mov [edx+12], edi
    mov [edx+16], esi
    mov [edx+20], eax
    ; a direct invocation to setjmp() returns zero
    xor eax, eax
    ret

; void longjmp(jmp_buf env, int val);
global longjmp
longjmp:
    mov edx, [esp+4]    ; edx = &env
    mov eax, [esp+8]    ; eax = val
    ; restore stack pointer
    mov esp, [edx+8]
    ; set our return address to be the same as the return
    ; address of the setjmp() call that filled this buffer
    mov ebx, [edx+20]
    mov [esp], ebx
    ; restore the other regs
    mov ebx, [edx+0]
    mov ebp, [edx+4]
    mov edi, [edx+12]
    mov esi, [edx+16]
    ; we always return != zero
    or eax, eax
    jne .done
    mov eax, 1
.done:
    ret

