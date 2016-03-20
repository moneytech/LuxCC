section .text

; long __raw_syscall_0(long);
global __raw_syscall_0
__raw_syscall_0:
    mov rax, rdi
    syscall
    ret

; long __raw_syscall_1(long, long);
global __raw_syscall_1
__raw_syscall_1:
    mov rax, rdi
    mov rdi, rsi
    syscall
    ret

; long __raw_syscall_2(long, long, long);
global __raw_syscall_2
__raw_syscall_2:
    mov rax, rdi
    mov rdi, rsi
    mov rsi, rdx
    syscall
    ret

; long __raw_syscall_3(long, long, long, long);
global __raw_syscall_3
__raw_syscall_3:
    mov rax, rdi
    mov rdi, rsi
    mov rsi, rdx
    mov rdx, rcx
    syscall
    ret

; long __raw_syscall_4(long, long, long, long, long);
global __raw_syscall_4
__raw_syscall_4:
    mov rax, rdi
    mov rdi, rsi
    mov rsi, rdx
    mov rdx, rcx
    mov r10, r8
    syscall
    ret
