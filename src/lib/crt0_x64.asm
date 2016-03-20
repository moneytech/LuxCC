extern __libc_init, main, exit

section .bss
__argc:
    resd 1
align 8
__argv:
    resq 1
global __env
__env:
    resq 1

section .text
global _start
_start:
    ; get argc
    mov rax, [rsp]
    mov [__argc], eax
    ; get argv
    lea rdx, [rsp+8]
    mov [__argv], rdx
    ; get env
    lea rax, [rdx+rax*8+8]
    mov [__env], rax

    call __libc_init

    ; do exit(main(argc, argv))
    mov rdi, [__argc]
    mov rsi, [__argv]
    call main
    mov rdi, rax
    call exit
