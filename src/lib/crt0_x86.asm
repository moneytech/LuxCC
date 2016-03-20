extern __libc_init, main, exit

section .bss
__argc:
    resd 1
__argv:
    resd 1
global __env
__env:
    resd 1

section .text
global _start
_start:
    ; get argc
    mov eax, [esp]
    mov [__argc], eax
    ; get argv
    lea edx, [esp+4]
    mov [__argv], edx
    ; get env
    lea eax, [edx+eax*4+4]
    mov [__env], eax

    call __libc_init

    ; do exit(main(argc, argv))
    push dword [__argv]
    push dword [__argc]
    call main
    push eax
    call exit
