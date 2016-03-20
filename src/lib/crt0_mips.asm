%extern __libc_init, main, exit

%section .bss
__argc:
    %res 4
__argv:
    %res 4
%global __env
__env:
    %res 4

%section .text
%global __start
__start:
    ; get argc
    lw $8, 0($29)
    sw $8, __argc
    ; get argv
    addu $9, $29, 4
    sw $9, __argv
    ; get env
    sll $8, $8, 2
    addu $9, $9, $8
    addu $9, $9, 4
    sw $9, __env

    jal __libc_init
    nop

    ; do exit(main(argc, argv))
    lw $4, __argc
    lw $5, __argv
    addu $29, $29, -16
    jal main
    nop
    move $4, $2
    jal exit
    nop
