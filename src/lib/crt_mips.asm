;
; Globals.
;
%section .bss
%alignb 4
_vars:

; standard file streams
%global stdin
stdin:
    %res 4
%global stdout
stdout:
    %res 4
%global stderr
stderr:
    %res 4

; main's arguments
_argc:
    %res 4
_argv:
    %res 4

; errno
%global errno
errno:
    %res 4

;
; Entry point.
;
%section .text
%global __start
__start:
    addu $29, $29, -16

    ; let the emulator set globals
    la $4, _vars
    jal _getvars
    nop

    ; call main()
%extern main    ; the user must define it!
    lw $4, _argc
    lw $5, _argv
    jal main
    nop

    ; call exit()
    move $4, $2
    jal exit
    nop

    ; addu $29, $29, 16

;
; Syscalls
;
_getvars:
    li $2, 0
    syscall
    jr $31
    nop
%global malloc
malloc:
    li $2, 1
    syscall
    jr $31
    nop
%global free
free:
    li $2, 2
    syscall
    jr $31
    nop
%global exit
exit:
    li $2, 3
    syscall
    jr $31
    nop
%global realloc
realloc:
    li $2, 4
    syscall
    jr $31
    nop
%global fputc
fputc:
    li $2, 5
    syscall
    jr $31
    nop
%global fgetc
fgetc:
    li $2, 6
    syscall
    jr $31
    nop
%global fread
fread:
    li $2, 7
    syscall
    jr $31
    nop
%global fwrite
fwrite:
    li $2, 8
    syscall
    jr $31
    nop
%global ferror
ferror:
    li $2, 9
    syscall
    jr $31
    nop
%global fopen
fopen:
    li $2, 10
    syscall
    jr $31
    nop
%global fclose
fclose:
    li $2, 11
    syscall
    jr $31
    nop
%global fseek
fseek:
    li $2, 12
    syscall
    jr $31
    nop
%global ftell
ftell:
    li $2, 13
    syscall
    jr $31
    nop
%global rewind
rewind:
    li $2, 14
    syscall
    jr $31
    nop
%global fgets
fgets:
    li $2, 15
    syscall
    jr $31
    nop
%global stat
stat:
    li $2, 16
    syscall
    jr $31
    nop
%global fileno
fileno:
    li $2, 17
    syscall
    jr $31
    nop
%global isatty
isatty:
    li $2, 18
    syscall
    jr $31
    nop
%global strtol
strtol:
    li $2, 19
    syscall
    jr $31
    nop
%global strtoul
strtoul:
    li $2, 20
    syscall
    jr $31
    nop
%global strtoll
strtoll:
    li $2, 21
    syscall
    jr $31
    nop
%global strtoull
strtoull:
    li $2, 22
    syscall
    jr $31
    nop
