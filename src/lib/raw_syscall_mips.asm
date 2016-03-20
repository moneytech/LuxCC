%section .text

; long __raw_syscall_0(long);
%global __raw_syscall_0
__raw_syscall_0:
    move $2, $4
    syscall
    jr $31
    nop

; long __raw_syscall_1(long, long);
%global __raw_syscall_1
__raw_syscall_1:
    move $2, $4
    move $4, $5
    syscall
    jr $31
    nop

; long __raw_syscall_2(long, long, long);
%global __raw_syscall_2
__raw_syscall_2:
    move $2, $4
    move $4, $5
    move $5, $6
    syscall
    jr $31
    nop

; long __raw_syscall_3(long, long, long, long);
%global __raw_syscall_3
__raw_syscall_3:
    move $2, $4
    move $4, $5
    move $5, $6
    move $6, $7
    syscall
    jr $31
    nop

; long __raw_syscall_4(long, long, long, long, long);
%global __raw_syscall_4
__raw_syscall_4:
    ; currently not needed
    jr $31
    nop
