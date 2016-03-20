section .text

; long __raw_syscall_0(long);
global __raw_syscall_0
__raw_syscall_0:
    push ebx
    mov eax, [esp+8]
    int byte 0x80
    pop ebx
    ret

; long __raw_syscall_1(long, long);
global __raw_syscall_1
__raw_syscall_1:
    push ebx
    mov eax, [esp+8]
    mov ebx, [esp+12]
    int byte 0x80
    pop ebx
    ret

; long __raw_syscall_2(long, long, long);
global __raw_syscall_2
__raw_syscall_2:
    push ebx
    mov eax, [esp+8]
    mov ebx, [esp+12]
    mov ecx, [esp+16]
    int byte 0x80
    pop ebx
    ret

; long __raw_syscall_3(long, long, long, long);
global __raw_syscall_3
__raw_syscall_3:
    push ebx
    mov eax, [esp+8]
    mov ebx, [esp+12]
    mov ecx, [esp+16]
    mov edx, [esp+20]
    int byte 0x80
    pop ebx
    ret

; long __raw_syscall_4(long, long, long, long, long);
global __raw_syscall_4
__raw_syscall_4:
    ; currently not needed
    ret
