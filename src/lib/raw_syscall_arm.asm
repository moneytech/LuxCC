.text

; long __raw_syscall_0(long);
.global __raw_syscall_0
__raw_syscall_0:
    stmdb r13!, {r7, r14}
    mov r7, r0
    swi #0
    ldmia r13!, {r7, r15}

; long __raw_syscall_1(long, long);
.global __raw_syscall_1
__raw_syscall_1:
    stmdb r13!, {r7, r14}
    mov r7, r0
    mov r0, r1
    swi #0
    ldmia r13!, {r7, r15}

; long __raw_syscall_2(long, long, long);
.global __raw_syscall_2
__raw_syscall_2:
    stmdb r13!, {r7, r14}
    mov r7, r0
    mov r0, r1
    mov r1, r2
    swi #0
    ldmia r13!, {r7, r15}

; long __raw_syscall_3(long, long, long, long);
.global __raw_syscall_3
__raw_syscall_3:
    stmdb r13!, {r7, r14}
    mov r7, r0
    mov r0, r1
    mov r1, r2
    mov r2, r3
    swi #0
    ldmia r13!, {r7, r15}

; long __raw_syscall_4(long, long, long, long, long);
.global __raw_syscall_4
__raw_syscall_4:
    stmdb r13!, {r7, r14}
    mov r7, r0
    mov r0, r1
    mov r1, r2
    mov r2, r3
    ldr r3, [r13, #8]
    swi #0
    ldmia r13!, {r7, r15}
