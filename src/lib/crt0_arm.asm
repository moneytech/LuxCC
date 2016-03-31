.extern __libc_init, main, exit

.bss
__argc:
    .res #4
__argv:
    .res #4
.global __env
__env:
    .res #4

.text
$a:
.global _start
_start:
    ; get argc
    ldr r0, [r13]
    ldr r3, =__argc
    str r0, [r3]
    ; get argv
    add r1, r13, #4
    ldr r3, =__argv
    str r1, [r3]
    ; get env
    add r0, r1, r0, LSL #2
    add r0, r0, #4
    ldr r3, =__env
    str r0, [r3]

    bl __libc_init

    ; do exit(main(argc, argv))
    ldr r0, =__argc
    ldr r0, [r0]
    ldr r1, =__argv
    ldr r1, [r1]
    bl main
    bl exit

    .ltorg
