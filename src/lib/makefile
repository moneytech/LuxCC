CC=../luxcc
X86_AS=../luxas/luxas -m32
X64_AS=../luxas/luxas -m64
MIPS_AS=../luxmips/luxasmips
ARM_AS=../luxarm/luxasarm
CFLAGS=-q
LIBC_X86_FILES=obj/x86/raw_syscall.o obj/x86/init.o obj/x86/stdio.o \
		obj/x86/unistd.o obj/x86/stdlib.o obj/x86/string.o \
		obj/x86/ctype.o  obj/x86/errno.o obj/x86/termios.o \
		obj/x86/syscall.o obj/x86/ioctl.o obj/x86/fcntl.o \
		obj/x86/stat.o obj/x86/signal.o obj/x86/assert.o \
		obj/x86/setjmp.o obj/x86/time.o obj/x86/times.o \
		obj/x86/wait.o

LIBC_X64_FILES=obj/x64/raw_syscall.o obj/x64/init.o obj/x64/stdio.o \
		obj/x64/unistd.o obj/x64/stdlib.o obj/x64/string.o \
		obj/x64/ctype.o obj/x64/errno.o obj/x64/termios.o \
		obj/x64/syscall.o obj/x64/ioctl.o obj/x64/fcntl.o \
		obj/x64/stat.o obj/x64/signal.o obj/x64/assert.o \
		obj/x64/setjmp.o obj/x64/time.o obj/x64/times.o \
		obj/x64/wait.o

LIBC_MIPS_FILES=obj/mips/raw_syscall.o obj/mips/init.o obj/mips/stdio.o \
		obj/mips/unistd.o obj/mips/stdlib.o obj/mips/string.o \
		obj/mips/ctype.o obj/mips/errno.o obj/mips/termios.o \
		obj/mips/syscall.o obj/mips/ioctl.o obj/mips/fcntl.o \
		obj/mips/stat.o obj/mips/signal.o obj/mips/assert.o \
		obj/mips/setjmp.o obj/mips/time.o obj/mips/times.o \
		obj/mips/wait.o

LIBC_ARM_FILES=obj/arm/raw_syscall.o obj/arm/init.o obj/arm/stdio.o \
		obj/arm/unistd.o obj/arm/stdlib.o obj/arm/string.o \
		obj/arm/ctype.o obj/arm/errno.o obj/arm/termios.o \
		obj/arm/syscall.o obj/arm/ioctl.o obj/arm/fcntl.o \
		obj/arm/stat.o obj/arm/signal.o obj/arm/assert.o \
		obj/arm/setjmp.o obj/arm/time.o obj/arm/times.o \
		obj/arm/wait.o

all: crt0 liblux libc luxmemcpy vm_lib

#
# crt0.o
#

crt0: obj/x86/crt0.o obj/x64/crt0.o obj/mips/crt0.o obj/arm/crt0.o

obj/x86/crt0.o: crt0_x86.asm
	$(X86_AS) crt0_x86.asm -o obj/x86/crt0.o

obj/x64/crt0.o: crt0_x64.asm
	$(X64_AS) crt0_x64.asm -o obj/x64/crt0.o

obj/mips/crt0.o: crt0_mips.asm
	$(MIPS_AS) crt0_mips.asm -o obj/mips/crt0.o

obj/arm/crt0.o: crt0_arm.asm
	$(ARM_AS) crt0_arm.asm -o obj/arm/crt0.o

#
# liblux.o
#

liblux: obj/x86/liblux.o obj/mips/liblux.o obj/arm/liblux.o

obj/x86/liblux.o: liblux.c
	$(CC) $(CFLAGS) -mx86 -z liblux.c -o liblux.asm && $(X86_AS) liblux.asm -o obj/x86/liblux.o && rm liblux.asm

obj/mips/liblux.o: liblux.c
	$(CC) $(CFLAGS) -mmips -z liblux.c -o liblux.asm && $(MIPS_AS) liblux.asm -o obj/mips/liblux.o && rm liblux.asm

obj/arm/liblux.o: liblux.c
	$(CC) $(CFLAGS) -marm -z liblux.c -o liblux.asm && $(ARM_AS) liblux.asm -o obj/arm/liblux.o && rm liblux.asm

#
# libc.a
#

libc: obj/x86/libc.a obj/x64/libc.a obj/mips/libc.a obj/arm/libc.a

obj/x86/libc.a: $(LIBC_X86_FILES)
	ar rcs obj/x86/libc.a $(LIBC_X86_FILES)

obj/x64/libc.a: $(LIBC_X64_FILES)
	ar rcs obj/x64/libc.a $(LIBC_X64_FILES)

obj/mips/libc.a: $(LIBC_MIPS_FILES)
	ar rcs obj/mips/libc.a $(LIBC_MIPS_FILES)

obj/arm/libc.a: $(LIBC_ARM_FILES)
	ar rcs obj/arm/libc.a $(LIBC_ARM_FILES)

#
# raw_syscall.o
#

obj/x86/raw_syscall.o: raw_syscall_x86.asm
	$(X86_AS) raw_syscall_x86.asm -o obj/x86/raw_syscall.o

obj/x64/raw_syscall.o: raw_syscall_x64.asm
	$(X64_AS) raw_syscall_x64.asm -o obj/x64/raw_syscall.o

obj/mips/raw_syscall.o: raw_syscall_mips.asm
	$(MIPS_AS) raw_syscall_mips.asm -o obj/mips/raw_syscall.o

obj/arm/raw_syscall.o: raw_syscall_arm.asm
	$(ARM_AS) raw_syscall_arm.asm -o obj/arm/raw_syscall.o

#
# setjmp.o
#

obj/x86/setjmp.o: setjmp_x86.asm
	$(X86_AS) setjmp_x86.asm -o obj/x86/setjmp.o

obj/x64/setjmp.o: setjmp_x64.asm
	$(X64_AS) setjmp_x64.asm -o obj/x64/setjmp.o

obj/mips/setjmp.o: setjmp_mips.asm
	$(MIPS_AS) setjmp_mips.asm -o obj/mips/setjmp.o

obj/arm/setjmp.o: setjmp_arm.asm
	$(ARM_AS) setjmp_arm.asm -o obj/arm/setjmp.o

#
# luxmemcpy
#

luxmemcpy: obj/mips/luxmemcpy.o obj/arm/luxmemcpy.o

obj/mips/luxmemcpy.o: mips_memcpy.asm
	$(MIPS_AS) mips_memcpy.asm -o obj/mips/luxmemcpy.o

obj/arm/luxmemcpy.o: arm_memcpy.asm
	$(ARM_AS) arm_memcpy.asm -o obj/arm/luxmemcpy.o

#
# vm_lib
#
vm_lib:
	make -C vm_lib

#
# libc .o files
#

obj/x86/%.o: %.c
	$(CC) $(CFLAGS) -mx86 -Z $*.c -o $*.asm && $(X86_AS) $*.asm -o obj/x86/$*.o && rm $*.asm

obj/x64/%.o: %.c
	$(CC) $(CFLAGS) -mx64 -Z $*.c -o $*.asm && $(X64_AS) $*.asm -o obj/x64/$*.o && rm $*.asm

obj/mips/%.o: %.c
	$(CC) $(CFLAGS) -mmips -Z $*.c -o $*.asm && $(MIPS_AS) $*.asm -o obj/mips/$*.o && rm $*.asm

obj/arm/%.o: %.c
	$(CC) $(CFLAGS) -marm -Z $*.c -o $*.asm && $(ARM_AS) $*.asm -o obj/arm/$*.o && rm $*.asm

clean:
	rm -f obj/x86/*.o obj/x86/*.a
	rm -f obj/x64/*.o obj/x64/*.a
	rm -f obj/mips/*.o obj/mips/*.a
	rm -f obj/arm/*.o obj/arm/*.a
	make -C vm_lib clean

.PHONY: all clean crt0 liblux libc luxmemcpy vm_lib
