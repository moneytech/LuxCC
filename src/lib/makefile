GETARCH = $(shell uname -i)
CC=../luxcc
AS=../luxas/luxas
VMAS=../luxvm/luxasvm
MIPSAS=../luxmips/luxasmips
ARMAS=../luxarm/luxasarm
LIBC_FILES=libc.c ctype.c errno.c stdio.c stdlib.c string.c
ifeq ($(GETARCH), i386)
	CRT = crt32
else
	CRT = crt64
endif

all: $(CRT).o libc.o liblux.o liblux_mips.o mips_memcpy.o liblux_arm.o arm_memcpy.o

#
# LuxVM stuff.
#

$(CRT).o: $(CRT).s
    ifeq ($(GETARCH), i386)
	$(VMAS) -vm32 crt32.s -o crt32.o
    else
	$(VMAS) -vm64 crt64.s -o crt64.o
    endif

libc.o: $(LIBC_FILES)
    ifeq ($(GETARCH), i386)
	$(CC) -q -mvm32 libc.c -o libc.s && $(VMAS) -vm32 libc.s -o libc.o
    else
	$(CC) -q -mvm64 libc.c -o libc.s && $(VMAS) -vm64 libc.s -o libc.o
    endif

liblux.o: liblux.c
    ifeq ($(GETARCH), i386)
	$(CC) -q -z -mx86 liblux.c -o liblux.asm && $(AS) liblux.asm -o liblux.o
    else
	@true
    endif

#
# MIPS stuff.
#

#crt_mips.o: crt_mips.asm
#	$(MIPSAS) crt_mips.asm -o crt_mips.o

mips_memcpy.o: mips_memcpy.asm
	$(MIPSAS) mips_memcpy.asm -o mips_memcpy.o

liblux_mips.o: liblux.c
	$(CC) -q -z -mmips liblux.c -o liblux_mips.asm && $(MIPSAS) liblux_mips.asm -o liblux_mips.o

#libc_mips.o: $(LIBC_FILES)
#	$(CC) -q -mmips libc.c -o libc_mips.asm && $(MIPSAS) libc_mips.asm -o libc_mips.o

#
# ARM stuff.
#

arm_memcpy.o: arm_memcpy.asm
	$(ARMAS) arm_memcpy.asm -o arm_memcpy.o

liblux_arm.o: liblux.c
	$(CC) -q -z -marm liblux.c -o liblux_arm.asm && $(ARMAS) liblux_arm.asm -o liblux_arm.o

clean:
	rm -f *.o

.PHONY: all clean
