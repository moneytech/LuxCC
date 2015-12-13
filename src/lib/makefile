GETARCH = $(shell uname -i)
CC=../luxcc
AS=../luxas/luxas
VMAS=../luxvm/luxvmas
LIBC_FILES=libc.c ctype.c errno.c misc.c stdio.c stdlib.c string.c
ifeq ($(GETARCH), i386)
	CRT = crt32
else
	CRT = crt64
endif

all: $(CRT).o libc.o liblux.o

$(CRT).o:
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

clean:
	rm -f *.o

.PHONY: all clean
