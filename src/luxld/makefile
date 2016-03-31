CC=gcc
CFLAGS=-c -g -Wall -Wno-switch
OBJS=luxld.o x86.o x64.o arm.o mips.o out.o copy.o

all: luxld

luxld: $(OBJS)
	$(CC) -o luxld $(OBJS) ../util.o ../ELF/ELF_util.o

../util.o:
	make -C .. util.o

../ELF/ELF_util.o:
	make -C ../ELF ELF_util.o

.c.o:
	$(CC) $(CFLAGS) $*.c

clean:
	rm -f $(OBJS) luxld

luxld.o: luxld.h x86.h x64.h mips.h arm.h ../util.h ../ELF/ELF_util.h
x86.o: x86.h luxld.h copy.h ../util.h
x64.o: x64.h luxld.h copy.h ../util.h
mips.o: mips.h luxld.h copy.h ../util.h
arm.o: arm.h luxld.h copy.h ../util.h
out.o: out.h luxld.h ../util.h ../ELF/ELF_util.h
copy.o: copy.h luxld.h ../util.h

.PHONY: all clean
