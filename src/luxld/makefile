CC=gcc
CFLAGS=-c -g -Wall -Wno-switch
OBJS=luxld.o x86.o x64.o arm.o mips.o out.o copy.o

all: luxld

luxld: $(OBJS)
	$(CC) -o luxld $(OBJS) ../util/util.o ../util/ELF_util.o

../util/util.o:
	make -C ../util util.o

../util/ELF_util.o:
	make -C ../util ELF_util.o

.c.o:
	$(CC) $(CFLAGS) $*.c

clean:
	rm -f $(OBJS) luxld

luxld.o: luxld.h x86.h x64.h mips.h arm.h ../util/util.h ../util/ELF_util.h
x86.o: x86.h luxld.h copy.h ../util/util.h
x64.o: x64.h luxld.h copy.h ../util/util.h
mips.o: mips.h luxld.h copy.h ../util/util.h
arm.o: arm.h luxld.h copy.h ../util/util.h
out.o: out.h luxld.h ../util/util.h ../util/ELF_util.h
copy.o: copy.h luxld.h ../util/util.h

.PHONY: all clean
