CC=gcc
CFLAGS=-c -g -Wall -Wno-switch -Wno-sign-conversion

all: luxas

luxas: luxasx86.o ../util.o ../arena.o ../ELF/ELF_util.o
	$(CC) -o luxasx86 luxasx86.o ../util.o ../arena.o ../ELF/ELF_util.o
../util.o:
	make -C .. util.o
../arena.o:
	make -C .. arena.o
../ELF/ELF_util.o:
	make -C ../ELF ELF_util.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f *.o luxasx86

luxasx86.o: luxas.h ../ELF/ELF_util.h ../util.h ../arena.h

.PHONY: all clean
