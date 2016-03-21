CC=gcc
CFLAGS=-c -g -Wall
OBJS = luxld32.o luxld64.o

all: luxld32 luxld64

luxld32: luxld32.o ../util.o ../arena.o ../ELF/ELF_util.o
	$(CC) -o luxld32 luxld32.o ../util.o ../arena.o ../ELF/ELF_util.o
luxld64: luxld64.o  ../util.o ../arena.o ../ELF/ELF_util.o
	$(CC) -o luxld64 luxld64.o ../util.o ../arena.o ../ELF/ELF_util.o
../util.o:
	make -C .. util.o
../arena.o:
	make -C .. arena.o
../ELF/ELF_util.o:
	make -C ../ELF ELF_util.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f $(OBJS) luxld32 luxld64

luxld.o: luxld.h ../util.h ../arena.h ../ELF/ELF_util.h

.PHONY: all clean
