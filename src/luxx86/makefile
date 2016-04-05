CC=gcc
CFLAGS=-c -g -Wall -Wno-switch -Wno-sign-conversion

all: luxasx86

luxasx86: luxasx86.o ../util/util.o ../util/arena.o ../util/ELF_util.o
	$(CC) -o luxasx86 luxasx86.o ../util/util.o ../util/arena.o ../util/ELF_util.o

../util/util.o:
	make -C ../util util.o

../util/arena.o:
	make -C ../util arena.o

../util/ELF_util.o:
	make -C ../util ELF_util.o

.c.o:
	$(CC) $(CFLAGS) $*.c

clean:
	rm -f *.o luxasx86

luxasx86.o: ../util/ELF_util.h ../util/util.h ../util/arena.h

.PHONY: all clean
