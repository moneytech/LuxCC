CC=gcc
CFLAGS=-c -g -Wall
OBJS = luxld.o

all: luxld

luxld: luxld.o ../util.o ../arena.o ../luxas/ELF_util.o
	$(CC) -o luxld luxld.o ../util.o ../arena.o ../luxas/ELF_util.o
../util.o:
	make -C .. util.o
../arena.o:
	make -C .. arena.o
../luxas/ELF_util.o:
	make -C ../luxas ELF_util.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f $(OBJS) luxld

luxld.o: luxld.h ../util.h ../arena.h ../luxas/ELF_util.h

.PHONY: all clean
