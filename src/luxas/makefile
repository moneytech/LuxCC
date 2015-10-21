CC=gcc
CFLAGS=-c -g -Wall -Wno-switch
OBJS = luxas.o ELF_util.o

all: luxas

luxas: $(OBJS) ../util.o ../arena.o
	$(CC) -o luxas $(OBJS) ../util.o ../arena.o
../util.o:
	make -C .. util.o
../arena.o:
	make -C .. arena.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f $(OBJS) luxas

luxas.o: luxas.h ELF_util.h ../util.h ../arena.h

.PHONY: all clean
