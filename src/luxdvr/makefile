CC=gcc
CFLAGS=-c -g -Wall
OBJS = luxdvr.o

all: luxdvr

luxdvr: luxdvr.o ../util.o ../str.o
	$(CC) -o luxdvr luxdvr.o ../util.o ../str.o
../util.o:
	make -C .. util.o
../str.o:
	make -C .. str.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f $(OBJS) luxdvr

luxdvr.o: ../util.h ../str.h

.PHONY: all clean
