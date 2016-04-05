CC=gcc
CFLAGS=-c -g -Wall
OBJS = luxdvr.o

all: luxdvr

luxdvr: luxdvr.o ../util/util.o ../util/str.o
	$(CC) -o luxdvr luxdvr.o ../util/util.o ../util/str.o

../util/util.o:
	make -C ../util util.o

../util/str.o:
	make -C ../util str.o

.c.o:
	$(CC) $(CFLAGS) $*.c

clean:
	rm -f $(OBJS) luxdvr

luxdvr.o: ../util/util.h ../util/str.h

.PHONY: all clean
