CC=gcc
CFLAGS=-c -g -Wall -Wno-switch -Wno-sign-conversion

all: luxasarm

luxasarm: luxasarm.o ../util/util.o ../util/ELF_util.o
	$(CC) -o luxasarm luxasarm.o ../util/util.o ../util/ELF_util.o

../util/util.o:
	make -C ../util util.o

../util/ELF_util.o:
	make -C ../util ELF_util.o

.c.o:
	$(CC) $(CFLAGS) $*.c

clean:
	rm -f *.o luxasarm

luxasarm.o: ../util/util.h ../util/ELF_util.h

.PHONY: all clean
