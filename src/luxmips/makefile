CC=gcc
CFLAGS=-c -g -Wall -Wno-switch -Wno-sign-conversion

all: luxasmips

luxasmips: luxasmips.o ../util/util.o ../util/ELF_util.o
	$(CC) -o luxasmips luxasmips.o ../util/util.o ../util/ELF_util.o

../util/util.o:
	make -C ../util util.o

../util/ELF_util.o:
	make -C ../util ELF_util.o

.c.o:
	$(CC) $(CFLAGS) $*.c

clean:
	rm -f *.o luxasmips

luxasmips.o: ../util/util.h ../util/ELF_util.h

.PHONY: all clean
