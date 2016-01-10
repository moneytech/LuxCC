CC=gcc
CFLAGS=-c -g -Wall -Wno-switch -Wno-sign-conversion

all: luxmipsemu luxasmips luxldmips

luxmipsemu: luxmipsemu.o
	$(CC) -m32 -o luxmipsemu luxmipsemu.o
luxasmips: luxasmips.o ../util.o
	$(CC) -o luxasmips luxasmips.o ../util.o ../luxas/ELF_util.o
luxldmips: luxldmips.o ../util.o
	$(CC) -o luxldmips luxldmips.o ../util.o ../luxas/ELF_util.o
../util.o:
	make -C .. util.o
../luxas/ELF_util.o:
	make -C ../luxas ELF_util.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f *.o luxmipsemu luxasmips
luxmipsemu.o: luxmipsemu.c
	$(CC) -m32 $(CFLAGS) luxmipsemu.c
luxasmips.o: ../util.h ../luxas/ELF_util.h
luxldmips.o: ../util.h ../luxas/ELF_util.h

.PHONY: all clean
