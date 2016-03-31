CC=gcc
CFLAGS=-c -g -Wall -Wno-switch -Wno-sign-conversion

all: ELF_util.o

ELF_util.o: ELF_util.c ELF_util.h

.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f *.o

.PHONY: all clean
