CC=gcc
CFLAGS=-c -g -Wall -Wno-switch -Wno-sign-conversion

all: luxasarm

luxasarm: luxasarm.o ../util.o
	$(CC) -o luxasarm luxasarm.o ../util.o ../ELF/ELF_util.o
../util.o:
	make -C .. util.o
../luxas/ELF_util.o:
	make -C ../ELF ELF_util.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f *.o luxasarm
luxasarm.o: ../util.h ../ELF/ELF_util.h

.PHONY: all clean
