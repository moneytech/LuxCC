GETARCH = $(shell uname -i)
CC=gcc
CFLAGS=-c -g -Wall -Wextra -Wconversion
ifeq ($(GETARCH),i386)
	VMOBJ = vm32.o
else
	VMOBJ = vm64.o
endif

all: luxvm luxvmas luxvmld

luxvm: $(VMOBJ) ../util.o operations.o
	$(CC) -o luxvm $(VMOBJ) ../util.o operations.o
luxvmas: as.o ../util.o operations.o
	$(CC) -o luxvmas as.o ../util.o operations.o
luxvmld: ld.o ../arena.o ../util.o
	$(CC) -o luxvmld ld.o ../arena.o ../util.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f *.o luxvm luxvmas luxvmld

$(VMOBJ): vm.h as.h operations.h
as.o: as.h vm.h ../util.h operations.h
ld.o: as.h ../arena.h ../util.h
operations.o: operations.h ../util.h vm.h
../arena.o: ../arena.h
	make -C .. arena.o
../util.o: ../util.h
	make -C .. util.o

.PHONY: all clean
