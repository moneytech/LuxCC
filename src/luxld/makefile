CC=gcc
CFLAGS=-c -g -pg -Wall -Wconversion -Wextra -Wno-switch -Wno-parentheses -Wno-missing-field-initializers \
-fwrapv -Wno-sign-conversion -Wno-conversion
OBJS = luxld.o
SRCS = luxld.c

all: luxld

luxld: luxld.o ../util.o ../arena.o ../luxas/ELF_util.o
	$(CC) -o luxld luxld.o ../util.o ../arena.o ../luxas/ELF_util.o
.c.o:
	$(CC) $(CFLAGS) $*.c
../util.o:
	make -C .. util.o
../arena.o:
	make -C .. arena.o
../luxas/ELF_util.o:
	make -C ../luxas ELF_util.o
clean:
	rm -f $(OBJS) luxld
depend:
	makedepend -- $(CFLAGS) -- $(SRCS) -Y
# DO NOT DELETE

luxld.o: luxld.h ../util.h ../arena.h ../luxas/ELF_util.h

.PHONY: all clean depend
