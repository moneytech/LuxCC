CC=gcc
CFLAGS=-c -g -pg -Wall -Wconversion -Wextra -Wno-switch -Wno-parentheses -Wno-missing-field-initializers \
-fwrapv -Wno-sign-conversion -Wno-conversion
OBJS = luxas.o ELF_util.o
SRCS = luxas.c ELF_util.c

all: luxas

luxas: $(OBJS) ../util.o ../arena.o
	$(CC) -o luxas $(OBJS) ../util.o ../arena.o
.c.o:
	$(CC) $(CFLAGS) $*.c
../util.o:
	make -C .. util.o
../arena.o:
	make -C .. arena.o
clean:
	rm -f $(OBJS) luxas
depend:
	makedepend -- $(CFLAGS) -- $(SRCS) -Y
# DO NOT DELETE

luxas.o: luxas.h ELF_util.h ../util.h ../arena.h

.PHONY: all clean depend
