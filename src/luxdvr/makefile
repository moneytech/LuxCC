CC=gcc
CFLAGS=-c -g -pg -Wall -Wconversion -Wextra -Wno-switch -Wno-parentheses -Wno-missing-field-initializers \
-fwrapv -Wno-sign-conversion -Wno-conversion
OBJS = luxdvr.o
SRCS = luxdvr.c

all: luxdvr

luxdvr: luxdvr.o ../util.o ../str.o
	$(CC) -o luxdvr luxdvr.o ../util.o ../str.o
.c.o:
	$(CC) $(CFLAGS) $*.c
../util.o:
	make -C .. util.o
../str.o:
	make -C .. str.o
clean:
	rm -f $(OBJS) luxdvr
depend:
	makedepend -- $(CFLAGS) -- $(SRCS) -Y
# DO NOT DELETE

luxdvr.o: ../util.h ../str.h

.PHONY: all clean depend
