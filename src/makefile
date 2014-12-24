CC=gcc
CFLAGS=-c -g -Wall -Wconversion -Wextra -Wno-switch -Wno-sign-conversion -Wno-enum-compare
PROG = main
OBJS = main.o pre.o lexer.o parser.o util.o sema.o
SRCS = main.c pre.c lexer.c parser.c util.c sema.c
LFLAGS = -lm

all: main

main: $(OBJS)
	$(CC) -o $(PROG) $(OBJS) $(LFLAGS)
vm: vm.c vm.h
	$(CC) -g -Wall -Wconversion -Wextra -Wno-switch -o vm vm.c
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f $(OBJS) $(PROG)
depend:
	makedepend -- $(CFLAGS) -- $(SRCS) -Y
# DO NOT DELETE

main.o: parser.h lexer.h pre.h
pre.o: pre.h util.h
lexer.o: lexer.h pre.h
parser.o: parser.h lexer.h pre.h
util.o: util.h
sema.o: sema.h
