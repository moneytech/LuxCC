CC=gcc
CFLAGS=-c -g -Wall -Wconversion -Wextra -Wno-switch -Wno-sign-conversion -Wno-enum-compare -Wno-parentheses
PROG = main
OBJS = main.o pre.o lexer.o parser.o util.o decl.o expr.o stmt.o
SRCS = main.c pre.c lexer.c parser.c util.c decl.c expr.c stmt.c
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
lexer.o: lexer.h pre.h util.h
parser.o: parser.h lexer.h pre.h util.h decl.h expr.h stmt.h
util.o: util.h
decl.o: decl.h parser.h lexer.h pre.h util.h expr.h
expr.o: expr.h parser.h lexer.h pre.h util.h decl.h
stmt.o: stmt.h parser.h lexer.h pre.h util.h decl.h
