CC=gcc
CFLAGS=-c -g -Wall -Wconversion -Wextra -Wno-switch -Wno-sign-conversion -Wno-enum-compare -Wno-parentheses
PROG = main
OBJS = main.o pre.o lexer.o parser.o util.o decl.o stmt_expr.o
SRCS = main.c pre.c lexer.c parser.c util.c decl.c stmt_expr.c
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
parser.o: parser.h lexer.h pre.h util.h decl.h stmt_expr.h
util.o: util.h
decl.o: decl.h parser.h lexer.h pre.h util.h
stmt_expr.o: stmt_expr.h parser.h lexer.h pre.h util.h decl.h
