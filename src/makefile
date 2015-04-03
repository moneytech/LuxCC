CC=gcc
CFLAGS=-c -g -pg -Wall -Wconversion -Wextra -Wno-switch -Wno-parentheses -Wno-missing-field-initializers \
-fwrapv -Wno-sign-conversion #-Wno-enum-compare
PROG = main
OBJS = main.o pre.o lexer.o parser.o util.o decl.o expr.o stmt.o ic.o arena.o
SRCS = main.c pre.c lexer.c parser.c util.c decl.c expr.c stmt.c ic.c arena.c
LFLAGS = -lm

all: main

main: $(OBJS) vm_cgen.o
	$(CC) -pg -o $(PROG) $(OBJS) vm_cgen.o $(LFLAGS)
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f $(OBJS) $(PROG)
depend:
	makedepend -- $(CFLAGS) -- $(SRCS) -Y
# DO NOT DELETE

main.o: parser.h lexer.h pre.h ic.h vm_cgen/vm_cgen.h parser.h
pre.o: pre.h util.h
lexer.o: lexer.h pre.h util.h
parser.o: parser.h lexer.h pre.h util.h decl.h expr.h stmt.h
util.o: util.h
decl.o: decl.h parser.h lexer.h pre.h util.h expr.h stmt.h arena.h
expr.o: expr.h parser.h lexer.h pre.h util.h decl.h
stmt.o: stmt.h parser.h lexer.h pre.h util.h decl.h expr.h
ic.o: ic.h parser.h lexer.h pre.h util.h decl.h expr.h
arena.o: arena.h
vm_cgen.o: vm_cgen/vm_cgen.c vm_cgen/vm_cgen.h as_lk_vm/vm.h util.h decl.h parser.h lexer.h pre.h expr.h
	$(CC) $(CFLAGS) vm_cgen/vm_cgen.c
