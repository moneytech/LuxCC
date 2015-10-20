CC=gcc
CFLAGS=-c -g -Wall -Wconversion -Wextra -Wno-switch -Wno-parentheses -Wno-missing-field-initializers \
-fwrapv -Wno-sign-conversion -Wno-unused-parameter -Wno-sign-compare -Wno-empty-body -Wno-conversion #-pg
PROG = luxcc
OBJS = luxcc.o pre.o lexer.o parser.o util.o decl.o expr.o stmt.o ic.o arena.o error.o loc.o bset.o str.o dflow.o opt.o
SRCS = luxcc.c pre.c lexer.c parser.c util.c decl.c expr.c stmt.c ic.c arena.c error.c loc.c bset.c str.c dflow.c opt.c
LFLAGS = -lm

all: $(PROG)

$(PROG): $(OBJS) vm_cgen.o x86_cgen.o
	$(CC) -o $(PROG) $(OBJS) vm_cgen.o x86_cgen.o $(LFLAGS) #-pg
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f *.o $(PROG)
depend:
	makedepend -- $(CFLAGS) -- $(SRCS) -Y
# DO NOT DELETE

luxcc.o: parser.h lexer.h pre.h ic.h bset.h vm_cgen/vm_cgen.h x86_cgen/x86_cgen.h
pre.o: pre.h util.h imp_lim.h error.h
lexer.o: lexer.h pre.h util.h error.h
parser.o: parser.h lexer.h pre.h util.h decl.h expr.h stmt.h error.h
util.o: util.h
decl.o: decl.h parser.h lexer.h pre.h util.h expr.h stmt.h arena.h imp_lim.h error.h
expr.o: expr.h parser.h lexer.h pre.h util.h decl.h error.h
stmt.o: stmt.h parser.h lexer.h pre.h util.h decl.h expr.h error.h
ic.o: ic.h parser.h lexer.h pre.h bset.h util.h decl.h expr.h arena.h imp_lim.h loc.h dflow.h
arena.o: arena.h util.h
error.o: error.h
loc.o: loc.h util.h imp_lim.h arena.h
bset.o: bset.h
str.o: str.h
dflow.o: dflow.h bset.h util.h ic.h parser.h lexer.h pre.h expr.h
opt.o: opt.h bset.h util.h ic.h expr.h
vm_cgen.o: vm_cgen/vm_cgen.h vm_cgen/vm_cgen.c decl.h parser.h lexer.h pre.h util.h expr.h stmt.h arena.h imp_lim.h error.h loc.h
	$(CC) $(CFLAGS) vm_cgen/vm_cgen.c
x86_cgen.o: x86_cgen/x86_cgen.c x86_cgen/x86_cgen.h decl.h parser.h lexer.h pre.h util.h expr.h ic.h arena.h imp_lim.h \
error.h bset.h str.h dflow.h
	$(CC) $(CFLAGS) x86_cgen/x86_cgen.c

.PHONY: all clean depend
