CC=gcc
CFLAGS=-c -g -fwrapv -Wall -Wconversion -Wno-switch -Wno-parentheses -Wno-sign-conversion
PROG = luxcc
OBJS = luxcc.o pre.o lexer.o parser.o util.o decl.o expr.o stmt.o ic.o arena.o error.o loc.o bset.o str.o dflow.o opt.o ast2c.o
SRCS = luxcc.c pre.c lexer.c parser.c util.c decl.c expr.c stmt.c ic.c arena.c error.c loc.c bset.c str.c dflow.c opt.c ast2c.c

all: $(PROG)

$(PROG): $(OBJS) vm32_cgen.o vm64_cgen.o x86_cgen.o x64_cgen.o mips_cgen.o
	$(CC) -o $(PROG) $(OBJS) vm32_cgen.o vm64_cgen.o x86_cgen.o x64_cgen.o mips_cgen.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f *.o $(PROG)
depend:
	makedepend -- $(CFLAGS) -- $(SRCS) -Y
# DO NOT DELETE

luxcc.o: parser.h lexer.h pre.h ic.h bset.h vm32_cgen/vm32_cgen.h vm64_cgen/vm64_cgen.h x86_cgen/x86_cgen.h x64_cgen/x64_cgen.h mips_cgen/mips_cgen.h
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
ast2c.o: ast2c.h str.h
vm32_cgen.o: vm32_cgen/vm32_cgen.h vm32_cgen/vm32_cgen.c decl.h parser.h lexer.h pre.h util.h expr.h stmt.h arena.h imp_lim.h error.h loc.h
	$(CC) $(CFLAGS) vm32_cgen/vm32_cgen.c
vm64_cgen.o: vm64_cgen/vm64_cgen.h vm64_cgen/vm64_cgen.c decl.h parser.h lexer.h pre.h util.h expr.h stmt.h arena.h imp_lim.h error.h loc.h
	$(CC) $(CFLAGS) vm64_cgen/vm64_cgen.c
x86_cgen.o: x86_cgen/x86_cgen.c x86_cgen/x86_cgen.h decl.h parser.h lexer.h pre.h util.h expr.h ic.h arena.h imp_lim.h \
error.h bset.h str.h dflow.h
	$(CC) $(CFLAGS) x86_cgen/x86_cgen.c
x64_cgen.o: x64_cgen/x64_cgen.c x64_cgen/x64_cgen.h decl.h parser.h lexer.h pre.h util.h expr.h ic.h arena.h imp_lim.h \
error.h bset.h str.h dflow.h
	$(CC) $(CFLAGS) x64_cgen/x64_cgen.c
mips_cgen.o: mips_cgen/mips_cgen.c mips_cgen/mips_cgen.h decl.h parser.h lexer.h pre.h util.h expr.h ic.h arena.h imp_lim.h \
error.h bset.h str.h dflow.h
	$(CC) $(CFLAGS) mips_cgen/mips_cgen.c

.PHONY: all clean depend
