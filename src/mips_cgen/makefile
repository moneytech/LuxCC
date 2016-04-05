CC=gcc
CFLAGS=-c -g -fwrapv -Wall -Wconversion -Wno-switch -Wno-parentheses -Wno-sign-conversion

all: mips_cgen.c mips_cgen.h ../decl.h ../parser.h ../lexer.h ../pre.h ../expr.h ../ic.h \
../imp_lim.h ../error.h ../dflow.h ../util/util.h ../util/arena.h ../util/bset.h ../util/str.h
	$(CC) $(CFLAGS) mips_cgen.c

clean:
	rm -f *.o

.PHONY: all clean
