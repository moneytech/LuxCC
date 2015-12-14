CC=gcc
CFLAGS=-c -g -Wall
OBJS = tester.o regex.o

all: tester

tester: tester.o regex.o
	$(CC) -o tester tester.o regex.o
.c.o:
	$(CC) $(CFLAGS) $*.c
clean:
	rm -f $(OBJS) tester

tester.o: regex.h
regex.o: regex.h

.PHONY: all clean
