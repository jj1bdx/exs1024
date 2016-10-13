# Makefile of test sequence generator for xorshift64star.c
# Written by Kenji Rikitake
# License: CC0 / public domain

CC = clang
CFLAGS = -O0 -g -Wall

OBJECTS = $(SOURCES:.c=.o)
SOURCES = xorshift1024star.c test.c test-jump.c test-jump-gen.c
TESTOBJECTS = xorshift1024star.c test.c
TESTEXEC = test
TESTJUMPOBJECTS = xorshift1024star.c test-jump.c
TESTJUMPEXEC = test-jump
TESTJUMPGENOBJECTS = xorshift1024star.c test-jump-gen.c
TESTJUMPGENEXEC = test-jump-gen

all: $(OBJECTS) $(TESTEXEC) $(TESTJUMPEXEC) $(TESTJUMPGENEXEC)

$(TESTEXEC): $(TESTOBJECTS)
	$(CC) $(LDFLAGS) $(TESTOBJECTS) -o $@

$(TESTJUMPEXEC): $(TESTJUMPOBJECTS)
	$(CC) $(LDFLAGS) $(TESTJUMPOBJECTS) -o $@

$(TESTJUMPGENEXEC): $(TESTJUMPGENOBJECTS)
	$(CC) $(LDFLAGS) $(TESTJUMPGENOBJECTS) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *~
