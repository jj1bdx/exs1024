# Makefile of test sequence generator for xorshift64star.c
# Written by Kenji Rikitake
# License: CC0 / public domain

CC = clang
CFLAGS = -O0 -g -Wall

OBJECTS = $(SOURCES:.c=.o)
SOURCES = xorshift1024star.c test.c test-jump.c
TESTOBJECTS = xorshift1024star.c test.c
TESTEXEC = test
TESTJUMPOBJECTS = xorshift1024star.c test-jump.c
TESTJUMPEXEC = test-jump

all: $(OBJECTS) $(TESTEXEC) $(TESTJUMPEXEC)

$(TESTEXEC): $(TESTOBJECTS)
	$(CC) $(LDFLAGS) $(TESTOBJECTS) -o $@

$(TESTJUMPEXEC): $(TESTJUMPOBJECTS)
	$(CC) $(LDFLAGS) $(TESTJUMPOBJECTS) -o $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *~
