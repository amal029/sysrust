CC=gcc-m
CXX=g++-m
CXXFLAGS=-O3 -std=c++26
CFLAGS=-c
CFILE=tt.c
CXXFILES=example_prog.cpp example_prog.h

all:	clean br bc
	$(CXX) $(CXXFLAGS) $(CXXFILES) *.o -o prog

br:
	cargo run -- -f example_prog.sysrs -g true

bc:
	$(CC) $(CFLAGS) $(CFILE)


run:
	./prog

clean:
	rm -rf prog $(CXXFILES) *.o
