CC=gcc-m
CXX=g++-m
CXXFLAGS=-O3 -std=c++23
CFLAGS=-c
CFILE=tt.c
CXXFILES=$(FNAME).cpp $(FNAME).h

ifdef	FNAME 
all:	br bc

br:
	cargo run -- -f $(FNAME).sysrs -g true

bc:
	$(CC) $(CFLAGS) $(CFILE)
	$(CXX) $(CXXFLAGS) $(CXXFILES) *.o -o $(FNAME)


run:
	./$(FNAME)

clean:
	rm -rf prog $(CXXFILES) *.o
else
PHONY:
	@echo "Provide file name FNAME=<>"
endif

