ifndef CC
CC=gcc-m
endif
ifndef CXX
CXX=g++-m
endif
CXXFLAGS=-Ofast -std=c++26 -fno-exceptions -march=native
CFLAGS=-c
CFILE=tt.c
CXXFILES=$(FNAME).cpp $(FNAME).h

ifdef	FNAME 
all:	bc

br:
	cargo build --release

bc:
	./target/release/sysrust -f $(FNAME).sysrs -b 100000000
	$(CC) $(CFLAGS) $(CFILE)
	$(CXX) $(CXXFLAGS) $(CXXFILES) *.o -o $(FNAME)


run:
	./$(FNAME)

clean:
	rm -rf a.out $(FNAME) $(CXXFILES) *.o
else
PHONY:
	@echo "Provide file name FNAME=<>"
endif

