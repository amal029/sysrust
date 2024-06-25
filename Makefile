CC=gcc-m
CXX=g++-m
CXXFLAGS=-O3 -std=c++23 -fno-exceptions -march=native
CFLAGS=-c
CFILE=tt.c
CXXFILES=$(FNAME).cpp $(FNAME).h

ifdef	FNAME 
all:	bc

br:
	cargo build --release

bc:
	./target/release/sysrust -f $(FNAME).sysrs
	$(CC) $(CFLAGS) $(CFILE)
	$(CXX) $(CXXFLAGS) $(CXXFILES) *.o -o $(FNAME)


run:
	./$(FNAME)

clean:
	rm -rf $(FNAME) $(CXXFILES) *.o
else
PHONY:
	@echo "Provide file name FNAME=<>"
endif

