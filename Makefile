CC=/opt/llvm/bin/clang
CXX=/opt/llvm/bin/clang++
CXXFLAGS=-Ofast -std=c++26 -fomit-frame-pointer -fno-exceptions -march=native --param max-inline-insns-single=100000 --param large-function-growth=1000000 --param large-stack-frame-growth=1000000 --param inline-unit-growth=1000000 -flto

CFLAGS=-c
CFILE=tt.c
CXXFILES=$(FNAME).cpp $(FNAME).h

ifdef	FNAME 

ifdef	BENCH
all:	bc
else
all:	be
endif

br:
	cargo build --release

bc:
	./target/release/sysrust -f $(FNAME).sysrs -b 1000000000
	$(CC) $(CFLAGS) $(CFILE)
	$(CXX) $(CXXFLAGS) $(CXXFILES) *.o

be:
	./target/release/sysrust -f $(FNAME).sysrs
	$(CC) $(CFLAGS) $(CFILE)
	$(CXX) $(CXXFLAGS) $(CXXFILES) *.o

run:
	./$(FNAME)

clean:
	rm -rf a.out $(FNAME) $(CXXFILES) *.o
else
PHONY:
	@echo "Provide file name FNAME=<>"
endif

