CXXFLAGS=-Wall -g -std=c++11 -lgmp -lgmpxx

STRUCTS = $(wildcard structs/*.h)

STRUCTS_O = $(STRUCTS:.h=.o)

LIBRARY = $(STRUCTS_O) translate.o evaluate.o

all: tmp test

translate.o: lex.l translate.y
	lex lex.l
	yacc translate.y
	$(CXX) $(CXXFLAGS) y.tab.c -g -c -o $@ \
		-Wno-unused-function #-Wno-class-memaccess

tmp: tmp.cpp $(LIBRARY)
	$(CXX) $(CXXFLAGS) -o $@ $^

test: test.cpp $(LIBRARY)
	$(CXX) $(CXXFLAGS) -o $@ $^

lint: *.h *.cpp structs/*.h structs/*.cpp
	cpplint $^

clean:
	rm -f lex.yy.c y.tab.c a.out tmp structs/*.o *.o

