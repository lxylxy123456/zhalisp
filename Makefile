CXXFLAGS=-Wall -g -std=c++11 -lgmp -lgmpxx

STRUCTS = $(wildcard structs/*.h)

STRUCTS_O = $(STRUCTS:.h=.o)

structs/%.o: structs/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^

translate.o: lex.l translate.y
	lex lex.l
	yacc translate.y
	$(CXX) $(CXXFLAGS) y.tab.c -g -c -o $@ \
		-Wno-unused-function -Wno-class-memaccess

tmp: tmp.cpp $(STRUCTS_O) translate.o evaluate.o
	$(CXX) $(CXXFLAGS) -o $@ $^

lint: *.cpp structs/*.cpp
	cpplint $^

clean:
	rm -f lex.yy.c y.tab.c a.out tmp structs/*.o *.o

