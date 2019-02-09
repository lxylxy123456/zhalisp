
CXXFLAGS=-Wall -g -std=c++11 -lgmp -lgmpxx \
		-Wno-unused-function -Wno-class-memaccess

STRUCTS = $(wildcard structs/*.h)

STRUCTS_O = $(STRUCTS:.h=.o)

structs/%.o: structs/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^

tmp: tmp.cpp $(STRUCTS_O) translate.o
	$(CXX) $(CXXFLAGS) -o $@ $^

structs.o: structs.cpp
	echo structs/
	$(CXX) $(CXXFLAGS) -c -o $@ $^

lint: $(SOURCES)
	cpplint $^

translate.o: lex.l translate.y
	lex lex.l
	yacc translate.y
	$(CXX) $(CXXFLAGS) y.tab.c -g -c -o $@

clean:
	rm -f lex.yy.c y.tab.c a.out tmp structs/*.o *.o

