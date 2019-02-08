
CXXFLAGS=-Wall -g -std=c++11

SOURCES = $(wildcard *.cpp)

TARGETS = $(SOURCES:.cpp=)

all:

ALL: $(TARGETS)

$(TARGETS): %: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

lint: $(SOURCES)
	cpplint $^

yacc: lex.l translate.y
	lex lex.l
	yacc translate.y
	$(CXX) -lgmp -lgmpxx y.tab.c -g

clean:
	rm -f $(TARGETS) a.out

