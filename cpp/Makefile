TR_FLAG = -DTAIL_RECU
LS_FLAG = -DLIMIT_STACK='0x800000 - 0x10000'
# O_FLAG = -O3
EXP_POS_FLAG = -DREMOVE_EXP_POS

CUSTOM_FLAGS = $(TR_FLAG) $(LS_FLAG) $(O_FLAG) $(EXP_POS_FLAG)

CXXFLAGS=-Wall -g -std=c++11 -lgmp -lgmpxx $(CUSTOM_FLAGS)

STRUCTS = $(wildcard structs/*.h)

STRUCTS_O = $(STRUCTS:.h=.o)

LIBRARY = $(STRUCTS_O) translate.o evaluate.o

all: backend test

translate.o: lex.l translate.y
	lex lex.l
	yacc translate.y
	$(CXX) $(CXXFLAGS) y.tab.c -g -c -o $@ -Wno-unused-function

backend: backend.cpp $(LIBRARY)
	$(CXX) $(CXXFLAGS) -o $@ $^

test: test.cpp $(LIBRARY)
	$(CXX) $(CXXFLAGS) -o $@ $^

lint: *.h *.cpp structs/*.h structs/*.cpp
	cpplint $^

clean:
	rm -f lex.yy.c y.tab.c a.out backend structs/*.o *.o vgcore.*

