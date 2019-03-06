TR_FLAG=-DTAIL_RECU
LS_FLAG=-DLIMIT_STACK='0x800000 - 0x10000'
# O_FLAG=-O3
EXP_POS_FLAG=-DREMOVE_EXP_POS
#PTR_FLAG=-DCUSTOM_PTR

CUSTOM_FLAGS = $(TR_FLAG) $(LS_FLAG) $(O_FLAG) $(EXP_POS_FLAG) $(PTR_FLAG)
LINKFLAGS=-lgmp -lgmpxx
CXXFLAGS=-Wall -g -std=c++11 $(CUSTOM_FLAGS)

STRUCTS = $(wildcard structs/*.h)

STRUCTS_O = $(STRUCTS:.h=.o)

LIBRARY = $(STRUCTS_O) translate.o evaluate.o test.o

all: backend

translate.o: lex.l translate.y
	lex lex.l
	yacc translate.y
	$(CXX) $(CXXFLAGS) y.tab.c -g -c -o $@ -Wno-unused-function

backend: backend.cpp $(LIBRARY)
	$(CXX) $(CXXFLAGS) $(LINKFLAGS) -o $@ $^

tmp: tmp.cpp $(LIBRARY)
	$(CXX) $(CXXFLAGS) $(LINKFLAGS) -o $@ $^

lint: *.h *.cpp structs/*.h structs/*.cpp
	cpplint $^

clean:
	rm -f lex.yy.c y.tab.c a.out backend test structs/*.o *.o vgcore.*

