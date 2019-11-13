ifeq ($(ALL),1)
TR := 1
LS := 1
EP := 1
PTR := 1
endif
ifeq ($(TR),1)
CUSTOM_FLAGS += -DTAIL_RECU
endif
ifeq ($(LS),1)
CUSTOM_FLAGS += -DLIMIT_STACK='0x800000 - 0x10000'
endif
ifeq ($(O),1)
CUSTOM_FLAGS += -O1
endif
ifeq ($(O),2)
CUSTOM_FLAGS += -O2
endif
ifeq ($(O),3)
CUSTOM_FLAGS += -O3
endif
ifeq ($(EP),1)
CUSTOM_FLAGS += -DREMOVE_EXP_POS
endif
ifeq ($(PTR),1)
CUSTOM_FLAGS += -DCUSTOM_PTR
endif

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
	rm -f lex.yy.c y.tab.c a.out backend tmp structs/*.o *.o vgcore.*

