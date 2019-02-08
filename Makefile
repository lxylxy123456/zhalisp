
CXXFLAGS=-Wall -g -std=c++11 -pthread

SOURCES = $(wildcard *.cpp)

TARGETS = $(SOURCES:.cpp=)

all:

ALL: $(TARGETS)

$(TARGETS): %: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

cpplint: $(SOURCES)
	cpplint $(SOURCES)

clean:
	rm -f $(TARGETS) a.out

