CXX      ?= g++
CXXFLAGS ?= -std=c++11 -O2 -Wall -Wextra
PYTHON   ?= python3

all: gasm

gasm: gasm.cpp gasm.h
	$(CXX) $(CXXFLAGS) -o $@ gasm.cpp

# AddressSanitizer + UBSan build, used by "make test-asan"
gasm-asan: gasm.cpp gasm.h
	$(CXX) -std=c++11 -g -O1 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=all -o $@ gasm.cpp

test: gasm
	GASM=./gasm $(PYTHON) -m unittest discover -s tests -t . -v

test-asan: gasm-asan
	GASM=./gasm-asan $(PYTHON) -m unittest discover -s tests -t . -v

clean:
	rm -f gasm gasm-asan

.PHONY: all test test-asan clean
