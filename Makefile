LLVM_CONFIG := $(shell which llvm-config-13)
LLVM_LIB := $(shell $(LLVM_CONFIG) --cppflags --ldflags --libs) -DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING
CC = g++trunk
OPT = -I . $(LLVM_LIB) --std=c++2b -static-libstdc++ -g -L /usr/local/lib/ -L /usr/local/lib64/
LIBS = -lfmt -lstdc++_libbacktrace -pthread

CPP_FILES := $(shell find ./src/ -name "*.cpp" -type f)
CPP_FILES := $(filter-out ./src/main.cpp, $(CPP_FILES))
OBJ_FILES := $(patsubst ./src/%.cpp, ./obj/%.o, $(CPP_FILES))
OBJ_FILES := ./obj/lexical/q_scanner.o $(OBJ_FILES)
OBJ_DIRS := $(shell find ./src/ -type d)
OBJ_DIRS := $(patsubst ./src/%, ./obj/%, $(OBJ_DIRS))

EXAMPLES := $(shell find ./examples/ -name "*.q" -type f)
EXAMPLES := $(patsubst %.q, %.o, $(EXAMPLES))
all: $(OBJ_DIRS) ./qcc

debug: 
	$(info $$var is [${OBJ_FILES}])

./qcc: $(OBJ_FILES) ./obj/main.o
	$(CC) $(OPT) -o $@ $^ $(LIBS)

run_tests: all $(EXAMPLES)

./examples/%.o: ./examples/%.q
	- ./qcc $< 2> $<.out

./obj/main.o: ./src/main.cpp
	$(CC) $(OPT) -c -o $@ $<

./obj/%.o: ./src/%.cpp ./src/%.hpp
	$(CC) $(OPT) -c $< -o $@

./obj/%:
	mkdir $@

./src/lexical/q_scanner.cpp ./src/lexical/q_scanner.hpp: ./src/lexical/q_scanner.ll
	flex -o $@ $^
	touch ./src/lexical/q_scanner.hpp

clean:
	- find ./obj/ -type f -name "*.o" -exec rm -f {} \;
	- find ./examples/ -type f -name "*.o" -exec rm -f {} \;
	- find ./examples/ -type f -name "*.q.out" -exec rm -f {} \;
	- rm -rf qcc ./src/lexical/q_scanner.cpp ./src/lexical/q_scanner.hpp
