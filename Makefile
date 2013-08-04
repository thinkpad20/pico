CPP=clang++
LIBS=bin/ast.o bin/compile.o bin/vm.o # bin/symbol.o bin/reduce.o
PARSE=bin/lex.o bin/parse.o

all: pico

run: pico
	./pico

pico: bin/lex.o bin/parse.o bin/engine.o bin/ast.o bin/compile.o bin/vm.o
	$(CPP) -o pico bin/engine.o $(PARSE) $(LIBS)

pico-simple: bin/lex.o bin/parse.o bin/engine.o bin/simple-ast.o
	$(CPP) -o pico bin/engine.o $(PARSE) bin/simple-ast.o

# generate C source from lex and yac files
src/lex.yy.cc: src/pico.l
	flex src/pico.l
	@mv *.cc src

src/pico.tab.c: src/lex.yy.cc src/pico.y include/location.hh include/position.hh
	bison -v --report=all --debug src/pico.y
	@mv *.c src
	@mv *.h src
	@mv *.hh src
	@mkdir -p aux
	@mv *.output aux

# object files
bin/engine.o: include/picoScanner.h bin/ast.o src/engine.cpp
	@mkdir -p bin
	$(CPP) -c -o bin/engine.o src/engine.cpp

bin/symbol.o: src/symbol.cpp include/ast.h
	@mkdir -p bin
	$(CPP) -c -o bin/symbol.o src/symbol.cpp

bin/vm.o: src/vm.cpp include/ast.h
	@mkdir -p bin
	$(CPP) -c -o bin/vm.o src/vm.cpp

bin/reduce.o: src/reduce.cpp include/ast.h
	@mkdir -p bin
	$(CPP) -c -o bin/reduce.o src/reduce.cpp

bin/compile.o: src/compile.cpp include/ast.h
	@mkdir -p bin
	$(CPP) -c -o bin/compile.o src/compile.cpp

bin/lex.o: src/lex.yy.cc bin/parse.o
	@mkdir -p bin
	$(CPP) -c -o bin/lex.o src/lex.yy.cc

bin/parse.o: src/pico.tab.c
	@mkdir -p bin
	$(CPP) -c -o bin/parse.o src/pico.tab.c

bin/ast.o: src/ast.cpp include/ast.h
	@mkdir -p bin
	$(CPP) -c -o bin/ast.o src/ast.cpp

bin/ast-simple.o: src/ast-simple.cpp include/ast-simple.h
	@mkdir -p bin
	$(CPP) -c -o bin/ast.o src/ast-simple.cpp

test: pico
	# cat tests/test.pc | ./pico
	cat tests/testcomp.pc | ./pico

debug_test: pico
	cat tests/test.pc | ./pico debug

clean:
	-rm src/pico.tab.c src/pico.tab.h src/location.hh src/position.hh src/stack.hh
	-rm src/lex.yy.cc
	-rm pico
