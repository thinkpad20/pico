CPP=g++
LIBS=bin/ast.o bin/symbol.o bin/reduce.o
PARSE=bin/lex.o bin/parse.o

all: pico

run: pico
	./pico

pico: bin/lex.o bin/parse.o bin/engine.o bin/ast.o bin/reduce.o bin/symbol.o
	g++ -o pico bin/engine.o $(PARSE) $(LIBS)

# generate C source from lex and yac files
src/lex.yy.cc: src/pico.l
	flex src/pico.l
	mv *.cc src

src/pico.tab.c: src/lex.yy.cc src/pico.y include/location.hh include/position.hh
	bison -v --report=all --debug src/pico.y
	mv *.c src
	mv *.h src
	mv *.hh src
	mkdir -p aux
	mv *.output aux

# object files
bin/engine.o: include/picoScanner.h bin/ast.o src/engine.cpp
	$(CPP) -c -o bin/engine.o src/engine.cpp

bin/symbol.o: src/symbol.cpp include/ast.h
	$(CPP) -c -o bin/symbol.o src/symbol.cpp

bin/reduce.o: src/reduce.cpp include/ast.h
	$(CPP) -c -o bin/reduce.o src/reduce.cpp

bin/lex.o: src/lex.yy.cc
	$(CPP) -c -o bin/lex.o src/lex.yy.cc

bin/parse.o: src/pico.tab.c
	$(CPP) -c -o bin/parse.o src/pico.tab.c

bin/ast.o: src/ast.cpp include/ast.h
	$(CPP) -c -o bin/ast.o src/ast.cpp

test: pico
	cat tests/test.pc | ./pico

debug_test: pico
	cat tests/test.pc | ./pico debug

clean:
	rm -rf pico.tab.c pico.tab.h location.hh position.hh stack.hh
	rm -rf lex.yy.cc
	rm -rf pico
