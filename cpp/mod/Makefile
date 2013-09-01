CPP=clang++
LIBS=ast.o
PARSE=lex.o parse.o

all: pico

run: pico
	./pico

pico: lex.o parse.o engine.o ast.o
	$(CPP) -o pico engine.o $(PARSE) $(LIBS)

# generate C source from lex and yacc files
lex.yy.cc: pico.l
	flex pico.l

pico.tab.c: lex.yy.cc pico.y
	bison -v --report=all --debug pico.y

# object files
engine.o: picoScanner.h ast.o engine.cpp
	$(CPP) -c -o engine.o engine.cpp

lex.o: lex.yy.cc parse.o
	$(CPP) -c -o lex.o lex.yy.cc

parse.o: pico.tab.c
	$(CPP) -c -o parse.o pico.tab.c

ast.o: ast.cpp ast.h
	$(CPP) -c -o ast.o ast.cpp

test: pico
	# cat tests/test.pc | ./pico
	cat test.pc | ./pico

debug_test: pico
	cat test.pc | ./pico debug

clean:
	-rm -f pico.tab.c pico.tab.h location.hh position.hh stack.hh
	-rm -f lex.yy.cc
	-rm -f pico
