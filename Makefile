all: pico

run: pico
	./pico

pico: lex.yy.cc pico.tab.c ast.cpp ast.h symbol.h symbol.cpp eval.cpp
	g++ lex.yy.cc parse.cpp pico.tab.c ast.cpp symbol.cpp eval.cpp -o pico
	
lex.yy.cc: pico.l
	flex pico.l

pico.tab.c: pico.y location.hh position.hh
	bison -v --report=all --debug pico.y

test: pico
	cat test.pc | ./pico

debug_test: pico
	cat test.pc | ./pico debug

clean:
	rm -rf pico.tab.c pico.tab.h location.hh position.hh stack.hh
	rm -rf lex.yy.cc
	rm -rf pico
