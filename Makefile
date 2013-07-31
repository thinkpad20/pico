all: pico

run: pico
	./pico

pico: lex.yy.cc pico.tab.c ast.cpp ast.h symbol.h symbol.cpp reduce.cpp
	g++ lex.yy.cc parse.cpp pico.tab.c ast.cpp symbol.cpp reduce.cpp -o pico

pico-mod: lex.yy.cc pico.tab.c ast-mod.cpp ast-mod.h symbol-mod.cpp reduce-mod.cpp
	g++ lex.yy.cc parse.cpp pico.tab.c ast-mod.cpp symbol-mod.cpp reduce-mod.cpp -o pico-mod
	
lex.yy.cc: pico.l
	flex pico.l

pico.tab.c: pico.y location.hh position.hh
	bison -v --report=all --debug pico.y

test: pico
	cat test.pc | ./pico

debug_test: pico
	cat test.pc | ./pico debug

mod-test: pico-mod
	cat test.pc | ./pico-mod debug	

clean:
	rm -rf pico.tab.c pico.tab.h location.hh position.hh stack.hh
	rm -rf lex.yy.cc
	rm -rf pico
