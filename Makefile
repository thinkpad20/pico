all: pico

run: pico
	./pico

pico: lex.yy.cc pico.tab.c expression.cpp expression.h
	g++ lex.yy.cc parse.cpp pico.tab.c expression.cpp -o pico
	
lex.yy.cc: pico.l
	flex pico.l

pico.tab.c: pico.y
	bison --report=all pico.y

debug: pico.y
	bison -v --report=all --debug pico.y
	flex pico.l
	g++ lex.yy.cc parse.cpp pico.tab.c expression.cpp -o pico-debug

clean:
	rm -rf pico.tab.c pico.tab.h location.hh position.hh stack.hh
	rm -rf lex.yy.cc
	rm -rf pico
