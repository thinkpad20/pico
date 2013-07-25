all: foobar

run: foobar
	./foobar

foobar: lex.yy.cc foobar.tab.c expression.cpp expression.h
	g++ lex.yy.cc parse.cpp foobar.tab.c expression.cpp -o foobar
	
lex.yy.cc: foobar.l
	flex foobar.l

foobar.tab.c: foobar.y
	bison --report=all foobar.y

debug: foobar.y
	bison -v --report=all --debug foobar.y
	flex foobar.l
	g++ lex.yy.cc parse.cpp foobar.tab.c expression.cpp -o foobar-debug

clean:
	rm -rf foobar.tab.c foobar.tab.h location.hh position.hh stack.hh
	rm -rf lex.yy.cc
	rm -rf foobar
