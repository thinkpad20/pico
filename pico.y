%{

#include "expression.h"
#define YYDEBUG 1
int yydebug = 1;

%}
%require "2.4.1"
%skeleton "lalr1.cc"
%defines
%define namespace "pico"
%define parser_class_name "BisonParser"
%error-verbose
%parse-param { pico::FlexScanner &scanner }
%lex-param   { pico::FlexScanner &scanner }

%debug

%code requires {
	// Forward-declare the Scanner class; the Parser needs to be assigned a 
	// Scanner, but the Scanner can't be declared without the Parser
	namespace pico {
		class FlexScanner;
		struct Expression;
      struct ExpressionList;
		struct Term;
      struct TermList;
		struct Assign;
		struct If;
      struct Var;
	}
}

%code {
	// Prototype for the yylex function
	static int yylex(pico::BisonParser::semantic_type * yylval, pico::FlexScanner &scanner);
}

%union {
   double fval;
   int ival;
   char cval;
   std::string *strval;
   pico::Expression *expr;
   pico::Term *term;
   pico::Assign *assign;
   pico::If *if_s;
   pico::Var *var;
   pico::ExpressionList *expr_list;
   pico::TermList *term_list;
}

%token FN INT FLOAT CHAR STRING ARRAY TABLE ALG WHEN
%token END IF THEN ANY RETURN IS DO IO IOFN LIST SL SR
%token LEQ GEQ EQ NEQ OR AND NOT ELSE
%token BOOL TRUE FALSE
%token <strval> ID SYMBOL
%token <strval> TYPENAME
%token <strval> STRING_LITERAL
%token <fval> FLOAT_LITERAL
%token <ival> INT_LITERAL
%token <cval> CHAR_LITERAL
%token <ival> INTEGER

%type <strval> type_name var_name
%type <expr> expr
%type <expr_list> pico exprs
%type <term> term opt_term literal invocation primary
%type <term_list> term_list
%type <var> var

%left '<' '>' LEQ GEQ EQ NEQ
%left AND OR
%left '+' '-'
%left '*' '/' '%'
%right NOT UNARY

%start pico

%%

pico: exprs { $$ = $1; $$->print(); } ;

exprs: expr '.' { $$ = new ExpressionList(); $$->push_back($1); }
    | pico expr '.' { $1->push_back($2); $$ = $1; }

expr
   : term { $$ = new Expression($1); }
   | var_name '=' term ',' expr { $$ = new Expression(new Assign(new Var($1), $3), $5); }
   | IF term THEN term ',' ELSE expr { $$ = new Expression(new If($2, $4), $7); }
   ;

term
   : invocation
   | term '<' invocation  { $$ = make_lt($1, $3); }
   | term '>' invocation  { $$ = make_gt($1, $3); }
   | term LEQ invocation  { $$ = make_leq($1, $3); }
   | term GEQ invocation  { $$ = make_leq($1, $3); }
   | term EQ invocation   { $$ = make_eq($1, $3); }
   | term NEQ invocation  { $$ = make_neq($1, $3); }
   | term AND invocation  { $$ = make_log_and($1, $3); }
   | term OR invocation   { $$ = make_log_or($1, $3); }
   | term '+' invocation  { $$ = make_add($1, $3); }
   | term '-' invocation  { $$ = make_sub($1, $3); }
   | term '*' invocation  { $$ = make_mult($1, $3); }
   | term '/' invocation  { $$ = make_div($1, $3); }
   | term '%' invocation  { $$ = make_mod($1, $3); }
   | NOT term             { $$ = make_log_not($2); }
   | '-' term %prec UNARY { $$ = make_neg($2); }
   ;

invocation
   : primary
   | invocation '(' term_list ')' { $$ = new Term(new Invoke($1, $3)); }
   ;

term_list
   : opt_term { $$ = new TermList(); $$->push_back($1); }
   | term_list ',' opt_term { $1->push_back($3); $$ = $1; }
   ;

opt_term
   : term
   | { $$ = NULL; }//{$$ = make_parens(new Expression()); }
   ;

primary
   : literal
   | var { $$ = new Term($1);}//{  }
   | '(' expr ')' { $$ = new Term($2); }//{ $$ = make_parens($2); }
   ;

var: var_name     { $$ = new Var($1); }
   | type_name var_name { $$ = new Var($2, $1); } ;//{ $2->type = $1; $$ = $2; } ;

type_name
   : ANY { $$ = new std::string("Any"); } 
   | INT { $$ = new std::string("Int"); }
   | FLOAT {$$ = new std::string("Float"); }
   | STRING {$$ = new std::string("String"); }
   | ARRAY { $$ = new std::string("Array"); }
   | LIST { $$ = new std::string("List"); }
   | TABLE { $$ = new std::string("Table"); }
   | TYPENAME 
   ;

literal
   : INT_LITERAL     { $$ = new Term($1); }
   | FLOAT_LITERAL   { $$ = new Term($1); }
   | STRING_LITERAL  { $$ = new Term($1); }
   | CHAR_LITERAL    { $$ = new Term($1); }
   | TRUE            { $$ = new Term(true); }
   | FALSE           { $$ = new Term(false); }
   ;

var_name: ID;

%%

// We have to implement the error function
void pico::BisonParser::error(const pico::BisonParser::location_type &loc, const std::string &msg) {
	std::cerr << "Error: " << msg << std::endl;
}

// Now that we have the Parser declared, we can declare the Scanner and implement
// the yylex function
#include "picoScanner.h"
static int yylex(pico::BisonParser::semantic_type * yylval, pico::FlexScanner &scanner) {
	return scanner.yylex(yylval);
}