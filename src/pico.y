%{

#include "../include/ast.h"
#include "../include/compile.h"
#define YYDEBUG 1
int yydebug = 1;
extern int yylineno;
extern pico::ExpressionList *pico::parsed_expressions;

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
   char *strval;
   pico::Expression *expr;
   pico::ExpressionList *expr_list;
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
%type <expr> expr term opt_term literal invocation primary var
%type <expr_list> pico exprs expr_list

%left '<' '>' LEQ GEQ EQ NEQ
%left AND OR
%left '+' '-'
%left '*' '/' '%'
%left '^'
%right NOT UNARY

%start pico

%%

pico: exprs ;

exprs: expr '.'      { parsed_expressions = new ExpressionList(); 
                       parsed_expressions->push_back($1); }
    | pico expr '.'  { $1->push_back($2); parsed_expressions = $1; }

expr
   : term
   | var_name '=' expr comma_or_newline expr 
   { 
      $$ = new Expression($1, $3, $5); 
   }
   | IF term THEN term ELSE expr             { $$ = new Expression($2, $4, $6); }
   ;

comma_or_newline
   : ',' | '\n'
   ;

term
   : invocation 
   | term '<' term  { $$ = Expression::make_binary_sym_call("<", $1, $3); }
   | term '>' term  { $$ = Expression::make_binary_sym_call(">", $1, $3); }
   | term LEQ term  { $$ = Expression::make_binary_sym_call("<=", $1, $3); }
   | term GEQ term  { $$ = Expression::make_binary_sym_call(">=", $1, $3); }
   | term EQ term   { $$ = Expression::make_binary_sym_call("==", $1, $3); }
   | term NEQ term  { $$ = Expression::make_binary_sym_call("!=", $1, $3); }
   | term AND term  { $$ = Expression::make_binary_sym_call("&&", $1, $3); }
   | term OR term   { $$ = Expression::make_binary_sym_call("||", $1, $3); }
   | term '+' term  { $$ = Expression::make_binary_sym_call("+", $1, $3); }
   | term '-' term  { $$ = Expression::make_binary_sym_call("-", $1, $3); }
   | term '*' term  { $$ = Expression::make_binary_sym_call("*", $1, $3); }
   | term '/' term  { $$ = Expression::make_binary_sym_call("/", $1, $3); }
   | term '%' term  { $$ = Expression::make_binary_sym_call("%", $1, $3); }
   | term '^' term  { $$ = Expression::make_binary_sym_call("^", $1, $3); }
   | NOT term             { $$ = Expression::make_unary_sym_call("!", $2); }
   | '-' term %prec UNARY { $$ = Expression::make_unary_sym_call("-", $2); }
   ;

invocation
   : primary                      { $$ = Expression::make_0ary_call($1); }
   | invocation '(' expr_list ')' { $$ = new Expression($1, $3); }
   ;

expr_list
   : opt_term                 { $$ = new ExpressionList(); $$->push_back($1); }
   | expr_list ',' opt_term   { $1->push_back($3); $$ = $1; }
   ;

opt_term
   : term
   | { $$ = Expression::BLANK_EXPR(); }
   ;

primary
   : literal
   | var          
   | '(' expr ')' { $$ = $2; }
   ;

var: var_name           { $$ = Expression::make_var($1); }
   | type_name var_name { $$ = Expression::make_unbound_var($1, $2); } ;

type_name
   : ANY       { $$ = strdup("Any"); } 
   | INT       { $$ = strdup("Int"); }
   | FLOAT     { $$ = strdup("Float"); }
   | STRING    { $$ = strdup("String"); }
   | ARRAY     { $$ = strdup("Array"); }
   | LIST      { $$ = strdup("List"); }
   | TABLE     { $$ = strdup("Table"); }
   | TYPENAME
   ;

literal
   : INT_LITERAL     { $$ = new Expression($1); }
   | FLOAT_LITERAL   { $$ = new Expression($1); }
   | STRING_LITERAL  { $$ = new Expression($1); }
   | CHAR_LITERAL    { $$ = new Expression($1); }
   | TRUE            { $$ = new Expression(true); }
   | FALSE           { $$ = new Expression(false); }
   ;

var_name: ID;

%%

void pico::BisonParser::error(const pico::BisonParser::location_type &loc, const std::string &msg) {
	std::cerr << "Error: " << msg << ", line " << yylineno << std::endl;
}

#include "../include/picoScanner.h"
static int yylex(pico::BisonParser::semantic_type * yylval, pico::FlexScanner &scanner) {
	return scanner.yylex(yylval);
}
