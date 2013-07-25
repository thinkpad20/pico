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
		struct Term;
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
}

%token FN INT FLOAT CHAR STRING ARRAY TABLE ALG WHEN
%token END IF THEN ANY RETURN IS DO IO IOFN LIST SL SR
%token LEQ GEQ EQ NEQ OR AND NOT ELSE NEWLINE
%token BOOL TRUE FALSE
%token <strval> ID SYMBOL
%token <strval> TYPENAME
%token <strval> STRING_LITERAL
%token <fval> FLOAT_LITERAL
%token <ival> INT_LITERAL
%token <cval> CHAR_LITERAL
%token <ival> INTEGER

%type <strval> typename
%type <expr> expr
%type <term> term mult_term unary_term literal invocation primary
%type <term> add_term comp_term and_term term_list opt_term
%type <ival> comp
%type <var> undef_var var

%left COMP
%left LOGICAL
%left ADDITIVE
%left MULTI
%left SYMBOL
%left UNARY

%start exprs

%%

exprs: expr | exprs expr ;

expr
   : term '.' { $$ = new Expression($1); }
   | var '=' term term_end expr { $$ = new Expression(new Assign($1, $3), $5); }
   | IF term THEN term term_end ELSE expr { $$ = new Expression(new If($2, $4), $7); }
   ;

term_end
   : NEWLINE | ','
   ;

term
   : and_term
   | term OR and_term { $$ = make_log_or($1, $3); }
   ;

and_term
   : comp_term
   | and_term AND comp_term { $$ = make_log_and($1, $3); }
   ;

comp_term
   : add_term
   | comp_term comp add_term
      {
         switch ($2) {
            case 0: $$ = make_eq($1, $3); break;
            case 1: $$ = make_lt($1, $3); break;
            case 2: $$ = make_gt($1, $3); break;
            case 3: $$ = make_leq($1, $3); break;
            case 4: $$ = make_neq($1, $3); break;
            case 5: $$ = make_geq($1, $3); break;
         }
      }
   ;

comp: EQ {$$ = 0;} | '<' {$$ = 1;} | '>' {$$ = 2;}
   | LEQ {$$ = 3;} | NEQ {$$ = 4;} | GEQ {$$ = 5;}
   ;

add_term
   : mult_term
   | mult_term '+' add_term { $$ = make_add($1, $3); printf("Found an addition\n"); }
   | mult_term '-' add_term { $$ = make_sub($1, $3); printf("Found a subtraction\n"); }
   ;

mult_term
   : unary_term
   | unary_term '*' mult_term { $$ = make_mult($1, $3); printf("Found a multiplication\n"); }
   | unary_term '/' mult_term { $$ = make_div($1, $3); printf("Found a division\n"); }
   ;

unary_term
   : invocation
   | '-' unary_term { $$ = make_neg($2); }
   | NOT unary_term { $$ = make_log_not($2); }
   ;

invocation
   : primary
   | invocation '(' term_list ')' 
      { 
         $$ = new Term(new Invoke($1, $3)); 
      }
   ;

term_list
   : opt_term
   | term_list ',' opt_term { $1->append($3); $$ = $1; }
   ;

opt_term
   : term
   | {$$ = make_parens(new Expression()); }
   ;

primary
   : literal
   | var { $$ = new Term($1); }
   | '(' expr ')' { $$ = make_parens($2); }
   | undef_var { $$ = new Term($1); }
   ;

undef_var: typename var { $2->type = $1; $$ = $2; } ;

typename
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
   : INT_LITERAL { $$ = new Term($1); }
   | FLOAT_LITERAL { $$ = new Term($1); }
   | STRING_LITERAL { $$ = new Term($1); }
   | CHAR_LITERAL { $$ = new Term($1); }
   ;

var: ID { $$ = new Var($1); } ;

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
