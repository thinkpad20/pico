%{

#include "ast.h"

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
   char *cstrval;
   const char *const_strval;
   std::string *strval;
   pico::Expression *expr;
   pico::ExpressionList *expr_list;
}

%token FN INT FLOAT CHAR STRING ARRAY TABLE ALG WHEN
%token END IF THEN ANY RETURN IS DO IO IOFN LIST SL SR
%token BOOL TRUE FALSE OR AND NOT ELSE
%token LEQ GEQ EQ NEQ 
%token <cstrval> IDENTIFIER SYMBOL ANON_RECURSE
%token <cstrval> TYPENAME
%token <cstrval> STRING_LITERAL
%token <fval> FLOAT_LITERAL
%token <ival> INT_LITERAL
%token <cval> CHAR_LITERAL
%token <ival> INTEGER

%type <expr> expression term call exponent unary multiplicative additive
%type <expr> var symbolic opt_expression log_or log_and comparison
%type <expr_list> pico exprs expression_list
%type <const_strval> comp_op

%start pico

%%

pico: exprs { $$ = $1; } ;

exprs: expression '.'      { parsed_expressions = $$ = new ExpressionList(); 
                              $$->push_back($1); }
    | pico expression '.'  { $1->push_back($2); parsed_expressions = $$ = $1; }

expression
   : symbolic
   | IDENTIFIER '=' expression ',' expression   { $$ = Assign(new std::string($1), $3, $5); }
   | IF term THEN term ELSE expression  { $$ = Conditional($2, $4, $6); }
   ;

symbolic: log_or
        | log_or SYMBOL symbolic { $$ = Binary(new std::string($2), $1, $3); }
        ;

log_or: log_and
      | log_and OR log_or { $$ = Binary(new std::string("||"), $1, $3); }
      ;

log_and: comparison
       | comparison AND log_and { $$ = Binary(new std::string("&&"), $1, $3); }
       ;

comparison: additive
          | additive comp_op comparison  { $$ = Binary(new std::string($2), $1, $3); }
          ;

comp_op: '>' { $$ = ">"; } | '<' { $$ = "<"; } | LEQ { $$ = "<=" ; } 
          | GEQ { $$ = ">="; } | EQ { $$ = "=="; } | NEQ { $$ = "!=" ; };

additive: multiplicative
        | multiplicative '+' additive { $$ = Binary(new std::string("+"), $1, $3); }
        | multiplicative '-' additive { $$ = Binary(new std::string("-"), $1, $3); }
        ;

multiplicative: unary
              | unary '*' multiplicative { $$ = Binary(new std::string("*"), $1, $3); }
              | unary '/' multiplicative { $$ = Binary(new std::string("/"), $1, $3); }
              ;

unary: exponent
     | '!' unary {$$ = Unary(new std::string("!"), $2); }
     | '-' unary {$$ = Unary(new std::string("-"), $2); }
     ;

exponent: call
        | call '^' exponent { $$ = Binary(new std::string("^"), $1, $3); }
        ;

call: term
    | call '(' expression_list ')' { $$ = Call($1, $3); }
    ;

expression_list: opt_expression { $$ = new ExpressionList($1); }
               | expression_list ',' opt_expression { $$ = $1, $$->push_back($3); }
               ;

opt_expression: expression | { $$ = NULL; };

term
   : INT_LITERAL        { $$ = Int($1); }
   | FLOAT_LITERAL      { $$ = Float($1); }
   | CHAR_LITERAL       { $$ = Char($1); }
   | STRING_LITERAL     { $$ = String(new std::string($1)); }
   | TRUE               { $$ = Bool(true); }
   | FALSE              { $$ = Bool(false); }
   | var
   | '{' expression '}' { $$ = Lambda($2); }
   | '(' expression ')' { $$ = $2; }
   ;

var: IDENTIFIER           { $$ = Var(new std::string($1)); }
   | IDENTIFIER ':' IDENTIFIER { $$ = Unbound(new std::string($1), new std::string($3)); }
   | ANON_RECURSE { $$ = Var(new std::string($1)); } 
   ;

%%

void pico::BisonParser::error(const pico::BisonParser::location_type &loc, const std::string &msg) {
   std::cerr << "Error: " << msg << ", line " << yylineno << std::endl;
}

#include "picoScanner.h"
static int yylex(pico::BisonParser::semantic_type * yylval, pico::FlexScanner &scanner) {
   return scanner.yylex(yylval);
}
