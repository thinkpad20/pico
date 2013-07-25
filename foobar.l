%{
#include "expression.h"
#include "foobarScanner.h"
static int block_comment_num = 0;
static char to_char(char *str);
int yylineno = 0;
%}

%option nodefault yyclass="FlexScanner" noyywrap c++


%x LINE_COMMENT
%x BLOCK_COMMENT

%%

Any                     { return foobar::BisonParser::token::ANY; }
Int                     { return foobar::BisonParser::token::INT; }
Float                   { return foobar::BisonParser::token::FLOAT; }
Char                    { return foobar::BisonParser::token::CHAR; }
List                    { return foobar::BisonParser::token::LIST; }
Array                   { return foobar::BisonParser::token::ARRAY; }
Table                   { return foobar::BisonParser::token::TABLE; }
alg                     { return foobar::BisonParser::token::ALG; }
fn                      { return foobar::BisonParser::token::FN; }
iofn                    { return foobar::BisonParser::token::IOFN; }
end                     { return foobar::BisonParser::token::END; }
if                      { return foobar::BisonParser::token::IF; }
then                    { return foobar::BisonParser::token::THEN; }
else                    { return foobar::BisonParser::token::ELSE; }
is                      { return foobar::BisonParser::token::IS; }
or                      { return foobar::BisonParser::token::OR; }
and                     { return foobar::BisonParser::token::AND; }
not                     { return foobar::BisonParser::token::NOT; }
when                    { return foobar::BisonParser::token::WHEN; }
"=="                     { return foobar::BisonParser::token::EQ; }
=                       { return '='; }
,                       { return ','; }
"&"                       { return '&'; }
"|"                       { return '|'; }
"^"                       { return '^'; }
"~"                       { return '~'; }
"."                     { return '.'; }
"+"                     { return '+'; }
"-"                     { return '-'; }
"*"                     { return '*'; }
"/"                     { return '/'; }
">"                     { return '>'; }
"<"                     { return '<'; }
"<<"                    { return foobar::BisonParser::token::SL; }
">>"                    { return foobar::BisonParser::token::SR; }
"!="                    { return foobar::BisonParser::token::NEQ; }
return                  { return foobar::BisonParser::token::RETURN; }
"->"                    { return foobar::BisonParser::token::RETURN; }
"<--"                   { BEGIN(LINE_COMMENT); }
<LINE_COMMENT>\n        { BEGIN(INITIAL); yylineno++; }
<LINE_COMMENT>.         { /* ignore */ }
"<{"                    { BEGIN(BLOCK_COMMENT); block_comment_num = 1; }
<BLOCK_COMMENT>"<{"     { block_comment_num++; }
<BLOCK_COMMENT>"}>"     { if ((--block_comment_num) == 0) BEGIN(INITIAL); }
<BLOCK_COMMENT>\n       { yylineno++; }
<BLOCK_COMMENT><<EOF>>  { puts("Warning: EOF encountered inside a block comment"); return EOF; }
<BLOCK_COMMENT>.        { /* ignore */ }
[A-Z][a-zA-Z0-9_]*   { yylval->strval = new std::string(yytext); 
                          return foobar::BisonParser::token::TYPENAME; }
[a-z_][a-zA-Z0-9_]*   { yylval->strval = new std::string(yytext); 
                          return foobar::BisonParser::token::ID; }
[-+]?[0-9]+             { yylval->ival = atoi(yytext); 
                          return foobar::BisonParser::token::INT_LITERAL; }
([0-9]+|([0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?)  { yylval->fval = atof(yytext); 
                                               return foobar::BisonParser::token::FLOAT_LITERAL; }
\"(\\.|[^\\"])*\"  { yylval->strval = new std::string(strndup(yytext+1, strlen(yytext) - 2)); 
                     return foobar::BisonParser::token::STRING_LITERAL; }
\'(\\.|[^\\'])*\'     { yylval->cval = to_char(yytext+1); 
                        return foobar::BisonParser::token::CHAR_LITERAL; }
[\*\+\-/<=>\^~.]+       { return foobar::BisonParser::token::SYMBOL; }
[ \t\r]+                  { /* ignore */ }
\n                      { yylineno++; }
.                       { return yytext[0]; }

%%

static char to_char(char *str) {
   if (strlen(str) <= 1) {
      fprintf(stderr, "Error: empty character constant (line %d)\n", yylineno);
      exit(1);
   } else if (str[0] != '\\') {
      return str[0];
   } else {
      if (strlen(str) == 1) {
         fprintf(stderr, "Error: somehow we got a single slash character\n");
         exit(1);
      }
      switch (str[1]) {
         case 'n': return '\n';
         case 'r': return '\r';
         case 't': return '\t';
         case 'a': return '\a';
         case 'b': return '\b';
         case 'f': return '\f';
         case 'v': return '\v';
         case '\'': return '\'';
         case '"': return '"';
         case '\\': return '\\';
         case '?': return '\?';
         case 'x':
            fprintf(stderr, "Error: unicode not yet supported (line %d)\n", yylineno);
            exit(1);
         default:
            fprintf(stderr, "Error: unrecognized escape sequence '\\%c' (line %d)\n", 
                                                                     str[1], yylineno);
            exit(1);
      }
   }
}
