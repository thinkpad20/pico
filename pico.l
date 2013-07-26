%{
#include "expression.h"
#include "picoScanner.h"
static int block_comment_num = 0;
static char to_char(char *str);
static std::string *get_string(const char *);
int yylineno = 0;
%}

%option nodefault yyclass="FlexScanner" noyywrap c++


%x LINE_COMMENT
%x BLOCK_COMMENT

%%

Any                     { return pico::BisonParser::token::ANY; }
Int                     { return pico::BisonParser::token::INT; }
Float                   { return pico::BisonParser::token::FLOAT; }
Char                    { return pico::BisonParser::token::CHAR; }
List                    { return pico::BisonParser::token::LIST; }
Array                   { return pico::BisonParser::token::ARRAY; }
Table                   { return pico::BisonParser::token::TABLE; }
if                      { return pico::BisonParser::token::IF; }
then                    { return pico::BisonParser::token::THEN; }
else                    { return pico::BisonParser::token::ELSE; }
is                      { return pico::BisonParser::token::IS; }
not                     { return pico::BisonParser::token::NOT; }
when                    { return pico::BisonParser::token::WHEN; }
True                    { return pico::BisonParser::token::WHEN; }
False                   { return pico::BisonParser::token::WHEN; }
"=="                    { return pico::BisonParser::token::EQ; }
"<="                    { return pico::BisonParser::token::LEQ; }
">="                    { return pico::BisonParser::token::GEQ; }
"!="                    { return pico::BisonParser::token::NEQ; }
"||"                    { return pico::BisonParser::token::OR; }
"&&"                    { return pico::BisonParser::token::AND; }
[+\-*/=><,.]            { return yytext[0]; }

"<--"                   { BEGIN(LINE_COMMENT); }
<LINE_COMMENT>.         { /* ignore */ }
<LINE_COMMENT>\n        { BEGIN(INITIAL); yylineno++; }
"<{"                    { BEGIN(BLOCK_COMMENT); block_comment_num = 1; }
<BLOCK_COMMENT>"<{"     { block_comment_num++; }
<BLOCK_COMMENT>\n       { yylineno++; }
<BLOCK_COMMENT><<EOF>>  { puts("Warning: EOF encountered inside a block comment"); return EOF; }
<BLOCK_COMMENT>.        { /* ignore */ }
<BLOCK_COMMENT>"}>"     { if ((--block_comment_num) == 0) BEGIN(INITIAL); }

[A-Z][a-zA-Z0-9_]*      { yylval->strval = new std::string(yytext); return pico::BisonParser::token::TYPENAME; }
[a-z_][a-zA-Z0-9_]*     { yylval->strval = new std::string(yytext); return pico::BisonParser::token::ID; }
[0-9]+                  { yylval->ival = atoi(yytext); return pico::BisonParser::token::INT_LITERAL; }
([0-9]*\.[0-9]+)        { yylval->fval = atof(yytext); return pico::BisonParser::token::FLOAT_LITERAL; }
\"(\\.|[^\\"])*\"       { yylval->strval = get_string(yytext); return pico::BisonParser::token::STRING_LITERAL; }
\'(\\.|[^\\'])*\'       { yylval->cval = to_char(yytext+1); return pico::BisonParser::token::CHAR_LITERAL; }

[ \t\r]+                { /* ignore */ }
\n                      { yylineno++;  }
.                       { printf("unrecognized character '%c' found\n", *yytext); return yytext[0]; }

%%

static std::string *get_string(const char *yytext) {
   char *c_str = strndup(yytext+1, strlen(yytext) - 2);
   std::string *res = new std::string(c_str);
   free(c_str);
   return res;
}

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
