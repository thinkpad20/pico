#ifndef __PICO_AST_H_
#define __PICO_AST_H_

#include "common.h"
#include <deque>

namespace pico {
struct Expression;

struct ExpressionList : public std::deque<Expression *> { 
   friend std::ostream& operator<<(std::ostream&, ExpressionList *&);
   void reduce_all();
};

struct Expression {
   enum Type { ASSIGN, IF, CALL, INT, FLOAT, BLANK,
               STRING, CHAR, BOOL, VAR, UNBOUND } t;
   union {
      int i;
      double f;
      std::string *str; // also doubles as a variable name
      char c;
      bool b;
      struct { std::string *var_name, *var_type; }; // declaring unbound variable
      struct { std::string *alias; Expression *rhs, *next; }; // assign statement
      struct { Expression *cond, *if_true, *if_false; }; // if statement
      struct { Expression *func; ExpressionList *args; }; // calling a function
   };

   Expression(): t(BLANK) {}
   Expression(int i): t(INT), i(i) {}
   Expression(double f): t(FLOAT), f(f) {}
   Expression(char c): t(CHAR), c(c) {}
   Expression(bool b): t(BOOL), b(b) {}
   Expression(std::string *str): t(STRING), str(str) {}
   Expression(std::string *var_name, std::string *var_type): 
      t(UNBOUND), var_name(var_name), var_type(var_type) {}
   Expression(const char *alias, Expression *rhs, Expression *next):
      t(ASSIGN), alias(new std::string(alias)), rhs(rhs), next(next) {}
   Expression(Expression *func, ExpressionList *args): 
      t(CALL), func(func), args(args) {}
   Expression(Expression *cond, Expression *if_true, Expression *if_false): 
      t(IF), cond(cond), if_true(if_true), if_false(if_false) {}
   static Expression *make_var(const char *vname);
   static Expression *make_unbound_var(const char *vname, const char *vtype);

   //wrapper functions for calls with 0, 1 and 2 args
   static Expression *make_0ary_call(Expression *func);
   static Expression *make_unary_sym_call(const char *symbol, Expression *e);
   static Expression *make_binary_sym_call(const char *symbol, Expression *e1, Expression *e2);

   static void init();
   static Expression *BLANK_EXPR();
   bool is_symbol(); bool is_literal();
   friend std::ostream& operator<<(std::ostream& os, Expression *&expr);
};

}

#endif