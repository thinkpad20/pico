#ifndef __PICO_AST_H_
#define __PICO_AST_H_

#include "common.h"
#include <deque>
#include <vector>
#include <map>

namespace pico {
struct Expression;

struct ExpressionList : public std::deque<Expression *> { 
   friend std::ostream& operator<<(std::ostream&, ExpressionList *&);
   void reduce_all();
};
extern ExpressionList *parsed_expressions;

struct Expression {
   enum Type { ASSIGN, IF, CALL, INT, FLOAT, BINARY, UNARY,
               STRING, CHAR, BOOL, VAR, UNBOUND, LAMBDA } t;
   union {
      int i;
      double f;
      std::string *str; // also doubles as a variable name
      char c;
      bool b;
      struct { std::string *unop; Expression *arg; }; // unary operators
      struct { std::string *binop; Expression *arg1, *arg2; }; // binary operators
      struct { Expression *expr; ExpressionList *params, *locals; }; // creates a new context
      struct { std::string *var_name, *var_type; }; // declaring unbound variable
      struct { std::string *alias; Expression *rhs, *next; }; // assign statement
      struct { Expression *cond, *if_true, *if_false; }; // if statement
      struct { Expression *func; ExpressionList *args; }; // calling a function
   };

   // Expression() {}
   Expression(int i): t(INT), i(i) {}
   Expression(double f): t(FLOAT), f(f) {}
   Expression(char c): t(CHAR), c(c) {}
   Expression(bool b): t(BOOL), b(b) {}
   Expression(std::string *func, Expression *expr): t(UNARY), arg(arg) {}
   Expression(std::string *func, Expression *e1, Expression *e2): t(BINARY), arg1(e1), arg2(e2) {}
   Expression(Expression *expr): t(LAMBDA), expr(expr) {}
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
   static Expression *make_unbound_var(const char *vtype, const char *vname);

   //wrapper functions for calls with 0, 1 and 2 args
   static Expression *make_0ary_call(Expression *func);
   static Expression *make_unary_sym_call(const char *symbol, Expression *e);
   static Expression *make_binary_sym_call(const char *symbol, Expression *e1, Expression *e2);

   static void init();
   static Expression *BLANK_EXPR();
   static Expression *ROOT_EXPR();
   bool is_symbol(); bool is_literal(); std::string *is_primitive_symbol();
   bool is_binary(); bool is_unary(); bool is_0ary();
   friend std::ostream& operator<<(std::ostream& os, Expression *expr);
};

// Named constructors
Expression *Int(int i);
Expression *Float(float f);
Expression *String(std::string *str);
Expression *Char(char c);
Expression *Bool(bool b);
Expression *Var(std::string *name);
Expression *Unbound(std::string *name, std::string *type);
Expression *Unary(std::string *op, Expression *e);
Expression *Binary(std::string *op, Expression *e1, Expression *e2);
Expression *Call(Expression *func, ExpressionList *args);
Expression *Lambda(Expression *e);
Expression *Assign(std::string *v, Expression *rhs, Expression *next);
Expression *Conditional(Expression *cond, Expression *if_true, Expression *if_false);

}

#endif