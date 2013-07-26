#pragma once

#include "common.h"
#include <map>
#include <deque>

namespace pico {

struct Term;
struct Expression;

struct ExpressionList : public std::deque<Expression *> {
   void print();
};

struct TermList : public std::deque<Term *> {
   void print();
};

struct Invoke {
   Term *func;
   TermList *term_list;
   Invoke(Term *func): func(func), term_list(NULL) {}
   Invoke(Term *func, TermList *term_list): func(func), term_list(term_list) {}
   ~Invoke();
};

struct Var {
   std::string *name, *type;
   Var(std::string *name): name(name), type(NULL) {}
   Var(std::string *name, std::string *type): name(name), type(type) {}
   ~Var();
};

struct Assign {
   Var *var; 
   Term *term;
   Assign(Var *var, Term *term): 
      var(var), term(term) {}
   ~Assign();
   void print();
};

struct If {
   Term *cond, *if_true; 
   If(Term *cond, Term *if_true):
      cond(cond), if_true(if_true) {}
   ~If();
   void print();
};

struct Expression {
   enum Type {
      TERM, 
      ASSIGN, 
      IF,
   } t;
   union {
      Term *term;
      struct {Assign *assign; Expression *next; } assign;
      struct {If *if_s; Expression *next;} if_s;
   };
   Expression *next;
   Expression(Term *term): t(TERM), term(term) {/*printf("made term expression\n");*/}
   Expression(If *if_s, Expression *next): t(IF) {
      this->if_s.if_s = if_s; 
      this->if_s.next = next;
   }
   Expression(Assign *a, Expression *next): t(ASSIGN) {
      this->assign.assign = a;
      this->assign.next = next;
   }
   ~Expression();
   void print();
   void append(Expression *expr);
};

struct Term {
   enum Type { 
      INVOKE, FLOAT, INT, STRING, VAR, CHAR,
      ADD, SUB, MULT, DIV, LT, BOOL, EXP,
      GT, LEQ, GEQ, EQ, NEQ, MOD, EMPTY,
      LOG_AND, LOG_OR, LOG_NOT, BIT_AND,
      BIT_OR, BIT_XOR, BIT_NOT, NEG, PARENS,
   } t;
   union {
      int ival;
      char cval;
      bool bval;
      double fval;
      std::string *strval; 
      Invoke *invoke;
      Var *var;
      struct {Term *term1, *term2;} binary;
      Term *unary;
      Expression *expr;
   };
   Term *next; // for term lists
   Term(): t(EMPTY) {} // for empty terms
   Term(double f): t(FLOAT), fval(f) {}
   Term(int i): t(INT), ival(i) {}
   Term(char c): t(CHAR), cval(c) {}
   Term(bool b): t(BOOL), bval(b) {}
   Term(std::string *str): t(STRING), strval(str) {}
   Term(const char *str): t(STRING), strval(new std::string(str)) {}
   Term(Var *var): t(VAR), var(var) {}
   Term(Invoke *inv): t(INVOKE), invoke(inv) {}
   Term(Term *term1, Term *term2, enum Type type) {
      binary.term1 = term1;
      binary.term2 = term2;
      t = type;
   }
   Term(Expression *expr): t(PARENS), expr(expr) {}
   Term(Term *term, enum Type type): t(type), unary(term) {}
   void print();
   void append(Term *term);
};

Term *make_add(Term *term1, Term *term2);
Term *make_sub(Term *term1, Term *term2);
Term *make_mult(Term *term1, Term *term2);
Term *make_div(Term *term1, Term *term2);
Term *make_mod(Term *term1, Term *term2);
Term *make_exp(Term *term1, Term *term2);
Term *make_eq(Term *term1, Term *term2);
Term *make_lt(Term *term1, Term *term2);
Term *make_gt(Term *term1, Term *term2);
Term *make_leq(Term *term1, Term *term2);
Term *make_neq(Term *term1, Term *term2);
Term *make_geq(Term *term1, Term *term2);
Term *make_log_and(Term *term1, Term *term2);
Term *make_log_or(Term *term1, Term *term2);
Term *make_log_not(Term *term);
Term *make_bit_and(Term *term1, Term *term2);
Term *make_bit_or(Term *term1, Term *term2);
Term *make_bit_xor(Term *term1, Term *term2);
Term *make_neg(Term *term);
Term *make_parens(Expression *expr);

static std::map<Term::Type, std::string> symdic;

void Initialize();

}