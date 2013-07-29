#ifndef __EXPRESSION_H_
#define __EXPRESSION_H_

#include "common.h"
#include <map>
#include <deque>

namespace pico {

struct ExpressionList : public std::deque<Expression *> { void print(); };
struct TermList : public std::deque<Term *> { void print(); };

void upInd(); void dnInd(); void prindent();

struct Var {
   std::string *name, *type;
   Var(char *name): name(new std::string(name)), type(NULL) { free(name); }
   Var(char *name, char *type): 
      name(new std::string(name)), 
      type(new std::string(type)) {free(name), free(type); }
   ~Var();
};

struct Term {
   enum Type { 
      INVOKE, FLOAT, INT, STRING, VAR, CHAR,
      ADD, SUB, MULT, DIV, LT, BOOL, EXP,
      GT, LEQ, GEQ, EQ, NEQ, MOD, UNRESOLVED,
      LOG_AND, LOG_OR, LOG_NOT, BIT_AND,
      BIT_OR, BIT_XOR, BIT_NOT, NEG, PARENS,
   } t;
   union {
      int ival;
      char cval;
      bool bval;
      double fval;
      std::string *strval; 
      struct {Term *func; TermList *term_list;} invoke;
      Var *var;
      struct {Term *term1, *term2;} binary;
      Term *unary;
      Expression *expr;
   };
   unsigned u; // number of unresolved subsymbols

   // literal instantiations
   Term(): t(UNRESOLVED), u(1) {} // for unresolved terms
   Term(double f): t(FLOAT), fval(f), u(0) {}
   Term(int i): t(INT), ival(i), u(0) {}
   Term(char c): t(CHAR), cval(c), u(0) {}
   Term(bool b): t(BOOL), bval(b), u(0) {}
   Term(char *str): t(STRING), u(0) {
      strval = new std::string(strndup(str+1, strlen(str) - 2));
      printf("created string, pointer is %p\n", strval);
      free(str);
   }

   // variable instantiation
   Term(Var *var): t(VAR), var(var), u(1) {}

   // recursive instantiation
   Term(Term *term1, Term *term2, enum Type type): t(type), u(term1->u + term2->u)
      { binary.term1 = term1; binary.term2 = term2; }
   Term(Expression *expr); // defined in cpp
   Term(Term *term, enum Type type): t(type), unary(term), u(term->u) {}
   Term(Term *func, TermList *term_list): t(INVOKE), u(func->u - term_list->size())
      { invoke.func = func; invoke.term_list = term_list; }
   ~Term();

   // utils
   void print();
   void print_info();

   bool is_eq(Term *other);

   // Evaluation functions
   bool stop; // flag set when term should not be resolved further
   static Term *eval(Term *term);
   static Term *add(Term *t1, Term *t2);
   static Term *sub(Term *t1, Term *t2);
   static Term *mult(Term *t1, Term *t2);
   static Term *div(Term *t1, Term *t2);
   static Term *mod(Term *t1, Term *t2);
   static Term *exp(Term *t1, Term *t2);
   static Term *neg(Term *t);
   static Term *land(Term *t1, Term *t2);
   static Term *lor(Term *t1, Term *t2);
   static Term *lnot(Term *t);
   static Term *eq(Term *t1, Term *t2);
   static Term *neq(Term *t1, Term *t2);
   static Term *lt(Term *t1, Term *t2);
   static Term *gt(Term *t1, Term *t2);
   static Term *leq(Term *t1, Term *t2);
   static Term *geq(Term *t1, Term *t2);
   static Term *band(Term *t1, Term *t2);
   static Term *bor(Term *t1, Term *t2);
   static Term *bnot(Term *t1, Term *t2);
   static Term *bxor(Term *t1, Term *t2);
   static unsigned unresolved(Term *term); // recursively counts the number of unresolved symbols in tree
   bool to_bool(void);
};

struct Expression {
   enum Type { TERM, ASSIGN, IF } t;
   union {
      Term *term;
      struct {Var *var; Term *term; Expression *next;} assign;
      struct {Term *cond, *if_true; Expression *if_false;} if_;
   };
   unsigned u;
   Expression(Term *term): t(TERM), term(term) { }
   Expression(Term *cond, Term *if_true, Expression *if_false)
      { t = IF; if_.cond = cond; if_.if_true = if_true; if_.if_false = if_false; }
   Expression(char *vname, Term *term, Expression *next)
      { t = ASSIGN; assign.var = new Var(vname); assign.term = term; assign.next = next; }
   ~Expression();
   void print();
   bool is_eq(Expression *other);
   static Term *eval(Expression *expr);
   static unsigned unresolved(Expression *expr);
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

void Initialize();

}

#endif