#ifndef __EXPRESSION_H_
#define __EXPRESSION_H_

#include "common.h"
#include <map>
#include <deque>

namespace pico {

struct ExpressionList : public std::deque<Expression *> { 
   friend std::ostream& operator<<(std::ostream&, ExpressionList *&);
   void reduce_all();
   void symtables_print();
};

struct Expression {
   enum Type { 
      ASSIGN, IF, UNRESOLVED, INVOKE, FLOAT, INT, STRING, VAR, CHAR,
      ADD, SUB, MULT, DIV, LT, BOOL, EXP, GT, LEQ, GEQ, EQ, NEQ, MOD,
      LOG_AND, LOG_OR, LOG_NOT, BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, NEG, 
   } t;
   union {
      int ival;
      char cval;
      bool bval;
      double fval;
      std::string *strval;
      struct {Expression *func; ExpressionList *expr_list;};
      struct {std::string *name, *type; };
      struct {Expression *expr1, *expr2;};
      Expression *unary;
      struct {std::string *vname; Expression *right_hand; Expression *next;};
      struct {Expression *cond, *if_true; Expression *if_false;};
   };
   unsigned u; // num unresolved exprs in this expr
   Expression *parent; // for symbol lookups
   typedef std::map<std::string, Expression *> SymTable;
   SymTable *symbol_table, *temps;
   std::deque<Expression *> *free_vars;
   Expression(): t(UNRESOLVED), u(1), parent(NULL) {
      // std::cout << "Creating new expression at address " << this << std::endl;
   }
   Expression(double f): t(FLOAT), fval(f), u(0), parent(NULL) {}
   Expression(int i): t(INT), ival(i), u(0), parent(NULL) {}
   Expression(char c): t(CHAR), cval(c), u(0), parent(NULL) {}
   Expression(bool b): t(BOOL), bval(b), u(0), parent(NULL) { }
   Expression(char *str): t(STRING), u(0), parent(NULL) {
      strval = new std::string(strndup(str+1, strlen(str) - 2));
      printf("created string, pointer is %p\n", strval);
      free(str);
   }
   // binary operation
   Expression(Expression *expr1, Expression *expr2, enum Type type): 
      t(type), expr1(expr1), expr2(expr2), u(expr1->u + expr2->u)
      { expr1->parent = expr2->parent = this; }
   // unary operation
   Expression(Expression *expr, enum Type type): t(type), unary(expr), u(expr->u) 
      { unary->parent = this; }
   // invocation
   Expression(Expression *func, ExpressionList *expr_list): 
      t(INVOKE), func(func), expr_list(expr_list), u(func->u - expr_list->size())
      { func->parent = this; }
   // if statement
   Expression(Expression *cond, Expression *if_true, Expression *if_false): 
      t(IF), cond(cond), if_true(if_true), if_false(if_false)
      { cond->set_parent(this); if_true->set_parent(this); if_false->set_parent(this); }
   // assignment statement
   Expression(char *vname, Expression *expr, Expression *next):
      t(ASSIGN), vname(new std::string(vname)), right_hand(expr), next(next)
      {  expr->set_parent(this); next->set_parent(expr); }
   ~Expression();

   friend std::ostream& operator<<(std::ostream& os, Expression *&expr);
   void print_info();

   bool is_eq(Expression *other);

   // static Expression *reduce(Expression *expr);
   Expression *reduce();
   static void reduce_update(Expression *&expr);

   unsigned unresolved();

   bool as_bool(void);
   bool is_binary() const; // useful check to see if it's a binary type
   bool is_unary() const;

   void set_parent(Expression *parent);
   //symbol table stuff
   Expression *sym_lookup(std::string *name);
   void sym_store(std::string *name, Expression *term);
   void sym_update(std::string *str, Expression *term);
   bool sym_contains(std::string *name) const;
   void add_free_var(std::string *str);
   void symtable_print(unsigned level = 0); // bottom-up symbol table print
   void symtable_print_forward(unsigned level = 0); //top-down symbol table print
   void symtable_print_single(); //top-down symbol table print

   // arithmetic operations
   static Expression *add(Expression *expr1, Expression *expr2);
   static Expression *sub(Expression *expr1, Expression *expr2);
   static Expression *mult(Expression *expr1, Expression *expr2);
   static Expression *div(Expression *expr1, Expression *expr2);
   static Expression *mod(Expression *expr1, Expression *expr2);
   static Expression *exp(Expression *expr1, Expression *expr2);
   static Expression *neg(Expression *t);
   static Expression *land(Expression *expr1, Expression *expr2);
   static Expression *lor(Expression *expr1, Expression *expr2);
   static Expression *lnot(Expression *t);
   static Expression *eq(Expression *expr1, Expression *expr2);
   static Expression *neq(Expression *expr1, Expression *expr2);
   static Expression *lt(Expression *expr1, Expression *expr2);
   static Expression *gt(Expression *expr1, Expression *expr2);
   static Expression *leq(Expression *expr1, Expression *expr2);
   static Expression *geq(Expression *expr1, Expression *expr2);
   static Expression *band(Expression *expr1, Expression *expr2);
   static Expression *bor(Expression *expr1, Expression *expr2);
   static Expression *bnot(Expression *expr1, Expression *expr2);
   static Expression *bxor(Expression *expr1, Expression *expr2);

   static Expression *make_add(Expression *expr1, Expression *expr2);
   static Expression *make_sub(Expression *expr1, Expression *expr2);
   static Expression *make_mult(Expression *expr1, Expression *expr2);
   static Expression *make_div(Expression *expr1, Expression *expr2);
   static Expression *make_mod(Expression *expr1, Expression *expr2);
   static Expression *make_exp(Expression *expr1, Expression *expr2);
   static Expression *make_eq(Expression *expr1, Expression *expr2);
   static Expression *make_lt(Expression *expr1, Expression *expr2);
   static Expression *make_gt(Expression *expr1, Expression *expr2);
   static Expression *make_leq(Expression *expr1, Expression *expr2);
   static Expression *make_neq(Expression *expr1, Expression *expr2);
   static Expression *make_geq(Expression *expr1, Expression *expr2);
   static Expression *make_log_and(Expression *expr1, Expression *expr2);
   static Expression *make_log_or(Expression *expr1, Expression *expr2);
   static Expression *make_log_not(Expression *expr);
   static Expression *make_bit_and(Expression *expr1, Expression *expr2);
   static Expression *make_bit_or(Expression *expr1, Expression *expr2);
   static Expression *make_bit_xor(Expression *expr1, Expression *expr2);
   static Expression *make_bit_not(Expression *expr);
   static Expression *make_neg(Expression *expr);
   static Expression *make_var(char *name);
   static Expression *make_var(char *type, char *name);
   static void init();
};

Expression *GLOBAL_UNRESOLVED();

}

#endif