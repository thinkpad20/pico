#ifndef __EXPRESSION_H_
#define __EXPRESSION_H_

#include "common.h"
#include <map>
#include <deque>

namespace pico {

struct ExpressionList : public std::deque<Expression *> { 
   void print(); 
   void reduce_all();
};

void upInd(); void dnInd(); void prindent();

struct Var {
   std::string *name, *type;
   Var(char *name): name(new std::string(name)), type(NULL) { free(name); }
   Var(char *name, char *type): 
      name(new std::string(name)), 
      type(new std::string(type)) {free(name), free(type); }
   ~Var();
};

struct Expression {
   enum Type { 
      ASSIGN, IF, UNRESOLVED,       
      INVOKE, FLOAT, INT, STRING, VAR, CHAR,
      ADD, SUB, MULT, DIV, LT, BOOL, EXP,
      GT, LEQ, GEQ, EQ, NEQ, MOD,
      LOG_AND, LOG_OR, LOG_NOT, BIT_AND,
      BIT_OR, BIT_XOR, BIT_NOT, NEG, } t;
   union {
      int ival;
      char cval;
      bool bval;
      double fval;
      std::string *strval; 
      struct {Expression *func; ExpressionList *expr_list;} invoke;
      struct {std::string *name, *type; } var;
      struct {Expression *expr1, *expr2;} binary;
      Expression *unary;
      struct {std::string *vname; Expression *expr; Expression *next;} assign;
      struct {Expression *cond, *if_true; Expression *if_false;} if_;
   };
   unsigned u; // num unresolved exprs in this expr
   Expression *parent; // for symbol lookups
   typedef std::map<std::string, Expression *> SymTable;
   SymTable symbol_table, temps;
   std::deque<Expression *> free_vars;
   Expression(): t(UNRESOLVED), u(1), parent(NULL) {}
   Expression(double f): t(FLOAT), fval(f), u(0), parent(NULL) {}
   Expression(int i): t(INT), ival(i), u(0), parent(NULL) {}
   Expression(char c): t(CHAR), cval(c), u(0), parent(NULL) {}
   Expression(bool b): t(BOOL), bval(b), u(0), parent(NULL) {}
   Expression(char *str): t(STRING), u(0), parent(NULL) {
      strval = new std::string(strndup(str+1, strlen(str) - 2));
      printf("created string, pointer is %p\n", strval);
      free(str);
   }
   // binary operation
   Expression(Expression *expr1, Expression *expr2, enum Type type): t(type), u(expr1->u + expr2->u)
      { binary.expr1 = expr1; binary.expr2 = expr2; binary.expr1->parent = binary.expr2->parent = this; }
   // unary operation
   Expression(Expression *expr, enum Type type): t(type), unary(expr), u(expr->u) { unary->parent = this; }
   // invocation
   Expression(Expression *func, ExpressionList *expr_list): t(INVOKE), u(func->u - expr_list->size())
      { invoke.func = func; invoke.expr_list = expr_list; func->parent = this; }
   // if statement
   Expression(Expression *cond, Expression *if_true, Expression *if_false)
      { t = IF; if_.cond = cond; if_.if_true = if_true; if_.if_false = if_false; 
         cond->parent = if_true->parent = if_false->parent = this; }
   // assignment statement
   Expression(char *vname, Expression *expr, Expression *next)
      { t = ASSIGN; assign.vname = new std::string(vname); 
         assign.expr = expr; assign.next = next; next->parent = this; }
   ~Expression();

   void print();
   void print_info();

   bool is_eq(Expression *other);

   // static Expression *reduce(Expression *expr);
   Expression *reduce();
   
   unsigned unresolved();

   bool to_bool(void);

   //symbol table stuff
   Expression *sym_lookup(std::string *name);
   void sym_store(std::string *name, Expression *term);
   void sym_update(std::string *str, Expression *term);
   bool sym_contains(std::string *name);
   void add_free_var(std::string *str);
   void symtable_print(unsigned level = 0);

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
};

extern Expression *GLOBAL_UNRESOLVED;

void Initialize();

}

#endif