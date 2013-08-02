#ifndef __PICO_VM_H_
#define __PICO_VM_H_

#include "common.h"
#include <deque>
#include <map>
#include <vector>
#include <list>

namespace pico {

struct Primitive {
   enum Type { INT, DOUBLE, CHAR, STRING, EMPTY, BOOL } t;
   union { int i; char c; double d; std::string *str; };
   Primitive(int i, Type t = INT): t(t), i(i) {}
   Primitive(char c): t(CHAR), c(c) {}
   Primitive(double d): t(DOUBLE), d(d) {}
   Primitive(std::string *str): t(STRING), str(str) {}
   Primitive operator+(Primitive other);
   Primitive operator-(Primitive other);
   Primitive operator*(Primitive other);
   Primitive operator/(Primitive other);
   Primitive operator==(Primitive other);
   Primitive operator<(Primitive other);
   Primitive operator>(Primitive other);   
   Primitive operator<=(Primitive other);
   Primitive operator>=(Primitive other);
   Primitive operator!=(Primitive other);
   Primitive operator-();
   Primitive operator!();
   Primitive operator&&(Primitive other);
   Primitive operator||(Primitive other);
   Primitive exp(Primitive other);
   Primitive operator%(Primitive other);
   friend std::ostream &operator<<(std::ostream &os, Primitive const &p);
};

struct Instruction {
   enum Type {PUSH, POP, GOTO, ELSEGOTO, LABEL, GOTO_LN, 
      ELSEGOTO_LN, PUSH_ARG, ADD, SUB, MULT, DIV, 
      NEG, CALL, EQ, LT, GT, LEQ, GEQ, NOT, NEQ, 
      AND, OR, RETURN, CALL_LN, MOD, EXP
   } t;
   union {
      Primitive *p;
      unsigned u;
      std::string *label;
      struct {std::string *funcname; unsigned nargs; };
      struct {unsigned line, num_args; };
   };
   Instruction(Type t): t(t) {}
   Instruction(Type t, unsigned u): t(t), u(u) {}
   Instruction(Type t, std::string *label): t(t), label(label) {}
   friend std::ostream &operator<<(std::ostream &os, Instruction *&inst);
   static Instruction *_push(int i, Primitive::Type t = Primitive::INT);
   static Instruction *_push(double d);
   static Instruction *_push(std::string *str);
   static Instruction *_push(char c);
   static Instruction *_pusharg(unsigned reg);
   static Instruction *_pop(unsigned reg);
   static Instruction *_goto(std::string *label);
   static Instruction *_goto(unsigned line);
   static Instruction *_elsegoto(std::string *label);
   static Instruction *_elsegoto(unsigned line);
   static Instruction *_label(std::string *label);
   static Instruction *_call(std::string *label, unsigned num_args);
   static Instruction *_call(unsigned line, unsigned num_args);
   static Instruction *_return(unsigned nrets);
   static Instruction *_add();
   static Instruction *_sub();
   static Instruction *_mult();
   static Instruction *_div();
   static Instruction *_mod();
   static Instruction *_exp();
   static Instruction *_eq();
   static Instruction *_neq();
   static Instruction *_lt();
   static Instruction *_gt();
   static Instruction *_leq();
   static Instruction *_geq();
   static Instruction *_and();
   static Instruction *_or();
   static Instruction *_not();
   static Instruction *_neg();
};

struct InstructionList {
   std::deque<Instruction *> initial_list;
   std::deque<Instruction *> list;
   std::deque<unsigned> trace;
   std::deque<Primitive> stack, regs;
   std::map<std::string, unsigned> labels;
   std::vector<std::string> label_list;
   unsigned pc;
   int instcount;
   bool fixed_list;
   InstructionList(unsigned num_regs);
   void run(std::string start);
   void execute(Instruction *inst);
   void append(Instruction *i);
   void print_stack();
   void print_regs();
   void print_labels();
   friend std::ostream &operator<<(std::ostream &os, InstructionList const &il);
   void load_arg(Primitive p);
   void fix_labels();
};

}

#endif