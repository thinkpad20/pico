#include "vm.h"
#include <deque>
#include <vector>
#include <map>

using namespace std;

namespace pico {

struct Primitive {
   enum Type { INT, DOUBLE, EMPTY } t;
   union { int i; double d; };
   Primitive(int i): t(INT), i(i) {}
   Primitive(double d): t(DOUBLE), d(d) {}
   Primitive operator+(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i + other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i + other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d + other.i);
      return Primitive(d + other.d);
   }
   Primitive operator-(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i - other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i - other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d - other.i);
      return Primitive(d - other.d);
   }
   Primitive operator*(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i * other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i * other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d * other.i);
      return Primitive(d * other.d);
   }
   Primitive operator/(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i / other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i / other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d / other.i);
      return Primitive(d / other.d);
   }
      Primitive operator==(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i == other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i == other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d == other.i);
      return Primitive(d == other.d);
   }
      Primitive operator<(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i < other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i < other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d < other.i);
      return Primitive(d < other.d);
   }
      Primitive operator>(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i > other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i > other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d > other.i);
      return Primitive(d > other.d);
   }
      Primitive operator<=(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i <= other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i <= other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d <= other.i);
      return Primitive(d <= other.d);
   }
      Primitive operator>=(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i >= other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i >= other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d >= other.i);
      return Primitive(d >= other.d);
   }
   Primitive operator!=(Primitive other) {
      if (t == INT && other.t == INT)
         return Primitive(i != other.i);
      if (t == INT && other.t == DOUBLE)
         return Primitive(i != other.d);
      if (t == DOUBLE && other.t == INT)
         return Primitive(d != other.i);
      return Primitive(d != other.d);
   }
   Primitive operator-() {
      if (t == INT) return Primitive(-i);
      return Primitive(-d);
   }
   Primitive operator!() {
      if (t == INT) return Primitive(!i);
      throw std::string("Can't NOT a double");
   }
   Primitive operator&&(Primitive other) {
      if (t == INT && other.t == INT) return Primitive(i && other.i);
      throw std::string("Can't AND a double");
   }
   Primitive operator||(Primitive other) {
      if (t == INT && other.t == INT) return Primitive(i || other.i);
      throw std::string("Can't OR a double");
   }
   friend std::ostream &operator<<(std::ostream &os, Primitive const &p) {
      if (p.t == INT) os << p.i;
      else os << p.d;
      return os;
   }
};

struct Instruction {
   enum Type {PUSH, POP, GOTO, ELSEGOTO, LABEL, GOTO_LN, 
      ELSEGOTO_LN, PUSH_ARG, ADD, SUB, MULT, DIV, 
      NEG, CALL, EQ, LT, GT, LEQ, GEQ, NOT, NEQ, 
      AND, OR, RETURN,
   } t;
   std::deque<Primitive> *stack, *regs;
   std::map<std::string, unsigned> *label_map;
   unsigned *prog_ctr, *return_to;
   Instruction() {}
   virtual void execute() = 0;
   virtual void print(std::ostream &os = cout) = 0;
};

struct Call : Instruction {
   std::string label;
   unsigned num_args;
   Call(std::string label, unsigned num_args): label(label), num_args(num_args) 
      { t = CALL; }
   void execute() {
      for (int i = 0; i < num_args; ++i) {
         (*regs)[i] = stack->back();
         stack->pop_back();
      }
      *return_to = *prog_ctr + 1;
      *prog_ctr = (*label_map)[label];
   }
   void print(std::ostream &os) {
      os << "\tcall " << label << " " << num_args; 
   }
};

struct Return : Instruction {
   unsigned num_args;
   Return(unsigned num_args): num_args(num_args) { t = RETURN; }
   void execute() {
      for (int i = 0; i < num_args; ++i) {
         stack->push_back((*regs)[i]);
      }
      *prog_ctr = *return_to;
   }
   void print(std::ostream &os) {
      os << "\treturn " << num_args; 
   }
};

struct Push : Instruction {
   Primitive p;
   Push(Primitive p) : p(p) { t = PUSH; }
   void execute() {
      stack->push_back(p);
      prog_ctr++;
   }
   void print(std::ostream &os) {
      os << "\tpush " << p; 
   }
};

struct PushArg : Instruction {
   unsigned reg;
   PushArg(unsigned reg): reg(reg) { t = PUSH_ARG; }
   void execute() {
      stack->push_back((*regs)[reg]);
      prog_ctr++;
   }
   void print(std::ostream &os) {
      os << "\tpush arg" << reg; 
   }
};

struct Pop : Instruction {
   unsigned reg;
   Pop(unsigned reg) : reg(reg) { t = POP; }
   void execute() {
      Primitive p = stack->back();
      // if (regs->size() < reg) regs->resize(reg + 1);
      (*regs)[reg] = p;
      stack->pop_back();
      prog_ctr++;
   }
   void print(std::ostream &os) {
      os << "\tpop " << reg;
      
   }
};

struct Goto : Instruction {
   string label;
   Goto(string label): label(label) { t = GOTO; }
   void execute() {
      *prog_ctr = (*label_map)[label];
   }
   void print(std::ostream &os) {
      os << "\tgoto '" << label << "\'";
      
   }
};

struct GotoLine : Instruction {
   unsigned line;
   GotoLine(unsigned line): line(line) { t = GOTO_LN; }
   void execute() { *prog_ctr = line; }
};

struct ElseGoto : Instruction {
   string label;
   ElseGoto(string label): label(label) { t = GOTO; }
   void execute() {
      if (stack->back().t != Primitive::INT)
         throw std::string("Error: not an int on the top of the stack");
      if (stack->back().i)
         (*prog_ctr)++;
      else
         *prog_ctr = (*label_map)[label]; 
   }
   void print(std::ostream &os) {
      os << "\telsegoto '" << label << "'";
      
   }
};

struct ElseGotoLine : Instruction {
   unsigned line;
   ElseGotoLine(unsigned line): line(line) { t = GOTO_LN; }
};

struct Label : Instruction {
   string name;
   Label(string name): name(name) { t = LABEL; }
   void execute() { (*label_map)[name] = ++*prog_ctr; }
   void print(std::ostream &os) {
      os << name << ":";
      
   }
};

struct Add : Instruction {
   Add() { t = ADD; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 + arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tadd";
   }
};

struct Sub : Instruction {
   Sub() { t = SUB; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 - arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tsub";
      
   }
};

struct Mult : Instruction {
   Mult() { t = MULT; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 * arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tmult";
      
   }
};

struct Div : Instruction {
   Div() { t = DIV; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 / arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tdiv";
      
   }
};

struct Eq : Instruction {
   Eq() { t = EQ; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 == arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\teq";
   }
};

struct Lt : Instruction {
   Lt() { t = LT; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 < arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tlt";
   }
};
struct Gt : Instruction {
   Gt() { t = GT; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 > arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tgt";
   }
};
struct Leq : Instruction {
   Leq() { t = LEQ; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 <= arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tleq";
   }
};
struct Geq : Instruction {
   Geq() { t = GEQ; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 >= arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tgeq";
   }
};

struct And : Instruction {
   And() { t = AND; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 && arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tand";
   }
};

struct Or : Instruction {
   Or() { t = OR; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 || arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tor";
   }
};

struct Not : Instruction {
   Not() { t = NOT; }
   void execute() {
      stack->push_back(!stack->back());
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tnot";
   }
};

struct Neq : Instruction {
   Neq() { t = NEQ; }
   void execute() {
      Primitive arg1 = stack->back();
      stack->pop_back();
      Primitive arg2 = stack->back();
      stack->pop_back();
      stack->push_back(arg1 != arg2);
      (*prog_ctr)++;
   }
   void print(std::ostream &os) {
      os << "\tneq";
   }
};

struct InstructionList {
   std::deque<Instruction *> list;
   std::deque<Primitive> stack, regs;
   std::map<string, unsigned> labels;
   std::vector<std::string> label_list;
   unsigned prog_ctr, return_to;
   InstructionList(unsigned num_regs) { 
      for (unsigned i = 0; i < num_regs; ++i) {
         regs.push_back(Primitive(0));
      }
   }
   void start() {
      unsigned &pc = prog_ctr;
      while (pc < list.size()) {
         cout << "Top of stack is: " << stack.back() << "\nExecuting: ";
         list[pc]->print(); cout << endl;
         int i;
         cin >> i;
         list[pc]->execute();
         print_stack();
      }
   }
   void push(Instruction *i) {
      i->stack = &stack;
      i->regs = &regs;
      i->label_map = &labels;
      i->prog_ctr = &prog_ctr;
      i->prog_ctr = &return_to;
      if (i->t == Instruction::LABEL) {
         Label *lbl = dynamic_cast<Label *>(i);
         labels[lbl->name] = list.size();
         label_list.push_back(lbl->name);
      } else {
         list.push_back(i); 
      }
   }
   void print_stack() {
      for (int i = 0; i < stack.size(); ++i) {
         std::cout << i << ": " << stack[i] << std::endl;
      }
   }
   void print_regs() {
      for (int i = 0; i < regs.size(); ++i) {
         std::cout << i << ": " << stack[i] << std::endl;
      }
   }

   void print_labels() {
      std::map<std::string, unsigned>::iterator it;
      for (int i = 0; i < label_list.size(); ++i) {
         std::cout << label_list[i] << ": " << labels[label_list[i]] << std::endl;
      }
   }
   friend std::ostream &operator<<(std::ostream &os, InstructionList const &il) {
      std::deque<Instruction *>::iterator it;
      for (int i = 0; i < il.list.size(); ++i) {
         Instruction *inst = il.list[i];
         os << i << ": ";
         inst->print();
         os << std::endl;
      }
      return os;
   }
   void load_arg(Primitive p) {
      static unsigned idx = 0;
      regs[idx++] = p;
   }
};

}

using namespace pico;

int main(int argc, char const *argv[])
{
   InstructionList instrs(20);
   instrs.push(new Label("fact.f"));
   instrs.push(new PushArg(0));
   instrs.push(new Push(2));
   instrs.push(new Lt());
   instrs.push(new ElseGoto("fact.f$1"));
   instrs.push(new PushArg(1));
   instrs.push(new Return(1));
   instrs.push(new Label("fact.f$1"));
   instrs.push(new PushArg(0));
   instrs.push(new Push(1));
   instrs.push(new Sub());
   instrs.push(new PushArg(1));
   instrs.push(new PushArg(0));
   instrs.push(new Mult());
   instrs.push(new Pop(1));
   instrs.push(new Pop(0));
   instrs.push(new Goto("fact.f"));
   instrs.push(new Label("fact"));
   instrs.push(new PushArg(0));
   instrs.push(new Push(1));
   instrs.push(new Call("fact.f", 2));
   cout << instrs << endl;
   instrs.print_labels();
   instrs.load_arg(Primitive(10));
   instrs.start();
   return 0;
}