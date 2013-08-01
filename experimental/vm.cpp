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
      AND, OR, RETURN, CALL_LN
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
   friend std::ostream &operator<<(std::ostream &os, Instruction *&inst) {
      if (inst->t != LABEL) os << "\t";
      switch (inst->t) {
         case PUSH:
         { os << "push " << *inst->p; break; }
         case PUSH_ARG:
         { os << "push arg" << inst->u; break; }
         case POP:
         { os << "pop " << inst->u; break; }
         case ADD:
         { os << "add"; break; }
         case SUB:
         { os << "sub"; break; }
         case MULT:
         { os << "mult"; break; }
         case DIV:
         { os << "div"; break; }
         case EQ:
         { os << "eq"; break; }
         case LT:
         { os << "lt"; break; }
         case GT:
         { os << "gt"; break; }
         case LEQ:
         { os << "leq"; break; }
         case GEQ:
         { os << "geq"; break; }
         case NEQ:
         { os << "neq"; break; }
         case NEG:
         { os << "neg"; break; }
         case AND:
         { os << "and"; break; }
         case OR:
         { os << "or"; break; }
         case NOT:
         { os << "not"; break; }
         case RETURN:
         { os << "return" << inst->u; break; }
         case CALL_LN:
         { os << "call " << inst->line << " " << inst->num_args; break; }
         case CALL:
         { os << "call " << *inst->funcname << " " << inst->nargs; break; }
         case GOTO:
         { os << "goto " << *inst->label; break; }
         case GOTO_LN:
         { os << "goto " << inst->u; break; }
         case ELSEGOTO:
         { os << "elsegoto " << *inst->label; break; }
         case ELSEGOTO_LN:
         { os << "elsegoto " << inst->u; break; }
         case LABEL:
         { os << "label \"" << *inst->label << "\""; break; }
      }
      return os;
   }

   static Instruction *_push(int i);
   static Instruction *_push(double d);
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

Instruction *Instruction::_push(int i) {
   Instruction *inst = new Instruction(PUSH);
   inst->p = new Primitive(i);
   return inst;
}
Instruction *Instruction::_push(double d) {
   Instruction *inst = new Instruction(PUSH);
   inst->p = new Primitive(d);
   return inst;
}
Instruction *Instruction::_pusharg(unsigned reg) {
   Instruction *inst = new Instruction(PUSH_ARG);
   inst->u = reg;
   return inst;
}
Instruction *Instruction::_pop(unsigned reg) {
   Instruction *inst = new Instruction(POP);
   inst->u = reg;
   return inst;
}
Instruction *Instruction::_goto(std::string *label) {
   Instruction *inst = new Instruction(GOTO);
   inst->label = label;
   return inst;
}
Instruction *Instruction::_goto(unsigned line) {
   Instruction *inst = new Instruction(GOTO_LN);
   inst->u = line;
   return inst;
}
Instruction *Instruction::_elsegoto(std::string *label) {
   Instruction *inst = new Instruction(ELSEGOTO);
   inst->label = label;
   return inst;
}
Instruction *Instruction::_elsegoto(unsigned line) {
   Instruction *inst = new Instruction(ELSEGOTO_LN);
   inst->u = line;
   return inst;
}
Instruction *Instruction::_label(std::string *label) {
   Instruction *inst = new Instruction(LABEL);
   inst->label = label;
   return inst;
}
Instruction *Instruction::_call(std::string *label, unsigned num_args) {
   Instruction *inst = new Instruction(CALL);
   inst->funcname = label;
   inst->nargs = num_args;
   return inst;
}

Instruction *Instruction::_call(unsigned line, unsigned num_args) {
   Instruction *inst = new Instruction(CALL_LN);
   inst->line = line;
   inst->num_args = num_args;
   return inst;
}

Instruction *Instruction::_return(unsigned nrets) {
   Instruction *inst = new Instruction(RETURN);
   inst->u = nrets;
   return inst;
}
Instruction *Instruction::_add() {
   Instruction *inst = new Instruction(ADD);
   return inst;
}
Instruction *Instruction::_sub() {
   Instruction *inst = new Instruction(SUB);
   return inst;
}
Instruction *Instruction::_mult() {
   Instruction *inst = new Instruction(MULT);
   return inst;
}
Instruction *Instruction::_div() {
   Instruction *inst = new Instruction(DIV);
   return inst;
}
Instruction *Instruction::_eq() {
   Instruction *inst = new Instruction(EQ);
   return inst;
}
Instruction *Instruction::_neq() {
   Instruction *inst = new Instruction(NEQ);
   return inst;
}
Instruction *Instruction::_lt() {
   Instruction *inst = new Instruction(LT);
   return inst;
}
Instruction *Instruction::_gt() {
   Instruction *inst = new Instruction(GT);
   return inst;
}
Instruction *Instruction::_leq() {
   Instruction *inst = new Instruction(LEQ);
   return inst;
}
Instruction *Instruction::_geq() {
   Instruction *inst = new Instruction(GEQ);
   return inst;
}
Instruction *Instruction::_and() {
   Instruction *inst = new Instruction(AND);
   return inst;
}
Instruction *Instruction::_or() {
   Instruction *inst = new Instruction(OR);
   return inst;
}
Instruction *Instruction::_not() {
   Instruction *inst = new Instruction(NOT);
   return inst;
}
Instruction *Instruction::_neg() {
   Instruction *inst = new Instruction(NEG);
   return inst;
}



struct InstructionList {
   std::deque<Instruction *> list;
   std::deque<unsigned> trace;
   std::deque<Primitive> stack, regs;
   std::map<string, unsigned> labels;
   std::vector<std::string> label_list;
   unsigned pc;
   int instcount;
   InstructionList(unsigned num_regs) { 
      for (unsigned i = 0; i < num_regs; ++i) {
         regs.push_back(Primitive(0));
      }
   }
   void run(string start) {
      pc = labels[start];
      while (pc < list.size()) {
         if (instcount++ > 200000) break;
         print_regs();
         cout << "\texecuting " << pc << ": " << list[pc] << endl;
         execute(list[pc]);
         // print_stack();
      }
      if (stack.size() > 0)
         cout << "Top of stack is " << stack.back() << ".\t\t";
   }

   void execute(Instruction *inst) {
      switch (inst->t) {
         case Instruction::PUSH:
         {
            stack.push_back(*inst->p);
            break;
         }
         case Instruction::PUSH_ARG:
         {
            stack.push_back(regs[inst->u]);
            break;
         }
         case Instruction::POP:
         {
            regs[inst->u] = stack.back();
            stack.pop_back();
            break;
         }
         case Instruction::ADD:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 + p2);
            break;
         }
         case Instruction::SUB:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 - p2);
            break;
         }
         case Instruction::MULT:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 * p2);
            break;
         }
         case Instruction::DIV:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 / p2);
            break;
         }
         case Instruction::EQ:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 == p2);
            break;
         }
         case Instruction::LT:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 < p2);
            break;
         }
         case Instruction::GT:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 > p2);
            break;
         }
         case Instruction::LEQ:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 <= p2);
            break;
         }
         case Instruction::GEQ:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 >= p2);
            break;
         }
         case Instruction::NEQ:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 != p2);
            break;
         }
         case Instruction::NEG:
         {
            Primitive p = stack.back();
            stack.pop_back();
            stack.push_back(-p);
            break;
         }
         case Instruction::AND:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 && p2);
            break;
         }
         case Instruction::OR:
         {
            Primitive p2 = stack.back();
            stack.pop_back();
            Primitive p1 = stack.back();
            stack.pop_back();
            stack.push_back(p1 || p2);
            break;
         }
         case Instruction::NOT:
         {
            Primitive p = stack.back();
            stack.pop_back();
            stack.push_back(!p);
            break;
         }
         case Instruction::RETURN:
         {
            pc = trace.back();
            trace.pop_back();
            return;
         }
         case Instruction::CALL:
         {
            for (int i = inst->num_args-1; i >= 0; --i) {
               regs[i] = stack.back();
               stack.pop_back();
            }
            trace.push_back(pc+1);
            pc = labels[*inst->funcname];
            return;
         }
         case Instruction::CALL_LN:
         {
            for (int i = inst->num_args-1; i >= 0; --i) {
               regs[i] = stack.back();
               stack.pop_back();
            }
            trace.push_back(pc+1);
            pc = inst->line;
            return;
         }
         case Instruction::GOTO:
         { pc = labels[*inst->label]; return; }
         case Instruction::GOTO_LN:
         { pc = inst->u; return; }
         case Instruction::ELSEGOTO:
         { 
            Primitive p = stack.back();
            stack.pop_back();
            if (p.t != Primitive::INT)
               throw string("ELSEGOTO called but TOS not an int");
            if (!p.i) {
               pc = labels[*inst->label]; 
               return;
            }
            break; 
         }
         case Instruction::ELSEGOTO_LN:
         { 
            Primitive p = stack.back();
            stack.pop_back();
            if (p.t != Primitive::INT)
               throw string("ELSEGOTO called but TOS not an int");
            if (!p.i) {
               pc = inst->line; 
               return;
            }
            break; 
         }
         case Instruction::LABEL:
            throw string("Shouldn't be seeing labels here!");
      }
      // if we've made it this far, we should increment the program counter
      ++pc;
   }

   void append(Instruction *i) {
      if (i->t == Instruction::LABEL) {
         labels[*i->label] = list.size();
         label_list.push_back(*i->label);
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
      cout << "Registers: [";
      bool first = true;
      for (int i = 0; i < regs.size(); ++i) {
         if (first) first = false; else cout << ", ";
         std::cout << regs[i];
      }
      cout << "] ";
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
         os << "\t" << i << ": " << inst << std::endl;
      }
      return os;
   }
   void load_arg(Primitive p) {
      static unsigned idx = 0;
      regs[idx++] = p;
   }

   void fix_labels() {
      for (int i = 0; i < list.size(); ++i) {
         if (list[i]->t == Instruction::GOTO) {
            list[i] = Instruction::_goto(labels[*list[i]->label]);
         }
         if (list[i]->t == Instruction::ELSEGOTO) {
            list[i] = Instruction::_elsegoto(labels[*list[i]->label]);
         }
         if (list[i]->t == Instruction::CALL) {
            list[i] = Instruction::_call(labels[*list[i]->funcname], list[i]->nargs);
         }
      }
   }
};

}

using namespace pico;

int main(int argc, char const *argv[])
{
   InstructionList instrs(4);
   instrs.append(Instruction::_label(new string("fact.f")));
   instrs.append(Instruction::_pusharg(0));
   instrs.append(Instruction::_push(2));
   instrs.append(Instruction::_lt());
   instrs.append(Instruction::_elsegoto(new string("fact.f$1")));
   instrs.append(Instruction::_pusharg(1));
   instrs.append(Instruction::_return(1));
   instrs.append(Instruction::_label(new string("fact.f$1")));
   instrs.append(Instruction::_pusharg(0));
   instrs.append(Instruction::_push(1));
   instrs.append(Instruction::_sub());
   instrs.append(Instruction::_pusharg(1));
   instrs.append(Instruction::_pusharg(0));
   instrs.append(Instruction::_mult());
   instrs.append(Instruction::_pop(1));
   instrs.append(Instruction::_pop(0));
   instrs.append(Instruction::_goto(new string("fact.f")));
   instrs.append(Instruction::_label(new string("fact")));
   instrs.append(Instruction::_pusharg(0));
   instrs.append(Instruction::_push(1));
   instrs.append(Instruction::_call(new string("fact.f"), 2));
   cout << instrs << endl;
   instrs.print_labels();
   instrs.fix_labels();
   cout << instrs << endl;
   instrs.load_arg(Primitive(15));
   cout << "going to start now..." << endl;
   instrs.run("fact");
   return 0;
}