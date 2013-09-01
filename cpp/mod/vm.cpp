#include "../include/vm.h"


using namespace std;

namespace pico {

Primitive Primitive::operator+(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i + other.i);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i + other.d);
   if (t == INT && other.t == CHAR)
      return Primitive(i + other.c);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d + other.i);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d + other.d);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d + other.c);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c + other.c);
   if (t == CHAR && other.t == INT)
      return Primitive(c + other.i);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c + other.d);
   if (t == STRING && other.t == STRING)
      return Primitive(new string(*str + *other.str));
   char buf[100]; sprintf(buf, "Types %d and %d cannot be added!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator-(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i - other.i);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i - other.d);
   if (t == INT && other.t == CHAR)
      return Primitive(i - other.c);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d - other.i);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d - other.c);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d - other.d);
   if (t == CHAR && other.t == INT)
      return Primitive(c - other.i);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c - other.c);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c - other.d);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be subbed!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator*(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i * other.i);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i * other.d);
   if (t == INT && other.t == CHAR)
      return Primitive(i * other.c);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d * other.i);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d * other.c);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d * other.d);
   if (t == CHAR && other.t == INT)
      return Primitive(c * other.i);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c * other.c);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c * other.d);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be multed!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator/(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i / other.i);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i / other.d);
   if (t == INT && other.t == CHAR)
      return Primitive(i / other.c);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d / other.i);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d / other.c);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d / other.d);
   if (t == CHAR && other.t == INT)
      return Primitive(c / other.i);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c / other.c);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c / other.d);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be divided!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator==(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i == other.i, BOOL);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i == other.d, BOOL);
   if (t == INT && other.t == CHAR)
      return Primitive(i == other.c, BOOL);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d == other.i, BOOL);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d == other.d, BOOL);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d == other.c, BOOL);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c == other.c, BOOL);
   if (t == CHAR && other.t == INT)
      return Primitive(c == other.i, BOOL);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c == other.d, BOOL);
   if (t == STRING && other.t == STRING)
      return Primitive(*str == *other.str, BOOL);
   if (t == BOOL && other.t == BOOL)
      return Primitive(i == other.i, BOOL);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be equated!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator<(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i < other.i, BOOL);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i < other.d, BOOL);
   if (t == INT && other.t == CHAR)
      return Primitive(i < other.c, BOOL);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d < other.i, BOOL);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d < other.d, BOOL);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d < other.c, BOOL);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c < other.c, BOOL);
   if (t == CHAR && other.t == INT)
      return Primitive(c < other.i, BOOL);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c < other.d, BOOL);
   if (t == STRING && other.t == STRING)
      return Primitive(*str < *other.str, BOOL);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be compared!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator>(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i > other.i, BOOL);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i > other.d, BOOL);
   if (t == INT && other.t == CHAR)
      return Primitive(i > other.c, BOOL);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d > other.i, BOOL);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d > other.d, BOOL);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d > other.c, BOOL);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c > other.c, BOOL);
   if (t == CHAR && other.t == INT)
      return Primitive(c > other.i, BOOL);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c > other.d, BOOL);
   if (t == STRING && other.t == STRING)
      return Primitive(*str > *other.str, BOOL);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be compared!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator<=(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i <= other.i, BOOL);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i <= other.d, BOOL);
   if (t == INT && other.t == CHAR)
      return Primitive(i <= other.c, BOOL);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d <= other.i, BOOL);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d <= other.d, BOOL);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d <= other.c, BOOL);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c <= other.c, BOOL);
   if (t == CHAR && other.t == INT)
      return Primitive(c <= other.i, BOOL);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c <= other.d, BOOL);
   if (t == STRING && other.t == STRING)
      return Primitive(*str <= *other.str, BOOL);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be compared!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator>=(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i >= other.i, BOOL);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i >= other.d, BOOL);
   if (t == INT && other.t == CHAR)
      return Primitive(i >= other.c, BOOL);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d >= other.i, BOOL);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d >= other.d, BOOL);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d >= other.c, BOOL);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c >= other.c, BOOL);
   if (t == CHAR && other.t == INT)
      return Primitive(c >= other.i, BOOL);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c >= other.d, BOOL);
   if (t == STRING && other.t == STRING)
      return Primitive(*str >= *other.str, BOOL);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be compared!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator!=(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i != other.i, BOOL);
   if (t == INT && other.t == DOUBLE)
      return Primitive(i != other.d, BOOL);
   if (t == INT && other.t == CHAR)
      return Primitive(i != other.c, BOOL);
   if (t == DOUBLE && other.t == INT)
      return Primitive(d != other.i, BOOL);
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(d != other.d, BOOL);
   if (t == DOUBLE && other.t == CHAR)
      return Primitive(d != other.c, BOOL);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c != other.c, BOOL);
   if (t == CHAR && other.t == INT)
      return Primitive(c != other.i, BOOL);
   if (t == CHAR && other.t == DOUBLE)
      return Primitive(c != other.d, BOOL);
   if (t == STRING && other.t == STRING)
      return Primitive(*str != *other.str, BOOL);
   if (t == BOOL && other.t == BOOL)
      return Primitive(i != other.i, BOOL);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be compared!", t, other.t);
   throw string(buf);
}
Primitive Primitive::exp(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive((int)pow((double)i, (double)other.i));
   if (t == INT && other.t == DOUBLE)
      return Primitive(pow((double)i, (double)other.d));
   if (t == DOUBLE && other.t == INT)
      return Primitive(pow((double)d, (double)other.i));
   if (t == DOUBLE && other.t == DOUBLE)
      return Primitive(pow((double)d, (double)other.d));
   char buf[100]; sprintf(buf, "Types %d and %d cannot be exped!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator%(Primitive other) {
   if (t == INT && other.t == INT)
      return Primitive(i != other.i);
   if (t == INT && other.t == CHAR)
      return Primitive(i != other.c);
   if (t == CHAR && other.t == INT)
      return Primitive(c != other.i);
   if (t == CHAR && other.t == CHAR)
      return Primitive(c % other.c);
   char buf[100]; sprintf(buf, "Types %d and %d cannot be modded!", t, other.t);
   throw string(buf);
}
Primitive Primitive::operator-() {
   if (t == INT) return Primitive(-i);
   if (t == DOUBLE) return Primitive(-d);
   if (t == CHAR) return Primitive(-c);
   char buf[100]; sprintf(buf, "Type %d cannot be negated!", t);
   throw string(buf);
}
Primitive Primitive::operator!() {
   if (t == BOOL) return Primitive(!i, BOOL);
   char buf[100]; sprintf(buf, "Type %d cannot be notted!", t);
   throw string(buf);
}
Primitive Primitive::operator&&(Primitive other) {
   if (t == BOOL && other.t == BOOL) return Primitive(i && other.i, BOOL);
   throw std::string("Can't AND anything except bools");
}
Primitive Primitive::operator||(Primitive other) {
   if (t == BOOL && other.t == BOOL) return Primitive(i || other.i, BOOL);
   throw std::string("Can't OR anything except bools");
}
std::ostream &operator<<(std::ostream &os, Primitive const &p) {
   if (p.t == Primitive::INT) os << p.i;
   else if (p.t == Primitive::DOUBLE) os << p.d;
   else if (p.t == Primitive::CHAR) os << p.c;
   else if (p.t == Primitive::DOUBLE) os << *p.str;
   else if (p.t == Primitive::BOOL) os << (p.i ? "TRUE" : "FALSE");
   return os;
}

ostream &operator<<(ostream &os, Instruction *&inst) {
      if (inst->t != Instruction::LABEL) os << "\t";
      switch (inst->t) {
         case Instruction::PUSH:
         { os << "push " << *inst->p; break; }
         case Instruction::PUSH_ARG:
         { os << "push arg" << inst->u; break; }
         case Instruction::POP:
         { os << "pop " << inst->u; break; }
         case Instruction::ADD:
         { os << "add"; break; }
         case Instruction::SUB:
         { os << "sub"; break; }
         case Instruction::MULT:
         { os << "mult"; break; }
         case Instruction::DIV:
         { os << "div"; break; }
         case Instruction::MOD:
         { os << "mod"; break; }
         case Instruction::EXP:
         { os << "exp"; break; }
         case Instruction::EQ:
         { os << "eq"; break; }
         case Instruction::LT:
         { os << "lt"; break; }
         case Instruction::GT:
         { os << "gt"; break; }
         case Instruction::LEQ:
         { os << "leq"; break; }
         case Instruction::GEQ:
         { os << "geq"; break; }
         case Instruction::NEQ:
         { os << "neq"; break; }
         case Instruction::NEG:
         { os << "neg"; break; }
         case Instruction::AND:
         { os << "and"; break; }
         case Instruction::OR:
         { os << "or"; break; }
         case Instruction::NOT:
         { os << "not"; break; }
         case Instruction::RETURN:
         { os << "return " << inst->u; break; }
         case Instruction::CALL_LN:
         { os << "call " << inst->line << " " << inst->num_args; break; }
         case Instruction::CALL:
         { os << "call " << *inst->funcname << " " << inst->nargs; break; }
         case Instruction::GOTO:
         { os << "goto " << *inst->label; break; }
         case Instruction::GOTO_LN:
         { os << "goto " << inst->u; break; }
         case Instruction::ELSEGOTO:
         { os << "elsegoto " << *inst->label; break; }
         case Instruction::ELSEGOTO_LN:
         { os << "elsegoto " << inst->u; break; }
         case Instruction::LABEL:
         { os << "label \"" << *inst->label << "\""; break; }
      }
      return os;
   }

Instruction *Instruction::_push(int i, Primitive::Type t) {
   Instruction *inst = new Instruction(PUSH);
   inst->p = new Primitive(i, t);
   return inst;
}
Instruction *Instruction::_push(double d) {
   Instruction *inst = new Instruction(PUSH);
   inst->p = new Primitive(d);
   return inst;
}
Instruction *Instruction::_push(string *str) {
   Instruction *inst = new Instruction(PUSH);
   inst->p = new Primitive(str);
   return inst;
}
Instruction *Instruction::_push(char c) {
   Instruction *inst = new Instruction(PUSH);
   inst->p = new Primitive(c);
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

Instruction *Instruction::_mod() {
   Instruction *inst = new Instruction(MOD);
   return inst;
}

Instruction *Instruction::_exp() {
   Instruction *inst = new Instruction(EXP);
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



InstructionList::InstructionList(unsigned num_regs) { 
   for (unsigned i = 0; i < num_regs; ++i) {
      regs.push_back(Primitive(0));
   }
   fixed_list = false;
}
void InstructionList::run(string start) {
   pc = labels[start];
   unsigned max_stack;
   while (pc < list.size()) {
      if (instcount++ > 200000) break;
      // print_regs();
      // cout << "\texecuting " << pc << ": " << list[pc] << endl;
      execute(list[pc]);
      if (stack.size() > max_stack) max_stack = stack.size();
      // print_stack();
   }
   if (stack.size() > 0)
      cout << "Top of stack is " << stack.back() << ".\t\t";
   cout << "max stack size was " << max_stack << endl;
}

void InstructionList::execute(Instruction *inst) {
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
      case Instruction::MOD:
      {
         Primitive p2 = stack.back();
         stack.pop_back();
         Primitive p1 = stack.back();
         stack.pop_back();
         stack.push_back(p1 % p2);
         break;
      }
      case Instruction::EXP:
      {
         Primitive p2 = stack.back();
         stack.pop_back();
         Primitive p1 = stack.back();
         stack.pop_back();
         stack.push_back(p1.exp(p2));
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
         if (p.t != Primitive::BOOL)
            throw string("ELSEGOTO called but TOS not a bool");
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
         if (p.t != Primitive::BOOL)
            throw string("ELSEGOTO called but TOS not a bool");
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

void InstructionList::append(Instruction *i) {
   cout << "Appending: " << i << endl;
   initial_list.push_back(i);
}

void InstructionList::print_stack() {
   for (int i = 0; i < stack.size(); ++i) {
      std::cout << i << ": " << stack[i] << std::endl;
   }
}
void InstructionList::print_regs() {
   cout << "Registers: [";
   bool first = true;
   for (int i = 0; i < regs.size(); ++i) {
      if (first) first = false; else cout << ", ";
      std::cout << regs[i];
   }
   cout << "] ";
}

void InstructionList::print_labels() {
   std::map<std::string, unsigned>::iterator it;
   for (int i = 0; i < label_list.size(); ++i) {
      std::cout << label_list[i] << ": " << labels[label_list[i]] << std::endl;
   }
}

std::ostream &operator<<(std::ostream &os, InstructionList const &il) {
   std::deque<Instruction *>::iterator it;
   unsigned line_no = 0;
   if (il.fixed_list)
      for (int i = 0; i < il.list.size(); ++i) {
         Instruction *inst = il.list[i];
         os << "\t" << i << ": " << inst << std::endl;
      }
   else
      for (int i = 0; i < il.initial_list.size(); ++i) {
         Instruction *inst = il.initial_list[i];
         if (inst->t != Instruction::LABEL)
            os << "\t" << line_no++ << ": " << inst << std::endl;
         else
            os << "\t" << inst << std::endl;
      }
   return os;
}
void InstructionList::load_arg(Primitive p) {
   static unsigned idx = 0;
   regs[idx++] = p;
}

void InstructionList::fix_labels() {
   // first pass: get line numbers from labels, remove the labels

   for (int i = 0; i != initial_list.size(); ++i) {   
      Instruction *inst = initial_list[i];
      if (inst->t == Instruction::LABEL) {

         labels[*inst->label] = list.size();
         label_list.push_back(*inst->label);
      } else {
         list.push_back(inst);
      }
   }
   // second pass: replace function calls with line numbers
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
   fixed_list = true;
}

}

// #define VIRTUAL_MACHINE_TEST
#ifdef VIRTUAL_MACHINE_TEST
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
   try {
      cout << instrs << endl;
      instrs.print_labels();
      instrs.fix_labels();
      cout << instrs << endl;
      instrs.load_arg(Primitive(11));
      cout << "going to start now..." << endl;
      instrs.run("fact");
   } catch (string s) {
      cout << "Exception: " << s << endl;
   }
   return 0;
}
#endif