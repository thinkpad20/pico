#include "../include/compile.h"
#include <string>
#include <deque>
#include <map>
#include <vector>

using namespace std;

namespace pico {

static void compile(Expression *expr, InstructionList &ilist);
static void compile_assign(Expression *expr, InstructionList &ilist);
static void compile_if(Expression *expr, InstructionList &ilist);
static void compile_literal(Expression *expr, InstructionList &ilist);
static void compile_unbound(Expression *expr, InstructionList &ilist);

string full_name();
string full_name(string name);

struct Context {
   map<string, string> symbol_table;
   vector<string> args;
   string name;
   Context(string name): name(name) {}
   bool contains(string name) { 
      return symbol_table.find(name) != symbol_table.end(); 
   }
   string get(string name) {
      return symbol_table[name];
   }
   void set(string name, string full_name) {
      symbol_table[name] = full_name;
   }
};

static deque<Context> context_stack;

void push_symbol_table(string name) {
   context_stack.push_back(Context(name));
   if (context_stack.size() > 1) {
      context_stack[context_stack.size()-2].set(name, full_name());
      cout << "added " << name << " -> " << full_name() << " mapping to "
           << context_stack[context_stack.size()-2].name << "'s symbol_table" << endl;
   }
}

void pop_symbol_table() { 
   context_stack.pop_back(); 
}

string full_name() {
   string res = context_stack[0].name;
   for (int i = 1; i < context_stack.size(); ++i)
      res += "." + context_stack[i].name;
   return res;
}

string full_name(string name) {
   string res = "";
   for (int i = 0; i < context_stack.size(); ++i) {
      if (context_stack[i].contains(name))
         return res + context_stack[i].name;
      else
         res += context_stack[i].name + ".";
   }
   res += name;
   return res;
}

namestr store_arg(string arg_name) {
   size_t num = context_stack.back().args.size();
   char name[100]; sprintf(name, "arg %lu", num);
   string namestr = string(name);
   context_stack.back().set(arg_name, namestr);
   return namestr;
}

ostream &operator<<(ostream &os, const Assignment &asn) {
   os << asn.vname << " -> " << asn.rval;
   return os;
}

deque<Assignment> get_assignments(Expression *expr, deque<Assignment> &assignments) {
   if (expr->t != Expression::ASSIGN) {
      return assignments;
   }
   Expression *rhs = expr->rhs;
   while (rhs->t == Expression::ASSIGN) {
      get_assignments(rhs, assignments);
      rhs = rhs->next;
   }
   assignments.push_back(Assignment(*expr->alias, rhs));
   return assignments;
}

map<string, Instruction *(*)()> bin_primitives;
map<string, Instruction *(*)()> un_primitives;
map<string, InstructionList> functions;

static void compile_if(Expression *expr, InstructionList &ilist) {
   cout << "compile_if hasn't been written yet" << endl;
}

static void compile_unbound(Expression *expr, InstructionList &ilist) {
   cout << "trying to compile unbound " << expr << endl;
   cout << "compile_unbound hasn't been written yet" << endl;

}

static void compile_literal(Expression *expr, InstructionList &ilist) {
   if (expr->t == Expression::INT) {
      ilist.append(Instruction::_push(expr->i));
   } else if (expr->t == Expression::FLOAT) {
      ilist.append(Instruction::_push(expr->f));
   } else if (expr->t == Expression::CHAR) {
      ilist.append(Instruction::_push(expr->c));
   } else if (expr->t == Expression::STRING) {
      ilist.append(Instruction::_push(expr->str));
   } else if (expr->t == Expression::BOOL) {
      ilist.append(Instruction::_push(expr->b, Primitive::BOOL));
   } else {
      throw string("Trying to compile a literal but idk what this is");
   }
}

static void compile_call(Expression *expr, InstructionList &ilist) {
   size_t nargs = expr->args->size();
   for (int i = 0; i < nargs; ++i)
      compile((*expr->args)[i], ilist);
   if (expr->is_symbol()) {
      if (nargs == 1 && un_primitives.find(*expr->func->str) != un_primitives.end())
         ilist.append(un_primitives[*expr->func->str]());
      else if (bin_primitives.find(*expr->func->str) != bin_primitives.end())
         ilist.append(bin_primitives[*expr->func->str]());
      else {
         cout << "trying to compile " << expr << endl;
         throw string("We can't deal with custom symbols yet");
      }
   } else {
      // then it's either an anonymous function, unbound, or a variable.
      if (expr->func->t == Expression::VAR) {
         string fullname = full_name(*expr->func->str);
         ilist.append(Instruction::_call(new string(fullname), nargs));
      } else if (expr->func->func->t == Expression::VAR) {
         string fullname = full_name(*expr->func->func->str);
         ilist.append(Instruction::_call(new string(fullname), nargs));
      } else {
         cout << "trying to compile " << expr << " func = " << expr->func->func << ", func->t = " << expr->func->t << endl;
         throw string("We can't deal with anonymous functions yet");
      }
   }
}

static void compile_assign(Expression *expr, InstructionList &ilist) {
   push_symbol_table(*expr->alias);
   Expression *rhs = expr->rhs;
   while (rhs->t == Expression::ASSIGN) {
      compile_assign(rhs, ilist);
      rhs = rhs->next;
   }
   // next go through the IF statements, since they also define new namespaces.
   while (rhs->t == Expression::IF) {
      compile_if(rhs, ilist);
      rhs = rhs->next;
   }
   cout << "compiling " << rhs << " and assigning to " << full_name() << endl;
   ilist.append(Instruction::_label(new string(full_name())));

   compile(rhs);
   ilist.append(Instruction::_return(1));
   pop_symbol_table();
}

static void compile(Expression *expr, InstructionList &ilist) {
   switch (expr->t) {
      case Expression::ASSIGN:
         return compile_assign(expr, ilist);
      case Expression::CALL:
         return compile_call(expr, ilist);
      case Expression::IF:
         return compile_if(expr, ilist);
      case Expression::UNBOUND:
         return compile_unbound(expr, ilist);
      default:
         return compile_literal(expr, ilist);
   }
}

InstructionList compile(Expression *expr) {
   InstructionList ilist;
   compile(expr, ilist);
   return ilist;
}



// deque<Assignment> get_assignments(ExpressionList *elist) {
//    deque<Assignment> assignments;
//    for (int i = 0; i < elist->size(); ++i) {
//       get_assignments((*elist)[i], assignments);
//    }
//    return assignments;
// }

void compile_init(string root_name) {
   push_symbol_table(root_name);
   bin_primitives["+"] = Instruction::_add;
   bin_primitives["-"] = Instruction::_sub;
   bin_primitives["*"] = Instruction::_mult;
   bin_primitives["/"] = Instruction::_div;
   bin_primitives["%"] = Instruction::_mod;
   bin_primitives["=="]= Instruction::_eq;
   bin_primitives["<"] = Instruction::_lt;
   bin_primitives[">"] = Instruction::_gt;
   bin_primitives["<="] = Instruction::_leq;
   bin_primitives[">="] = Instruction::_geq;
   bin_primitives["&&"] = Instruction::_and;
   bin_primitives["||"] = Instruction::_or;
   bin_primitives["!"] = Instruction::_not;
   bin_primitives["^"] = Instruction::_exp;
   un_primitives["-"] = Instruction::_neg;
   un_primitives["!"] = Instruction::_not;
}

}