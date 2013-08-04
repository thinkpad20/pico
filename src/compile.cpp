#include "../include/compile.h"
#include <string>
#include <deque>
#include <map>
#include <vector>

using namespace std;

namespace pico {

static map<string, Instruction *(*)()> func_map;

struct SymbolInfo {
   enum Status {OK, NOT_FOUND} s;
   string full_name;
   Expression *expr;
   Expression::Type type;
   unsigned arg_num;
   SymbolInfo(): s(NOT_FOUND) {}
   SymbolInfo(string full_name, Expression *expr): 
      s(OK), full_name(full_name), expr(expr) {}
   SymbolInfo(string full_name, Expression::Type t, unsigned arg_num): 
      s(OK), full_name(full_name), type(t), arg_num(arg_num) {}
};

struct Unbound { 
   string name; Expression::Type t; 
   Unbound(string name, Expression::Type t): name(name), t(t) {}
};

struct SymbolTable {
   map<string, SymbolInfo> table;
   unsigned num_args;
   SymbolTable *next;
   SymbolTable(SymbolTable *next): num_args(0), next(next) {}
};

struct NameStack {
   deque<string> stack;
   NameStack *push(string name) {
      stack.push_back(name);
      return this;
   }
   void pop() {
      stack.pop_back();
   }
   string render() {
      string res = "";
      bool first = true;
      for (int i = 0; i < stack.size(); ++i) {
         if (first) first = false; else res += ".";
         res += stack[i];
      }
      return res;
   }
};

struct FunctionInfo {
   string name; // if anonymous, has a compiler-provided name
   vector<Expression::Type> return_types, arg_types;
};

static NameStack name_stack;

SymbolTable *symtable = NULL;

static void push_symtable() {
   symtable = new SymbolTable(symtable);
}

static void pop_symtable() {
   if (!symtable) throw string("SymbolTable is null");
   SymbolTable *temp = symtable;
   symtable = symtable->next;
   delete temp;
}

// static void initialize() {
//    push_symtable();
//    func_map["+"] = Add;
//    func_map["-"] = Sub;
//    func_map["*"] = Mult;
//    func_map["/"] = Div;
//    func_map["%"] = Mod;
//    func_map["=="]= Eq;
//    func_map["<"] = Lt;
//    func_map[">"] = Gt;
//    func_map["<="] = Leq;
//    func_map[">="] = Geq;
//    func_map["&&"] = And;
//    func_map["||"] = Or;
//    func_map["!"] = Not;
//    func_map["^"] = Exp;
// }

// for bound variables
static void store(string name, string full_name, Expression *expr) {
   symtable->table[name] = SymbolInfo(full_name, expr);
}

// for unbound vars
static void store(string name, string full_name, Expression::Type t) {
   if (symtable->table.find(name) != symtable->table.end())
      throw string("Error: symbol '" + name + "' already in table!");
   symtable->table[name] = SymbolInfo(full_name, t, symtable->num_args++);
}

// gets the number of an unbound variable
static int arg_num(string vname) {
   if (symtable->table.find(vname) == symtable->table.end())
      throw string("Error: symbol '" + vname + "' already in table!");
   return symtable->table[vname].arg_num;
}

static SymbolInfo lookup_single(string var, SymbolTable *s) {
   if (s->table.find(var) != s->table.end())
      return s->table[var];
   return SymbolInfo();
}

static SymbolInfo lookup(string var) {
   SymbolTable *s = symtable;
   while (s) {
      SymbolInfo res = lookup_single(var, s);
      if (res.s == SymbolInfo::OK) return res;
      s = s->next;
   }
   return SymbolInfo();
}

ostream &operator<<(ostream &os, const Assignment &asn) {
   os << asn.vname << " -> " << asn.rval;
   return os;
}

deque<Assignment> compile(Expression *expr, deque<Assignment> &assignments) {
   // cout << "call to compile on expression " << expr << endl;
   if (expr->t != Expression::ASSIGN) {
      // cout << "This isn't an assignment expression. Weird?" << endl;
      return assignments;
   }
   Expression *rhs = expr->rhs;
   // cout << "rhs of this assignment is " << rhs << endl;
   while (rhs->t == Expression::ASSIGN) {
      compile(rhs, assignments);
      rhs = rhs->next;
   }
   assignments.push_back(Assignment(*expr->alias, rhs));
   return assignments;
}

deque<Assignment> compile(ExpressionList *elist) {
   deque<Assignment> assignments;
   for (int i = 0; i < elist->size(); ++i) {
      compile((*elist)[i], assignments);
   }
   return assignments;
}

}