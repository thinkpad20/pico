#include "../include/compile.h"
#include <string>
#include <deque>
#include <map>
#include <vector>

using namespace std;

namespace pico {

struct SymbolInfo {
   enum Status {OK, NOT_FOUND} s;
   string full_name;
   Expression *expr;
   SymbolInfo(): s(NOT_FOUND) {}
   SymbolInfo(string full_name, Expression *expr): 
      s(OK), full_name(full_name), expr(expr) {}
};

struct SymbolTable {
   map<string, SymbolInfo> table;
   vector<string> args;
   SymbolTable *next;
   SymbolTable(SymbolTable *next): next(next) {}
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

static void initialize() {
   push_symtable();
}

static void store(string name, string full_name, Expression *expr) {
   symtable->table[name] = SymbolInfo(full_name, expr);
}

static int arg_num(string vname) {
   for (int i = 0; i < symtable->args.size(); ++i) {
      if (vname == symtable->args[i]) return i;
   }
   return -1;
}

static unsigned add_arg(string vname) {
   if (arg_num(vname) >= 0)
      throw string("Error: trying to add the same variable twice in the same scope");
   symtable->args.push_back(vname);
   return symtable->args.size();
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

static void _compile(InstructionList &insts, Expression *expr, unsigned ecnt) {
   cout << "Compiling: " << expr << endl;
   if (expr->is_binary()){
      _compile(insts, expr->expr1, ecnt);
      _compile(insts, expr->expr2, ecnt);
   }
   if (expr->is_unary())
      _compile(insts, expr->unary, ecnt);

   switch (expr->t) {
      // primitives
      case Expression::INT: 
      {
         insts.append(Instruction::_push(expr->ival));
         break;
      }
      case Expression::FLOAT:
         insts.append(Instruction::_push(expr->fval));
         break;
      case Expression::CHAR:
         insts.append(Instruction::_push(expr->cval));
         break;
      case Expression::STRING:
         insts.append(Instruction::_push(expr->strval));
         break;
      case Expression::BOOL:
         insts.append(Instruction::_push(expr->bval, Primitive::BOOL));
         break;
      case Expression::VAR:
      {
         if (expr->type) {
            int i = arg_num(*expr->name);
            if (i < 0) // then it hasn't been added to the table yet
               i = add_arg(*expr->name);
            insts.append(Instruction::_pusharg(i));
         } else {
            SymbolInfo res = lookup(*expr->name);
            if (res.s != SymbolInfo::OK) {
               char buf[250]; sprintf(buf, "Error: %s has no type and is not defined "
                                           "in this scope", expr->name->c_str());
               throw string(buf);
            }
            // then it's a function, we need to call it... this needs work!
            unsigned unr = res.expr->unresolved();
            insts.append(Instruction::_call(new string(res.full_name), unr));
         }
         break;
      }
      case Expression::INVOKE: // this should really be the same as a variable!
      {
         for (unsigned i = 0; i < expr->expr_list->size(); ++i) {
            _compile(insts, (*expr->expr_list)[i], ecnt);
         }
         //if this expression is a variable, we can just call it
         if (expr->func->t == Expression::VAR) {
            SymbolInfo info = lookup(*expr->func->name);
            insts.append(Instruction::_call(new string(info.full_name), expr->expr_list->size()));
         //otherwise, we have a lamba so we need to create an anonymous context
         } else {
            char lbl[100]; sprintf(lbl, "$%d", ecnt);
            string *lblname = new string(name_stack.render() + lbl);
            insts.append(Instruction::_call(lblname, expr->expr_list->size()));
            insts.append(Instruction::_label(lblname));
            _compile(insts, expr->func, ecnt + 1);
         }
         break;
      }

      // logical flow
      case Expression::IF:
      {
         _compile(insts, expr->cond, ecnt);
         char lbl[100]; sprintf(lbl, "$%d", ecnt);
         name_stack.push(lbl);
         insts.append(Instruction::_elsegoto(new string(name_stack.render())));
         _compile(insts, expr->if_true, ecnt);
         insts.append(Instruction::_label(new string(name_stack.render())));
         _compile(insts, expr->if_false, ecnt + 1);
         break;
      }
      case Expression::ASSIGN:
      {
         cout << "Assignment! current expression: " << expr << endl;
         cout << "current namespace is '" << name_stack.render() << "'" << endl;
         name_stack.push(*expr->vname);
         store(*expr->vname, name_stack.render(), expr->right_hand);
         Expression *first_non_assignment = expr->right_hand;
         // compile all of the sub-assignments
         while (first_non_assignment->t == Expression::ASSIGN) {
            _compile(insts, first_non_assignment, 0);
            first_non_assignment = first_non_assignment->next;
         }
         cout << "found first non-assignment, which is: " << first_non_assignment << endl;
         // make a label for the function name
         insts.append(Instruction::_label(new string(name_stack.render())));
         name_stack.pop();
         // compile the instructions for the right-hand of this assignment
         _compile(insts, first_non_assignment, 0);
         _compile(insts, expr->next, 0);
         break;
      }

      // arithmetic
      case Expression::ADD:
      {
         insts.append(Instruction::_add());
         break;
      }
      case Expression::SUB:
      {
         insts.append(Instruction::_sub());
         break;
      }
      case Expression::MULT:
      {
         insts.append(Instruction::_mult());
         break;
      }
      case Expression::DIV:
      {
         insts.append(Instruction::_div());
         break;
      }
      case Expression::MOD:
      {
         insts.append(Instruction::_mod());
         break;
      }
      case Expression::EXP:
      {
         insts.append(Instruction::_exp());
         break;
      }
      case Expression::EQ:
      {
         insts.append(Instruction::_eq());
         break;
      }
      case Expression::NEQ:
      {
         insts.append(Instruction::_neq());
         break;
      }
      case Expression::LT:
      {
         insts.append(Instruction::_lt());
         break;
      }
      case Expression::GT:
      {
         insts.append(Instruction::_gt());
         break;
      }
      case Expression::LEQ:
      {
         insts.append(Instruction::_leq());
         break;
      }
      case Expression::GEQ:
      {
         insts.append(Instruction::_geq());
         break;
      }
      case Expression::LOG_AND:
      {
         insts.append(Instruction::_and());
         break;
      }
      case Expression::LOG_OR:
      {
         insts.append(Instruction::_or());
         break;
      }
      case Expression::LOG_NOT:
      {
         insts.append(Instruction::_not());
         break;
      }
      case Expression::NEG:
      {
         insts.append(Instruction::_neg());
         break;
      }

      case Expression::UNRESOLVED:
         throw string("why are we seeing unresolved variables?");
      // default:
      case Expression::BIT_AND:
      case Expression::BIT_OR:
      case Expression::BIT_XOR:
      case Expression::BIT_NOT:
         throw string("we can't handle this stuff yet!");
   }
}

InstructionList compile(Expression *expr) {
   InstructionList ilist(10);
   Expression *root = new Expression(strdup("root"), expr, new Expression(0));
   initialize();
   _compile(ilist, root, 0);
   cout << "Generated compiled code:" << endl;
   cout << ilist << endl;
   return ilist;
}

void compile_all(ExpressionList *expr_list) {
   try {
      for (int i = 0; i < expr_list->size(); ++i) {
         compile((*expr_list)[i]);
      }
   } catch (string s) {
      cout << s << endl;
   }
}


}