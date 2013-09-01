#include "compile.h"
#include <map>
#include <deque>

using namespace std;
namespace pico {

static InstructionList compile (Expression *expr, InstructionList &insts);

struct SymTable {
   map<string, Expression *> table;
   SymTable *next;
   SymTable(SymTable *next): next(next) {}
};

static SymTable *symtable;

void compile_init(void) {
   symtable = new SymTable(NULL);
}

void symtable_push(void) { symtable = new SymTable(symtable); }
void symtable_pop(void) { 
   if (!symtable) {
      throw string("Error: can't pop from empty table!");
   }
   SymTable *temp = symtable;
   symtable = symtable->next;
   delete temp; 
}

void symtable_store(string str, Expression *expr) {
   symtable->table[str] = expr;
}

Expression *symtable_lookup(string str) {
   SymTable *s = symtable;
   while (s) {
      if (s->table.find(str) != s->table.end())
         return s->table[str];
      s = s->next;
   }
   return NULL;
}

static string render(deque<string> nspace) {
   string res = "";
   for (int i = 0; i != res.size(); ++i) {
      res += nspace[i];
   }
   return res;
}

static Expression *lookup(deque<map<string, Expression *> > symbol_table, string symbol) {
   for (int i = symbol_table.size() - 1; i >= 0; ++i) {
      if (symbol_table[i].find(symbol) != symbol_table[i].end())
         return symbol_table[i][symbol];
   }
   return NULL;
}

static Expression *eval (Expression *expr, 
                         ExpressionList args, 
                         ExpressionList locals, 
                         deque<map<string, Expression *> > symbol_table) {
   switch (expr->t) {
      case Expression::EXPR: {
         symbol_table.push_back(map<string, Expression *>);
         return eval(expr, args, locals, symbol_table);
      }
      case Expression::ASSIGN: {
         // later we'll want to make sure this variable hasn't been assigned yet
         Expression *res = eval(expr->rhs, args, locals, symbol_table);
         locals.push_back(res);
         symbol_table.back()[*expr->alias] = res;
         return eval(expr->next);
      }
      case Expression::IF:  {
         throw string("We don't know how to compile IF statements yet :(");
      }
      case Expression::CALL: {
         string *symbol = expr->func->is_primitive_symbol();
         if (symbol) {
            Expression *left = eval(expr->args[0], args, locals);
            Expression *right = eval(expr->args[0], args, locals);
            if (symbol == "+") {
               if (left->t == )
            } else if (symbol == "-") {
               if (func->is_unary()) {

               } else {

               }
            } else if (symbol == "*") {
               
            } else if (symbol == "/") {
               
            } else if (symbol == ">") {
               
            } else if (symbol == "<") {
               
            } else if (symbol == ">=") {
               
            } else if (symbol == "<=") {
               
            } else if (symbol == "==") {
               
            } else if (symbol == "!=") {
               
            } else if (symbol == "&&") {
               
            } else if (symbol == "&&") {
               
            }
         }
      }

      case Expression::VAR: { return symtable_lookup(*expr->str); }
      // literals...
      case Expression::INT:
      case Expression::FLOAT:
      case Expression::CHAR: 
      case Expression::STRING:
      case Expression::BOOL:
      { return expr; }
   }
}

}