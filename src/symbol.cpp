#include "../include/ast.h"
#include <map>
#include <vector>

using namespace std;

namespace pico {

Expression *GLOBAL_UNRESOLVED = NULL;

Expression *Expression::sym_lookup(string *str) {
   // printf("looking up %s\n", str->c_str());
   if (symbol_table.find(*str) != symbol_table.end())
      return symbol_table[*str];
   printf("'%s' not found in table!\n", str->c_str());
   if (!parent) { 
      if (!GLOBAL_UNRESOLVED) GLOBAL_UNRESOLVED = new Expression(); 
      return GLOBAL_UNRESOLVED;
   }
   return parent->sym_lookup(str);
   // printf("'%s' resolves to: ", str->c_str()); res->print(); puts("");
   // printf(", with %u unresolved syms\n", Expression::unresolved(res)); fflush(stdout);
}

void Expression::sym_store(string *str, Expression *expr) {
   printf("storing '%s'?\n", str->c_str()); fflush(stdout);
   // printf("Storing %s, creating new thunk: ", str->c_str()); expr->print(); puts("");
   if (sym_contains(str)) {
      char buf[1000]; sprintf(buf, "Error: symbol %s already exists in table, can't assign.\n", str->c_str());
      throw string(buf);
   }
   symbol_table[*str] = new Expression(expr);
}

void Expression::sym_update(string *str, Expression *expr) {
   printf("updating symtable!~+~+~+~+~+~+~+~+~+~+\n");
   if (symbol_table.find(*str) == symbol_table.end()) {
      if (expr->t != Expression::UNRESOLVED) {
         printf("error\n"); fflush(stdout);
         throw string("Hey, I can't update a variable that doesn't exist!");
      } else {
         symbol_table[*str] = new Expression(new Expression());
      }
   }
   Expression *cur = symbol_table[*str];
   printf("'%s' was previously %p: ", str->c_str(), cur); cur->print();
   printf(", is now %p: ", expr); expr->print(); puts("");
   bool iseq = cur->is_eq(expr);
   printf("\n%s", iseq ? "equal" : "not equal");
   if (!iseq) {
      printf(", so updating: "); expr->print(); puts(""); fflush(stdout);
      printf("deleting %p\n", symbol_table[*str]); fflush(stdout);
      delete symbol_table[*str];
      symbol_table[*str] = expr;
      printf("finished updating %s, is now ", str->c_str()); fflush(stdout);
      expr->print(); puts(""); fflush(stdout);
   }
}

void Expression::symtable_print(unsigned level) {
   if (!level) printf("Symbol table contains:\n");
   printf("Level %d:\n", level);
   std::map<string, Expression *>::iterator it;
   for (it=symbol_table.begin(); it!=symbol_table.end(); ++it) {
      printf("'%s' -> ", it->first.c_str()); it->second->print(); puts("");
   }
   if (parent) parent->symtable_print(level + 1);
}

bool Expression::sym_contains(string *name) {
   return symbol_table.find(*name) != symbol_table.end();
}

void Expression::add_free_var(string *str) {
   if (symbol_table.find(*str) != symbol_table.end()) 
      return;  
   size_t index = free_vars.size();
   free_vars.push_back(GLOBAL_UNRESOLVED); 
   symbol_table[*str] = free_vars[index];
   printf("added local variable %s into slot %lu\n", str->c_str(), index);
}

}