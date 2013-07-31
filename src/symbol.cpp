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
   if (!parent) { 
      if (!GLOBAL_UNRESOLVED) GLOBAL_UNRESOLVED = new Expression(); 
      return GLOBAL_UNRESOLVED;
   }
   return parent->sym_lookup(str);
   // printf("'%s' resolves to: ", str->c_str()); res->print(); puts("");
   // printf(", with %u unresolved syms\n", Expression::unresolved(res)); fflush(stdout);
}

void Expression::sym_store(string *str, Expression *expr) {
   // cout << "storing '" << *str << "' to " << expr << endl;
   if (sym_contains(str)) {
      char buf[1000]; 
      sprintf(buf, "Error: symbol %s already exists in table, can't assign.\n", str->c_str());
      throw string(buf);
   }
   symbol_table[*str] = expr;
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
   cout << '\'' << str << "' was previously " << cur << ", is now " << expr << endl;
   bool iseq = cur->is_eq(expr);
   printf("\n%s", iseq ? "equal" : "not equal");
   if (!iseq) {
      cout << ", so updating to: " << cout << expr << endl;
   }
}

void Expression::symtable_print(unsigned level) {
   if (!level) cout << "Symbol for expression " << this << " contains:" << endl;
   printf("Level %d: (", level);
   std::map<string, Expression *>::iterator it;
   bool first = true;
   for (it=symbol_table.begin(); it!=symbol_table.end(); ++it) {
      if (first) first = false; else cout << ", ";
      cout << it->first << " -> " << it->second;
   }
   cout << ")" << endl;
   // printf("this is %p, parent is %p\n", this, parent);
   if (parent) parent->symtable_print(level + 1);
}

bool Expression::sym_contains(string *name) const {
   return symbol_table.find(*name) != symbol_table.end();
}

void Expression::add_free_var(string *str) {
   // cout << "adding a new variable " << *str << endl;
   // symtable_print();
   if (symbol_table.find(*str) != symbol_table.end()) 
      return;  
   size_t index = free_vars.size();
   free_vars.push_back(GLOBAL_UNRESOLVED); 
   symbol_table[*str] = free_vars[index];
   // cout << "added local variable " << *str << " into slot " << index << endl;
}

}