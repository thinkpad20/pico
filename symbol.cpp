#include "symbol.h"
#include "ast.h"
#include <map>

using namespace std;

namespace pico {

struct Symstack {
   map<string, Expression *> m;
   Symstack *next;
   Symstack(): next(NULL) {}
   Symstack(Symstack *next): next(next) {}
};

static Symstack *symbol_table;

void sym_push() { symbol_table = new Symstack(symbol_table); }

void sym_pop() {
   Symstack *s = symbol_table;
   symbol_table = symbol_table->next;
   delete s;
}

static Term *lookup(string *str, Symstack *s) {
   if (!s) return new Term();
   if (s->m.find(*str) != s->m.end()) return s->m[*str]->term;
   return lookup(str, s->next);
}

Term *sym_lookup(string *str) {
   printf("looking up %s\n", str->c_str());
   if (!symbol_table) symbol_table = new Symstack();
   Term *res = lookup(str, symbol_table);
   printf("'%s' resolves to: ", str->c_str()); res->print(); puts("yup");
   // printf(", with %u unresolved syms\n", Term::unresolved(res)); fflush(stdout);
   return res;
}

void sym_store(string *str, Term *term) {
   if (!symbol_table) symbol_table = new Symstack();
   // printf("Storing %s, creating new thunk: ", str->c_str()); term->print(); puts("");
   if (symbol_table->m.find(*str) != symbol_table->m.end()) {
      throw string("Error: symbol %s already exists in table, can't assign.\n", str->c_str());
   }
   symbol_table->m[*str] = new Expression(term);
}

void update_symbol(string *str, Term *term) {
   printf("updating symtable!~+~+~+~+~+~+~+~+~+~+\n");
   if (symbol_table->m.find(*str) == symbol_table->m.end())
      throw string("Hey, I can't update a variable that doesn't exist!");
   Term *cur = symbol_table->m[*str]->term;
   printf("calling update symbol, '%s' was previously: ", str->c_str()); cur->print();
   bool iseq = cur->is_eq(term) || (cur->t == Term::UNRESOLVED && term->t == Term::UNRESOLVED);
   printf("%s\n", iseq ? "equal" : "not equal");
   if (!iseq) {
      printf("\nnow: "); term->print(); puts(""); fflush(stdout);
      delete symbol_table->m[*str]->term;
      symbol_table->m[*str]->term = term;
   }
}

void sym_print(void) {
   Symstack *s = symbol_table;
   int level = 0;
   printf("Symbol table contains:\n");
   while (s) {
      printf("Level %d:\n", level++);
      for (std::map<string,Expression *>::iterator it=s->m.begin(); it!=s->m.end(); ++it) {
         printf("'%s' -> ", it->first.c_str()); it->second->print(); puts("");
      }
      s = s->next;
   }
}

bool sym_contains(string *name) {
   return symbol_table && symbol_table->m.find(*name) != symbol_table->m.end();
}

}