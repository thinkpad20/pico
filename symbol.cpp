#include "symbol.h"
#include "expression.h"
#include <map>

using namespace std;

namespace pico {

struct Symstack {
   map<Var *, Term *> m;
   Symstack *next;
   Symstack(): next(NULL) {}
   Symstack(Symstack *next): next(next) {}
};

static Symstack *symbol_table;

void push() {
   symbol_table = new Symstack(symbol_table);
}

void pop() {
   Symstack *s = symbol_table;
   symbol_table = symbol_table->next;
   delete s;
}

Term *lookupR(Var *var, Symstack *s) {
   if (!s) return NULL;
   if (s->m.find(var) != s->m.end()) return s->m[var];
   return lookupR(var, s->next);
}

Term *lookup(Var *var) {
   if (!symbol_table) symbol_table = new Symstack();
   return lookupR(var, symbol_table);
}

void store(Var *var, Term *term) {
   if (!symbol_table) symbol_table = new Symstack();
   if (symbol_table->m.find(var) != symbol_table->m.end()) {
      fprintf(stderr, "Error: symbol %s already exists in table, can't assign.\n", var->name->c_str());
   }
   symbol_table->m[var] = term;
   printf("_~_~_~_~_~_~_~_~__~_~_Stored ");
   if (var->type) printf("%s ", var->type->c_str());
   printf("%s to: \n", var->name->c_str()); fflush(stdout);
   term->print(); puts("");
}

}