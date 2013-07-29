#include "symbol.h"
#include "ast.h"
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

void sym_push() {
   symbol_table = new Symstack(symbol_table);
}

void sym_pop() {
   Symstack *s = symbol_table;
   symbol_table = symbol_table->next;
   delete s;
}

static Term *lookup(Var *var, Symstack *s) {
   if (!s) return NULL;
   if (s->m.find(var) != s->m.end()) return s->m[var];
   return lookup(var, s->next);
}

Term *sym_lookup(Var *var) {
   if (!symbol_table) symbol_table = new Symstack();
   return lookup(var, symbol_table);
}

void sym_store(Var *var, Term *term) {
   if (!symbol_table) symbol_table = new Symstack();
   if (symbol_table->m.find(var) != symbol_table->m.end()) {
      fprintf(stderr, "Error: symbol %s already exists in table, can't assign.\n", var->name->c_str());
   }
   symbol_table->m[var] = term;
   printf("_~_~_~_~_~_~_~_~__~_~_Stored ");
   if (var->type) printf("%s ", var->type->c_str());
   printf("%s\n", var->name->c_str()); fflush(stdout);
}

}