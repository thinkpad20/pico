#include "symbol.h"
#include "ast.h"
#include <map>
#include <vector>

using namespace std;

namespace pico {
typedef Expression Thunk;
Term *GLOBAL_UNRESOLVED_TERM = NULL;
Expression *GLOBAL_UNRESOLVED_EXPR = NULL;

struct Symstack {
   map<string, Thunk *> m;
   vector<Thunk *> locals;
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

static Term *lookup(string *str, Symstack *s) {
   if (!s) { 
      if (!GLOBAL_UNRESOLVED_EXPR) {
         GLOBAL_UNRESOLVED_TERM = new Term();
         GLOBAL_UNRESOLVED_EXPR = new Expression(GLOBAL_UNRESOLVED_TERM, 
                                                 Expression::UNRESOLVED);
      }
      return GLOBAL_UNRESOLVED_TERM; 
   }
   if (s->m.find(*str) != s->m.end()) return s->m[*str]->term;
   return lookup(str, s->next);
}

Term *sym_lookup(string *str) {
   // printf("looking up %s\n", str->c_str());
   if (!symbol_table) symbol_table = new Symstack();
   Term *res = lookup(str, symbol_table);
   if (!res) printf("'%s' not found in table!\n", str->c_str());
   // printf("'%s' resolves to: ", str->c_str()); res->print(); puts("");
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

void sym_update(string *str, Term *term) {
   // printf("updating symtable!~+~+~+~+~+~+~+~+~+~+\n");
   if (symbol_table->m.find(*str) == symbol_table->m.end()) {
      if (term->t != Term::UNRESOLVED) {
         printf("error\n"); fflush(stdout);
         throw string("Hey, I can't update a variable that doesn't exist!");
      } else {
         symbol_table->m[*str] = new Expression(new Term());
      }
   }
   Term *cur = symbol_table->m[*str]->term;
   // printf("'%s' was previously: ", str->c_str()); cur->print();
   bool iseq = cur->is_eq(term) || (cur->t == Term::UNRESOLVED && term->t == Term::UNRESOLVED);
   // printf("\n%s", iseq ? "equal" : "not equal");
   if (!iseq) {
      // printf(", so updating: "); term->print(); puts(""); fflush(stdout);
      delete symbol_table->m[*str]->term;
      symbol_table->m[*str]->term = term;
      // printf("finished updating %s\n", str->c_str()); fflush(stdout);
   }
}

void sym_print(void) {
   Symstack *s = symbol_table;
   int level = 0;
   printf("Symbol table contains:\n");
   while (s) {
      printf("Level %d:\n", level++);
      for (std::map<string, Thunk *>::iterator it=s->m.begin(); it!=s->m.end(); ++it) {
         printf("'%s' -> ", it->first.c_str()); it->second->print(); puts("");
      }
      s = s->next;
   }
}

bool sym_contains(string *name) {
   return symbol_table && symbol_table->m.find(*name) != symbol_table->m.end();
}

void add_local(string *str) {
   size_t index = symbol_table->locals.size();
   symbol_table->locals.push_back(GLOBAL_UNRESOLVED_EXPR); 
   symbol_table->m[*str] = symbol_table->locals[index];
   printf("added local variable %s pointing to spot %p\n", str->c_str(), symbol_table->locals[index]);
}

}