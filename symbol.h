#ifndef __PICO_SYMBOL_H_
#define __PICO_SYMBOL_H_

#include "common.h"

namespace pico 
{

extern Term *GLOBAL_UNRESOLVED_TERM;
extern Expression *GLOBAL_UNRESOLVED_EXPR;

void sym_push();
void sym_pop();
Term *sym_lookup(std::string *name);
void sym_store(std::string *name, Term *term);
void sym_update(std::string *str, Term *term);
bool sym_contains(std::string *name);
void add_local(std::string *str);
void symtable_print(void);
}

#endif