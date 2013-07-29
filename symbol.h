#ifndef __PICO_SYMBOL_H_
#define __PICO_SYMBOL_H_

#include "common.h"

namespace pico 
{

void sym_push();
void sym_pop();
Term *sym_lookup(std::string *name);
void sym_store(std::string *name, Term *term);
void update_symbol(std::string *str, Term *term);
bool sym_contains(std::string *name);

}

#endif