#ifndef __PICO_SYMBOL_H_
#define __PICO_SYMBOL_H_

#include "common.h"

namespace pico {

void sym_push();
void sym_pop();
Var *sym_lookup(std::string *var_name);
void sym_store(Var *var);

}

#endif