#ifndef __PICO_SYMBOL_H_
#define __PICO_SYMBOL_H_

#include "common.h"

namespace pico {

void sym_push();
void sym_pop();
Term *sym_lookup(Var *var);
void sym_store(Var *var, Term *term);

}

#endif