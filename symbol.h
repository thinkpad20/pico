#ifndef __PICO_SYMBOL_H_
#define __PICO_SYMBOL_H_

#include "common.h"

namespace pico {

void push();
void pop();
Term *lookup(Var *var);
void store(Var *var, Term *term);

}

#endif