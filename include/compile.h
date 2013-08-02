#ifndef __PICO_COMPILE_H_
#define __PICO_COMPILE_H_

#include "../include/ast.h"
#include "../include/vm.h"

namespace pico {

InstructionList compile(Expression *expr);
void compile_all(ExpressionList *elist);

}

#endif