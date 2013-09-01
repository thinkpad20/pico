#ifndef __COMPILE_H_
#define __COMPILE_H_ value

#include "common.h"
#include "ast.h"
#include "vm.h"

namespace pico {

void compile_init(void);
InstructionList compile(Expression *expr);

}

#endif
