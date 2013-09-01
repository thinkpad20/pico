#ifndef __PICO_COMPILE_H_
#define __PICO_COMPILE_H_

#include "../include/ast.h"
#include "../include/vm.h"

namespace pico {

struct Assignment {
   std::string vname;
   Expression *rval;
   Assignment(std::string vname, Expression *rval): vname(vname), rval(rval) {}
   friend std::ostream &operator<<(std::ostream &os, const Assignment &asn);
};

std::deque<Assignment> get_assignments(Expression *, std::deque<Assignment> &);
std::deque<Assignment> get_assignments(ExpressionList *);
void compile_init(std::string root_name = "root");
InstructionList compile(Expression *expr);

}

#endif