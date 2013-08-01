#include "../include/ast.h"
#include <cmath> // for pow
#include <vector>

using std::map;
using std::string;
using std::vector;
using std::endl;
using std::cout;

namespace pico {

Expression *Expression::reduce() {
   if (is_binary()) { reduce_update(expr1); reduce_update(expr2); }
   if (is_unary()) { reduce_update(unary); }
   if (t == IF) { reduce_update(cond); }
   if (t != VAR && t != ASSIGN && unresolved()) return this; // var might need to be stored
   switch(t) {
      case ASSIGN:
      {  reduce_update(right_hand);
         sym_store(vname, right_hand);
         return next->reduce(); }
      case IF:
      {  if (cond->as_bool()) return if_true->reduce();
         else return if_false->reduce(); }
      case UNRESOLVED:
      {  return this; }
      case VAR:
      {  Expression *target = sym_lookup(name);
         if (target == GLOBAL_UNRESOLVED()) {   
            add_free_var(name);
            return this; 
         }
         return target; // target is already reduced
      }
      case INVOKE:
      { throw std::string("Can't handle invoke yet :(\n"); }
      case ADD: { return add(expr1, expr2); }
      case SUB: { return sub(expr1, expr2); }
      case MULT: { return mult(expr1, expr2); }
      case DIV: { return div(expr1, expr2); }
      case MOD: { return mod(expr1, expr2); }
      case EXP: { return exp(expr1, expr2); }
      case NEG: { return neg(unary); }
      case LOG_AND: { return land(expr1, expr2); }
      case LOG_OR: { return lor(expr1, expr2); }
      case LOG_NOT: { return lnot(unary); }
      case EQ: { return eq(expr1, expr2); }
      case NEQ: { return neq(expr1, expr2); }
      case LT: { return lt(expr1, expr2); }
      case GT: { return gt(expr1, expr2); }
      case LEQ: { return leq(expr1, expr2); }
      case GEQ: { return geq(expr1, expr2); }
      case BIT_AND: { return band(expr1, expr2); }
      case BIT_OR: { return bor(expr1, expr2); }
      case BIT_NOT: { return bnot(expr1, expr2); }
      case BIT_XOR: { return bxor(expr1, expr2); }
      //literals
      case BOOL: case INT: case FLOAT: case CHAR: case STRING: { return this; }
   }
}

void Expression::reduce_update(Expression *&expr) {
   Expression *reduced = expr->reduce();
   if (reduced != expr) {
      // cout << "Reduced " << expr << " to " << reduced << endl;
      delete expr;
      expr = reduced;
   }
}

unsigned Expression::unresolved() {
   // printf("calling unresolved on %p: ", t); fflush(stdout); t->print(); puts("");
   // printf("t = %d\n", t->t); fflush(stdout);
   switch (t) {
      case ASSIGN: 
      { return right_hand->unresolved() + next->unresolved(); }
      case IF: 
      { return cond->unresolved() 
                  + if_true->unresolved() 
                  + if_false->unresolved(); }
      case BOOL: case STRING: case CHAR: case INT: case FLOAT:
      { return 0; }
      case VAR: {
         // printf("looking up a variable %s\n", var->name->c_str()); fflush(stdout);
         Expression *res = sym_lookup(name);
         // printf("res is: "); fflush(stdout); res->print();
         if (res->t == UNRESOLVED) { return 1; }
         return res->unresolved();
      }
      case UNRESOLVED: 
      { return 1; }
      case INVOKE:
      { u = func->unresolved() - expr_list->size(); return u; }
      case ADD: case SUB: case MOD: case MULT:
      case DIV: case BIT_AND: case BIT_OR: case BIT_XOR:
      case LOG_AND: case LOG_OR: case EXP: case EQ:
      case NEQ: case LT: case GT: case LEQ: case GEQ:
      { u = expr1->unresolved() + expr2->unresolved(); return u; }
      case LOG_NOT: case NEG: case BIT_NOT:
      { u = unary->unresolved(); return u; }
   }
}

bool Expression::is_eq(Expression *other) {
   // printf("Checking if "); print(); printf(" == "); other->print(); puts("");
   if (t != other->t) return false;
   if (is_binary()) {
      return expr1->is_eq(other->expr1) && expr2->is_eq(other->expr2);
   }
   if (is_unary()) {
      return unary->is_eq(other->unary);
   }
   if (t == ASSIGN) { 
      return vname == other->vname 
            && right_hand == other->right_hand 
            && next == other->next; 
   }
   if (t == IF) { 
      return cond == other->cond
         && if_true == other->if_true
         && if_false == other->if_false;
   }
   if (t == INVOKE) {
      cout << "WARNING, we haven't implemented invoke equality yet" << endl;
      return func->is_eq(other->func) && expr_list == other->expr_list;
   }
   switch (t) {
      case INT: { return ival == other->ival; }
      case FLOAT: { return fval == other->fval; }
      case CHAR: { return cval == other->cval; }
      case STRING: { return strval == other->strval; }
      case BOOL: { return bval == other->bval; }
      case VAR: { return *vname == *other->vname; }
      case UNRESOLVED:  { return false; }
      default:
         throw new string("Type unaccounted for in is_eq");
   }
}

Expression *Expression::add(Expression *expr1, Expression *expr2) {
   if (expr1->t == INT) {
      switch (expr2->t) {
         case INT: { return new Expression(expr1->ival + expr2->ival); }
         case FLOAT: { return new Expression(expr1->ival + expr2->ival); }
         default: break;
      }
   }
   if (expr1->t == FLOAT) {
      switch (expr2->t) {
         case INT: { return new Expression(expr1->ival + expr2->ival); }
         case FLOAT: { return new Expression(expr1->ival + expr2->ival); }
         default: break;
      }
   }
   throw std::string("We can only add ints and floats so far");
}

Expression *Expression::sub(Expression *t1, Expression *t2) {
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Expression(t1->ival - t2->ival); }
         case FLOAT: { return new Expression(t1->ival - t2->ival); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Expression(t1->ival - t2->ival); }
         case FLOAT: { return new Expression(t1->ival - t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only sub ints and floats so far");
}

Expression *Expression::mult(Expression *t1, Expression *t2) {
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { Expression *res = new Expression(t1->ival * t2->ival); 
            return res;}
         case FLOAT: { return new Expression(t1->ival * t2->ival); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Expression(t1->ival * t2->ival); }
         case FLOAT: { return new Expression(t1->ival * t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only mult ints and floats so far");
}

Expression *Expression::div(Expression *t1, Expression *t2) {
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Expression(t1->ival / t2->ival); }
         case FLOAT: { return new Expression(t1->ival / t2->ival); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Expression(t1->ival / t2->ival); }
         case FLOAT: { return new Expression(t1->ival / t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only div ints and floats so far");
}

Expression *Expression::mod(Expression *t1, Expression *t2) {
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Expression(t1->ival % t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only mod ints so far");
}

Expression *Expression::exp(Expression *t1, Expression *t2) {
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Expression((int)pow(t1->ival, t2->ival)); }
         case FLOAT: { return new Expression(pow(t1->ival, t2->fval)); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Expression(pow(t1->fval, t2->ival)); }
         case FLOAT: { return new Expression(pow(t1->fval, t2->fval)); }
         default: break;
      }
   }
   throw std::string("We can only exp ints and floats so far");
}

Expression *Expression::neg(Expression *t) {
   if (t->t == INT)
      return new Expression(-t->ival);
   if (t->t == FLOAT)
      return new Expression(-t->fval);
   throw std::string("We can only negate ints and floats so far");
}

Expression *Expression::land(Expression *t1, Expression *t2) {
   // we have two resolved expressions, check if both bools
   if (t1->t == BOOL && t2->t == BOOL) {
      return new Expression(t1->bval && t2->bval);
   }
   throw std::string("Can't logical AND anything except bools");
}

Expression *Expression::lor(Expression *t1, Expression *t2) {
   // we have two resolved expressions, check if both bools
   if (t1->t == BOOL && t2->t == BOOL) {
      return new Expression(t1->bval || t2->bval);
   }
   throw std::string("Can't logical OR anything except bools");
}

Expression *Expression::lnot(Expression *t) {
   if (t->t == BOOL)
      return new Expression(!t->bval);
   throw std::string("We can only NOT bools so far");
}

Expression *Expression::eq(Expression *t1, Expression *t2) {
   // we have two resolved expressions
   if (t1->t == t2->t) {
      switch (t1->t) {
         // check if literals are same
         case INT:   { return new Expression(t1->ival == t2->ival); }
         case FLOAT: { return new Expression(t1->fval == t2->fval); }
         case CHAR:  { return new Expression(t1->cval == t2->cval); }
         case STRING: { return new Expression(*t1->strval == *t2->strval); } //string ptrs must be deref'd
         case BOOL:  { return new Expression(t1->bval == t2->bval); }
         case VAR: { 
            throw std::string("eq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: { return new Expression(false); } // might want to check function equality, later...
      }
   }
   throw std::string("Can't test equality on mismatched types");
}

Expression *Expression::neq(Expression *t1, Expression *t2) {
   // we have two resolved expressions
   if (t1->t == t2->t) {
      switch (t1->t) {
         // check if literals are same
         case INT: { return new Expression(t1->ival != t2->ival); }
         case FLOAT: { return new Expression(t1->fval != t2->fval); }
         case CHAR: { return new Expression(t1->cval != t2->cval); }
         case STRING: { return new Expression(*t1->strval != *t2->strval); } //string ptrs must be deref'd
         case BOOL: { return new Expression(t1->bval != t2->bval); }
         case VAR: { 
            throw std::string("neq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: { return new Expression(true); } // might want to check function equality, later...
      }
   }
   throw std::string("Can't test inequality on mismatched types");
}

Expression *Expression::lt(Expression *t1, Expression *t2) {
   // we have two resolved expressions
   if (t1->t == INT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->ival < t2->ival); }
         case FLOAT: { return new Expression(t1->ival < t2->fval); }
         case CHAR: { return new Expression(t1->ival < t2->cval); }
         case VAR: { 
            throw std::string("lt: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->fval < t2->ival); }
         case FLOAT: { return new Expression(t1->fval < t2->fval); }
         case CHAR: { return new Expression(t1->fval < t2->cval); }
         case VAR: { 
            throw std::string("lt: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == CHAR) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->cval < t2->ival); }
         case FLOAT: { return new Expression(t1->cval < t2->fval); }
         case CHAR: { return new Expression(t1->cval < t2->cval); }
         case VAR: { 
            throw std::string("lt: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == STRING) {
      switch (t2->t) {
         // check if literals are same
         case STRING: { return new Expression(*t1->strval < *t2->strval); }
         case VAR: { 
            throw std::string("lt: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   throw std::string("lt: Can't compare mismatched types");
}

Expression *Expression::gt(Expression *t1, Expression *t2) {
   // we have two resolved expressions
   if (t1->t == INT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->ival > t2->ival); }
         case FLOAT: { return new Expression(t1->ival > t2->fval); }
         case CHAR: { return new Expression(t1->ival > t2->cval); }
         case VAR: { 
            throw std::string("gt: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->fval > t2->ival); }
         case FLOAT: { return new Expression(t1->fval > t2->fval); }
         case CHAR: { return new Expression(t1->fval > t2->cval); }
         case VAR: { 
            throw std::string("gt: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == CHAR) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->cval > t2->ival); }
         case FLOAT: { return new Expression(t1->cval > t2->fval); }
         case CHAR: { return new Expression(t1->cval > t2->cval); }
         case VAR: { 
            throw std::string("gt: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == STRING) {
      switch (t2->t) {
         // check if literals are same
         case STRING: { return new Expression(*t1->strval > *t2->strval); }
         case VAR: { 
            throw std::string("gt: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   throw std::string("lt: Can't compare mismatched types");
}

Expression *Expression::geq(Expression *t1, Expression *t2) {
   // we have two resolved expressions
   if (t1->t == INT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->ival >= t2->ival); }
         case FLOAT: { return new Expression(t1->ival >= t2->fval); }
         case CHAR: { return new Expression(t1->ival >= t2->cval); }
         case VAR: { 
            throw std::string("geq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->fval >= t2->ival); }
         case FLOAT: { return new Expression(t1->fval >= t2->fval); }
         case CHAR: { return new Expression(t1->fval >= t2->cval); }
         case VAR: { 
            throw std::string("geq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == CHAR) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->cval >= t2->ival); }
         case FLOAT: { return new Expression(t1->cval >= t2->fval); }
         case CHAR: { return new Expression(t1->cval >= t2->cval); }
         case VAR: { 
            throw std::string("geq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == STRING) {
      switch (t2->t) {
         // check if literals are same
         case STRING: { return new Expression(*t1->strval >= *t2->strval); }
         case VAR: { 
            throw std::string("geq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   throw std::string("geq: Can't compare mismatched types");
}

Expression *Expression::leq(Expression *t1, Expression *t2) {
   // we have two resolved expressions
   if (t1->t == INT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->ival <= t2->ival); }
         case FLOAT: { return new Expression(t1->ival <= t2->fval); }
         case CHAR: { return new Expression(t1->ival <= t2->cval); }
         case VAR: { 
            throw std::string("leq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->fval <= t2->ival); }
         case FLOAT: { return new Expression(t1->fval <= t2->fval); }
         case CHAR: { return new Expression(t1->fval <= t2->cval); }
         case VAR: { 
            throw std::string("leq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == CHAR) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Expression(t1->cval <= t2->ival); }
         case FLOAT: { return new Expression(t1->cval <= t2->fval); }
         case CHAR: { return new Expression(t1->cval <= t2->cval); }
         case VAR: { 
            throw std::string("leq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == STRING) {
      switch (t2->t) {
         // check if literals are same
         case STRING: { return new Expression(*t1->strval <= *t2->strval); }
         case VAR: { 
            throw std::string("leq: Can't handle variables yet!");
            // return new Expression(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   throw std::string("leq: Can't compare mismatched types");
}


Expression *Expression::band(Expression *t1, Expression *t2) {
   throw std::string("bitwise and not implemented yet");
}

Expression *Expression::bor(Expression *t1, Expression *t2) {
   throw std::string("bitwise bor not implemented yet");
}

Expression *Expression::bnot(Expression *t1, Expression *t2) {
   throw std::string("bitwise not not implemented yet");
}

Expression *Expression::bxor(Expression *t1, Expression *t2) {
   throw std::string("bitwise bxor not implemented yet");
}

bool Expression::as_bool() {
   if (t == BOOL) return bval;
   throw std::string("Error: as_bool called on something that's not a boolean.");
}

void ExpressionList::reduce_all() {
   ExpressionList::iterator it;
   for (it = begin(); it != end(); ++it) {
      Expression::reduce_update(*it);
   }
}

}