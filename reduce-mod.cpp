#include "ast-mod.h"
#include <cmath> // for pow

namespace pico {

int e_num = 0;
int callno = 0;

Expression *Expression::reduce(Expression *expr) {
   // int n = e_num++;
   // printf("==============reducing %d: ", n); expr->print(); puts("");
   if (!expr) {
      printf("expr was null\n");
      return GLOBAL_UNRESOLVED;
   }
   if (expr == GLOBAL_UNRESOLVED) 
      return expr;
   // int thiscall = callno++;
   // printf("%d calling reduce on expr: ", thiscall);
   // expr->print_info(); printf(" "); expr->print(); puts("");
   switch(expr->t) {
      case ASSIGN:
      {  printf("this is an assign! Here's what's in our symbol table:\n"); fflush(stdout);
         expr->symtable_print(); fflush(stdout);
         puts("");
         if (!expr->sym_contains(expr->assign.var->name))
            expr->sym_store(expr->assign.var->name, expr->assign.expr);
         return reduce(expr->assign.next); }
      case IF:
      {
         printf("this is an if!\n"); fflush(stdout);
         if (expr->if_.cond->unresolved() > 0) 
            return new Expression(expr); // stop reducing
         if (Expression::reduce(expr->if_.cond)->to_bool()) 
            return Expression::reduce(expr->if_.if_true); // only reduce one of the conditions
         else
            return reduce(expr->if_.if_false);
      }
      case UNRESOLVED:
      {
         printf("This is unresolved, can't evaluate further.\n");
         return expr;
      }
      case ADD: { return add(expr->binary.expr1, expr->binary.expr2); }
      case SUB: { return sub(expr->binary.expr1, expr->binary.expr2); }
      case MULT: { Expression *res = mult(expr->binary.expr1, expr->binary.expr2); 
                   //printf("finishing call %d, res %p: ", thiscall, res); res->print(); puts(""); 
                   return res;}
      case DIV: { return div(expr->binary.expr1, expr->binary.expr2); }
      case MOD: { return mod(expr->binary.expr1, expr->binary.expr2); }
      case EXP: { return exp(expr->binary.expr1, expr->binary.expr2); }
      case NEG: { return neg(expr->unary); }
      case LOG_AND: { return land(expr->binary.expr1, expr->binary.expr2); }
      case LOG_OR: { return lor(expr->binary.expr1, expr->binary.expr2); }
      case LOG_NOT: { return lnot(expr->unary); }
      case EQ: { return eq(expr->binary.expr1, expr->binary.expr2); }
      case NEQ: { return neq(expr->binary.expr1, expr->binary.expr2); }
      case LT: { return lt(expr->binary.expr1, expr->binary.expr2); }
      case GT: { return gt(expr->binary.expr1, expr->binary.expr2); }
      case LEQ: { return leq(expr->binary.expr1, expr->binary.expr2); }
      case GEQ: { return geq(expr->binary.expr1, expr->binary.expr2); }
      case BIT_AND: { return band(expr->binary.expr1, expr->binary.expr2); }
      case BIT_OR: { return bor(expr->binary.expr1, expr->binary.expr2); }
      case BIT_NOT: { return bnot(expr->binary.expr1, expr->binary.expr2); }
      case BIT_XOR: { return bxor(expr->binary.expr1, expr->binary.expr2); }
      //literals
      case BOOL: case INT: case FLOAT: case CHAR: case STRING: { return expr; }
      case VAR:
      { 
         Expression *target = expr->sym_lookup(expr->var->name);
         if (target == GLOBAL_UNRESOLVED) {
            printf("found an unresolved expr\n"); fflush(stdout);
            expr->add_free_var(expr->var->name);
            printf("we're going to return: "); expr->print(); puts("");
            printf("which has unresolved of %u\n", expr->unresolved());
            return expr;
         }
         Expression *result = reduce(target);
         expr->sym_update(expr->var->name, result);
         // printf("finished sym_update\n"); fflush(stdout);
         return result; 
      }
      case INVOKE:      { 
         
         throw std::string("Can't handle invoke yet :(\n"); 
      }
   }
}

unsigned Expression::unresolved() {
   // printf("calling unresolved on %p: ", t); fflush(stdout); t->print(); puts("");
   // printf("t = %d\n", t->t); fflush(stdout);
   switch (t) {
      case ASSIGN: 
      { return assign.expr->unresolved() + assign.next->unresolved(); }
      case IF: 
      { return if_.cond->unresolved() 
                  + if_.if_true->unresolved() 
                  + if_.if_false->unresolved(); }
      case BOOL: case STRING: case CHAR: case INT: case FLOAT:
      { return 0; }
      case VAR: {
         // printf("looking up a variable %s\n", var->name->c_str()); fflush(stdout);
         Expression *res = sym_lookup(var->name);
         // printf("res is: "); fflush(stdout); res->print();
         if (res->t == UNRESOLVED) { return 1; }
         return res->unresolved();
      }
      case UNRESOLVED: 
      { return 1; }
      case INVOKE:
      { u = invoke.func->unresolved() - invoke.expr_list->size(); return u; }
      case ADD: case SUB: case MOD: case MULT:
      case DIV: case BIT_AND: case BIT_OR: case BIT_XOR:
      case LOG_AND: case LOG_OR: case EXP: case EQ:
      case NEQ: case LT: case GT: case LEQ: case GEQ:
      { u = binary.expr1->unresolved() + binary.expr2->unresolved(); return u; }
      case LOG_NOT: case NEG: case BIT_NOT:
      { u = unary->unresolved(); return u; }
   }
}

bool Expression::is_eq(Expression *other) {
   // printf("Checking if "); print(); printf(" == "); other->print(); puts("");
   if (t != other->t) return false;
   switch (t) {
      case ASSIGN: { return assign.var == other->assign.var 
                            && assign.expr == other->assign.expr 
                            && assign.next == other->assign.next; }
      case IF: { return if_.cond == other->if_.cond
                        && if_.if_true == other->if_.if_true
                        && if_.if_false == other->if_.if_false; }
      // binary
      case ADD: 
      case SUB: 
      case MULT:                
      case DIV: 
      case MOD: 
      case EXP: 
      case LOG_AND:
      case LOG_OR: 
      case EQ: 
      case NEQ:
      case LT: 
      case GT: 
      case LEQ:
      case GEQ:
      case BIT_AND:
      case BIT_OR: 
      case BIT_NOT:
      case BIT_XOR:
      { return binary.expr1->is_eq(other->binary.expr1) && binary.expr2->is_eq(other->binary.expr2); }
      // unary
      case NEG:
      case LOG_NOT:
      { return unary->is_eq(other->unary); }
      //literals
      case INT: { return ival == other->ival; }
      case FLOAT: { return fval == other->fval; }
      case CHAR: { return cval == other->cval; }
      case STRING: { return strval == other->strval; }
      case BOOL: { return bval == other->bval; }
      case VAR: { return *var->name == *other->var->name; }
      case UNRESOLVED:  { return false; }
      case INVOKE:      
      {  printf("WARNING, we haven't implemented invoke equality yet\n");
         return invoke.func->is_eq(other->invoke.func) && invoke.expr_list == other->invoke.expr_list; }
   }
}

Expression *Expression::add(Expression *t1, Expression *t2) {
   // printf("Adding: "); t1->print(); printf(" and "); t2->print(); puts("");
   // Expression *temp1 = t1, *temp2 = t2;
   // printf("going to subreduce, %p %p\n", t1, t2); fflush(stdout);
   t1 = reduce(t1); t2 = reduce(t2);
   // printf("add reduced, %p %p, seeing if unresolved\n", t1, t2); fflush(stdout);
   // check if reduction should proceed
   unsigned u1 = t1->unresolved();
   // printf("u1 = %u\n", u1); fflush(stdout);
   unsigned u2 = t2->unresolved();
   // printf("u2 = %u\n", u2); fflush(stdout);
   if (u1 || u2) { 
      // printf("add unresolved\n");
      return make_add(t1, t2); 
   }
   // printf("not unresolved, constant expr\n");
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Expression(t1->ival + t2->ival); }
         case FLOAT: { return new Expression(t1->ival + t2->ival); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Expression(t1->ival + t2->ival); }
         case FLOAT: { return new Expression(t1->ival + t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only add ints and floats so far");
}

Expression *Expression::sub(Expression *t1, Expression *t2) {
   // printf("Subtracting: "); t1->print(); printf(" and "); t2->print(); puts("");
   t1 = reduce(t1); t2 = reduce(t2);
   // printf("sub reduceuated, seeing if unresolved\n"); fflush(stdout);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      // printf("sub unresolved\n");
      return make_sub(t1, t2); 
   }
   // printf("not unresolved, constant expr\n");
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
   // printf("Multiplying: "); t1->print(); printf(" and "); t2->print(); puts("");
   t1 = reduce(t1); 
   // printf("reduceuating t2\n");
   t2 = reduce(t2);
   // printf("done reduceuating t2\n");
   // printf("mult reduceuated, seeing if unresolved\n"); fflush(stdout);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      // printf("mult is unresolved\n");
      return make_mult(t1, t2); 
   }
   // printf("not unresolved, constant expr\n");
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { Expression *res = new Expression(t1->ival * t2->ival); 
            // printf("\nthe result is %p: ", res); res->print(); puts(""); 
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
   // printf("Dividing: "); t1->print(); printf(" and "); t2->print(); puts("");
   t1 = reduce(t1); t2 = reduce(t2);
   // printf("div reduceuated, seeing if unresolved\n"); fflush(stdout);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) {
      // printf("div is unresolved\n");
      return make_div(t1, t2); 
   }
   // printf("not unresolved, constant expr\n");
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      return make_mod(t1, t2); 
   }
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Expression(t1->ival % t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only mod ints so far");
}

Expression *Expression::exp(Expression *t1, Expression *t2) {
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      return make_exp(t1, t2); 
   }
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
   t = reduce(t);
   // check if reduceuation should proceed
   if (t->unresolved()) { 
      return make_neg(t); 
   }
   if (t->t == INT)
      return new Expression(-t->ival);
   if (t->t == FLOAT)
      return new Expression(-t->fval);
   throw std::string("We can only negate ints and floats so far");
}

Expression *Expression::land(Expression *t1, Expression *t2) {
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      return make_log_and(t1, t2);
   }
   // we have two resolved expressions, check if both bools
   if (t1->t == BOOL && t2->t == BOOL) {
      return new Expression(t1->bval && t2->bval);
   }
   throw std::string("Can't logical AND anything except bools");
}

Expression *Expression::lor(Expression *t1, Expression *t2) {
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      return make_log_or(t1, t2); 
   }
   // we have two resolved expressions, check if both bools
   if (t1->t == BOOL && t2->t == BOOL) {
      return new Expression(t1->bval || t2->bval);
   }
   throw std::string("Can't logical OR anything except bools");
}

Expression *Expression::lnot(Expression *t) {
   t = reduce(t);
   // check if reduceuation should proceed
   if (t->unresolved()) { 
      return make_log_not(t); 
   }
   if (t->t == BOOL)
      return new Expression(!t->bval);
   throw std::string("We can only NOT bools so far");
}

Expression *Expression::eq(Expression *t1, Expression *t2) {
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      return make_eq(t1, t2);
   }
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      return make_neq(t1, t2);
   }
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      return make_lt(t1, t2);
   }
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
   if (t1->unresolved() || t2->unresolved()) { 
      return make_gt(t1, t2);
   }
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

bool Expression::to_bool() {
   if (t == BOOL) return bval;
   throw std::string("Error: to_bool called on something that's not a boolean.");
}

void ExpressionList::reduce_all() {
   ExpressionList::iterator it;
   for (it = begin(); it != end(); ++it) {
      printf("Reducing: "); fflush(stdout); (*it)->print(); fflush(stdout);
      Expression::reduce(*it);
      printf("OK\n"); fflush(stdout);
   }
}

}