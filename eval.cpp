#include "symbol.h"
#include "ast.h"
#include <cmath> // for pow

namespace pico {

Term *Expression::eval(Expression *expr) {
   switch(expr->t) {
      case Expression::TERM:
      { 
         return Term::eval_update(expr->term);
      }
      case Expression::ASSIGN:
      {  sym_store(expr->assign.var, expr->assign.term);
         return eval(expr->assign.next); }
      case Expression::IF:
      {
         Term::eval_update(expr->if_.cond);
         if (Term::unresolved(expr->if_.cond) > 0) return new Term(expr); // stop evaluating
         if (expr->if_.cond->to_bool()) 
            return Term::eval(expr->if_.if_true); // only evaluate one of the conditions
         else
            return eval(expr->if_.if_false);
      }
   }
}

Term *Term::eval_update(Term *&term) {
   // evaluate the term
   // if the result of evaluation is different than the original
      // delete the original
      // replace the original with the evaluated term
   Term *res = eval(term);
   if (res != term) {
      delete term;
      term = res;
   }
   return term;
}

Term *Term::eval(Term *term) {
   switch (term->t) {
      case ADD: { return add(term->binary.term1, term->binary.term2); }
      case SUB: { return sub(term->binary.term1, term->binary.term2); }
      case MULT: { return mult(term->binary.term1, term->binary.term2); }
      case DIV: { return div(term->binary.term1, term->binary.term2); }
      case MOD: { return mod(term->binary.term1, term->binary.term2); }
      case EXP: { return exp(term->binary.term1, term->binary.term2); }
      case NEG: { return neg(term->unary); }
      case LOG_AND: { return land(term->binary.term1, term->binary.term2); }
      case LOG_OR: { return lor(term->binary.term1, term->binary.term2); }
      case LOG_NOT: { return lnot(term->unary); }
      case EQ: { return eq(term->binary.term1, term->binary.term2); }
      case NEQ: { return neq(term->binary.term1, term->binary.term2); }
      case LT: { return lt(term->binary.term1, term->binary.term2); }
      case GT: { return gt(term->binary.term1, term->binary.term2); }
      case LEQ: { return leq(term->binary.term1, term->binary.term2); }
      case GEQ: { return geq(term->binary.term1, term->binary.term2); }
      case BIT_AND: { return band(term->binary.term1, term->binary.term2); }
      case BIT_OR: { return bor(term->binary.term1, term->binary.term2); }
      case BIT_NOT: { return bnot(term->binary.term1, term->binary.term2); }
      case BIT_XOR: { return bxor(term->binary.term1, term->binary.term2); }
      //literals
      case BOOL: { return mod(term->binary.term1, term->binary.term2); }
      case PARENS: { return Expression::eval(term->expr); }
      case INT: case FLOAT: case CHAR: case STRING: { return term; }
      case VAR: { return eval(sym_lookup(term->var)); }
      case UNRESOLVED: { printf("Can't handle UNRESOLVED yet :(\n"); exit(1); }
      case INVOKE: { printf("Can't handle invoke yet :(\n"); exit(1); }
      // case INVOKE: { printf("Can't handle UNRESOLVED yet :(\n"); exit(1); }
   }
}

unsigned Term::unresolved(Term *t) {
   if (!t) return 1;
   switch (t->t) {
      case BOOL: case STRING: case CHAR:
      case INT: case FLOAT:
      { return 0; }
      case VAR: 
      { t->u = unresolved(sym_lookup(t->var)); return t->u;}
      case UNRESOLVED: 
      { return 1; }
      case INVOKE:
      { t->u = unresolved(t->invoke.func) - t->invoke.term_list->size(); return t->u; }
      case ADD: case SUB: case MOD: case MULT:
      case DIV: case BIT_AND: case BIT_OR: case BIT_XOR:
      case LOG_AND: case LOG_OR: case EXP: case EQ:
      case NEQ: case LT: case GT: case LEQ: case GEQ:
      { t->u = unresolved(t->binary.term1) + unresolved(t->binary.term2); return t->u; }
      case LOG_NOT: case NEG: case BIT_NOT:
      { t->u = unresolved(t->unary); return t->u; }
      case PARENS:
      { t->u = Expression::unresolved(t->expr); return t->u; }
   }
}

unsigned Expression::unresolved(Expression *e) {
   if (!e) return 1;
   switch (e->t) {
      case TERM: 
      { return Term::unresolved(e->term); }
      case ASSIGN: 
      { return Term::unresolved(e->assign.term) + unresolved(e->assign.next); }
      case IF: 
      { return Term::unresolved(e->if_.cond) 
                  + Term::unresolved(e->if_.if_true) 
                  + unresolved(e->if_.if_false); }
   }
}

Term *Term::add(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      return make_add(t1, t2); 
   }
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Term(t1->ival + t2->ival); }
         case FLOAT: { return new Term(t1->ival + t2->ival); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Term(t1->ival + t2->ival); }
         case FLOAT: { return new Term(t1->ival + t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only add ints and floats so far");
}

Term *Term::sub(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      return make_sub(t1, t2); 
   }
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Term(t1->ival - t2->ival); }
         case FLOAT: { return new Term(t1->ival - t2->ival); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Term(t1->ival - t2->ival); }
         case FLOAT: { return new Term(t1->ival - t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only sub ints and floats so far");
}

Term *Term::mult(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      return make_mult(t1, t2); 
   }
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Term(t1->ival * t2->ival); }
         case FLOAT: { return new Term(t1->ival * t2->ival); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Term(t1->ival * t2->ival); }
         case FLOAT: { return new Term(t1->ival * t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only mult ints and floats so far");
}

Term *Term::div(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      return make_div(t1, t2); 
   }
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Term(t1->ival / t2->ival); }
         case FLOAT: { return new Term(t1->ival / t2->ival); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Term(t1->ival / t2->ival); }
         case FLOAT: { return new Term(t1->ival / t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only div ints and floats so far");
}

Term *Term::mod(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      return make_mod(t1, t2); 
   }
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Term(t1->ival % t2->ival); }
         default: break;
      }
   }
   throw std::string("We can only mod ints so far");
}

Term *Term::exp(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      return make_exp(t1, t2); 
   }
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { return new Term((int)pow(t1->ival, t2->ival)); }
         case FLOAT: { return new Term(pow(t1->ival, t2->fval)); }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         case INT: { return new Term(pow(t1->fval, t2->ival)); }
         case FLOAT: { return new Term(pow(t1->fval, t2->fval)); }
         default: break;
      }
   }
   throw std::string("We can only exp ints and floats so far");
}

Term *Term::neg(Term *t) {
   eval_update(t);
   // check if evaluation should proceed
   if (unresolved(t)) { 
      return make_neg(t); 
   }
   if (t->t == INT)
      return new Term(-t->ival);
   if (t->t == FLOAT)
      return new Term(-t->fval);
   throw std::string("We can only negate ints and floats so far");
}

Term *Term::land(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      return make_log_and(t1, t2);
   }
   // we have two resolved expressions, check if both bools
   if (t1->t == BOOL && t2->t == BOOL) {
      return new Term(t1->bval && t2->bval);
   }
   throw std::string("Can't logical AND anything except bools");
}

Term *Term::lor(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      return make_log_or(t1, t2); 
   }
   // we have two resolved expressions, check if both bools
   if (t1->t == BOOL && t2->t == BOOL) {
      return new Term(t1->bval || t2->bval);
   }
   throw std::string("Can't logical OR anything except bools");
}

Term *Term::lnot(Term *t) {
   eval_update(t);
   // check if evaluation should proceed
   if (unresolved(t)) { 
      return make_log_not(t); 
   }
   if (t->t == BOOL)
      return new Term(!t->bval);
   throw std::string("We can only NOT bools so far");
}

Term *Term::eq(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) {
      return make_eq(t1, t2);
   }
   // we have two resolved expressions
   if (t1->t == t2->t) {
      switch (t1->t) {
         // check if literals are same
         case INT:   { return new Term(t1->ival == t2->ival); }
         case FLOAT: { return new Term(t1->fval == t2->fval); }
         case CHAR:  { return new Term(t1->cval == t2->cval); }
         case STRING: { return new Term(*t1->strval == *t2->strval); } //string ptrs must be deref'd
         case BOOL:  { return new Term(t1->bval == t2->bval); }
         case VAR: { 
            throw std::string("eq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: { return new Term(false); } // might want to check function equality, later...
      }
   }
   throw std::string("Can't test equality on mismatched types");
}

Term *Term::neq(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) {
      return make_neq(t1, t2);
   }
   // we have two resolved expressions
   if (t1->t == t2->t) {
      switch (t1->t) {
         // check if literals are same
         case INT: { return new Term(t1->ival != t2->ival); }
         case FLOAT: { return new Term(t1->fval != t2->fval); }
         case CHAR: { return new Term(t1->cval != t2->cval); }
         case STRING: { return new Term(*t1->strval != *t2->strval); } //string ptrs must be deref'd
         case BOOL: { return new Term(t1->bval != t2->bval); }
         case VAR: { 
            throw std::string("neq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: { return new Term(true); } // might want to check function equality, later...
      }
   }
   throw std::string("Can't test inequality on mismatched types");
}

Term *Term::lt(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) {
      return make_lt(t1, t2);
   }
   // we have two resolved expressions
   if (t1->t == INT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->ival < t2->ival); }
         case FLOAT: { return new Term(t1->ival < t2->fval); }
         case CHAR: { return new Term(t1->ival < t2->cval); }
         case VAR: { 
            throw std::string("lt: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->fval < t2->ival); }
         case FLOAT: { return new Term(t1->fval < t2->fval); }
         case CHAR: { return new Term(t1->fval < t2->cval); }
         case VAR: { 
            throw std::string("lt: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == CHAR) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->cval < t2->ival); }
         case FLOAT: { return new Term(t1->cval < t2->fval); }
         case CHAR: { return new Term(t1->cval < t2->cval); }
         case VAR: { 
            throw std::string("lt: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == STRING) {
      switch (t2->t) {
         // check if literals are same
         case STRING: { return new Term(*t1->strval < *t2->strval); }
         case VAR: { 
            throw std::string("lt: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   throw std::string("lt: Can't compare mismatched types");
}

Term *Term::gt(Term *t1, Term *t2) {
   eval_update(t1);
   eval_update(t2);
   // check if evaluation should proceed
   if (unresolved(t1) || unresolved(t2)) {
      return make_gt(t1, t2);
   }
   // we have two resolved expressions
   if (t1->t == INT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->ival > t2->ival); }
         case FLOAT: { return new Term(t1->ival > t2->fval); }
         case CHAR: { return new Term(t1->ival > t2->cval); }
         case VAR: { 
            throw std::string("gt: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->fval > t2->ival); }
         case FLOAT: { return new Term(t1->fval > t2->fval); }
         case CHAR: { return new Term(t1->fval > t2->cval); }
         case VAR: { 
            throw std::string("gt: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == CHAR) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->cval > t2->ival); }
         case FLOAT: { return new Term(t1->cval > t2->fval); }
         case CHAR: { return new Term(t1->cval > t2->cval); }
         case VAR: { 
            throw std::string("gt: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == STRING) {
      switch (t2->t) {
         // check if literals are same
         case STRING: { return new Term(*t1->strval > *t2->strval); }
         case VAR: { 
            throw std::string("gt: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   throw std::string("lt: Can't compare mismatched types");
}

Term *Term::geq(Term *t1, Term *t2) {
   // we have two resolved expressions
   if (t1->t == INT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->ival >= t2->ival); }
         case FLOAT: { return new Term(t1->ival >= t2->fval); }
         case CHAR: { return new Term(t1->ival >= t2->cval); }
         case VAR: { 
            throw std::string("geq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->fval >= t2->ival); }
         case FLOAT: { return new Term(t1->fval >= t2->fval); }
         case CHAR: { return new Term(t1->fval >= t2->cval); }
         case VAR: { 
            throw std::string("geq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == CHAR) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->cval >= t2->ival); }
         case FLOAT: { return new Term(t1->cval >= t2->fval); }
         case CHAR: { return new Term(t1->cval >= t2->cval); }
         case VAR: { 
            throw std::string("geq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == STRING) {
      switch (t2->t) {
         // check if literals are same
         case STRING: { return new Term(*t1->strval >= *t2->strval); }
         case VAR: { 
            throw std::string("geq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   throw std::string("geq: Can't compare mismatched types");
}

Term *Term::leq(Term *t1, Term *t2) {
   // we have two resolved expressions
   if (t1->t == INT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->ival <= t2->ival); }
         case FLOAT: { return new Term(t1->ival <= t2->fval); }
         case CHAR: { return new Term(t1->ival <= t2->cval); }
         case VAR: { 
            throw std::string("leq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == FLOAT) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->fval <= t2->ival); }
         case FLOAT: { return new Term(t1->fval <= t2->fval); }
         case CHAR: { return new Term(t1->fval <= t2->cval); }
         case VAR: { 
            throw std::string("leq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == CHAR) {
      switch (t2->t) {
         // check if literals are same
         case INT: { return new Term(t1->cval <= t2->ival); }
         case FLOAT: { return new Term(t1->cval <= t2->fval); }
         case CHAR: { return new Term(t1->cval <= t2->cval); }
         case VAR: { 
            throw std::string("leq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   if (t1->t == STRING) {
      switch (t2->t) {
         // check if literals are same
         case STRING: { return new Term(*t1->strval <= *t2->strval); }
         case VAR: { 
            throw std::string("leq: Can't handle variables yet!");
            // return new Term(*t1->var->name == *t2->var->name); 
         }
         default: break;
      }
   }
   throw std::string("leq: Can't compare mismatched types");
}


Term *Term::band(Term *t1, Term *t2) {
   throw std::string("bitwise and not implemented yet");
}

Term *Term::bor(Term *t1, Term *t2) {
   throw std::string("bitwise bor not implemented yet");
}

Term *Term::bnot(Term *t1, Term *t2) {
   throw std::string("bitwise not not implemented yet");
}

Term *Term::bxor(Term *t1, Term *t2) {
   throw std::string("bitwise bxor not implemented yet");
}

bool Term::to_bool() {
   if (t == BOOL) return bval;
   throw std::string("Error: to_bool called on something that's not a boolean.");
}

}