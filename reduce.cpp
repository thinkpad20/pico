#include "symbol.h"
#include "ast.h"
#include <cmath> // for pow

namespace pico {

Term *Expression::reduce(Expression *expr) {
   switch(expr->t) {
      case Expression::TERM:
      { 
         Term *res = Term::reduce(expr->term);
         if (!res->is_eq(expr->term)) 
         {  delete expr->term;
            expr->term = res; }
         return res;
      }
      case Expression::ASSIGN:
      {  sym_store(expr->assign.var->name, expr->assign.term);
         return reduce(expr->assign.next); }
      case Expression::IF:
      {
         if (Term::unresolved(expr->if_.cond) > 0) 
            return new Term(expr); // stop reducing
         if (Term::reduce(expr->if_.cond)->to_bool()) 
            return Term::reduce(expr->if_.if_true); // only reduce one of the conditions
         else
            return reduce(expr->if_.if_false);
      }
      case Expression::UNRESOLVED:
      {
         printf("Unresolved, can't evaluate further.\n");
         return GLOBAL_UNRESOLVED_TERM;
      }
   }
}

int e_num = 0;
int callno = 0;

Term *Term::reduce(Term *term) {
   // int n = e_num++;
   // printf("==============reducing %d: ", n); term->print(); puts("");
   if (!term) {
      printf("term was null\n");
      return new Term();
   }
   if (term == GLOBAL_UNRESOLVED_TERM) 
      return term;
   // int thiscall = callno++;
   // printf("%d calling reduce on term: ", thiscall);
   // term->print_info(); printf(" "); term->print(); puts("");
   switch (term->t) {
      case ADD: { return add(term->binary.term1, term->binary.term2); }
      case SUB: { return sub(term->binary.term1, term->binary.term2); }
      case MULT: { Term *res = mult(term->binary.term1, term->binary.term2); 
                   //printf("finishing call %d, res %p: ", thiscall, res); res->print(); puts(""); 
                   return res;}
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
      case PARENS: { return Expression::reduce(term->expr); }
      case BOOL: case INT: case FLOAT: case CHAR: case STRING: { return term; }
      case VAR:
      { 
         Term *target = sym_lookup(term->var->name);
         if (target == GLOBAL_UNRESOLVED_TERM) {
            printf("found an unresolved term\n"); fflush(stdout);
            add_local(term->var->name);
            printf("we're going to return: "); term->print(); puts("");
            printf("which has unresolved of %u\n", unresolved(term));
            return term;
         }
         Term *result = reduce(target);
         sym_update(term->var->name, result);
         // printf("finished sym_update\n"); fflush(stdout);
         return result; 
      }
      case UNRESOLVED:  { return term; }
      case INVOKE:      { throw std::string("Can't handle invoke yet :(\n"); }
   }
}

unsigned Term::unresolved(Term *t) {
   printf("calling unresolved on %p: ", t); fflush(stdout); t->print(); puts("");
   if (!t) return 1;
   if (t == GLOBAL_UNRESOLVED_TERM) return 1;
   printf("t = %d\n", t->t); fflush(stdout);
   switch (t->t) {
      case BOOL: case STRING: case CHAR: case INT: case FLOAT:
      { return 0; }
      case VAR: {
         printf("looking up a variable\n"); fflush(stdout);
         Term *res = sym_lookup(t->var->name);
         printf("res is: "); fflush(stdout); res->print();
         if (res->t == UNRESOLVED) {
            delete res;
            return 1;
         }
         return unresolved(res);
      }
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
      case UNRESOLVED:
      { return 1; }
   }
}

bool Expression::is_eq(Expression *other) {
   // printf("Checking if "); print(); printf(" == "); other->print(); puts("");
   if (t != other->t) return false;
   switch (t) {
      case TERM: { return term == other->term; }
      case ASSIGN: { return assign.var == other->assign.var 
                            && assign.term == other->assign.term 
                            && assign.next == other->assign.next; }
      case IF: { return if_.cond == other->if_.cond
                        && if_.if_true == other->if_.if_true
                        && if_.if_false == other->if_.if_false; }
      case UNRESOLVED: { return false; }
   }
}

bool Term::is_eq(Term *other) {
   // printf("Checking if "); print(); printf(" == "); other->print(); puts("");
   if (t != other->t) return false;
   switch (t) {
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
      { return binary.term1 == other->binary.term1 && binary.term2 == other->binary.term2; }
      // unary
      case NEG:
      case LOG_NOT:
      { return unary == other->unary; }
      //literals
      case PARENS: { return expr == other->expr; }
      case INT: { return ival == other->ival; }
      case FLOAT: { return fval == other->fval; }
      case CHAR: { return cval == other->cval; }
      case STRING: { return strval == other->strval; }
      case BOOL: { return bval == other->bval; }
      case VAR: { return var == other->var; }
      case UNRESOLVED:  { return false; }
      case INVOKE:      
      { return invoke.func == other->invoke.func && invoke.term_list == other->invoke.term_list; }
   }
}

Term *Term::add(Term *t1, Term *t2) {
   // printf("Adding: "); t1->print(); printf(" and "); t2->print(); puts("");
   Term *temp1 = t1, *temp2 = t2;
   // printf("going to subreduceuate, %p %p\n", t1, t2); fflush(stdout);
   t1 = reduce(t1); t2 = reduce(t2);
   // printf("add reduceuated, %p %p, seeing if unresolved\n", t1, t2); fflush(stdout);
   // check if reduceuation should proceed
   unsigned u1 = unresolved(t1);
   // printf("u1 = %u\n", u1); fflush(stdout);
   unsigned u2 = unresolved(t2);
   // printf("u2 = %u\n", u2); fflush(stdout);
   if (u1 || u2) { 
      // printf("add unresolved\n");
      return make_add(t1, t2); 
   }
   // printf("not unresolved, constant expr\n");
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
   // printf("Subtracting: "); t1->print(); printf(" and "); t2->print(); puts("");
   t1 = reduce(t1); t2 = reduce(t2);
   // printf("sub reduceuated, seeing if unresolved\n"); fflush(stdout);
   // check if reduceuation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      // printf("sub unresolved\n");
      return make_sub(t1, t2); 
   }
   // printf("not unresolved, constant expr\n");
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
   // printf("Multiplying: "); t1->print(); printf(" and "); t2->print(); puts("");
   t1 = reduce(t1); 
   // printf("reduceuating t2\n");
   t2 = reduce(t2);
   // printf("done reduceuating t2\n");
   // printf("mult reduceuated, seeing if unresolved\n"); fflush(stdout);
   // check if reduceuation should proceed
   if (unresolved(t1) || unresolved(t2)) { 
      // printf("mult is unresolved\n");
      return make_mult(t1, t2); 
   }
   // printf("not unresolved, constant expr\n");
   if (t1->t == INT) {
      switch (t2->t) {
         case INT: { Term *res = new Term(t1->ival * t2->ival); 
            // printf("\nthe result is %p: ", res); res->print(); puts(""); 
            return res;}
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
   // printf("Dividing: "); t1->print(); printf(" and "); t2->print(); puts("");
   t1 = reduce(t1); t2 = reduce(t2);
   // printf("div reduceuated, seeing if unresolved\n"); fflush(stdout);
   // check if reduceuation should proceed
   if (unresolved(t1) || unresolved(t2)) {
      // printf("div is unresolved\n");
      return make_div(t1, t2); 
   }
   // printf("not unresolved, constant expr\n");
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
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
   t = reduce(t);
   // check if reduceuation should proceed
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
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
   t = reduce(t);
   // check if reduceuation should proceed
   if (unresolved(t)) { 
      return make_log_not(t); 
   }
   if (t->t == BOOL)
      return new Term(!t->bval);
   throw std::string("We can only NOT bools so far");
}

Term *Term::eq(Term *t1, Term *t2) {
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
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
   t1 = reduce(t1); t2 = reduce(t2);
   // check if reduceuation should proceed
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