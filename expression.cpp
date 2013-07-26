#include "expression.h"

using std::string;

namespace pico {

static int indent = 0;

void prindent() {
   puts("");
   for (int i=0; i<indent; ++i) 
      printf("\t");
}

void upInd() { ++indent; prindent(); }
void dnInd() { --indent; prindent(); }

void Initialize() {
   symdic[Term::ADD] = "+";
   symdic[Term::SUB] = "-";
   symdic[Term::MULT] = "*";
   symdic[Term::DIV] = "/";
   symdic[Term::LT] = "<";
   symdic[Term::GT] = ">";
   symdic[Term::EQ] = "==";
   symdic[Term::NEQ] = "!=";
   symdic[Term::GEQ] = ">=";
   symdic[Term::LEQ] = "<=";
   symdic[Term::LOG_AND] = "&&";
   symdic[Term::LOG_OR] = "||";
   symdic[Term::LOG_NOT] = "!";
   symdic[Term::BIT_AND] = "&";
   symdic[Term::BIT_OR] = "|";
   symdic[Term::BIT_XOR] = "^";
   symdic[Term::BIT_NOT] = "~";
}

Expression::~Expression() { 
   switch (t) {
      case TERM: delete term; break;
      case ASSIGN: delete assign.assign; delete assign.next; break;
      case IF: delete if_s.if_s; delete if_s.next; break;
      default: break;
   }
   if (next) delete next; 
}
Var::~Var() { delete name; }
If::~If() { delete cond; delete if_true; }
Assign::~Assign() { delete var; delete term; }
Invoke::~Invoke() { delete func; if (term_list) delete term_list; }

void Assign::print() {
   printf("%s = (", var->name->c_str());
   upInd();
   term->print();
   dnInd();
   printf(")"); fflush(stdout);
}

void If::print() {
   printf("IF "); fflush(stdout);
   cond->print(); puts("");
   printf("THEN "); fflush(stdout);
   upInd();
   if_true->print();
   dnInd();
}

void Expression::print() {
   switch(t) {
      case IF: 
      {
         if_s.if_s->print();
         printf("ELSE ");
         upInd();
         if_s.next->print();
         dnInd();
         return;
      }
      case ASSIGN:
      {
         assign.assign->print();
         assign.next->print();
         return;
      }
      case TERM:
      {
         term->print();
         return;
      }
      default:
         printf("UNKNOWN EXPRESSION"); fflush(stdout);
         return;
   }
}

void Term::print() {
   if (t != VAR && t != INT && t != STRING && t != FLOAT && t != INVOKE && t != CHAR)
      {  }
   switch (t) {
      case EMPTY: 
      {
         printf("(Empty term)"); fflush(stdout);
         return;
      }
      case INVOKE:
      {
         printf("call ("); fflush(stdout);
         upInd();
         invoke->func->print();
         dnInd();
         printf(", with(");  fflush(stdout);
         upInd();
         invoke->term_list->print();
         dnInd();
         printf(")");  fflush(stdout);
         return;
      }
      case FLOAT:
      {
         printf("flt %f", fval); fflush(stdout);
         return;
      }
      case INT:
      {
         printf("int %d", ival); fflush(stdout);
         return;
      }
      case CHAR:
      {
         printf("char %c", cval); fflush(stdout);
         return;
      }
      case BOOL:
      {
         printf("%s", bval == true ? "TRUE" : "FALSE"); fflush(stdout);
         return;
      }
      case STRING:
      {
         printf("\"%s\"", strval->c_str()); fflush(stdout);
         return;
      }
      case VAR:
      {
         printf("var %s", var->name->c_str()); fflush(stdout);
         if (var->type) printf(" (%s)", var->type->c_str());
         return;
      }
      case ADD: case SUB: case MULT: case DIV:
      case LT: case GT: case GEQ: case LEQ:
      case EQ: case NEQ: case LOG_AND: case LOG_OR:
      case BIT_AND: case BIT_OR: case BIT_XOR:
      {
         printf("(");
         binary.term1->print(); fflush(stdout);
         printf(" %s ", symdic[t].c_str()); fflush(stdout);
         binary.term2->print(); fflush(stdout);
         printf(")");
         break;
      }
      case LOG_NOT: case BIT_NOT:
      {
         printf("%s(", symdic[t].c_str());
         unary->print();
         printf(")");
         break;
      }
      case PARENS:
      {
         unary->print();
         return;
      }
      default:
         printf("UNKNOWN TERM"); fflush(stdout);
         break;
   }
}

Term *make_add(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::ADD);
}
Term *make_sub(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::SUB);
}
Term *make_mult(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::MULT);
}
Term *make_div(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::DIV);
}
Term *make_mod(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::MOD);
}
Term *make_exp(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::EXP);
}
Term *make_eq(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::EQ);
}
Term *make_lt(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::LT);
}
Term *make_gt(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::GT);
}
Term *make_leq(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::LEQ);
}
Term *make_geq(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::GEQ);
}

Term *make_neq(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::NEQ);
}

Term *make_log_and(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::LOG_AND);
}
Term *make_log_or(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::LOG_OR);
}
Term *make_log_not(Term *term)  {
   return new Term(term, Term::LOG_NOT);
}
Term *make_bit_and(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::BIT_AND);
}
Term *make_bit_or(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::BIT_OR);
}
Term *make_bit_xor(Term *term1, Term *term2) {
   return new Term(term1, term2, Term::BIT_XOR);
}

Term *make_bit_not(Term *term) {
   return new Term(term, Term::BIT_NOT);
}

Term *make_neg(Term *term) {
   return new Term(term, Term::NEG);
}

void Expression::append(Expression *expr) {
   puts("Appending expression");
   expr->print();
   Expression *expr_list = this;
   while (expr_list) {
      if (expr_list->next) 
         expr_list = expr_list->next;
      else {
         expr_list->next = expr;
         return;
      }
   }
}

void Term::append(Term *term) {
   Term *term_list = this;
   while (term_list) {
      if (term_list->next) 
         term_list = term_list->next;
      else {
         term_list->next = term;
         return;
      }
   }
}

void ExpressionList::print() {
   std::deque<Expression *>::iterator it;
   bool first = true;
   printf("exprs[");
   for (it = begin(); it != end(); it++) {
      if (first) first = false; else printf(", ");  fflush(stdout);
      (*it)->print();
   }
   printf("]");
}

void TermList::print() {
   std::deque<Term *>::iterator it;
   bool first = true;
   printf("terms[");
   for (it = begin(); it != end(); it++) {
      if (first) first = false; else printf(", ");  fflush(stdout);
      (*it)->print();
   }
   printf("]");
}

}