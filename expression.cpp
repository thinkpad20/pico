#include "expression.h"

using std::string;

namespace pico {

static int indent = 0;
static std::map<Term::Type, std::string> symdic;

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
   symdic[Term::MOD] = "%";
}

Expression::~Expression() { 
   switch (t) {
      case TERM: delete term; break;
      case ASSIGN: delete assign.term; delete assign.var; delete assign.next; break;
      case IF: delete if_.cond; delete if_.if_true; delete if_.if_false; break;
      default: break;
   }
   if (next) delete next; 
}

Var::~Var() { delete name; if (type) delete type; }

Term::~Term() {
   switch (t) {
      case INVOKE:
      {
         delete invoke.func; if (invoke.term_list) delete invoke.term_list; 
         return;
      }
      default:
         printf("Can't delete most types of terms...\n");
         return;
   }
}

void Expression::print() {
   // printf("PRINTING EXPR %p, type %d\n", this, t);
   switch(t) {
      case IF: 
      {
         printf("IF "); fflush(stdout);
         upInd();
         if_.cond->print(); puts("");
         dnInd();
         printf("THEN "); fflush(stdout);
         upInd();
         if_.if_true->print();
         dnInd();
         printf("ELSE ");
         upInd();
         if_.if_false->print();
         dnInd();
         return;
      }
      case ASSIGN:
      {
         printf("%s = (", assign.var->name->c_str());
         upInd();
         assign.term->print();
         dnInd();
         printf("),"); fflush(stdout);
         --indent;
         upInd();
         assign.next->print();
         ++indent;
         dnInd();
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
   // printf("PRINTING TERM %p, type %d\n", this, t);
   switch (t) {
      case EMPTY: 
      {
         printf("(Empty term)"); fflush(stdout);
         return;
      }
      case INVOKE:
      {
         if (invoke.func->t != Term::VAR)
            printf("("); fflush(stdout);
         invoke.func->print();
         if (invoke.func->t != Term::VAR)
            printf(") on "); fflush(stdout);
         printf("(");  fflush(stdout);
         invoke.term_list->print();
         printf(")"); fflush(stdout);
         return;
      }
      case FLOAT:
      {
         printf("%f", fval); fflush(stdout);
         return;
      }
      case INT:
      {
         printf("%d", ival); fflush(stdout);
         return;
      }
      case CHAR:
      {
         printf("%c", cval); fflush(stdout);
         return;
      }
      case BOOL:
      {
         printf("%s", bval ? "TRUE" : "FALSE"); fflush(stdout);
         return;
      }
      case STRING:
      {
         printf("\"%s\"", strval->c_str()); fflush(stdout);
         return;
      }
      case VAR:
      {
         if (var->type) printf("%s ", var->type->c_str());
         printf("%s", var->name->c_str()); fflush(stdout);
         return;
      }
      case ADD: case SUB: case MULT: case DIV: case LT: case GT: case GEQ: case LEQ: case MOD:
      case EQ: case NEQ: case LOG_AND: case LOG_OR: case BIT_AND: case BIT_OR: case BIT_XOR:
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
         expr->print();
         return;
      }
      default:
         printf("UNKNOWN TERM %d\n", t); fflush(stdout);
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
   printf("EXPLIST");
   upInd();
   for (it = begin(); it != end(); it++) {
      if (first) first = false; else printf(", ");  fflush(stdout);
      (*it)->print();
   }
   dnInd();
}

void TermList::print() {
   std::deque<Term *>::iterator it;
   bool first = true;
   for (it = begin(); it != end(); it++) {
      if (first) first = false; else printf(", ");  fflush(stdout);
      (*it)->print();
   }
}

}