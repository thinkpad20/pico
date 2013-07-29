#include "ast.h"

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
   symdic[Term::INT] = "INT";
   symdic[Term::FLOAT] = "FLOAT";
   symdic[Term::CHAR] = "CHAR";
   symdic[Term::STRING] = "STRING";
   symdic[Term::BOOL] = "BOOL";
   symdic[Term::VAR] = "VAR";
   symdic[Term::INVOKE] = "INVOKE";
   symdic[Term::UNRESOLVED] = "UNRESOLVED";
   symdic[Term::PARENS] = "PARENS";
}

Expression::~Expression() { 
   switch (t) {
      case TERM: delete term; break;
      case ASSIGN: delete assign.term; delete assign.var; delete assign.next; break;
      case IF: delete if_.cond; delete if_.if_true; delete if_.if_false; break;
      default: break;
   }
}

Var::~Var() { delete name; if (type) delete type; }

Term::~Term() {
   switch (t) {
      case ADD:   { delete binary.term1; delete binary.term2; break; }
      case SUB:   { delete binary.term1; delete binary.term2; break; }
      case MULT:  { delete binary.term1; delete binary.term2; break; }
      case DIV:   { delete binary.term1; delete binary.term2; break; }
      case MOD:   { delete binary.term1; delete binary.term2; break; }
      case EXP:   { delete binary.term1; delete binary.term2; break; }
      case LOG_AND:  { delete binary.term1; delete binary.term2; break; }
      case LOG_OR:   { delete binary.term1; delete binary.term2; break; }
      case LOG_NOT:  { delete unary; break; }
      case NEG:   { delete unary; break; }
      case EQ:    { delete binary.term1; delete binary.term2; break; }
      case NEQ:   { delete binary.term1; delete binary.term2; break; }
      case LT:    { delete binary.term1; delete binary.term2; break; }
      case GT:    { delete binary.term1; delete binary.term2; break; }
      case LEQ:   { delete binary.term1; delete binary.term2; break; }
      case GEQ:   { delete binary.term1; delete binary.term2; break; }
      case BIT_AND: { delete binary.term1; delete binary.term2; break; }
      case BIT_OR:   { delete binary.term1; delete binary.term2; break; }
      case BIT_NOT:  { delete binary.term1; delete binary.term2; break; }
      case BIT_XOR:  { delete binary.term1; delete binary.term2; break; }
      case BOOL:     { delete binary.term1; delete binary.term2; break; }
      case PARENS:   { delete expr; break; }
      case INT: case FLOAT: case CHAR: { break; }
      case STRING:   { fflush(stdout); delete strval; break; }
      case VAR:      { delete var; break; }
      case UNRESOLVED:  { break; }
      case INVOKE:      { delete invoke.func; delete invoke.term_list; break; }
   }
}

void Term::print_info() {
   if (symdic.find(t) == symdic.end()) printf("Unknown type\n");
   else printf("type %s", symdic[t].c_str());
   if (t == VAR) printf(" (name %s)", var->name->c_str());
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

Term::Term(Expression *expr): t(PARENS), expr(expr), u(expr->u) {} // can't be defined in header

void Term::print() {
   // printf("PRINTING TERM %p, type %d\n", this, t);
   switch (t) {
      case UNRESOLVED: 
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