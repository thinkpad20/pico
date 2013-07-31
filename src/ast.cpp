#include "../include/ast.h"

using namespace std;

namespace pico {

static int indent = 0;
static map<Expression::Type, string> symdic;

void prindent() {
   puts("");
   for (int i=0; i<indent; ++i) 
      printf("\t");
}

void upInd() { ++indent; prindent(); }
void dnInd() { --indent; prindent(); }

void Initialize() {
   symdic[Expression::ADD] = "+";
   symdic[Expression::SUB] = "-";
   symdic[Expression::MULT] = "*";
   symdic[Expression::DIV] = "/";
   symdic[Expression::LT] = "<";
   symdic[Expression::GT] = ">";
   symdic[Expression::EQ] = "==";
   symdic[Expression::NEQ] = "!=";
   symdic[Expression::GEQ] = ">=";
   symdic[Expression::LEQ] = "<=";
   symdic[Expression::EXP] = "^";
   symdic[Expression::LOG_AND] = "&&";
   symdic[Expression::LOG_OR] = "||";
   symdic[Expression::LOG_NOT] = "!";
   symdic[Expression::BIT_AND] = "&";
   symdic[Expression::BIT_OR] = "|";
   symdic[Expression::BIT_XOR] = "^^";
   symdic[Expression::BIT_NOT] = "~";
   symdic[Expression::MOD] = "%";
   symdic[Expression::INT] = "INT";
   symdic[Expression::FLOAT] = "FLOAT";
   symdic[Expression::CHAR] = "CHAR";
   symdic[Expression::STRING] = "STRING";
   symdic[Expression::BOOL] = "BOOL";
   symdic[Expression::VAR] = "VAR";
   symdic[Expression::INVOKE] = "INVOKE";
   symdic[Expression::UNRESOLVED] = "UNRESOLVED";
}

Expression::~Expression() {

}

void Expression::print_info() {
   if (symdic.find(t) == symdic.end()) 
      printf("Unknown type\n");
   else 
      printf("type %s", symdic[t].c_str());
   if (t == VAR) 
      printf(" (name %s)", var.name->c_str());
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
         printf("%s = (", assign.vname->c_str());
         upInd();
         if (sym_contains(assign.vname))
            sym_lookup(assign.vname)->print();
         else
            assign.expr->print();
         dnInd();
         printf("),"); fflush(stdout);
         --indent;
         upInd();
         assign.next->print();
         ++indent;
         dnInd();
         return;
      }
      case UNRESOLVED: 
      {
         printf("(Empty expr)"); fflush(stdout);
         return;
      }
      case INVOKE:
      {
         if (invoke.func->t != Expression::VAR)
            printf("("); fflush(stdout);
         invoke.func->print();
         if (invoke.func->t != Expression::VAR)
            printf(") on "); fflush(stdout);
         printf("(");  fflush(stdout);
         invoke.expr_list->print();
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
         if (var.type) printf("%s ", var.type->c_str());
         printf("%s", var.name->c_str()); fflush(stdout);
         return;
      }
      case ADD: case SUB: case MULT: case DIV: case LT: case GT: case GEQ: 
      case LEQ: case MOD: case EQ: case NEQ: case LOG_AND: case LOG_OR: 
      case BIT_AND: case BIT_OR: case BIT_XOR: case EXP:
      {
         printf("(");
         binary.expr1->print(); fflush(stdout);
         printf(" %s ", symdic[t].c_str()); fflush(stdout);
         binary.expr2->print(); fflush(stdout);
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
      default:
         printf("UNKNOWN TERM %d\n", t); fflush(stdout);
         break;
   }
}

Expression *Expression::make_add(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::ADD);
}
Expression *Expression::make_sub(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::SUB);
}
Expression *Expression::make_mult(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::MULT);
}
Expression *Expression::make_div(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::DIV);
}
Expression *Expression::make_mod(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::MOD);
}
Expression *Expression::make_exp(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::EXP);
}
Expression *Expression::make_eq(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::EQ);
}
Expression *Expression::make_lt(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::LT);
}
Expression *Expression::make_gt(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::GT);
}
Expression *Expression::make_leq(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::LEQ);
}
Expression *Expression::make_geq(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::GEQ);
}

Expression *Expression::make_neq(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::NEQ);
}

Expression *Expression::make_log_and(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::LOG_AND);
}
Expression *Expression::make_log_or(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::LOG_OR);
}
Expression *Expression::make_log_not(Expression *expr)  {
   return new Expression(expr, Expression::LOG_NOT);
}
Expression *Expression::make_bit_and(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::BIT_AND);
}
Expression *Expression::make_bit_or(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::BIT_OR);
}
Expression *Expression::make_bit_xor(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::BIT_XOR);
}

Expression *Expression::make_bit_not(Expression *expr) {
   return new Expression(expr, Expression::BIT_NOT);
}

Expression *Expression::make_neg(Expression *expr) {
   return new Expression(expr, Expression::NEG);
}

Expression *Expression::make_var(char *name) {
   Expression *expr = new Expression();
   expr->t = Expression::VAR;
   expr->var.name = new string(name);
   free(name);
   return expr;
}

Expression *Expression::make_var(char *type, char *name) {
   Expression *expr = make_var(name);
   expr->var.type = new string(type);
   free(type);
   return expr;
}

void ExpressionList::print() {
   deque<Expression *>::iterator it;
   bool first = true;
   printf("EXPLIST");
   upInd();
   for (it = begin(); it != end(); it++) {
      if (first) first = false; else printf(", ");  fflush(stdout);
      (*it)->print();
   }
   dnInd();
}


ostream& operator<<(ostream& os, const Expression& expr) {
   switch(expr.t) {
      case Expression::IF: 
      {
         printf("IF "); fflush(stdout);
         upInd();
         expr.if_.cond->print(); puts("");
         dnInd();
         printf("THEN "); fflush(stdout);
         upInd();
         expr.if_.if_true->print();
         dnInd();
         printf("ELSE ");
         upInd();
         expr.if_.if_false->print();
         dnInd();
         return os;
      }
      case Expression::ASSIGN:
      {
         printf("%s = (", expr.assign.vname->c_str());
         upInd();
         if (expr.sym_contains(expr.assign.vname))
            expr.sym_lookup(expr.assign.vname)->print();
         else
            expr.assign.expr->print();
         dnInd();
         printf("),"); fflush(stdout);
         --indent;
         upInd();
         expr.assign.next->print();
         ++indent;
         dnInd();
         return os;
      }
      case Expression::UNRESOLVED: 
      {
         printf("(Empty expr)"); fflush(stdout);
         return os;
      }
      case Expression::INVOKE:
      {
         os << "(" << expr.invoke.func << ") on (" << expr.invoke.expr_list << ")";
         return os;
      }
      case Expression::FLOAT:
      {
         os << expr.fval;
         return os;
      }
      case Expression::INT:
      {
         os << expr.ival;
         return os;
      }
      case Expression::CHAR:
      {
         os << expr.cval;
         return os;
      }
      case Expression::BOOL:
      {
         os << (expr.bval ? "TRUE" : "FALSE");
         return os;
      }
      case Expression::STRING:
      {
         os << expr.strval;
         return os;
      }
      case Expression::VAR:
      {
         if (expr.var.type) os << expr.var.type;
         os << expr.var.name;
         return os;
      }
      case Expression::ADD: case Expression::SUB: case Expression::MULT: 
      case Expression::DIV: case Expression::LT: case Expression::GT: 
      case Expression::GEQ: case Expression::LEQ: case Expression::MOD: 
      case Expression::EQ: case Expression::NEQ: case Expression::LOG_AND: 
      case Expression::LOG_OR: case Expression::BIT_AND: 
      case Expression::BIT_OR: case Expression::BIT_XOR: case Expression::EXP:
      {
         os << "(" << expr.binary.expr1 << " " << symdic[expr.t]
            << " " << expr.binary.expr2 << ")";
         break;
      }
      case Expression::LOG_NOT: case Expression::BIT_NOT:
      {
         os << symdic[expr.t] << "(" << expr.unary << ")";
         break;
      }
      default:
         os << "UNKNOWN EXPR " << expr.t;
         break;
   }
   return os;
}

}