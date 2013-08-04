#include "../include/ast.h"

using namespace std;

namespace pico {

static map<Expression::Type, string> symdic;

void Expression::init() {
   symdic[ADD] = "+";
   symdic[SUB] = "-";
   symdic[MULT] = "*";
   symdic[DIV] = "/";
   symdic[LT] = "<";
   symdic[GT] = ">";
   symdic[EQ] = "==";
   symdic[NEQ] = "!=";
   symdic[GEQ] = ">=";
   symdic[LEQ] = "<=";
   symdic[EXP] = "^";
   symdic[LOG_AND] = "&&";
   symdic[LOG_OR] = "||";
   symdic[LOG_NOT] = "!";
   symdic[BIT_AND] = "&";
   symdic[BIT_OR] = "|";
   symdic[BIT_XOR] = "^^";
   symdic[BIT_NOT] = "~";
   symdic[NEG] = "-";
   symdic[MOD] = "%";
   symdic[INT] = "INT";
   symdic[FLOAT] = "FLOAT";
   symdic[CHAR] = "CHAR";
   symdic[STRING] = "STRING";
   symdic[BOOL] = "BOOL";
   symdic[VAR] = "VAR";
   symdic[INVOKE] = "INVOKE";
   symdic[UNRESOLVED] = "UNRESOLVED";
}

Expression::~Expression() {
   //TODO
}

void Expression::print_info() {
   if (symdic.find(t) == symdic.end()) 
      printf("Unknown type\n");
   else 
      printf("type %s", symdic[t].c_str());
   if (t == VAR) 
      printf(" (name %s)", vname->c_str());
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
   expr->name = new string(name);
   expr->type = NULL;
   free(name);
   return expr;
}

Expression *Expression::make_var(char *type, char *name) {
   Expression *expr = make_var(name);
   expr->type = new string(type);
   free(type);
   return expr;
}

ostream& operator<<(ostream& os, ExpressionList *&exprlist) {
   ExpressionList::iterator it;
   bool first = true;
   os << "ExprList(";
   for (it = exprlist->begin(); it != exprlist->end(); it++) {
      if (first) first = false; else os << ", ";
      os << "Expr(" << *it << ")";
   }
   os << ")";
   return os;
}


ostream& operator<<(ostream& os, Expression *&expr) {
   switch(expr->t) {
      case Expression::IF: 
      {
         os << "IF " << expr->cond << " THEN " << expr->if_true 
            << " ELSE " << expr->if_false;
         return os;
      }
      case Expression::ASSIGN:
      {
         os << *expr->vname << " = (" << expr->right_hand << "), " << expr->next;
         return os;
      }
      case Expression::UNRESOLVED: 
      {
         printf("(empty expr)"); fflush(stdout);
         return os;
      }
      case Expression::INVOKE:
      {
         os << "(" << expr->func << ") on (" << expr->expr_list << ")";
         return os;
      }
      case Expression::FLOAT:
      {
         os << expr->fval;
         return os;
      }
      case Expression::INT:
      {
         os << expr->ival;
         return os;
      }
      case Expression::CHAR:
      {
         os << expr->cval;
         return os;
      }
      case Expression::BOOL:
      {
         os << (expr->bval ? "TRUE" : "FALSE");
         return os;
      }
      case Expression::STRING:
      {
         os << expr->strval;
         return os;
      }
      case Expression::VAR:
      {
         if (expr->type) os << *expr->type << " ";
         os << *expr->name;
         return os;
      }
      case Expression::ADD: case Expression::SUB: case Expression::MULT: 
      case Expression::DIV: case Expression::LT: case Expression::GT: 
      case Expression::GEQ: case Expression::LEQ: case Expression::MOD: 
      case Expression::EQ: case Expression::NEQ: case Expression::LOG_AND: 
      case Expression::LOG_OR: case Expression::BIT_AND: 
      case Expression::BIT_OR: case Expression::BIT_XOR: case Expression::EXP:
      {
         os << "(" << expr->expr1 << " " << symdic[expr->t]
            << " " << expr->expr2 << ")";
         break;
      }
      case Expression::LOG_NOT: case Expression::BIT_NOT: case Expression::NEG:
      {
         os << symdic[expr->t] << "(" << expr->unary << ")";
         break;
      }
      default:
         os << "UNKNOWN EXPR " << expr->t;
         break;
   }
   return os;
}

bool Expression::is_binary() const {
   switch(t) {
      case ADD: case SUB: case MULT: 
      case DIV: case LT: case GT: 
      case GEQ: case LEQ: case MOD: 
      case EQ: case NEQ: case LOG_AND: 
      case LOG_OR: case BIT_AND: 
      case BIT_OR: case BIT_XOR: case EXP:
      {
         return true;
      }
      default:
         return false;
   }
}

bool Expression::is_unary() const {
   return (t == BIT_NOT || t == LOG_NOT || t == NEG);
}

void Expression::set_parent(Expression *parent) { this->parent = parent; }

}