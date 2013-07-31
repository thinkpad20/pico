#include "ast-mod.h"

using std::string;

namespace pico {

static int indent = 0;
static std::map<Expression::Type, std::string> symdic;

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

// Expression::~Expression() { 
//    switch (t) {
//       case TERM: delete expr; break;
//       case ASSIGN: delete assign.expr; delete assign.var; delete assign.next; break;
//       case IF: delete if_.cond; delete if_.if_true; delete if_.if_false; break;
//       default: break;
//    }
// }

Var::~Var() { delete name; if (type) delete type; }

Expression::~Expression() {
   // printf("call to expr destruct, t is %d, %p\n", t, this);
   // switch (t) {
   //    case ADD:
   //    case SUB:
   //    case MULT:  
   //    case DIV:   
   //    case MOD:   
   //    case EXP:   
   //    case LOG_AND: 
   //    case LOG_OR:  
   //    case EQ:    
   //    case NEQ:   
   //    case LT:    
   //    case GT:    
   //    case LEQ:   
   //    case GEQ:   
   //    case BIT_AND: 
   //    case BIT_OR:  
   //    case BIT_NOT: 
   //    case BIT_XOR: { printf("deleting %p and %p\n", binary.expr1, binary.expr2); 
   //                    binary.expr1->print(); binary.expr2->print(); 
   //                    delete binary.expr1; delete binary.expr2; break; }
   //    case LOG_NOT:  { delete unary; break; }
   //    case NEG:   { delete unary; break; }
   //    case SINGLETON:   { delete expr; break; }
   //    case INT: case FLOAT: case CHAR: case BOOL: { break; }
   //    case STRING:   { fflush(stdout); delete strval; break; }
   //    case VAR:      { /*delete var;*/ break; }
   //    case UNRESOLVED:  { break; }
   //    case INVOKE:      { delete invoke.func; delete invoke.expr_list; break; }
   // }
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

Expression *make_add(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::ADD);
}
Expression *make_sub(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::SUB);
}
Expression *make_mult(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::MULT);
}
Expression *make_div(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::DIV);
}
Expression *make_mod(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::MOD);
}
Expression *make_exp(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::EXP);
}
Expression *make_eq(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::EQ);
}
Expression *make_lt(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::LT);
}
Expression *make_gt(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::GT);
}
Expression *make_leq(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::LEQ);
}
Expression *make_geq(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::GEQ);
}

Expression *make_neq(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::NEQ);
}

Expression *make_log_and(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::LOG_AND);
}
Expression *make_log_or(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::LOG_OR);
}
Expression *make_log_not(Expression *expr)  {
   return new Expression(expr, Expression::LOG_NOT);
}
Expression *make_bit_and(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::BIT_AND);
}
Expression *make_bit_or(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::BIT_OR);
}
Expression *make_bit_xor(Expression *expr1, Expression *expr2) {
   return new Expression(expr1, expr2, Expression::BIT_XOR);
}

Expression *make_bit_not(Expression *expr) {
   return new Expression(expr, Expression::BIT_NOT);
}

Expression *make_neg(Expression *expr) {
   return new Expression(expr, Expression::NEG);
}

Expression *make_var(char *name) {
   Expression *expr = new Expression();
   expr->t = Expression::VAR;
   expr->var.name = new std::string(name);
   free(name);
   return expr;
}

Expression *make_var(char *type, char *name) {
   Expression *expr = make_var(name);
   expr->var.type = new std::string(type);
   free(type);
   return expr;
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

}