#include "ast.h"
#include <map>

using namespace std;

namespace pico {

static map<const char *, Expression *> named_functions;
static Expression *blank_expr = NULL;
static Expression *root_expr = NULL;
ExpressionList *parsed_expressions = NULL;

static const char *primitive_symbols[] = 
{ "+", "-", "*", "/", "%", "==", 
  "<", ">", "<=", ">=", "&&",
  "||", "!", "^" };

void Expression::init() {
   for (int i = 0; i < sizeof(primitive_symbols)/sizeof(const char *); ++i)
      named_functions[primitive_symbols[i]] = make_var(primitive_symbols[i]);
}

ExpressionList *get_expressions() {
   return parsed_expressions;
}

Expression *Expression::make_var(const char *name) {
   Expression *res = new Expression(new string(name));
   res->t = VAR;
   return res;
}

string *Expression::is_primitive_symbol() {
   if (t != VAR) return NULL;
   for (int i = 0; i < sizeof(primitive_symbols)/sizeof(char*); ++i)
      if (*str == primitive_symbols[i]) return str;
   return NULL;
}

Expression *Expression::make_unbound_var(const char *vtype, const char *vname) {
   return new Expression(new string(vname), new string(vtype));
}

Expression *Expression::make_0ary_call(Expression *func) {
   // assignments and literals we can keep as-is
   if (func->is_literal() || func->t == ASSIGN 
      || func->t == UNBOUND || func->t == LAMBDA) {
      return func;
   } else {
      return new Expression(func, new ExpressionList()); // this explist will be empty
   }
}

Expression *Expression::make_binary_sym_call(const char *symbol, Expression *e1, Expression *e2) {
   ExpressionList *list = new ExpressionList();
   list->push_back(e1); list->push_back(e2);
   return new Expression(named_functions[symbol], list);
}

Expression *Expression::make_unary_sym_call(const char *symbol, Expression *e) {
   ExpressionList *list = new ExpressionList();
   list->push_back(e);
   return new Expression(named_functions[symbol], list);
}

bool Expression::is_symbol() {
   if (t != CALL) return false;
   if (func->t != VAR) return false;
   if (args->size() > 2) return false;
   for (int i = 0; i < func->str->size(); ++i) {
      char c = (*func->str)[i];
      if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_') 
         return false;
   }
   return true;
}

bool Expression::is_literal() {
   return t == INT || t == FLOAT || t == CHAR || t == STRING || t == BOOL;
}

bool Expression::is_binary() {
   return t == CALL && args->size() == 2;
}
bool Expression::is_unary() {
   return t == CALL && args->size() == 1;
}
bool Expression::is_0ary() {
   return t == CALL && args->size() == 0;
}

ostream& operator<<(ostream& os, Expression *expr) {
   switch(expr->t) {
      case Expression::IF: 
      {
         os << "if " << expr->cond << " then " << expr->if_true 
            << " else " << expr->if_false;
         break;
      }
      case Expression::ASSIGN:
      {
         os << *expr->alias << " = " << expr->rhs << ", " << expr->next;
         break;
      }
      case Expression::UNBOUND: 
      {
         os << *expr->var_type << " " << *expr->var_name;
         break;
      }
      case Expression::CALL:
      {
         if (expr->is_symbol()) { 
            if (expr->is_unary()) {
               os << *expr->func->str << (*expr->args)[0];
            } else {
               os << "(" << (*expr->args)[0] << " " << *expr->func->str << " " << (*expr->args)[1] << ")";
            }
         } else {
            if (expr->func->t == Expression::VAR || expr->func->t == Expression::UNBOUND)
               os << expr->func;
            else
               os << expr->func;
            if (!expr->is_0ary()) 
               os << "(" << expr->args << ")";
         }
         break;
      }
      case Expression::FLOAT:
      {
         os << expr->f;
         break;
      }
      case Expression::INT:
      {
         os << expr->i;
         break;
      }
      case Expression::CHAR:
      {
         os << expr->c;
         break;
      }
      case Expression::BOOL:
      {
         os << (expr->b ? "True" : "False");
         break;
      }
      case Expression::STRING:
      {
         os << '"' << *expr->str << '"';
         break;
      }
      case Expression::VAR:
      {
         os << *expr->str;
         break;
      }
      case Expression::LAMBDA:
      {
         os << "{" << expr->expr << "}";
         break;
      }
      default:
         os << "UNKNOWN EXPR " << expr->t;
         break;
   }
   return os;
}

ostream& operator<<(ostream& os, ExpressionList *&exprlist) {
   ExpressionList::iterator it;
   bool first = true;
   int i = 0;
   for (it = exprlist->begin(); it != exprlist->end(); it++) {
      if (first) first = false; else os << ", ";
      os << *it;
   }
   return os;
}

// Named constructors
Expression *Int(int i) { return new Expression(i); }
Expression *Float(float f) { return new Expression(f); } 
Expression *String(std::string *str) { return new Expression(str); }
Expression *Char(char c) { return new Expression(c); }
Expression *Bool(bool b) { return new Expression(b); }
Expression *Var(std::string *name) { 
   Expression *res = new Expression(name); res->t = Expression::VAR; return res; 
}
Expression *Unbound(std::string *name, std::string *type) { return new Expression(name, type); }
Expression *Unary(std::string *op, Expression *e) { return new Expression(op, e); }
Expression *Binary(std::string *op, Expression *e1, Expression *e2) { 
   return new Expression(op, e1, e2); 
}
Expression *Call(Expression *func, ExpressionList *args) { return new Expression(func, args); }
Expression *Lambda(Expression *e) { return new Expression(e); }
Expression *Assign(std::string *v, Expression *rhs, Expression *next) { 
   return new Expression(v, rhs, next); 
}
Expression *Conditional(Expression *cond, Expression *if_true, Expression *if_false) { 
   return new Expression(cond, if_true, if_false); 
}

}