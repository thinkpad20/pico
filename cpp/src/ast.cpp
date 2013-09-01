#include "../include/ast.h"
#include <map>

using namespace std;

namespace pico {

static map<const char *, Expression *> named_functions;
static Expression *blank_expr = NULL;
static Expression *root_expr = NULL;
ExpressionList *parsed_expressions = NULL;

static const char *primitive_symbols[] = {
   "+", "-", "*", "/", "%", "==", "<", ">", "<=", ">=", "&&", "||", "!", "^"
};

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

Expression *Expression::make_unbound_var(const char *vtype, const char *vname) {
   return new Expression(new string(vname), new string(vtype));
}

Expression *Expression::make_0ary_call(Expression *func) {
   // assignments and literals we can keep as-is
   if (func->is_literal() || func->t == ASSIGN || func->t == UNBOUND) {
      return func;
   } else {
      return new Expression(func, new ExpressionList()); // this explist will be empty
   }
}

Expression *Expression::BLANK_EXPR() {
   if (!blank_expr) {
      blank_expr = new Expression();
   }
   return blank_expr;
}

Expression *Expression::ROOT_EXPR() {
   if (!root_expr) {
      root_expr = new Expression();
   }
   return root_expr;
}

Expression *Expression::make_binary_sym_call(const char *symbol, Expression *e1, Expression *e2) {
   if (named_functions.find(symbol) == named_functions.end()) {
      cout << "Encountered new binary symbol " << symbol << ", adding it to the table" << endl;
      named_functions[symbol] = make_var(symbol);
   }
   ExpressionList *list = new ExpressionList();
   list->push_back(e1); list->push_back(e2);
   return new Expression(named_functions[symbol], list);
}

Expression *Expression::make_unary_sym_call(const char *symbol, Expression *e) {
   if (named_functions.find(symbol) == named_functions.end()) {
      cout << "Encountered new unary symbol " << symbol << ", adding it to the table" << endl;
      named_functions[symbol] = make_var(symbol);
   }
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
         os << "IF " << expr->cond << " THEN " << expr->if_true 
            << " ELSE " << expr->if_false;
         return os;
      }
      case Expression::ASSIGN:
      {
         os << *expr->alias << " = (" << expr->rhs << "), " << expr->next;
         return os;
      }
      case Expression::UNBOUND: 
      {
         os << *expr->var_type << " " << *expr->var_name;
         return os;
      }
      case Expression::CALL:
      {
         if (expr->is_symbol()) { 
            if (expr->is_unary()) {
               os << *expr->func->str << "(" << (*expr->args)[0] << ")";
            } else {
               os << "(" << (*expr->args)[0] << " " << *expr->func->str << " " << (*expr->args)[1] << ")";
            }
         } else {
            if (expr->func->t == Expression::VAR || expr->func->t == Expression::UNBOUND)
               os << expr->func;
            else
               os << "call (" << expr->func << ")";
            if (!expr->is_0ary()) 
               os << " on (" << expr->args << ")";
         }
         return os;
      }
      case Expression::FLOAT:
      {
         os << expr->f;
         return os;
      }
      case Expression::INT:
      {
         os << expr->i;
         return os;
      }
      case Expression::CHAR:
      {
         os << expr->c;
         return os;
      }
      case Expression::BOOL:
      {
         os << (expr->b ? "TRUE" : "FALSE");
         return os;
      }
      case Expression::STRING:
      {
         os << '"' << *expr->str << '"';
         return os;
      }
      case Expression::VAR:
      {
         os << *expr->str;
         return os;
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

}