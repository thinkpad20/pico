#include "../include/compile.h"
#include <string>
#include <deque>
#include <map>
#include <vector>

using namespace std;

namespace pico {

struct SymbolInfo {
   enum Status {OK, NOT_FOUND} s;
   string full_name;
   Expression *expr;
   Expression::Type type;
   unsigned arg_num;
   SymbolInfo(): s(NOT_FOUND) {}
   SymbolInfo(string full_name, Expression *expr): 
      s(OK), full_name(full_name), expr(expr) {}
   SymbolInfo(string full_name, Expression::Type t, unsigned arg_num): 
      s(OK), full_name(full_name), type(t), arg_num(arg_num) {}
};

struct Unbound { 
   string name; Expression::Type t; 
   Unbound(string name, Expression::Type t): name(name), t(t) {}
};

struct SymbolTable {
   map<string, SymbolInfo> table;
   unsigned num_args;
   SymbolTable *next;
   SymbolTable(SymbolTable *next): num_args(0), next(next) {}
};

struct NameStack {
   deque<string> stack;
   NameStack *push(string name) {
      stack.push_back(name);
      return this;
   }
   void pop() {
      stack.pop_back();
   }
   string render() {
      string res = "";
      bool first = true;
      for (int i = 0; i < stack.size(); ++i) {
         if (first) first = false; else res += ".";
         res += stack[i];
      }
      return res;
   }
};

struct FunctionInfo {
   string name; // if anonymous, has a compiler-provided name
   vector<Expression::Type> return_types, arg_types;
};

// struct FunctionTypeTable {
//    enum PicoType { INT, CHAR, STRING, BOOL, LIST, ALG, NONE }; // alg is user-defined
//    struct TypeTrie {
//       Expression::Type this_type;
//       bool accepting;
//       map<Expression::Type, TypeTrie *> links;
//       TypeTrie(Expression::Type type): this_type(type), accepting(false) {}
//    };
//    map<string, TypeTrie *> funcs;
//    TypeTrie *add_to_trie(TypeTrie *trie, vector<Expression::Type> types, int idx = 0) {
//       if (idx == types.size()) return trie;
//       if (!trie) trie = new TypeTrie(types[idx]);
//    }
//    void store_func(string name) {// for funcs with no args
//       if (funcs.find(name) != funcs.end())
//          throw string("Function already exists in table");
//       funcs[name] = NULL; // NULL means that it accepts no arguments
//    }
//    void store_func(string fname, vector<Expression::Type> types) {
//       types[fname] = add_to_trie(types);
//    }
// };

static NameStack name_stack;

SymbolTable *symtable = NULL;

static void push_symtable() {
   symtable = new SymbolTable(symtable);
}

static void pop_symtable() {
   if (!symtable) throw string("SymbolTable is null");
   SymbolTable *temp = symtable;
   symtable = symtable->next;
   delete temp;
}

static void initialize() {
   push_symtable();
}

// for bound variables
static void store(string name, string full_name, Expression *expr) {
   symtable->table[name] = SymbolInfo(full_name, expr);
}

// for unbound vars
static void store(string name, string full_name, Expression::Type t) {
   if (symtable->table.find(name) != symtable->table.end())
      throw string("Error: symbol '" + name + "' already in table!");
   symtable->table[name] = SymbolInfo(full_name, t, symtable->num_args++);
}

// gets the number of an unbound variable
static int arg_num(string vname) {
   if (symtable->table.find(vname) == symtable->table.end())
      throw string("Error: symbol '" + vname + "' already in table!");
   return symtable->table[vname].arg_num;
}

static SymbolInfo lookup_single(string var, SymbolTable *s) {
   if (s->table.find(var) != s->table.end())
      return s->table[var];
   return SymbolInfo();
}

static SymbolInfo lookup(string var) {
   SymbolTable *s = symtable;
   while (s) {
      SymbolInfo res = lookup_single(var, s);
      if (res.s == SymbolInfo::OK) return res;
      s = s->next;
   }
   return SymbolInfo();
}

static void _compile(InstructionList &insts, Expression *expr, unsigned ecnt) {
   cout << "Compiling: " << expr << endl;
   if (expr->is_binary()){
      _compile(insts, expr->expr1, ecnt);
      _compile(insts, expr->expr2, ecnt);
   }
   if (expr->is_unary())
      _compile(insts, expr->unary, ecnt);

   switch (expr->t) {
      // primitives
      case Expression::INT: 
      {
         insts.append(Instruction::_push(expr->ival));
         break;
      }
      case Expression::FLOAT:
         insts.append(Instruction::_push(expr->fval));
         break;
      case Expression::CHAR:
         insts.append(Instruction::_push(expr->cval));
         break;
      case Expression::STRING:
         insts.append(Instruction::_push(expr->strval));
         break;
      case Expression::BOOL:
         insts.append(Instruction::_push(expr->bval, Primitive::BOOL));
         break;
      case Expression::VAR:
      {
         if (expr->type) {
            int i = arg_num(*expr->name);
            insts.append(Instruction::_pusharg(i));
         } else {
            SymbolInfo res = lookup(*expr->name);
            if (res.s != SymbolInfo::OK) {
               char buf[250]; sprintf(buf, "Error: %s has no type and is not defined "
                                           "in this scope", expr->name->c_str());
               throw string(buf);
            }
            // then it's a function, we need to call it... this needs work!
            unsigned unr = res.expr->unresolved();
            insts.append(Instruction::_call(new string(res.full_name), unr));
         }
         break;
      }
      case Expression::INVOKE: // this should really be the same as a variable!
      {
         for (unsigned i = 0; i < expr->expr_list->size(); ++i) {
            _compile(insts, (*expr->expr_list)[i], ecnt);
         }
         //if this expression is a variable, we can just call it
         if (expr->func->t == Expression::VAR) {
            SymbolInfo info = lookup(*expr->func->name);
            insts.append(Instruction::_call(new string(info.full_name), expr->expr_list->size()));
         //otherwise, we have a lamba so we need to create an anonymous context
         } else {
            char lbl[100]; sprintf(lbl, "$%d", ecnt);
            string *lblname = new string(name_stack.render() + lbl);
            insts.append(Instruction::_call(lblname, expr->expr_list->size()));
            insts.append(Instruction::_label(lblname));
            _compile(insts, expr->func, ecnt + 1);
         }
         break;
      }

      // logical flow
      case Expression::IF:
      {
         _compile(insts, expr->cond, ecnt);
         char lbl[100]; sprintf(lbl, "$%d", ecnt);
         name_stack.push(lbl);
         insts.append(Instruction::_elsegoto(new string(name_stack.render())));
         _compile(insts, expr->if_true, ecnt);
         insts.append(Instruction::_label(new string(name_stack.render())));
         _compile(insts, expr->if_false, ecnt + 1);
         break;
      }
      case Expression::ASSIGN:
      {
         cout << "Assignment! current expression: " << expr << endl;
         cout << "current namespace is '" << name_stack.render() << "'" << endl;
         name_stack.push(*expr->vname);
         Expression *content = expr->right_hand;
         cout << "lhs: " << *expr->vname << " rhs: " << expr->right_hand << endl;
         store(*expr->vname, name_stack.render(), expr->right_hand);
         // compile all of the sub-assignments
         cout << "searching for the first non-assignment within " << *expr->vname << endl;
         if (content->t == Expression::ASSIGN) {
            _compile(insts, content, 0);
            content = content->next;
         }
         cout << "found first non-assignment within "<< *expr->vname << ", which is: " << content << endl;
         // make a label for the function name
         cout << "appending a label " << name_stack.render() << endl;
         insts.append(Instruction::_label(new string(name_stack.render())));
         name_stack.pop();
         // compile the instructions for the right-hand of this assignment
         _compile(insts, content, 0);
         insts.append(Instruction::_return(1));
         _compile(insts, expr->next, 0);
         break;
      }

      // arithmetic
      case Expression::ADD:
      {
         insts.append(Instruction::_add());
         break;
      }
      case Expression::SUB:
      {
         insts.append(Instruction::_sub());
         break;
      }
      case Expression::MULT:
      {
         insts.append(Instruction::_mult());
         break;
      }
      case Expression::DIV:
      {
         insts.append(Instruction::_div());
         break;
      }
      case Expression::MOD:
      {
         insts.append(Instruction::_mod());
         break;
      }
      case Expression::EXP:
      {
         insts.append(Instruction::_exp());
         break;
      }
      case Expression::EQ:
      {
         insts.append(Instruction::_eq());
         break;
      }
      case Expression::NEQ:
      {
         insts.append(Instruction::_neq());
         break;
      }
      case Expression::LT:
      {
         insts.append(Instruction::_lt());
         break;
      }
      case Expression::GT:
      {
         insts.append(Instruction::_gt());
         break;
      }
      case Expression::LEQ:
      {
         insts.append(Instruction::_leq());
         break;
      }
      case Expression::GEQ:
      {
         insts.append(Instruction::_geq());
         break;
      }
      case Expression::LOG_AND:
      {
         insts.append(Instruction::_and());
         break;
      }
      case Expression::LOG_OR:
      {
         insts.append(Instruction::_or());
         break;
      }
      case Expression::LOG_NOT:
      {
         insts.append(Instruction::_not());
         break;
      }
      case Expression::NEG:
      {
         insts.append(Instruction::_neg());
         break;
      }

      case Expression::UNRESOLVED:
         throw string("why are we seeing unresolved variables?");
      // default:
      case Expression::BIT_AND:
      case Expression::BIT_OR:
      case Expression::BIT_XOR:
      case Expression::BIT_NOT:
         throw string("we can't handle this stuff yet!");
   }
}


/*

could have a table of VM instructions to carry out all arithmetic operations; then we could treat
them more or less like functions. For example:

plus#(int,int):
   push arg 0
   push arg 1
   add
   return

plus#(int,float):
   call plus#(int, int) // can use this because internally VM can 
   return               // tell... although we might simplify that too

plus#(int,float):
   call plus#(int, int)
   return

plus#(float,float):
   call plus#(int, int)
   return

///
///alternatively
///

plus#(int,int):
   push arg 0
   push arg 1
   add_int
   return

plus#(float,int):
   push arg 1
   push arg 0
   add_int_float
   return

plus#(int,float):
   push arg 0
   push arg 1
   add_int_float
   return

plus#(float,float):
   push arg 0
   push arg 1
   add_float
   return

/// minus is similar

*/


// this would be better to do with functions! 
// As in, "do we have an instance of + for char and int?"
static Expression::Type get_ret_type(Expression *expr) {
   if (expr->is_binary()) {
      Expression::Type t1 = get_ret_type(expr->expr1),
                       t2 = get_ret_type(expr->expr2);
      if (t1 == t2) return t1;
      if (t1 == Expression::INT) {
         if (t2 == Expression::FLOAT)
            return Expression::FLOAT;
         throw string("INT cannot be combined with that type (binary statement)");
      }
      if (t1 == Expression::FLOAT) {
         if (t2 == Expression::INT)
            return Expression::FLOAT;
         throw string("FLOAT cannot be combined with that type (binary statement)");
      }
      throw string("Mismatched type error (binary statement)");
   }
   if (expr->is_unary()) {
      // we'll do more robust type checking later
      return get_ret_type(expr->unary);
   }

   switch (expr->t) {
      case Expression::IF:
      {
         Expression::Type t1 = get_ret_type(expr->if_true),
                          t2 = get_ret_type(expr->if_false);
         if (t1 == t2) return t1;
         throw string("Mismatched type error in if statement");  
      }
      case Expression::ASSIGN:
      { return get_ret_type(expr->next); }
      case Expression::VAR:
      { 
         SymbolInfo si = lookup(*expr->name);
         if (si.s == SymbolInfo::NOT_FOUND) 
            throw string("Symbol " + *expr->name + " was not found, can't check type");
         return get_ret_type(si.expr);
      }
      case Expression::INT:
      case Expression::FLOAT:
      case Expression::STRING:
      case Expression::CHAR:
      case Expression::BOOL:
      { return expr->t; }
      case Expression::INVOKE:
      { return get_ret_type(expr->func); }
      default:
      { 
         char buf[100]; sprintf(buf, "Can't check expressions of type %d", expr->t);
         throw string(buf); 
      }
   }
}

static void get_arg_types_r(vector<Expression::Type> &types, Expression *expr);
static void get_arg_types_r(vector<Expression::Type> &types, ExpressionList *expr_list) {
   ExpressionList::iterator it;
   for (it = expr_list->begin(); it != expr_list->end(); ++it) {
      get_arg_types_r(types, *it);
   }
}

static void get_arg_types_r(vector<Expression::Type> &types, Expression *expr) {
   if (expr->is_binary()) {
      get_arg_types_r(types, expr->expr1);
      get_arg_types_r(types, expr->expr2);
   }
   else if (expr->is_unary()) {
      get_arg_types_r(types, expr->unary);
   }
   else if (expr->t == Expression::IF) {
      get_arg_types_r(types, expr->cond);
      get_arg_types_r(types, expr->if_true);
      get_arg_types_r(types, expr->if_false);
   }
   else if (expr->t == Expression::ASSIGN) {
      get_arg_types_r(types, expr->right_hand);
      get_arg_types_r(types, expr->next);
   }
   else if (expr->t == Expression::INVOKE) {
      get_arg_types_r(types, expr->func);
      get_arg_types_r(types, expr->expr_list);
   }
   else if (expr->t == Expression::VAR) {
      if (expr->type != NULL) {
         cout << "found arg " << *expr->name << endl;
         if (*expr->type == "Int") types.push_back(Expression::INT);
         else if (*expr->type == "Float") types.push_back(Expression::FLOAT);
         else if (*expr->type == "Char") types.push_back(Expression::CHAR);
         else if (*expr->type == "String") types.push_back(Expression::STRING);
         else throw string("Can't handle type " + *expr->type + " yet");
      } else {
         SymbolInfo si = lookup(*expr->name);
         if (si.s == SymbolInfo::NOT_FOUND) 
            throw string("Variable '" + si.full_name + "'' was not found!");
         get_arg_types_r(types, si.expr);
      }
   }
   else 
      cout << "get_arg_types_r ignoring type " << expr->t << endl;
}

static vector<Expression::Type> get_arg_types(Expression *expr) {
   vector<Expression::Type> types;
   get_arg_types_r(types, expr);
   return types;
}

// FunctionInfo get_info(Expression *assign_expr) {
//    if (assign_expr->t == Expression::ASSIGN)
//       throw string("Error: get_info only works on assignment expressions");
//    FunctionInfo info;
//    info.name = assign_expr->vname;
// }

InstructionList compile(Expression *expr) {
   InstructionList ilist(10);
   Expression *root = new Expression(strdup("root"), expr, new Expression(0));
   initialize();
   _compile(ilist, root, 0);
   cout << "Generated compiled code:" << endl;
   cout << ilist << endl;
   return ilist;
}

void compile_all(ExpressionList *expr_list) {
   try {
      for (int i = 0; i < expr_list->size(); ++i) {
         // compile((*expr_list)[i]);
         Expression *expr = (*expr_list)[i];
         if (expr->t == Expression::ASSIGN) {
            cout << "this is an assign, let's see what its args are" << endl;
            vector<Expression::Type> types = get_arg_types(expr->right_hand);
            for (int i = 0; i < types.size(); ++i) {
               cout << "Type " << i << ": " << types[i] << endl;
            }
         }
         cout << "ret type of that expression is " << get_ret_type(expr) << endl;
      }
   } catch (string s) {
      cout << s << endl;
   }
}

struct Call {
   bool no_function;
   string fname;
   vector<Term> args;
};

struct If {
   Call *cond, *if_true, *if_false;
};

struct Return {
   enum Type { IF, CALL } t;
   union { If *if_; Call *call; };
};

struct Function {
   string name;
   vector<Function> children;
   Return *ret;
};


}