package pico;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.lang.Math;

public abstract class PicoExpression {
   protected enum BinaryType { 
      ADD, SUB, DIV, MULT, CONCAT, EXP, EQ,
      AND, OR, LT, GT, LEQ, GEQ, NEQ, MOD,
   }
   protected enum UnaryType { 
      NOT, NEG, PARENS
   }
   protected Map<String, PicoExpression> symbols;
   protected int u;
   protected PicoExpression superExpr;
   protected PicoExpression() { symbols = new HashMap<String, PicoExpression>(); }
   public abstract PicoExpression reduce() throws Exception;
   protected PicoExpression lookup(String name) {
      if (symbols.containsKey(name)) return symbols.get(name);
      if (superExpr == null) return null;
      return superExpr.lookup(name);
   }
   protected void store(String name, PicoExpression expr) {
      symbols.put(name, expr);
   }
   static PicoExpression unimpMethod(String name) throws Exception {
      String msg = name + " not implemented for this type";
      throw new Exception(msg);  
   }

   public static PicoExpression make_add(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.ADD); 
   }
   public static PicoExpression make_sub(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.SUB); 
   }
   public static PicoExpression make_mult(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.MULT); 
   }
   public static PicoExpression make_div(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.DIV); 
   }
   public static PicoExpression make_mod(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.MOD); 
   }
   public static PicoExpression make_exp(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.EXP); 
   }
   public static PicoExpression make_concat(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.CONCAT); 
   }
   public static PicoExpression make_eq(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.EQ); 
   }
   public static PicoExpression make_neq(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.NEQ); 
   }
   public static PicoExpression make_and(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.AND); 
   }
   public static PicoExpression make_or(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.OR); 
   }
   public static PicoExpression make_lt(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.LT); 
   }
   public static PicoExpression make_gt(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.GT); 
   }
   public static PicoExpression make_leq(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.LEQ); 
   }
   public static PicoExpression make_geq(PicoExpression one, PicoExpression other) { 
      return new PicoBinary(one, other, BinaryType.GEQ); 
   }
   public static PicoExpression make_neg(PicoExpression one) throws Exception { 
      return new PicoUnary(one, UnaryType.NEG); 
   }
   public static PicoExpression make_not(PicoExpression one) throws Exception { 
      return new PicoUnary(one, UnaryType.NOT); 
   }
   
   public boolean isResolved() throws Exception { throw new Exception("isResolved not implemented"); }
   public boolean toBool() throws Exception { throw new Exception("toBool not implemented"); }
   public Integer getInt() throws Exception { throw new Exception("getInt not implemented"); }
   public Double getFloat() throws Exception { throw new Exception("getFloat not implemented"); }
   public Character getChar() throws Exception { throw new Exception("getChar not implemented"); }
   public String getString() throws Exception { throw new Exception("getString not implemented"); }
   public Boolean getBool() throws Exception { throw new Exception("getBool not implemented"); }







   static PicoExpression add(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_add(expr1, expr2); 
      }
      if (expr1 instanceof PicoInt) {
         if (expr2 instanceof PicoInt) { return new PicoInt (expr1.getInt() + expr2.getInt()); }
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(expr1.getInt() + expr2.getFloat()); }
      }
      if (expr1 instanceof PicoFloat) {
         if (expr2 instanceof PicoInt) { return new PicoFloat(expr1.getFloat() + expr2.getInt()); }
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(expr1.getFloat() + expr2.getFloat()); }
      }
      throw new Exception("We can only add ints and floats so far");
   }

   static PicoExpression sub(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_sub(expr1, expr2); 
      }
      if (expr1 instanceof PicoInt) {
         if (expr2 instanceof PicoInt) { return new PicoInt (expr1.getInt() - expr2.getInt()); }
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(expr1.getInt() - expr2.getFloat()); }
      }
      if (expr1 instanceof PicoFloat) {
         if (expr2 instanceof PicoInt) { return new PicoFloat(expr1.getFloat() - expr2.getInt()); }
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(expr1.getFloat() - expr2.getFloat()); }
      }
      throw new Exception("We can only sub ints and floats so far");
   }

   static PicoExpression mult(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); 
      expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_mult(expr1, expr2); 
      }
      if (expr1 instanceof PicoInt) {
         if (expr2 instanceof PicoInt) { PicoExpression res = new PicoInt (expr1.getInt() * expr2.getInt()); 
               return res;}
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(expr1.getInt() * expr2.getFloat()); }
      }
      if (expr1 instanceof PicoFloat) {
         if (expr2 instanceof PicoInt) { return new PicoFloat(expr1.getFloat() * expr2.getInt()); }
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(expr1.getFloat() * expr2.getFloat()); }
      }
      throw new Exception("We can only mult ints and floats so far");
   }

   static PicoExpression div(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) {
         return make_div(expr1, expr2); 
      }
      if (expr1 instanceof PicoInt) {
         if (expr2 instanceof PicoInt) { return new PicoInt (expr1.getInt() / expr2.getInt()); }
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(expr1.getInt() / expr2.getFloat()); }
      }
      if (expr1 instanceof PicoFloat) {
         if (expr2 instanceof PicoInt) { return new PicoFloat(expr1.getFloat() / expr2.getInt()); }
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(expr1.getFloat() / expr2.getFloat()); }
      }
      throw new Exception("We can only div ints and floats so far");
   }

   static PicoExpression mod(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_mod(expr1, expr2); 
      }
      if (expr1 instanceof PicoInt) {
         if (expr2 instanceof PicoInt) { return new PicoInt (expr1.getInt() % expr2.getInt()); }
      }
      throw new Exception("We can only mod ints so far");
   }

   static PicoExpression exp(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_exp(expr1, expr2); 
      }
      if (expr1 instanceof PicoInt) {
         if (expr2 instanceof PicoInt) { return new PicoInt ((int)pow(expr1.getInt(), expr2.getInt())); }
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(pow(expr1.getInt(), expr2.getFloat())); }
      }
      if (expr1 instanceof PicoFloat) {
         if (expr2 instanceof PicoInt) { return new PicoFloat(pow(expr1.getFloat(), expr2.getInt())); }
         else if (expr2 instanceof PicoFloat) { return new PicoFloat(pow(expr1.getFloat(), expr2.getFloat())); }
      }
      throw new Exception("We can only exp ints and floats so far");
   }

   static PicoExpression neg(PicoExpression t) {
      t = reduce(t);
      if (unresolved(t)) { 
         return make_neg(t); 
      }
      if (t instanceof PicoInt)
         return new PicoInt (-t.getInt());
      if (t instanceof PicoFloat)
         return new PicoFloat(-t.getFloat());
      throw new Exception("We can only negate ints and floats so far");
   }

   static PicoExpression land(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_log_and(expr1, expr2);
      }
      if (expr1 instanceof PicoBool && expr2 instanceof PicoBool) {
         return new PicoBool(expr1.getBool() && expr2.getBool());
      }
      throw new Exception("Can't logical AND anything except bools");
   }

   static PicoExpression lor(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_log_or(expr1, expr2); 
      }
      if (expr1 instanceof PicoBool && expr2 instanceof PicoBool) {
         return new PicoBool(expr1.getBool() || expr2.getBool());
      }
      throw new Exception("Can't logical OR anything except bools");
   }

   static PicoExpression lnot(PicoExpression t) {
      t = reduce(t);
      if (unresolved(t)) { 
         return make_log_not(t); 
      }
      if (t instanceof PicoBool)
         return new PicoBool(!t.getBool());
      throw new Exception("We can only NOT bools so far");
   }

   static PicoExpression eq(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_eq(expr1, expr2);
      }
      if (expr1->t == expr2->t) {
         switch (expr1->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt)   { return new PicoBool(expr1.getInt() == expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getFloat() == expr2.getFloat()); }
            else if (expr2 instanceof PicoChar)  { return new PicoBool(expr1.getChar() == expr2.getChar()); }
            else if (expr2 instanceof PicoString) { return new PicoBool(expr1.getString().equals(expr2.getString())); }
            case BOOL:  { return new PicoBool(expr1.getBool() == expr2.getBool()); }
            case VAR: { 
               throw new Exception("eq: Can't handle variables yet!");
            }
            default: { return new PicoBool(false); } // might want to check function equality, later...
         }
      }
      throw new Exception("Can't test equality on mismatched types");
   }

   static PicoExpression neq(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_neq(expr1, expr2);
      }
      if (expr1->t == expr2->t) {
         switch (expr1->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getInt() != expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getFloat() != expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getChar() != expr2.getChar()); }
            else if (expr2 instanceof PicoString) { return new PicoBool(!expr1.getString().equals(expr2.getString())); }
            case BOOL: { return new PicoBool(expr1.getBool() != expr2.getBool()); }
            case VAR: { 
               throw new Exception("neq: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: { return new PicoBool(true); } // might want to check function equality, later...
         }
      }
      throw new Exception("Can't test inequality on mismatched types");
   }

   static PicoExpression lt(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      // check if reduceuation should proceed
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_lt(expr1, expr2);
      }
      // we have two resolved expressions
      if (expr1 instanceof PicoInt) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getInt() < expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getInt() < expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getInt() < expr2.getChar()); }
            case VAR: { 
               throw new Exception("lt: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      if (expr1 instanceof PicoFloat) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getFloat() < expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getFloat() < expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getFloat() < expr2.getChar()); }
            case VAR: { 
               throw new Exception("lt: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      if (expr1 instanceof PicoChar) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getChar() < expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getChar() < expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getChar() < expr2.getChar()); }
            case VAR: { 
               throw new Exception("lt: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      if (expr1 instanceof PicoString) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoString) { return new PicoBool(expr1.getString().compareTo(expr2.getString()) < 0); }
            case VAR: { 
               throw new Exception("lt: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      throw new Exception("lt: Can't compare mismatched types");
   }

   static PicoExpression gt(PicoExpression expr1, PicoExpression expr2) {
      expr1 = expr1.reduce(); expr2 = expr2.reduce();
      // check if reduceuation should proceed
      if (expr1.unresolved() > 0 || expr2.unresolved() > 0) { 
         return make_gt(expr1, expr2);
      }
      // we have two resolved expressions
      if (expr1 instanceof PicoInt) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getInt() > expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getInt() > expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getInt() > expr2.getChar()); }
            case VAR: { 
               throw new Exception("gt: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      if (expr1 instanceof PicoFloat) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getFloat() > expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getFloat() > expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getFloat() > expr2.getChar()); }
            case VAR: { 
               throw new Exception("gt: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      if (expr1 instanceof PicoChar) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getChar() > expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getChar() > expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getChar() > expr2.getChar()); }
            case VAR: { 
               throw new Exception("gt: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      if (expr1 instanceof PicoString) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoString) { return new PicoBool(expr1.getString().compareTo(expr2.getString()) > 0); }
            case VAR: { 
               throw new Exception("gt: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      throw new Exception("lt: Can't compare mismatched types");
   }

   static PicoExpression geq(PicoExpression expr1, PicoExpression expr2) {
      // we have two resolved expressions
      if (expr1 instanceof PicoInt) {
         if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getInt() >= expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getInt() >= expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getInt() >= expr2.getChar()); }
            case VAR: { 
               throw new Exception("geq: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      if (expr1 instanceof PicoFloat) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getFloat() >= expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getFloat() >= expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getFloat() >= expr2.getChar()); }
            case VAR: { 
               throw new Exception("geq: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      if (expr1 instanceof PicoChar) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoInt) { return new PicoBool(expr1.getChar() >= expr2.getInt()); }
            else if (expr2 instanceof PicoFloat) { return new PicoBool(expr1.getChar() >= expr2.getFloat()); }
            else if (expr2 instanceof PicoChar) { return new PicoBool(expr1.getChar() >= expr2.getChar()); }
            case VAR: { 
               throw new Exception("geq: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      if (expr1 instanceof PicoString) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoString) { return new PicoBool(expr1.getString().compareTo(expr2.getString()) >= 0); }
            case VAR: { 
               throw new Exception("geq: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      throw new Exception("geq: Can't compare mismatched types");
   }

   static PicoExpression leq(PicoExpression expr1, PicoExpression expr2) {
      // we have two resolved expressions
      if (expr1 instanceof PicoInt) {
         if (expr2 instanceof PicoInt)
            return new PicoBool(expr1.getInt() <= expr2.getInt());
         else if (expr2 instanceof PicoFloat)
            return new PicoBool(expr1.getInt() <= expr2.getFloat());
         else if (expr2 instanceof PicoChar)
            return new PicoBool(expr1.getInt() <= expr2.getChar());
         else
            throw new Exception("leq: Can't handle variables yet!");
      }
      if (expr1 instanceof PicoFloat) {
         switch (expr2->t) {
         if (expr2 instanceof PicoInt)
            return new PicoBool(expr1.getFloat() <= expr2.getInt());
         else if (expr2 instanceof PicoFloat)
            return new PicoBool(expr1.getFloat() <= expr2.getFloat());
         else if (expr2 instanceof PicoChar)
            return new PicoBool(expr1.getFloat() <= expr2.getChar());
         else
            throw new Exception("leq: Can't handle variables yet!");
         }
      }
      if (expr1 instanceof PicoChar) {
         switch (expr2->t) {
            if (expr2 instanceof PicoInt)
               return new PicoBool(expr1.getChar() <= expr2.getInt());
            else if (expr2 instanceof PicoFloat)
               return new PicoBool(expr1.getChar() <= expr2.getFloat());
            else if (expr2 instanceof PicoChar)
               return new PicoBool(expr1.getChar() <= expr2.getChar());
            else
               throw new Exception("leq: Can't handle variables yet!");
         }
      }
      if (expr1 instanceof PicoString) {
         switch (expr2->t) {
            // check if literals are same
            else if (expr2 instanceof PicoString) { return new PicoBool(expr1.getString().compareTo(expr2.getString()) <= 0); }
            case VAR: { 
               throw new Exception("leq: Can't handle variables yet!");
               // return new PicoBool(*expr1->var->name == *expr2->var->name); 
            }
            default: break;
         }
      }
      throw new Exception("leq: Can't compare mismatched types");
   }









}

class PicoIf extends PicoExpression {
   PicoExpression cond, if_true, if_false;
   public PicoIf(PicoExpression cond, PicoExpression if_true, PicoExpression if_false) {
      super();
      this.cond = cond; this.if_true = if_true; this.if_false = if_false;
   }
   public PicoExpression reduce() throws Exception {
      cond = cond.reduce();
      if (!cond.isResolved()) return this;
      if (cond.toBool()) return if_true.reduce();
      else return if_false.reduce();
   }

   @Override
   public String toString() {
      return "IF " + cond + " THEN " + if_true + ", ELSE " + if_false;
   }
}

class PicoAssign extends PicoExpression {
   String name; PicoExpression thunk, next;
   public PicoAssign(String name, PicoExpression thunk, PicoExpression next) {
      super();
      this.name = name; this.thunk = thunk; this.next = next;
   }
   public PicoExpression reduce() throws Exception { 
      if (lookup(name) == null) store(name, thunk);
      return next.reduce();
   }
   @Override
   public String toString() {
      return "ASSIGN " + name + " = " + thunk + ", " + next;
   }
}

class PicoUnresolved extends PicoExpression {
   public PicoUnresolved() {}
   public PicoExpression reduce() { return this; }
   @Override
   public String toString() {
      return "UNRESOLVED";
   }
}

class PicoInt extends PicoExpression {
   Integer i;
   public PicoInt(Integer i) { this.i = i; }
   public PicoInt(int i) { this.i = new Integer(i); }
   public PicoExpression reduce() { return this; }
   public PicoExpression add(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoInt(i + other.getInt());
      else if (other instanceof PicoChar)
         return new PicoInt(i + other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(i + other.getFloat());
      else
         throw new Exception("Can't add with an int");
   }
   public PicoExpression sub(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoInt(i - other.getInt());
      else if (other instanceof PicoChar)
         return new PicoInt(i - other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(i - other.getFloat());
      else
         throw new Exception("Can't add with an int");
   }
   public PicoExpression mult(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoInt(i * other.getInt());
      else if (other instanceof PicoChar)
         return new PicoInt(i * other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(i * other.getFloat());
      else
         throw new Exception("Can't add with an int");
   }
   public PicoExpression div(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoInt(i / other.getInt());
      else if (other instanceof PicoChar)
         return new PicoInt(i / other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(i / other.getFloat());
      else
         throw new Exception("Can't add with an int");
   }
   public PicoExpression mod(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoInt(i % other.getInt());
      else if (other instanceof PicoChar)
         return new PicoInt(i % other.getChar());
      else
         throw new Exception("Can't mod with an int");
   }
   public PicoExpression exp(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoInt((int)Math.pow(i, other.getInt()));
      if (other instanceof PicoChar)
         return new PicoInt((int)Math.pow(i, other.getChar()));
      else if (other instanceof PicoFloat)
         return new PicoFloat((int)Math.pow(i, other.getFloat()));
      else
         throw new Exception("Can't exp with an int");
   }
   public PicoExpression neg() {
      return new PicoInt(-i);
   }
   public Integer getInt() { return i; }
   
   @Override
   public String toString() {
      return i.toString();
   }

}

class PicoFloat extends PicoExpression {
   Double f;
   public PicoFloat(Double f) { this.f = f; }
   public PicoFloat(double f) { this.f = new Double(f); }
   public PicoExpression reduce() { return this; }
   public Double getFloat() { return f; }
   public PicoExpression add(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoFloat(f + other.getInt());
      else if (other instanceof PicoChar)
         return new PicoFloat(f + other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(f + other.getFloat());
      else
         throw new Exception("Can't add with a float");
   }
   public PicoExpression sub(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoFloat(f - other.getInt());
      else if (other instanceof PicoChar)
         return new PicoFloat(f - other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(f - other.getFloat());
      else
         throw new Exception("Can't sub with a float");
   }
   public PicoExpression mult(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoFloat(f * other.getInt());
      else if (other instanceof PicoChar)
         return new PicoFloat(f * other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(f * other.getFloat());
      else
         throw new Exception("Can't mult with a float");
   }
   public PicoExpression div(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoFloat(f / other.getInt());
      else if (other instanceof PicoChar)
         return new PicoFloat(f / other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(f / other.getFloat());
      else
         throw new Exception("Can't div with a float");
   }
   public PicoExpression exp(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoFloat(Math.pow(f, other.getInt()));
      else if (other instanceof PicoChar)
         return new PicoFloat(Math.pow(f, other.getChar()));
      else if (other instanceof PicoFloat)
         return new PicoFloat(Math.pow(f, other.getFloat()));
      else
         throw new Exception("Can't exp with a float");
   }
   public PicoExpression neg() {
      return new PicoFloat(-f);
   }
   @Override
   public String toString() {
      return f.toString();
   }

}

class PicoBool extends PicoExpression {
   Boolean b;
   public PicoBool(boolean b) { this.b = new Boolean(b); }
   public PicoBool(Boolean b) { this.b = b; }
   public PicoExpression reduce() { return this; }
   public Boolean getBool() { return b; }
   public PicoExpression and(PicoExpression other) throws Exception {
      if (other instanceof PicoBool)
         return new PicoBool(b && other.getBool());
      else
         throw new Exception("Can't and with a bool");
   }
   public PicoExpression or(PicoExpression other) throws Exception {
      if (other instanceof PicoBool)
         return new PicoBool(b || other.getBool());
      else
         throw new Exception("Can't and with a bool");
   }
   public PicoExpression not() {
      return new PicoBool(!b);
   }
   @Override
   public String toString() {
      return b.toString();
   }
}

class PicoChar extends PicoExpression {
   Character c;
   public PicoChar(char c) { this.c = new Character(c); }
   public PicoChar(int i) { this.c = new Character((char)i); }
   public PicoChar(Character c) { this.c = c; }
   public PicoExpression reduce() { return this; }
   public Character getChar() { return c; }
      public PicoExpression add(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoChar(c + other.getInt());
      else if (other instanceof PicoChar)
         return new PicoChar(c + other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(c + other.getFloat());
      else
         throw new Exception("Can't add with a char");
   }
   public PicoExpression sub(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoChar(c - other.getInt());
      else if (other instanceof PicoChar)
         return new PicoChar(c - other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(c - other.getFloat());
      else
         throw new Exception("Can't add with a char");
   }
   public PicoExpression mult(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoChar(c * other.getInt());
      else if (other instanceof PicoChar)
         return new PicoChar(c * other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(c * other.getFloat());
      else
         throw new Exception("Can't add with a char");
   }
   public PicoExpression div(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoChar(c / other.getInt());
      else if (other instanceof PicoChar)
         return new PicoChar(c / other.getChar());
      else if (other instanceof PicoFloat)
         return new PicoFloat(c / other.getFloat());
      else
         throw new Exception("Can't add with a char");
   }
   public PicoExpression mod(PicoExpression other) throws Exception {
      if (other instanceof PicoInt)
         return new PicoChar(c % other.getInt());
      else if (other instanceof PicoChar)
         return new PicoChar(c % other.getChar());
      else
         throw new Exception("Can't mod with a char");
   }
   public PicoExpression exp(PicoExpression other) throws Exception {
      throw new Exception("Can't exp with a char");
   }
   public PicoExpression concat(PicoExpression other) throws Exception {
      if (other instanceof PicoString)
         return new PicoString(c + other.getString());
      else if (other instanceof PicoChar) {
         String s = "";
         s += c;
         s += other.getChar();
         return new PicoString(s);
      }
      else
         throw new Exception("Can't concat with a char");
   }
   @Override
   public String toString() {
      return c.toString();
   }
}

class PicoString extends PicoExpression {
   String s;
   public PicoString(String s) { this.s = s; }
   public PicoExpression reduce() { return this; }
   public String getString() { return s; }
   public PicoExpression concat(PicoExpression other) throws Exception {
      if (other instanceof PicoString)
         return new PicoString(s + other.getString());
      else if (other instanceof PicoChar) {
         s += other.getChar();
         return this;
      }
      else
         throw new Exception("Can't concat with a string");
   }
   @Override
   public String toString() {
      return "\"" + s + "\"";
   }
}

class PicoBinary extends PicoExpression {
   PicoExpression expr1, expr2;
   BinaryType type;
   public PicoBinary(PicoExpression expr1, PicoExpression expr2, BinaryType type) {
      this.expr1 = expr1; this.expr2 = expr2; this.type = type;
   }
   public PicoExpression reduce() throws Exception {
      expr1 = expr1.reduce();
      expr2 = expr2.reduce();
      switch (type) {
         case ADD: return expr1.add(expr2);
         case SUB: return expr1.sub(expr2);
         case DIV: return expr1.div(expr2);
         case MULT: return expr1.mult(expr2);
         case CONCAT: return expr1.concat(expr2);
         case AND: return expr1.and(expr2);
         case OR: return expr1.or(expr2);
         case LT: return expr1.lt(expr2);
         case GT: return expr1.gt(expr2);
         case LEQ: return expr1.leq(expr2);
         case GEQ: return expr1.geq(expr2);
         case NEQ: return expr1.neq(expr2);
         default:
            throw new Exception("Unrecognized BinaryType");
      }
   }

   public boolean isResolved() throws Exception { throw new Exception("isResolved not implemented"); }
   public boolean toBool() throws Exception { throw new Exception("toBool not implemented"); }
   public PicoExpression add(PicoExpression other) throws Exception { return expr1.reduce(expr2); }
   public PicoExpression sub(PicoExpression other) throws Exception { return expr1.add(expr2); }
   public PicoExpression mult(PicoExpression other) throws Exception { return unimpMethod("mult"); }
   public PicoExpression div(PicoExpression other) throws Exception { return unimpMethod("div"); }
   public PicoExpression mod(PicoExpression other) throws Exception { return unimpMethod("mod"); }
   public PicoExpression exp(PicoExpression other) throws Exception { return unimpMethod("exp"); }
   public PicoExpression concat(PicoExpression other) throws Exception { return unimpMethod("concat"); }
   public PicoExpression eq(PicoExpression other) throws Exception { return unimpMethod("eq"); }
   public PicoExpression neq(PicoExpression other) throws Exception { return unimpMethod("neq"); }
   public PicoExpression and(PicoExpression other) throws Exception { return unimpMethod("and"); }
   public PicoExpression or(PicoExpression other) throws Exception { return unimpMethod("or"); }
   public PicoExpression lt(PicoExpression other) throws Exception { return unimpMethod("lt"); }
   public PicoExpression gt(PicoExpression other) throws Exception { return unimpMethod("gt"); }
   public PicoExpression leq(PicoExpression other) throws Exception { return unimpMethod("leq"); }
   public PicoExpression geq(PicoExpression other) throws Exception { return unimpMethod("gt"); }
   public PicoExpression neg() throws Exception { return unimpMethod("neg"); }
   public PicoExpression not() throws Exception { return unimpMethod("not"); }

   @Override
   public String toString() {
      switch (type) {
         case ADD: return expr1.toString() + " + " + expr2.toString();
         case SUB: return expr1.toString() + " - " + expr2.toString();
         case DIV: return expr1.toString() + " / " + expr2.toString();
         case MULT: return expr1.toString() + " * " + expr2.toString();
         case CONCAT: return expr1.toString() + " ++ " + expr2.toString();
         case AND: return expr1.toString() + " && " + expr2.toString();
         case OR: return expr1.toString() + " || " + expr2.toString();
         case LT: return expr1.toString() + " < " + expr2.toString();
         case GT: return expr1.toString() + " > " + expr2.toString();
         case LEQ: return expr1.toString() + " <= " + expr2.toString();
         case GEQ: return expr1.toString() + " >= " + expr2.toString();
         case NEQ: return expr1.toString() + " != " + expr2.toString();
         default: return "unknown binary type";
      }
   }
}

class PicoUnary extends PicoExpression {
   PicoExpression expr;
   UnaryType type;
   public PicoUnary(PicoExpression expr, UnaryType type) {
      this.expr = expr; this.type = type;
   }
   public PicoExpression reduce() throws Exception { 
      switch (type) {
         case NOT: return expr.not();
         case NEG: return expr.neg();
         default:
            throw new Exception("Unrecognized UnaryType");
      }
   }
   @Override
   public String toString() { 
      switch (type) {
         case NOT: return "!" + expr.toString();
         case NEG: return "-" + expr.toString();
         default: return "Unrecognized UnaryType";
      }
   }
}

class PicoInvoke extends PicoExpression {
   PicoExpression expr;
   Collection<PicoExpression> args;
   public PicoInvoke(PicoExpression expr, Collection<PicoExpression> args) {
      this.expr = expr; this.args = args;
   }
   public PicoExpression reduce() throws Exception { throw new Exception("can't reduce invocations yet"); }
   @Override
   public String toString() { 
      return "can't print invokes yet";
   }
}

class PicoVar extends PicoExpression {
   String name, type;
   public PicoVar(String name) { this.name = name; }
   public PicoVar(String name, String type) { this.name = name; this.type = type; }
   public PicoExpression reduce() throws Exception { throw new Exception("can't reduce vars"); }
   @Override
   public String toString() { 
      return "can't print vars yet";
   }
}