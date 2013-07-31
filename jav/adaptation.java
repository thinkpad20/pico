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