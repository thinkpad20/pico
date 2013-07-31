package pico;

public class Main {
   static PicoExpression expr;
   public static void main(String[] args) throws Exception {
      expr = PicoExpression.make_add(new PicoFloat(5.5), new PicoInt(6));
      // expr = PicoExpression.make_mult(expr, new PicoFloat(7.89));
      // expr = PicoExpression.make_lt(expr, new PicoInt(20));
      expr = expr.reduce();
      System.out.print(expr);
   }
}