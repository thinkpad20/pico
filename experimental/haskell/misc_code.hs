
{-

Ex:

a(1)(2)(3,4,5)

invs = [[1], [2], [3,4,5]]

this should be 

call
  call
    call
      a
      [1]
    [2]
  [3,4,5]

* genCalls should return a Call object
* a Call object needs an expression and a list of expressions
* primary is a type Call, to which we can apply the Call function
  to make it a type Expression

genCalls :: Call -> [[Expression]] -> Call
genCalls c [] = c -- if there are no more expressions left to process, just return c
genCalls c [es] = FunctionCall (Call (genCalls c (init es))) (last es)


-}

{-
Parses the following grammar:

expression -> call
            | identifier '=' expression ',' expression
            | "if" expression "then" expression "else" expression

call -> logical 
      | call "(" expression ("," expression)* ")"

symbolic -> logical
          | logical symbol symbolic
          | symbol symbolic

logical -> unaryLog
         | unaryLog "&&" logical
         | unaryLog "||" logical

unaryLog -> comparison
          | '!' unaryLog

comparison -> additive
            | additive '>' comparison
            | additive '<' comparison
            | additive '<=' comparison
            | additive '>=' comparison
            | additive '==' comparison
            | additive '!=' comparison

additive -> multiplicative
        | multiplicative '+' additive
        | multiplicative '-' additive

multiplicative -> unary
         | unary '*' multiplicative
         | unary '/' multiplicative
         | unary '%' multiplicative

unary -> literal
       | '!' unary
       | '_' unary        

term -> intConstant
      | floatConstant
      | charConstant
      | stringConstant
      | identifier
      | identifier ':' identifier
      | '(' expression ')'
      | '{' expression '}'

-}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

--instance Show Literal where
--  show (Int i) = show i
--  show (Float f) = show f
--  show (String s) = show s
--  show (Char c) = show c
--  show (Bool b) = show b

--instance Show Expr where
--  show (Literal l) = show l
--  show (Var v) = v
--  show (Symbol s) = s
--  show (Unbound t v) = t ++ " " ++ v
--  show (If cond ifTrue ifFalse) = "if " ++ show cond ++ " then " ++ 
--                                  show ifTrue ++ ", else " ++ show ifFalse
--  show (Lambda e) = "{" ++ show e ++ "}"
--  show (Assign s e next) = s ++ " = " ++ show e ++ ", " ++ show next
--  show (Call e es) = 
--    case e of
--      Symbol s -> 
--        case es of 
--          [ex] -> s ++ show ex
--          (x:y:_) -> show x ++ " " ++ s ++ " " ++ show y
--      otherwise ->
--        show e ++ showExprList es True
--      where 
--        showExprList [] True = ""
--        showExprList [] False = ")"
--        showExprList (e:es) True = "(" ++ show e ++ showExprList es False
--        showExprList (e:es) False = ", " ++ show e ++ showExprList es False
