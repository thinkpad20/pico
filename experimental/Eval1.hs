import qualified Data.Map as M

data Type = NumType
          | StringType
          deriving (Show)

type Param = (String, Type)
data ParamDec = WithNames [Param]
              | NoNames [Type]
              deriving (Show)

data Expr = Number Double
          | String String
          | Var String
          | Param Param
          | Assign String Expr
          | AssignFunc String ParamDec Expr
          | Recurse
          | Lambda Expr
          | If Expr Expr Expr
          | Binary String Expr Expr
          | Call Expr [Maybe Expr]
          deriving (Show)


fact = AssignFunc "fact" (NoNames [NumType]) (
         Call
          (Lambda (
            If 
              (Binary "<" (Param ("n", NumType)) (Number 2))
              (Param ("acc", NumType))
              (Call Recurse [Just (Binary "-" (Var "n") (Number 1)), 
                             Just (Binary "*" (Var "acc") (Var "n"))])
          ))
          [Nothing, Just (Number 1)]
      )

type Context = ([M.Map String Expr], Int, String)

compile :: Expr -> [String]
compile e = fst $ compile' e ([M.empty], 0, "") where
  compile' :: Expr -> [String] -> Context -> ([String], Context)
  compile' (Number n) insts ctx = (("push " ++ show n):insts, ctx)
  compile' ()