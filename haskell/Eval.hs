module PicoEval where

import PicoAST
import qualified Data.Map as Map
import Data.Ratio

-- FunctionRecord stores the name, parameter types and return type
-- of all of the functions we're aware of (MAYBE NOT NECESSARY)
--data FunctionRecord = FunV String [PType] deriving (Ord, Eq, Show)
type Env = [Map.Map String Value] -- variable names -> definitions ! Later this will be a stack
type Args = [Value]
-- a Value is the result of an evaluation, either a literal value or a function (for now)
data Value = 
  NumV Double
  | CharV Char
  | StringV String
  | BoolV Bool
  | FunV Env Expression
  | ArgV String PType
  deriving (Show, Eq, Ord)

instance Num Value where
  NumV a + NumV b = NumV $ a + b
  l + r = error $ "Can't add " ++ show l ++ ", " ++ show r
  NumV a - NumV b = NumV $ a - b
  l - r = error $ "Can't subtract " ++ show l ++ ", " ++ show r
  NumV a * NumV b = NumV $ a * b
  l * r = error $ "Can't multiply " ++ show l ++ ", " ++ show r
  negate (NumV a) = NumV $ negate a
  negate x = error $ "Can't negate " ++ show x
  abs (NumV a) = NumV $ abs a
  abs x = error $ "Can't abs " ++ show x
  signum (NumV a) = NumV $ signum a
  signum x = error $ "Can't signum " ++ show x
  fromInteger = NumV . fromInteger

instance Fractional Value where
  NumV a / NumV b = NumV $ a/b
  l / r = error $ "Can't divide " ++ show l ++ ", " ++ show r
  --fromRational :: Rational
  fromRational r = NumV $ (fromInteger . numerator $ r) / (fromInteger . denominator $ r)

(&&&) :: Value -> Value -> Value
BoolV a &&& BoolV b = BoolV $ a && b
l &&& r = error $ "Can't AND " ++ show l ++ ", " ++ show r

(|||) :: Value -> Value -> Value
BoolV a ||| BoolV b = BoolV $ a || b
l ||| r = error $ "Can't AND " ++ show l ++ ", " ++ show r

(***) :: Value -> Value -> Value
NumV a *** NumV b = NumV $ a ** b
l *** r = error $ "Can't exponentiate " ++ show (l, r)

toExpr (NumV n) = PNum n
toExpr (CharV c) = PChar c
toExpr (StringV s) = PString s
toExpr (BoolV b) = PBool b
toExpr (ArgV v t) = Unbound v t
toExpr v = error $ "Can't convert " ++ show v ++ " into an Expression"

fLookup :: Env -> String -> Maybe Value
fLookup [] _ = Nothing
fLookup (env:envs) name = 
  case Map.lookup name env of
    Just v -> Just v
    Nothing -> fLookup envs name

updateEnv :: Env -> Env -> Env
updateEnv (e1:es) (e2:_) = (Map.union e2 e1) : es

tos :: Env -> Map.Map String Value
tos (env:_) = env

addVar :: String -> Value -> Env -> Env
addVar s v (env:envs) = (Map.insert s v env):envs

--evalStart :: Expression -> Value
evalStart e = eval [Map.empty] [] e
  --where (val, _, _, _) = 

pushEnv :: Env -> Env
pushEnv env = (Map.empty):env

popEnv :: Env -> Env
popEnv (env:envs) = envs
popEnv [] = error "Can't pop from an empty stack"

eval :: Env -> Args -> Expression -> (Value, Env, Args, PType)
eval env args (PNum n)        = (NumV n, env, args, NumT)
eval env args (PChar c)       = (CharV c, env, args, CharT)
eval env args (PString s)     = (StringV s, env, args, StringT)
eval env args (PBool b)       = (BoolV b, env, args, BoolT)
eval env []   (Unbound nm t)  = (ArgV nm t, addVar nm (ArgV nm t) env, [], t)
eval env (a:as) (Unbound nm t) = (a, addVar nm a env, as, t) -- later we'll typecheck

eval env args (Var name) =
  case fLookup env name of
    Just (FunV env' expr) -> eval (updateEnv env' env) args expr
    Nothing -> error $ "Variable " ++ name ++ " not defined in scope"

eval env args (Binary sym l r) =
  case sym of
    "+" -> evalNumOp (+)
    "-" -> evalNumOp (-)
    "*" -> evalNumOp (*)
    "/" -> evalNumOp (/)
    "^" -> evalNumOp (***)
    ">" -> evalCompOp (>)
    "<" -> evalCompOp (<)
    "<=" -> evalCompOp (<=)
    ">=" -> evalCompOp (>=)
    "==" -> evalCompOp (==)
    "!=" -> evalCompOp (/=)
    "&&" -> evalLogOp (&&&)
    "||" -> evalLogOp (|||)
    otherwise -> error $ "we can't handle " ++ sym ++ " yet"
    where 
      (valL, envL, argsL, tL) = eval env args l -- get result of left side and new env/args
      (valR, envR, argsR, tR) = eval envL argsL r -- pipe left's env/args in for right side
      evalNumOp op = 
        case (tL, tR) of
          (NumT, NumT) -> 
            case (valL, valR) of
              (NumV _, NumV _) -> (op valL valR, envR, argsR, NumT)
              (NumV _, ArgV _ _) -> newBin envR (toExpr valL) (toExpr valR)
              (ArgV _ _, NumV _) -> newBin envR (toExpr valL) (toExpr valR)
              (ArgV _ _, ArgV _ _) -> newBin envR (toExpr valL) (toExpr valR)
              (NumV _, FunV env' expr') -> newBin (updateEnv envR env') (toExpr valL) expr'
              (FunV env' expr', NumV _) -> newBin (updateEnv envR env') expr' (toExpr valR)
              (ArgV _ _, FunV env' expr') -> newBin (updateEnv envR env') (toExpr valL) expr'
              (FunV env' expr', ArgV _ _) -> newBin (updateEnv envR env') expr' (toExpr valR)
              (FunV env1 expr1, FunV env2 expr2) ->
                newBin (updateEnv (updateEnv envR env1) env2) expr1 expr2
              where newBin env' e1 e2 = (FunV env' $ Binary sym e1 e2, env', argsR, NumT)
          otherwise -> typErr tL tR
      evalCompOp op =
        case (tL, tR) of
          (NumT, NumT) -> (BoolV $ op valL valR, envR, argsR, BoolT)
          (CharT, CharT) -> (BoolV $ op valL valR, envR, argsR, BoolT)
          otherwise -> typErr tL tR
      evalLogOp op = -- doesn't short-circuit, yet
        case (tL, tR) of
          (BoolT, BoolT) -> (op valL valR, envR, argsR, BoolT)
          otherwise -> typErr tL tR
      typErr tL tR = error $ sym ++ " can't be applied to " ++ show (tL, tR)

eval env args (Unary sym e) = error "Can't evaluate unary yet"

eval env args (Lambda e) = eval env args e

eval env args (Assign name expr next) = 
  let (rhs, envR, argsR, tR) = eval env args expr in
  eval (addVar name rhs envR) argsR next

eval env args (Conditional c t f) =
  let 
    (cV, envC, argsC, tC) = eval env args c 
    (tV, envT, argsT, tT) = eval envC argsC t 
    (fV, envF, argsF, tF) = eval envT argsT f 
  in
  if tC /= BoolT then error "Type of a condition must be Bool"
    else if tT /= tF then error "Types of branches in if statement don't match"
      else
        case cV of
        BoolV True -> eval envC argsC t
        BoolV False -> eval envC argsC f
        FunV env' expr | tC == BoolT ->
          (FunV (updateEnv env' envC) (Conditional expr t f), envC, argsC, tF)
        otherwise -> error "if statement condition doesn't resolve to a bool"

eval env args (Call f es) = error "Can't evaluate call yet"