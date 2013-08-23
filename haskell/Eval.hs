module PicoEval where

import PicoAST
import qualified Data.Map as Map
import Data.Ratio
import Debug.Trace

type Env = [Map.Map String Value] -- stack of (variable names -> definitions)

-- a Value is the result of an evaluation, either a literal value or a function (for now)
data Value = 
  NumV Double
  | CharV Char
  | StringV String
  | BoolV Bool
  | FunV Env Expression
  | ArgV String PType
  | NullV
  deriving (Show, Eq, Ord)

-- args are evaluated prior to being applied, so they're type Value
type Args = [Value]

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
addVar s v (env:envs) = trace ("Mapping var " ++ s ++ " to " ++ show v) $ 
  if s == "n" then
    case v of (FunV _ _) -> error "hey, what gives"
              _ -> (Map.insert s v env):envs
    else (Map.insert s v env):envs

evalStart :: Expression -> (Value, Env, Args, PType)
evalStart e = eval [Map.empty] [] e

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
-- When we see an unbound variable, if there are no arguments, leave it as unbound (ArgV)
eval env []   (Unbound nm t)  = (ArgV nm t, addVar nm (ArgV nm t) env, [], t)
-- if there are arguments, but it's a Null arg, treat it as if there weren't any argument.
eval env (NullV:as) (Unbound nm t)  = (ArgV nm t, addVar nm (ArgV nm t) env, as, t)
-- otherwise, consume the argument and store the mapping. Later we'll typecheck
eval env (a:as) (Unbound nm t) = (a, addVar nm a env, as, t)

eval env args (Var name) =
  trace ("looking up " ++ name ++ ": " ++ show (fLookup env name)) $
    case fLookup env name of
      Just (FunV env' expr) -> trace (name ++ " is a fun") $ eval (updateEnv env' env) args expr
      Just (NumV n) -> (NumV n, env, args, NumT)
      Just (BoolV b) -> (BoolV b, env, args, BoolT)
      Just (CharV c) -> (CharV c, env, args, CharT)
      Just (StringV s) -> (StringV s, env, args, StringT)
      Just (ArgV n t) -> (ArgV n t, env, args, t)
      Just x -> error $ "wtf is this thing: " ++ show x
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
          (NumT, NumT) -> numTBinEval (op valL valR) NumT
          otherwise -> typErr tL tR
      evalCompOp op =
        case (tL, tR) of
          (NumT, NumT) -> numTBinEval (BoolV $ op valL valR) BoolT
          (CharT, CharT) -> (BoolV $ op valL valR, envR, argsR, BoolT)
          otherwise -> typErr tL tR
      evalLogOp op = -- doesn't short-circuit, yet
        case (tL, tR) of
          (BoolT, BoolT) -> (op valL valR, envR, argsR, BoolT)
          otherwise -> typErr tL tR
      typErr tL tR = error $ sym ++ " can't be applied to " ++ show (tL, tR)
      numTBinEval ev typ = trace ("evaling binop, valL, valR = " ++ show (valL, valR)) $
        case (valL, valR) of
          (NumV _, NumV _) -> (ev, envR, argsR, typ)
          (NumV _, ArgV _ _) -> newBin envR (toExpr valL) (toExpr valR)
          (ArgV _ _, NumV _) -> newBin envR (toExpr valL) (toExpr valR)
          (ArgV _ _, ArgV _ _) -> newBin envR (toExpr valL) (toExpr valR)
          (NumV _, FunV env' expr') -> newBin (updateEnv envR env') (toExpr valL) expr'
          (FunV env' expr', NumV _) -> newBin (updateEnv envR env') expr' (toExpr valR)
          (ArgV _ _, FunV env' expr') -> newBin (updateEnv envR env') (toExpr valL) expr'
          (FunV env' expr', ArgV _ _) -> newBin (updateEnv envR env') expr' (toExpr valR)
          (FunV env1 expr1, FunV env2 expr2) ->
            newBin (updateEnv (updateEnv envR env1) env2) expr1 expr2
          where newBin env' e1 e2 = (FunV env' $ Binary sym e1 e2, env', argsR, typ)

eval env args (Unary sym e) =
  let (val, env', args', t) = eval env args e in
  case sym of 
    "-" ->
      case t of 
        NumT -> 
          case val of
            NumV _ -> (negate val, env', args', NumT)
            ArgV _ _ -> newUn env' (toExpr val)
            FunV env'' expr -> newUn (updateEnv env' env'') expr
            where newUn envi ex = (FunV envi $ Unary sym ex, envi, args', NumT)
        otherwise -> error $ "Error: " ++ sym ++ " can't be applied to " ++ show t
    "!" -> 
      case t of
        BoolT ->
          case val of
            BoolV b -> (BoolV $ not b, env', args', BoolT)
            ArgV _ _ -> newUn env' (toExpr val)
            FunV env'' expr -> newUn (updateEnv env' env'') expr
            where newUn envi ex = (FunV envi $ Unary sym ex, envi, args', BoolT)
        otherwise -> error $ "Error: " ++ sym ++ " can't be applied to " ++ show t
    otherwise -> error "bloops"

eval env args (Lambda e) =
  let (val, env', args', t) = trace ("evaling lmda, args are " ++ show args) $ eval (pushEnv env) args e in
  (FunV env e, env, args, t)

eval env args (Assign name expr next) = 
  let (rhs, envR, argsR, tR) = trace ("evaling rhs, args are " ++ show args) $ eval env args expr in
  trace ("result of eval is " ++ show rhs) $
    eval (addVar name rhs envR) argsR next

eval env args (Conditional c t f) =
  let 
    (cV, envC, argsC, tC) = trace ("evaling condition ") $ eval env args c 
    (tV, envT, argsT, tT) = trace ("cond eval'd to " ++ show cV) $ eval envC argsC t 
    (fV, envF, argsF, tF) = trace ("cond eval'd to " ++ show cV) $ eval envT argsT f 
  in
  trace ("evaluating a conditional, args are " ++ show args) $
    if tC /= BoolT then error "Type of a condition must be Bool"
      else if tT /= tF then error "Types of branches in if statement don't match"
        else
          case cV of
          BoolV True -> eval envC argsC t
          BoolV False -> eval envC argsC f
          FunV env' expr | tC == BoolT ->
            (FunV (updateEnv env' envC) (Conditional expr t f), envC, argsC, tF)
          otherwise -> error "if statement condition doesn't resolve to a bool"

eval env args (Call f es) = trace ("choosing whether to call " ++ show f ++ " with args "++ show es) $
  let
    evArgs = (map (toFunV env) es)
    toFunV env (Just e) = FunV env e
    toFunV env Nothing = NullV
    shouldEval = not $ hasArgV evArgs
    hasArgV [] = trace "no argv found" False
    hasArgV ((ArgV _ _):_) = True
    hasArgV (_ : as) = hasArgV as
  in
  if shouldEval then
    trace ("calling " ++ show f ++ " with args " ++ show evArgs) $ eval env (evArgs ++ args) f
    else trace ("decided not to eval") (FunV env (Call f es), env, args, NumT)
