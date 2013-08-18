module PicoEval where

import PicoAST
import qualified Data.Map as Map

-- FunctionRecord stores the name, parameter types and return type
-- of all of the functions we're aware of (MAYBE NOT NECESSARY)
--data FunctionRecord = FunV String [PType] deriving (Ord, Eq, Show)
type Env = Map.Map String Value -- variable names -> definitions
type Args = [Value]
-- a Value is the result of an evaluation, either a literal value or a function (for now)
data Value = 
  NumV Double
  | CharV Char
  | StringV String
  | BoolV Bool
  | FunV Env Expression
  | ArgV String PType
  deriving Show

fLookup :: Env -> String -> Maybe Value
fLookup env name = Map.lookup name env

-- initial map of functions -- all dummies right now, only to return types
--baseOpers :: Env
--baseOpers = Map.fromList $ 
--              [ (FunV op)
--                  | op <- ops, t1 <- [IntT, FloatT], t2 <- [IntT, FloatT]]
--                ++ 
--              [ (FunV "%"  [IntT, IntT], unb IntT),
--                (FunV "-"  [IntT], unb IntT),
--                (FunV "-"  [FloatT], unb FloatT),
--                (FunV "&&" [BoolT, BoolT], unb BoolT),
--                (FunV "||" [BoolT, BoolT], unb BoolT),
--                (FunV "!"  [BoolT], unb BoolT) ]
--            where ops = 
--                    [ "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=" ]
--                  intFloat t1 t2 = 
--                    if (t1 == IntT) && (t2 == IntT) then IntT else FloatT
--                  unb = ( FunV (Map.empty) ) . (Unbound "")

baseOpers = Map.empty

getType :: Expression -> PType
getType = getType' baseOpers

getType' :: Env -> Expression -> PType
getType' _ (PNum n)    = NumT
getType' _ (PChar c)     = CharT
getType' _ (PString s)   = StringT
getType' _ (PBool b)     = BoolT
getType' _ (Unbound v t) = t

getType' env (Var v) =
  case fLookup env v of
    Just (FunV env' expr) -> getType' (Map.union env' env) expr
    Nothing -> error $ "Can't get type of " ++ v ++ ", not defined in scope"

getType' env (Binary sym l r) = 
  let tL = getType' env l
      tR = getType' env r
      f = fLookup env sym in
  if (sym `elem` [ "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="])
    then if tL == NumT && tR == NumT then NumT
      else error $ "Type mismatch on " ++ sym
  else if (sym `elem` ["&&", "||"])
    then if tL == BoolT && tR == BoolT then BoolT
      else error $ "Type mismatch on " ++ sym
  else case f of 
        Just (FunV env' expr) -> getType' (Map.union env' env) expr
        Just _ -> error "Non-sensical result found in binary getType evaluation"
        Nothing -> error ("Symbol " ++ sym ++ " is not defined for types " 
                            ++ (show tL) ++ ", " ++ (show tR))

getType' env (Unary sym e) = undefined

getType' env (Lambda e) = getType' env e
getType' env (Assign _ _ next) = getType' env next
getType' env (Conditional _ t f) =
  let tType = getType' env t
      fType = getType' env f in
  if tType == fType
    then tType
    else error $ "Both branches of a conditional statement must return same type"
getType' env (Call f es) =
  case f of
    Var name ->
      case fLookup env name of
        Just (FunV env' expr) -> getType' (Map.union env' env) expr
        Just _ -> error "Non-sensical result found in n-ary getType evaluation"
        Nothing -> error ("Symbol " ++ name ++ " is not defined for types given")
    Lambda e -> getType' env e
    otherwise -> error "Call can only be made on lambda or reference to one"


{-
Ah, we have our function that goes and finds the arguments and their types.
We can run that function whenever we hit a new lambda, and grab all of the 
unbound variables. This is good!
-}

-- updates the environment. Later we'll probably want to do 
-- checks for namespace collisions, etc
updateEnv :: Env -> Env -> Env
updateEnv e1 e2 = Map.union e2 e1

addVar :: String -> Value -> Env -> Env
addVar s v env = Map.insert s v env

evalStart :: Expression -> Value
evalStart e =
  let (val, _, _) = eval baseOpers [] e in
  val

eval :: Env -> Args -> Expression -> (Value, Env, Args)
eval env args (PNum n)      = (NumV n, env, args)
eval env args (PChar c)     = (CharV c, env, args)
eval env args (PString s)   = (StringV s, env, args)
eval env args (PBool b)     = (BoolV b, env, args)
eval env [] (Unbound nm t)  = (ArgV nm t, addVar nm (ArgV nm t) env, [])
eval env (arg:args) (Unbound nm t) = (arg, addVar nm arg env, args)

eval env args (Var v) =
  case fLookup env v of
    Just (FunV env' expr) -> eval (updateEnv env' env) args expr
    Nothing -> error $ "Variable " ++ v ++ " not defined in scope"

eval env args (Binary sym l r) = 
  let
    (valL, envL, argsL) = eval env args l -- get result of left side and new environment
    (valR, envR, argsR) = eval envL argsL r -- pipe left's environment in for right side
  in
  case sym of
    "+" -> 
      case valL of 
        NumV nL ->
          case valR of 
            NumV nR -> (NumV (nL + nR), envR, argsR)
            ArgV name NumT -> 
              (FunV envR (Binary "+" (PNum nL) (Unbound name NumT)), envR, argsR)
            ArgV _ t -> error $ "+ only accepts NumT, right side was a " ++ show t
            otherwise -> error "poop piles"
        otherwise -> error "double poop piles"
    otherwise -> error $ "we can't handle " ++ sym ++ " yet"

eval env args (Unary sym e) = undefined
-- When we're evaluating a lambda, we need to typecheck args against its args
eval env args (Lambda e) = undefined

eval env args (Assign name expr next) = 
  let (rhs, envR, argsR) = eval env args expr in
  eval (Map.insert name rhs envR) argsR next

eval env args (Conditional c t f) =
  let (cond, envC, argsC) = eval env args c in
  case cond of
    BoolV True -> eval envC argsC t
    BoolV False -> eval envC argsC f
    FunV env' expr | getType expr == BoolT ->
      (FunV (Map.union env' envC) (Conditional expr t f), envC, argsC)
    otherwise -> error "Condition in if statement doesn't resolve to a bool"

eval env args (Call f es) = undefined

findParams :: Expression -> [(String, PType)]
findParams (Unbound v t) = [(v, t)]
findParams (Assign _ r n) = findParams r ++ findParams n
findParams (Conditional c t f) = findParams c ++ findParams t ++ findParams f
findParams (Binary _ l r) = findParams l ++ findParams r
findParams (Unary _ e) = findParams e
findParams (Call f es) = findParams f ++ (foldl1 (++) $ map findParams' es) where
  findParams' (Just e) = findParams e
  findParams' _ = []
findParams _ = []

--case f of
  --  Var name ->
  --    case fLookup env name types of
  --      Just (FunV env' expr) -> eval (Map.union env' env) expr
  --      Just _ -> error "Non-sensical result found in n-ary getType evaluation"
  --      Nothing -> error ("Symbol " ++ name ++ " is not defined for types " ++ (show types))
  --  Lambda e -> eval env e
  --  otherwise -> error "Call can only be made on lambda or reference to one"
  --where types = map (eval' env) es
  --      eval' env (Just expr) = eval env expr
  --      eval' env Nothing = error "Can't deal with partial application yet"

--FunV env' expr | getType expr == NumT -> 
          --  -- want to return a FunV which is the LHS added to this FunV
          --  FunV (updateEnv )