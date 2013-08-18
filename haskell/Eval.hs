module PicoEval where

import PicoAST
import qualified Data.Map as Map

-- FunctionRecord stores the name, parameter types and return type
-- of all of the functions we're aware of
data FunctionRecord = FR String [PType] deriving (Ord, Eq, Show)
type Env = Map.Map FunctionRecord Value -- function names/types -> definitions
-- a Value is the result of an evaluation, either a literal value or a function (for now)
data Value = 
  IntVal Int
  | FloatVal Double
  | CharVal Char
  | StringVal String
  | BoolVal Bool
  | FunctionDef Env Expression
  | Argument -- hack? for until we figure out how to do this stuff
  deriving Show

fLookup :: Env -> String -> [PType] -> Maybe Value
fLookup env name ts = Map.lookup (FR name ts) env

-- initial map of functions -- all dummies right now, only to return types
baseOpers :: Env
baseOpers = Map.fromList $ 
              [ (FR op [t1, t2], unb $ intFloat t1 t2)
                  | op <- ops, t1 <- [IntT, FloatT], t2 <- [IntT, FloatT]]
                ++ 
              [ (FR "%"  [IntT, IntT], unb IntT),
                (FR "-"  [IntT], unb IntT),
                (FR "-"  [FloatT], unb FloatT),
                (FR "&&" [BoolT, BoolT], unb BoolT),
                (FR "||" [BoolT, BoolT], unb BoolT),
                (FR "!"  [BoolT], unb BoolT) ]
            where ops = 
                    [ "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=" ]
                  intFloat t1 t2 = 
                    if (t1 == IntT) && (t2 == IntT) then IntT else FloatT
                  unb = ( FunctionDef (Map.empty) ) . (Unbound "")

getType :: Expression -> PType
getType = getType' baseOpers

getType' :: Env -> Expression -> PType
getType' _ (PInt i)      = IntT
getType' _ (PFloat f)    = FloatT
getType' _ (PChar c)     = CharT
getType' _ (PString s)   = StringT
getType' _ (PBool b)     = BoolT
getType' _ (Unbound v t) = t

getType' env (Var v) =
  case fLookup env v [] of
    Just (FunctionDef env' expr) -> getType' (Map.union env' env) expr
    Nothing -> error $ "Can't get type of " ++ v ++ ", not defined in scope"

getType' env (Binary sym l r) = 
  let ts = map (getType' env) [l,r]
      f = fLookup env sym ts in
  case f of 
    Just (FunctionDef env' expr) -> getType' (Map.union env' env) expr
    Just _ -> error "Non-sensical result found in binary getType evaluation"
    Nothing -> error ("Symbol " ++ sym ++ 
                      " is not defined for types " ++ (show ts))

getType' env (Unary sym e) = 
  let t = getType' env e
      f = fLookup env sym [t] in
  case f of 
    Just (FunctionDef env' expr) -> getType' (Map.union env' env) expr
    Just _ -> error "Non-sensical result found in unary getType evaluation"
    Nothing -> error ("Symbol " ++ sym ++ " is not defined for type " ++ (show t))

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
      case fLookup env name types of
        Just (FunctionDef env' expr) -> getType' (Map.union env' env) expr
        Just _ -> error "Non-sensical result found in n-ary getType evaluation"
        Nothing -> error ("Symbol " ++ name ++ " is not defined for types " ++ (show types))
    Lambda e -> getType' env e
    otherwise -> error "Call can only be made on lambda or reference to one"
  where types = map (getType'' env) es
        getType'' env (Just expr) = getType' env expr
        getType'' env Nothing = error "Can't deal with partial application yet"


{-
Ah, we have our function that goes and finds the arguments and their types.
We can run that function whenever we hit a new lambda, and grab all of the 
unbound variables. This is good!
-}

eval :: Env -> Expression -> (Value, PType, Env)
eval env (PInt i)      = (IntVal i, IntT, env)
eval env (PFloat f)    = (FloatVal f, FloatT, env)
eval env (PChar c)     = (CharVal c, CharT, env)
eval env (PString s)   = (StringVal s, StringT, env)
eval env (PBool b)     = (BoolVal b, BoolT, env)
eval env (Unbound v t) = (Argument, t, env)

eval env (Var v) =
  case fLookup env v [] of
    Just (FunctionDef env' expr) -> eval (Map.union env' env) expr
    Nothing -> error $ "Can't get type of " ++ v ++ ", not defined in scope"

eval env (Binary sym l r) = 
  let (ts, es) = unzip $ map (eval env) [l,r] -- separate the types and 
                                              -- expressions
      f = fLookup env sym ts in -- see if there's a function with this 
                                -- name and these parameter types
  case f of -- if f is a function, insert the values into its environment and eval
    Just (FunctionDef env' expr) -> eval (Map.insert  env') expr
    Just _ -> error "Non-sensical result found in binary getType evaluation"
    Nothing -> error ("Symbol " ++ sym ++ 
                      " is not defined for types " ++ (show ts))

eval env (Unary sym e) = 
  let t = eval env e
      f = fLookup env sym [t] in
  case f of 
    Just (FunctionDef env' expr) -> eval (Map.union env' env) expr
    Just _ -> error "Non-sensical result found in unary getType evaluation"
    Nothing -> error ("Symbol " ++ sym ++ " is not defined for type " ++ (show t))
-- When we're evaluating a lambda, we need to go in and find all of its unbound
-- variables (a.k.a. parameters) first.
eval env (Lambda e) = 
  let params = findParams e
  -- fold over the list of parameters, adding each one to the environment
      env' = foldr Map.insert env (map toKey params)
      toKey (vName, vType) = FunctionDef vName [] vType
  in
  eval env e
eval env (Assign _ _ next) = eval env next
eval env (Conditional _ t f) =
  let tType = eval env t
      fType = eval env f in
  if tType == fType
    then tType
    else error $ "Both branches of a conditional statement must return same type"
eval env (Call f es) = 
  case f of
    Var name ->
      case fLookup env name types of
        Just (FunctionDef env' expr) -> eval (Map.union env' env) expr
        Just _ -> error "Non-sensical result found in n-ary getType evaluation"
        Nothing -> error ("Symbol " ++ name ++ " is not defined for types " ++ (show types))
    Lambda e -> eval env e
    otherwise -> error "Call can only be made on lambda or reference to one"
  where types = map (eval' env) es
        eval' env (Just expr) = eval env expr
        eval' env Nothing = error "Can't deal with partial application yet"

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