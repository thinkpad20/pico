module PicoEval where

import PicoAST
import PicoParser (run)
import qualified Data.Map as Map
import Data.List (intercalate)
import Debug.Trace

newtype Val = Val (Expression, [SymTable])
newtype SymTable = SymTable { getTable :: Map.Map String Val }
type Context = ([SymTable], [Maybe Val])

instance Show SymTable where
  show (SymTable mp) = "<table>" ++ ((intercalate ",") . (map showPairs) $ Map.toList mp) ++ "</table>" where
    showPairs (name, val) = name ++ "=>" ++ show val

instance Show Val where
  show (Val (e, syms)) = case syms of
    [] -> "(" ++ show e ++ ")"
    otherwise -> "(λ{" ++ show e ++ ", " ++ show syms ++ "})"

{---------- Symbol table operations ----------}

sLookup :: String -> [SymTable] -> Maybe Val
sLookup _ [] = Nothing
sLookup name ((SymTable tbl):tbls) = case Map.lookup name tbl of
  Just v -> Just v
  Nothing -> sLookup name tbls

sLookupSingle :: String -> [SymTable] -> Maybe Val
sLookupSingle _ [] = Nothing
sLookupSingle name tbls = Map.lookup name ((getTable . head) tbls)

sAdd :: String -> Val -> [SymTable] -> [SymTable]
sAdd s r [] = [SymTable $ Map.singleton s r]
sAdd s r ((SymTable tbl):tbls) = (SymTable $ Map.insert s r tbl):tbls

{------------- Utility functions -------------}
lAnd :: Expression -> Expression -> Expression
lAnd (Bool b1) (Bool b2) = Bool $ b1 && b2
lAnd e1 e2 = error $ "&& only works on booleans; we got " ++ show (e1, e2)

lOr :: Expression -> Expression -> Expression
lOr (Bool b1) (Bool b2) = Bool $ b1 || b2
lOr e1 e2 = error $ "|| only works on booleans; we got " ++ show (e1, e2)

(===) :: Expression -> Expression -> Expression
(Bool b1) === (Bool b2) = Bool $ b1 == b2
(Number n1) === (Number n2) = Bool $ n1 == n2
(PChar c1) === (PChar c2) = Bool $ c1 == c2
(PString s1) === (PString s2) = Bool $ s1 == s2

{---------------- Evaluator ------------------}
eval' input = eval (run input) ([], [])
evalWith args input = eval (run input) ([], (map dummyVal args)) where
  dummyVal ex = Just $ Val (ex, [])

eval :: Expression -> Context -> (Val, Context)
{--------------- Primitives --------------}
eval e@(Number  _) ctx = (Val (e, []), ctx)
eval e@(PChar   _) ctx = (Val (e, []), ctx)
eval e@(PString _) ctx = (Val (e, []), ctx)
eval e@(Bool    _) ctx = (Val (e, []), ctx)

{--------------- Variables ---------------}
eval (Var name) (syms0, args0) = trace ("looking up " ++ name ++ ", resolves to " ++ show (sLookup name syms0)) $ case sLookup name syms0 of
  Just (Val (e, syms1)) -> 
    let (val, (syms2, args1)) = eval e (syms1, args0) in
    (val, (syms0, args1))
  Nothing -> error $ "Symbol " ++ name ++ " is not defined in this scope"
eval u@(Unbound name _) (syms, args) = 
  case sLookupSingle name syms of 
    Nothing ->
      case args of
        [] -> (Val (u, []), (sAdd name (Val (u, [])) syms, []))
        (Nothing:as) -> (Val (u, []), (sAdd name (Val (u, [])) syms, []))
        ((Just a):as) -> trace ("consuming one argument to fill " ++ name ++": (" ++ show a ++ "). new args is " ++ show as) (a, (sAdd name a syms, as))
    Just _ -> error $ "Variable " ++ name ++ " has already been used in this scope"

{--------------- Assignment --------------}
eval (Assign name rhs next) (syms, args) = trace ("evaluating the assignment of " ++ name ++ " to " ++ show rhs ++ " under context " ++ show (syms,args)) $ 
  case sLookupSingle name syms of
    Nothing ->
      let
        -- map name to (unevaluated) RHS and current symbol table
        symsWithName = trace ("1" ++ name ++ ") storing " ++ name ++ " to " ++ show (Val (rhs, syms))) sAdd name (Val (rhs, syms)) syms
        -- evaluate right-hand side with this mapping
        (rhsVal, (newSyms, newArgs)) = trace ("2" ++ name ++ ") evaluating " ++ show rhs ++ " with " ++ show (symsWithName, args)) eval rhs (symsWithName, args)
        -- update symbol table with evaluated RHS
        --newSyms = trace ("3" ++ name ++ ") updating symbol table with " ++ name ++ " stored to " ++ show rhsVal) sAdd name rhsVal symsWithName
      in
      -- pass this updated context into the evaluation of the next expression
      trace ("4" ++ name ++ ") evaluating " ++ show next ++ " with " ++ show (newSyms, newArgs)) eval next (newSyms, newArgs)
    Just _ -> error $ name ++ " cannot be redeclared in the same scope"

{------------ Binary functions -----------}
eval (Binary op l r) ctx = case op of
  "+" -> evalNumOp (+)
  "-" -> evalNumOp (-)
  "*" -> evalNumOp (*)
  "/" -> evalNumOp (/)
  "^" -> evalNumOp (**)
  ">" -> evalCompOp (>)
  "<" -> evalCompOp (<)
  "<=" -> evalCompOp (<=)
  ">=" -> evalCompOp (>=)
  "==" -> evalCompOp (==)
  "!=" -> evalCompOp (/=)
  "&&" -> evalLogOp (&&)
  "||" -> evalLogOp (||)
  otherwise -> error $ "we can't handle " ++ op ++ " yet"
  where 
    (valL, ctxL) = trace ("LEFT: context before evaluating " ++ show l ++ " was " ++ show ctx) eval l ctx
    (valR, ctxR) = trace ("RIGHT: context before evaluating " ++ show r ++ " was " ++ show ctxL) eval r ctxL
    {- in all of these, if we can resolve it we do; otherwise we return -}
    evalNumOp o = case (valL, valR) of
      (Val (Number a, _), Val (Number b, _)) -> trace ("passing on context: " ++ show ctxR) (Val(Number $ o a b, []), ctxR)
      otherwise -> (Val (Binary op l r, []), ctxR)
    evalCompOp o = case (valL, valR) of
      (Val (a@(Number _), _), Val (b@(Number _), _)) -> (Val(Bool $ o a b, []), ctxR)
      (Val (a@(Bool _), _), Val (b@(Bool _), _)) -> (Val(Bool $ o a b, []), ctxR)
      (Val (Number _, _), Val (Bool _, _)) -> error $ "Type mismatch: " ++ show (l,r)
      (Val (Bool _, _), Val (Number _, _)) -> error $ "Type mismatch: " ++ show (l,r)
      otherwise -> (Val (Binary op l r, []), ctxR)
    evalLogOp o = case (valL, valR) of
      (Val (Bool a, _), Val (Bool b, _)) -> (Val(Bool $ o a b, []), ctxR)
      otherwise -> (Val (Binary op l r, []), ctxR)

{------------ Unary functions -----------}
eval (Unary op e) ctx = case op of
  "-" -> evalNumOp negate
  otherwise -> error $ "we can't handle " ++ op ++ " yet"
  where
    (v, ctx') = eval e ctx
    evalNumOp oper = case v of
      Val (Number a, _) -> (Val (Number $ negate a, []), ctx')
      otherwise -> (Val (Unary op e, []), ctx')

{---------- Lambda expressions ----------}
eval (Lambda e) (syms, args) = trace ("evaluating lmbda, context is " ++ show (syms, args) ++ ", expression is " ++ show e) (res, ctx) where
  ctx = (syms, args')
  (v@(Val (e', c)), (_, args')) = trace ("result of evaluation is " ++ show (eval e (syms, args))) $ eval e (syms, args)
  res = case e' of
    Number n -> v
    PString s -> v
    PChar c -> v
    Bool b -> v
    other@(_) -> trace ("returning new lambda: " ++ show (Val ((Lambda other), syms))) Val ((Lambda other), syms)

{---------- Call with arguments ---------}
eval (Call f es) ctx = eval f ctx' where
  -- map an evaluation over all of the expressions in the list, carrying the context through
  -- each time. These expressions are the arguments to the functions we're calling.
  (args', (syms, args)) = eval' es ctx
  -- append these arguments to the top of our argument list, and evaluate the function with them.
  ctx' = trace ("constructing this context: " ++ show (syms, args' ++ args) ++ "result is: " ++ show (eval f (syms, args' ++ args))) (syms, args' ++ args)
  eval' [] context = ([], context)
  eval' ([Just x]) context = 
    let (v, context') = eval x context in
    ([Just v], context')
  eval' ([Nothing]) context = ([Nothing], context)
  eval' (e:es) context = 
    let (vs, context') = eval' es context in
    case e of
      Just x -> 
        let (v, context'') = eval x context' in 
        ((Just v):vs, context'')
      Nothing ->
        (Nothing:vs, context')

eval _ _ = error "plops"