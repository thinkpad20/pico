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
  show (SymTable mp) = (intercalate ",") . (map showPairs) $ Map.toList mp where
    showPairs (name, val) = name ++ "=>" ++ show val

instance Show Val where
  show (Val (e, syms)) = case syms of
    [] -> show e
    otherwise -> "λ{" ++ show e ++ ", " ++ show syms ++ "}"

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

--sUnion :: [SymTable] -> [SymTable] -> [SymTable]
--sUnion [] [] = []
--sUnion [] (s:ss) -> (s:ss)
--sUnion (s:ss) [] -> (s:ss)
--sUnion (s1:ss1) (s2:ss2) = (Map.union s1 s2):ss2

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
eval (Var name) (syms, args) = trace ("looking up " ++ name ++ ", resolves to " ++ show (sLookup name syms)) $ case sLookup name syms of
  Just (Val (e, syms')) -> trace ("evaling with context " ++ show ctx ++ ", will eval to " ++ show (eval e ctx)) eval e ctx where ctx = (syms', args)
  Nothing -> error $ "Symbol " ++ name ++ " is not defined in this scope"
eval u@(Unbound name _) (syms, args) = 
  case sLookupSingle name syms of 
    Nothing ->
      case args of
        [] -> (Val (u, []), (sAdd name (Val (u, [])) syms, []))
        (Nothing:as) -> (Val (u, []), (sAdd name (Val (u, [])) syms, []))
        ((Just a):as) -> trace ("consuming one argument " ++ show a ++ " new args is " ++ show as) (a, (sAdd name a syms, as))
    Just _ -> error $ "Variable " ++ name ++ " has already been used in this scope"

{--------------- Assignment --------------}
eval (Assign name rhs next) (syms, args) = case sLookupSingle name syms of
  Nothing ->
    eval next (syms', args') where
      (valR, (symsR, args')) = eval rhs (syms, args)
      syms' = (sAdd name valR symsR)
  Just _ -> error $ name ++ " cannot be redeclared in the same scope"

{------------ Binary functions -----------}
eval (Binary op l r) ctx0 = case op of
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
    (valL, ctx1) = trace ("context before evaluating " ++ show l ++ " was " ++ show ctx0) eval l ctx0
    (valR, ctx2) = trace ("context before evaluating " ++ show l ++ " was " ++ show ctx1) eval r ctx1
    {- in all of these, if we can resolve it we do; otherwise we return -}
    evalNumOp o = case (valL, valR) of
      (Val (Number a, _), Val (Number b, _)) -> trace ("passing on context: " ++ show ctx2) (Val(Number $ o a b, []), ctx2)
      otherwise -> (Val (Binary op l r, []), ctx2)
    evalCompOp o = case (valL, valR) of
      (Val (a@(Number _), _), Val (b@(Number _), _)) -> (Val(Bool $ o a b, []), ctx2)
      (Val (a@(Bool _), _), Val (b@(Bool _), _)) -> (Val(Bool $ o a b, []), ctx2)
      (Val (Number _, _), Val (Bool _, _)) -> error $ "Type mismatch: " ++ show (l,r)
      (Val (Bool _, _), Val (Number _, _)) -> error $ "Type mismatch: " ++ show (l,r)
      otherwise -> (Val (Binary op l r, []), ctx2)
    evalLogOp o = case (valL, valR) of
      (Val (Bool a, _), Val (Bool b, _)) -> (Val(Bool $ o a b, []), ctx2)
      otherwise -> (Val (Binary op l r, []), ctx2)

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
eval (Lambda e) (syms, args) = trace ("evaluating lmbda, e = " ++ show e ++ ", args = " ++ show args) (res, ctx) where
  ctx = (syms, args')
  (v@(Val (e', c)), (_, args')) = eval e (syms, args)
  res = case e' of
    Number n -> v
    PString s -> v
    PChar c -> v
    Bool b -> v
    other@(_) -> Val ((Lambda other), syms)

{---------- Call with arguments ---------}
eval (Call f es) ctx = eval f ctx' where
  (args', (syms, args)) = eval' es ctx
  ctx' = trace ("constructing these args: " ++ show (args' ++ args)) (syms, args' ++ args)
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