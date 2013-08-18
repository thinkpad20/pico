module PicoEval where

import PicoAST
import qualified Data.Map as Map

-- FunctionRecord stores the name, parameter types and return type
-- of all of the functions we're aware of
data FunctionRecord = FunctionRecord String [PType] deriving (Ord, Eq, Show)
type FuncMap = Map.Map FunctionRecord FunctionVal -- func names qualified with types 
-- a Value is the result of an evaluation, either a literal value or a function (for now)
data Value = 
  IntVal Int
  | FloatVal Double
  | CharVal Char
  | StringVal String
  | BoolVal Bool
  | FunctionVal Env String Expression
  deriving Show

findFVal :: FuncMap -> String -> [PType] -> Maybe PType
findFVal mp name ts = lookup mp (FunctionRecord name ts)


numerOps = [ "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=" ]
numerTypes = [IntT, FloatT]
baseOpers = [ FunctionRecord op [t1, t2] (if t1 == t2 && t1 == IntT then IntT else FloatT) 
                                      | op <- numerOps, t1 <- numerTypes, t2 <- numerTypes]
              ++ [
                FunctionRecord "%" [IntT, IntT] IntT,
                FunctionRecord "-" [IntT] IntT,
                FunctionRecord "-" [FloatT] FloatT,
                FunctionRecord "&&" [BoolT, BoolT] BoolT,
                FunctionRecord "||" [BoolT, BoolT] BoolT,
                FunctionRecord "!" [BoolT] BoolT
              ]

reduce :: Expression -> SymbolTable -> Expression
-- reduce will take an AST and reduce it to the simplest possible
-- AST which provides the same result. For example, (1 + 2) * 3 -> 9.
-- Literal values are easy:
reduce p@(PInt i) _       = p
reduce p@(PFloat f) _     = p
reduce p@(PChar c) _      = p
reduce p@(PString s) _    = p
reduce p@(PBool b) _      = p
reduce p@(Unbound v t) _  = p -- we'll need to do some storage on these...

-- Bound variables will require a lookup
reduce (Var name) env = 
  case symLookup env name of
    Just expr -> expr
    Nothing -> error $ "Symbol " ++ name ++ " is not defined in this scope"

-- Reducing assignments means reducing the right side, and adding this to the 
-- symbol table, and then continuing on the next expression
reduce (Assign name rhs next) env = Assign name newRhs newNext where
  newRhs = reduce rhs env
  newNext = reduce next $ symStore env name newRhs
  symStore (top:rest) n e = ((n, e):top):rest

-- Reducing a lambda means reducing its contents, inside a new context
reduce (Lambda expr) env = Lambda $ reduce expr ([]:env)

-- Reducing an if expression means reducing each of its internal expressions
reduce (Conditional c t f) env = 
  Conditional (reduce c env) (reduce t env) (reduce f env)

-- Reducing a function call, for now we just return it as-is...
reduce (Call e es) _ = Call e es

-- Binary and unary expressions are a bit more involved:
reduce (Binary op e1 e2) env = 
  case pOper (reduce e1 env) (reduce e2 env) env of
    Just expr -> expr
    Nothing -> error $ "Type mismatch on binary operation " ++ op
    where pOper = case op of
                      "+" -> pAdd
                      "-" -> pSub
                      "*" -> pMult
                      "/" -> pDiv 
                      "%" -> pMod 
                      "<" -> pLt
                      ">" -> pGt
                      "<=" -> pLeq
                      ">=" -> pGeq
                      "==" -> pEq
                      "!=" -> pNeq
                      "&&" -> pAnd
                      "||" -> pOr
                      "^" -> pExp
                      otherwise -> pBinSymbol op

reduce (Unary op e) env = 
  case pOper (reduce e env) env of
    Just expr -> expr
    Nothing -> error $ "Type mismatch on unary operation " ++ op
  where pOper = case op of
                    "-" -> pNeg
                    "!" -> pNot
                    otherwise -> pUnSymbol op

pAdd :: Expression -> Expression -> SymbolTable -> Maybe Expression
pAdd e1 e2 sym = Just $ e1 + e2

pSub :: Expression -> Expression -> SymbolTable -> Maybe Expression
pSub = undefined

pMult :: Expression -> Expression -> SymbolTable -> Maybe Expression
pMult = undefined

pDiv :: Expression -> Expression -> SymbolTable -> Maybe Expression
pDiv = undefined

pMod :: Expression -> Expression -> SymbolTable -> Maybe Expression
pMod = undefined

pLt :: Expression -> Expression -> SymbolTable -> Maybe Expression
pLt = undefined

pGt :: Expression -> Expression -> SymbolTable -> Maybe Expression
pGt = undefined

pLeq :: Expression -> Expression -> SymbolTable -> Maybe Expression
pLeq = undefined

pGeq :: Expression -> Expression -> SymbolTable -> Maybe Expression
pGeq = undefined

pEq :: Expression -> Expression -> SymbolTable -> Maybe Expression
pEq = undefined

pNeq :: Expression -> Expression -> SymbolTable -> Maybe Expression
pNeq = undefined

pAnd :: Expression -> Expression -> SymbolTable -> Maybe Expression
pAnd = undefined

pOr :: Expression -> Expression -> SymbolTable -> Maybe Expression
pOr = undefined

pExp :: Expression -> Expression -> SymbolTable -> Maybe Expression
pExp = undefined

pBinSymbol :: String -> Expression -> Expression -> SymbolTable -> Maybe Expression
pBinSymbol = undefined

pNeg :: Expression -> SymbolTable -> Maybe Expression
pNeg = undefined

pNot :: Expression -> SymbolTable -> Maybe Expression
pNot = undefined

pUnSymbol :: String -> Expression -> SymbolTable -> Maybe Expression
pUnSymbol = undefined

symLookup :: SymbolTable -> String -> Maybe Expression
symLookup = undefined

getType :: Expression -> FuncMap -> PType
getType (PInt i)      _ = IntT
getType (PFloat f)    _ = FloatT
getType (PChar c)     _ = CharT
getType (PString s)   _ = StringT
getType (PBool b)     _ = BoolT
getType (Unbound v t) _ = t

getType (Var v) funcs =
  case symLookup funcs v of
    Just expr -> getType expr funcs
    Nothing -> error $ "Can't get type of " ++ v ++ ", not defined in scope"

getType (Binary sym l r) funcs = 
  let lType = getType l funcs
      rType = getType r funcs 
      fVal = findFVal sym lType rType in
  case fVal of 
    Just 
  if lType == rType then lType 
  else error $ "Left-right mismatch in binary expression"

getType (Unary _ e) funcs = getType e funcs
getType (Lambda e) funcs = getType e funcs
getType (Assign _ _ next) funcs = getType next funcs
getType (Conditional _ t f) funcs =
  let tType = getType t funcs
      fType = getType f funcs in
  if tType == fType
    then tType
    else error $ "Both branches of a conditional statement must return same type"
getType (Call f es) funcs = 
  let funcType = getType f

getNumUnbound :: Expression -> Int
getNumUnbound expr = count expr where
  count (Unbound _ _) = 1
  count (Lambda e) = count e
  count (Unary _ e) = count e
  count (Binary _ l r) = count l + count r
  count (Assign _ rhs next) = count rhs + count next
  count (Conditional c t f) = count c + count t + count f
  count (Call f es) = count f + (foldl1 (+) (map count' es)) where
    count' (Just expr) = count expr
    count' _ = 0
  count _ = 0

findArgs :: Expression -> [(String, PType)]
findArgs (Unbound v t) = [(v, typ)]
  where typ = case t of
                "int" -> IntT
                "float" -> FloatT
                "char" -> CharT
                "string" -> StringT
                "bool" -> BoolT
                otherwise -> CustomT t
findArgs (Assign _ r n) = findArgs r ++ findArgs n
findArgs (Conditional c t f) = findArgs c ++ findArgs t ++ findArgs f
findArgs (Binary _ l r) = findArgs l ++ findArgs r
findArgs (Unary _ e) = findArgs e
findArgs (Call f es) = findArgs f ++ (foldl1 (++) $ map findArgs' es) where
  findArgs' (Just e) = findArgs e
  findArgs' _ = []
findArgs _ = []