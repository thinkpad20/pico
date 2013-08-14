module PicoEval where

import PicoAST

data PType = 
  IntT 
  | FloatT 
  | CharT 
  | BoolT 
  | StringT 
  | FunctionT [PType] PType
  | NewT String
  deriving (Ord, Show, Eq)

type SymbolTable = [[(String, Expression)]]

data FunctionRecord = FunctionRecord String [PType] PType deriving (Ord, Eq, Show)

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
reduce (Var name) tbl = 
  case symLookup tbl name of
    Just expr -> expr
    Nothing -> error $ "Symbol " ++ name ++ " is not defined in this scope"

-- Reducing assignments means reducing the right side, and adding this to the 
-- symbol table, and then continuing on the next expression
reduce (Assign name rhs next) tbl = Assign name newRhs newNext where
  newRhs = reduce rhs tbl
  newNext = reduce next $ symStore tbl name newRhs
  symStore (top:rest) n e = ((n, e):top):rest

-- Reducing a lambda means reducing its contents, inside a new context
reduce (Lambda expr) tbl = Lambda $ reduce expr ([]:tbl)

-- Reducing an if expression means reducing each of its internal expressions
reduce (Conditional c t f) tbl = 
  Conditional (reduce c tbl) (reduce t tbl) (reduce f tbl)

-- Reducing a function call, for now we just return it as-is...
reduce (Call e es) _ = Call e es

-- Binary and unary expressions are a bit more involved:
reduce (Binary op e1 e2) tbl = 
  case pOper (reduce e1 tbl) (reduce e2 tbl) tbl of
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

reduce (Unary op e) tbl = 
  case pOper (reduce e tbl) tbl of
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

getType :: Expression -> SymbolTable -> Either String PType
getType (PInt i)      _ = Right IntT
getType (PFloat f)    _ = Right FloatT
getType (PChar c)     _ = Right CharT
getType (PString s)   _ = Right StringT
getType (PBool b)     _ = Right BoolT
getType (Unbound v t) tbl =
  case t of
    "int" -> Right IntT
    "float" -> Right FloatT
    "char" -> Right CharT
    "string" -> Right StringT
    "bool" -> Right BoolT
    otherwise -> Right $ NewT t -- later we'll do a lookup on this type

getType (Var v) tbl =
  case symLookup tbl v of
    Just expr -> getType expr tbl
    Nothing -> Left $ "Can't get type of " ++ v ++ ", not defined in scope"

getType (Binary _ l r) tbl = do
  lType <- getType l tbl
  rType <- getType r tbl
  if lType == rType 
    then return lType 
    else Left "Left-right mismatch in binary expression"

getType (Unary _ e) tbl = getType e tbl
getType (Lambda e) tbl = getType e tbl
getType (Assign _ _ next) tbl = getType next tbl
getType (Conditional _ t f) tbl = do
  tType <- getType t tbl
  fType <- getType f tbl
  if tType == fType
    then return tType
    else Left "Both branches of a conditional statement must return same type"

getType (Call _ _) _ = undefined -- this is a toughie

getNumUnbound :: Expression -> Int
getNumUnbound expr = count expr where
  count (Unbound _ _) = 1
  count (Lambda e) = count e
  count (Unary _ e) = count e
  count (Binary _ l r) = count l + count r
  count (Call f es) = count f + (foldl1 (+) (map count' es)) where
    count' (Just expr) = count expr
    count' _ = 0
  count (Assign _ rhs next) = count rhs + count next
  count (Conditional c t f) = count c + count t + count f
  count _ = 0