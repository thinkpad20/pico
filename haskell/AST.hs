module PicoAST where

import Data.List (intercalate)
import Data.Ratio

data Expression = 
  Number Double
  | PString String
  | PChar Char
  | Bool Bool
  | Var String
  | Unbound String PType
  | Unary String Expression
  | Binary String Expression Expression
  | Call Expression [Maybe Expression]
  | Lambda Expression
  | Assign String Expression Expression
  | Conditional Expression Expression Expression
  deriving (Eq, Ord)

data PType = 
  NumT  
  | CharT 
  | BoolT 
  | StringT 
  | CustomT String
  | FunctionT [PType] PType
  deriving (Ord, Show, Eq)

instance Show Expression where
  show (Number n) = show n
  show (PString s) = show s
  show (PChar c) = show c
  show (Bool b) = show b
  show (Var v) = v
  show (Unbound varName typeName) = varName ++ ":" ++ show typeName
  show (Unary s e) = s ++ show e
  show (Binary s l r) = "(" ++ show l ++ " " ++ s ++ " " ++ show r ++ ")"
  show (Call e es) = show e ++ "(" ++ (intercalate ", " (map showME es)) ++ ")"
    where showME (Just expr) = show expr
          showME Nothing = ""
  show (Lambda e) = "{" ++ show e ++ "}"
  show (Assign v e n) = v ++ " = " ++ show e ++ ", " ++ show n
  show (Conditional c t f) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show f

instance Num Expression where
  Number a + Number b = Number $ a + b
  l + r = error $ "Can't add " ++ show l ++ ", " ++ show r
  Number a - Number b = Number $ a - b
  l - r = error $ "Can't subtract " ++ show l ++ ", " ++ show r
  Number a * Number b = Number $ a * b
  l * r = error $ "Can't multiply " ++ show l ++ ", " ++ show r
  negate (Number a) = Number $ negate a
  negate x = error $ "Can't negate " ++ show x
  abs (Number a) = Number $ abs a
  abs x = error $ "Can't abs " ++ show x
  signum (Number a) = Number $ signum a
  signum x = error $ "Can't signum " ++ show x
  fromInteger = Number . fromInteger

instance Fractional Expression where
  Number a / Number b = Number $ a/b
  l / r = error $ "Can't divide " ++ show l ++ ", " ++ show r
  fromRational r = Number $ (fromInteger . numerator $ r) / (fromInteger . denominator $ r)