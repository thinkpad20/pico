module PicoAST where

import Data.List (intercalate)

data Expression = 
  PNum Double
  | PString String
  | PChar Char
  | PBool Bool
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
  show (PNum n) = show n
  show (PString s) = show s
  show (PChar c) = show c
  show (PBool b) = show b
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