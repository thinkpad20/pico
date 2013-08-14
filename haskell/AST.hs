module PicoAST where

import Data.List (intercalate)

data Expression = 
  PInt Int
  | PFloat Double
  | PString String
  | PChar Char
  | PBool Bool
  | Var String
  | Unbound String String
  | Unary String Expression
  | Binary String Expression Expression
  | Call Expression [Maybe Expression]
  | Lambda Expression
  | Assign String Expression Expression
  | Conditional Expression Expression Expression
  deriving Eq

instance Show Expression where
  show (PInt i) = show i
  show (PFloat f) = show f
  show (PString s) = show s
  show (PChar c) = show c
  show (PBool b) = show b
  show (Var v) = v
  show (Unbound varName typeName) = varName ++ ":" ++ typeName
  show (Unary s e) = s ++ show e
  show (Binary s l r) = "(" ++ show l ++ " " ++ s ++ " " ++ show r ++ ")"
  show (Call e es) = show e ++ "(" ++ (intercalate ", " (map showME es)) ++ ")"
    where showME (Just expr) = show expr
          showME Nothing = ""
  show (Lambda e) = "{" ++ show e ++ "}"
  show (Assign v e n) = v ++ " = " ++ show e ++ ", " ++ show n
  show (Conditional c t f) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show f

instance Num Expression where
   PInt a + PInt b = PInt $ a + b
   PInt a + PFloat b = PFloat $ fromIntegral a + b
   PFloat a + PInt b = PFloat $ a + fromIntegral b
   PFloat a + PFloat b = PFloat $ a + b
   _ + _ = undefined
   PInt a - PInt b = PInt $ a - b
   PInt a - PFloat b = PFloat $ fromIntegral a - b
   PFloat a - PInt b = PFloat $ a - fromIntegral b
   PFloat a - PFloat b = PFloat $ a - b
   _ - _ = undefined
   PInt a * PInt b = PInt $ a * b
   PInt a * PFloat b = PFloat $ fromIntegral a * b
   PFloat a * PInt b = PFloat $ a * fromIntegral b
   PFloat a * PFloat b = PFloat $ a * b
   _ * _ = undefined
   negate (PInt a) = PInt (-a)
   negate (PFloat a) = PFloat (-a)
   negate _ = undefined
   abs (PInt a) = PInt $ abs a
   abs (PFloat a) = PFloat $ abs a
   abs _ = undefined
   signum (PInt a) = PInt $ signum a
   signum (PFloat a) = PFloat $ signum a
   signum _ = undefined
   fromInteger i = (PInt $ fromInteger i)
