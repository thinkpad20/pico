import Text.ParserCombinators.Parsec
import Control.Applicative hiding (spaces, (<|>), Int, many)
import Data.List

data Expression = 
  Int Int
  | Float Double
  | String String
  | Char Char
  | Bool Bool
  | Var String
  | Unbound String String
  | UnaryCall String Expression
  | BinaryCall String Expression Expression
  | FunctionCall Expression [Expression]
  | Lambda Expression
  | Assign String Expression Expression
  | Conditional Expression Expression Expression

instance Show Expression where
  show (Int i) = show i
  show (Float f) = show f
  show (String s) = show s
  show (Char c) = show c
  show (Bool b) = show b
  show (Var v) = v
  show (Unbound varName typeName) = varName ++ ":" ++ typeName
  show (UnaryCall s e) = s ++ show e
  show (BinaryCall s l r) = "(" ++ show l ++ " " ++ s ++ " " ++ show r ++ ")"
  show (FunctionCall e es) = show e ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
  show (Lambda e) = "{" ++ show e ++ "}"
  show (Assign v e n) = v ++ " = " ++ show e ++ ", " ++ show n
  show (Conditional c t f) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show f

run :: Show a => Parser a -> String -> IO ()
run p input =
  case parse (p <* eof ) "" input of
    Left err ->
      do putStr "parse error at "
         print err
    Right x -> print x

expression :: Parser Expression
expression = 
  try assignment
  <|> try conditional
  <|> logOr
  

assignment :: Parser Expression
assignment =
  do spaces
     varName <- identifier
     spaces >> char '=' >> spaces
     rightHand <- expression
     spaces >> char ',' >> spaces
     next <- expression
     return $ Assign varName rightHand next

conditional :: Parser Expression
conditional =
  do spaces >> string "if" >> spaces
     condition <- expression
     spaces >> string "then" >> spaces
     ifTrue <- expression
     spaces >> string "else" >> spaces
     ifFalse <- expression
     return $ Conditional condition ifTrue ifFalse

-- from lowest to highest precedence; higher prec will be parsed first
logOr = binary ["||"] logAnd
logAnd = binary ["&&"] comparative
comparative = binary ["<=", ">=", "==", "!=", "<", ">"] additive
additive = binary ["+", "-"] multiplicative
multiplicative = binary ["*", "/"] exponential
exponential = binary ["^"] unary

binary :: [String] -> Parser Expression -> Parser Expression
-- takes a list of operators (syms) and the next higher-precedence rule,
-- parses a binary expression using one of the operators in the list.
binary (sym:syms) next = 
  try getOp <|> next
  where getOp = do spaces
                   left <- next
                   spaces
                   op <- (foldl (<|>) ((try.string) sym) (map (try.string) syms))
                   spaces
                   right <- binary (sym:syms) next
                   spaces
                   return (BinaryCall op left right)

unary :: Parser Expression
unary =
  try go <|> call
  where go = do spaces
                op <- string "-" <|> string "!"
                spaces
                right <- unary
                spaces
                return (UnaryCall op right)

call :: Parser Expression
call =
  try go <|> term
  where go = do spaces
                func <- term
                spaces
                char '('
                exprs <- sepBy expression (spaces >> char ',' >> spaces)
                char ')'
                spaces
                return $ FunctionCall func exprs

term :: Parser Expression
term =
  try floatConstant 
  <|> intConstant
  <|> stringConstant
  <|> charConstant
  <|> boolConstant
  <|> try unbound
  <|> var
  <|> lambda
  <|> parens

floatConstant :: Parser Expression
floatConstant = 
  do
    first <- many1 digit
    dot <- char '.'
    rest <- many1 digit
    return . Float . read $ first ++ (dot : rest)

intConstant :: Parser Expression
intConstant = (Int . read) <$> (many1 digit)

charConstant :: Parser Expression
charConstant = 
  do
    char '\''
    cs <- many1 (noneOf "\'")
    char '\''
    return $ 
      case cs of
        '\\' : c : _ -> 
          case c of
            'n' ->  Char '\n'
            '\\' -> Char '\\'
            'r' ->  Char '\r'
            't' ->  Char '\t'
            'b' ->  Char '\b'
            'a' ->  Char '\a'
            '0' ->  Char '\0'
            otherwise -> Char c
        c : _ -> Char c -- if it's not a '\' then just return the first character

boolConstant :: Parser Expression
boolConstant = do  
  s <- (string "True" <|> string "False")
  case s of
    "True"  -> return $ Bool True
    "False" -> return $ Bool False

stringConstant :: Parser Expression
stringConstant = char '"' >> many (noneOf "\"") >>= \s -> char '"' >> return (String s)

var :: Parser Expression
var = Var <$> identifier

unbound :: Parser Expression
unbound = 
  do
    spaces
    varName <- identifier
    spaces >> char ':' >> spaces
    typeName <- identifier
    spaces
    return $ Unbound varName typeName

identifier :: Parser String
identifier = many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))

symbol :: Parser Expression
symbol =
  do
    first <- oneOf "!$%&|*+-/:<?>@^~#"
    rest <- many $ oneOf "!$%&|*+-/-><?>@^~#"
    return . Var $ first : rest

lambda :: Parser Expression
lambda =
  do
    spaces >> char '{' >> spaces
    expr <- expression
    spaces >> char '}' >> spaces
    return $ Lambda expr

parens :: Parser Expression
parens =
  do
    spaces >> char '(' >> spaces
    expr <- expression
    spaces >> char ')' >> spaces
    return expr