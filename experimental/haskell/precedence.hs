import Text.ParserCombinators.Parsec
import Control.Applicative hiding (spaces, (<|>), Int, many)
import Data.List
import Debug.Trace (traceShow)

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
  | Call Expression [Expression]
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
  show (Call e es) = show e ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
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
  <|> symbolic

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
symbolic = binary [symbol] logOr
logOr = binary [string "||"] logAnd
logAnd = binary [string "&&"] comparative
comparative = binary (map (try.string) ["<=", ">=", "<", ">", "==", "!="]) additive
additive = binary (map string ["+", "-"]) multiplicative
multiplicative = binary (map string ["*", "/", "%"]) exponential
exponential = binary [string "^"] unary

binary :: [Parser String] -> Parser Expression -> Parser Expression
-- takes a list of operators (syms) and the next higher-precedence rule,
-- parses a binary expression using one of the operators in the list.
binary ps next = chainl1 next getOp
  where getOp = do spaces
                   s <- foldl1 (<|>) ps -- OR all of the parser rules in the list
                   spaces
                   return $ BinaryCall s

unary :: Parser Expression
unary =
  go <|> call
  where go = do spaces
                op <- string "-" <|> string "!"
                spaces
                right <- unary
                spaces
                return (UnaryCall op right)

call :: Parser Expression
call = 
  do spaces
     t <- term
     spaces
     as <- args
     spaces 
     return $ foldl Call t as
     where 
      args :: Parser [[Expression]]
      args = many exprLists
      exprLists :: Parser [Expression]
      exprLists = do schar '('
                     es <- sepBy expression $ schar ','
                     schar ')'
                     return es

term :: Parser Expression
term =
  number
  <|> stringConstant
  <|> charConstant
  <|> boolConstant
  <|> try unbound
  <|> var
  <|> lambda
  <|> parens

number :: Parser Expression
-- parses either an int or a float
number = 
  do first <- many1 digit
     rest <- optionMaybe $ do dot <- char '.'
                              ns <- many1 digit
                              return (dot:ns)
     return $ case rest of
                Just ns -> Float . read $ first ++ ns
                Nothing -> (Int . read) first

charConstant :: Parser Expression
charConstant = 
  do char '\''
     cs <- many1 (noneOf "\'")
     char '\''
     return $ Char (escapeChar cs)

escapeChar :: String -> Char
escapeChar cs = 
  case cs of
    '\\' : c : _ -> 
      case c of
        'n' -> '\n'
        '\\' -> '\\'
        'r' -> '\r'
        't' -> '\t'
        'b' -> '\b'
        'a' -> '\a'
        '0' -> '\0'
        otherwise -> c
    c : _ -> c -- if it's not a '\' then just return the first character

boolConstant :: Parser Expression
boolConstant = do  
  s <- string "True" <|> string "False"
  case s of
    "True"  -> return $ Bool True
    "False" -> return $ Bool False

stringConstant :: Parser Expression
stringConstant = do {char '"'; s <- many (noneOf "\""); char '"'; return $ String s }

var :: Parser Expression
var = Var <$> identifier

unbound :: Parser Expression
unbound = 
  do { spaces; varName <- identifier; schar ':'; typeName <- identifier; spaces;
     return $ Unbound varName typeName }

identifier :: Parser String
identifier = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

symbol :: Parser String
symbol =
  do first <- oneOf "!$%&|*+-/:<?>@^~#"
     rest <- many $ oneOf "!$%&|*+=-/-><?>@^~#"
     return $ first : rest

lambda :: Parser Expression
lambda = do { schar '{'; expr <- expression; schar '}'; return $ Lambda expr }

parens :: Parser Expression
parens = do { schar '('; expr <- expression; schar ')'; return expr }

schar :: Char -> Parser Char
schar c = do { spaces; ch <- char c; spaces; return ch }