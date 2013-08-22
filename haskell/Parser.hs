module PicoParser where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding (spaces, (<|>), many)
import PicoAST

run :: String -> Expression
run input = case runSingle expression input of
              Right expr -> expr
              Left err -> error $ show err

runSingle :: Parser Expression -> String -> Either ParseError Expression
runSingle p input = parse (p <* eof) "pico" input

expression :: Parser Expression
expression = 
  try assignment
  <|> try conditional
  <|> symbolic

assignment :: Parser Expression
assignment =
  do spaces
     varName <- identifier
     schar '='
     rightHand <- expression
     schar ','
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

-- Listed from lowest to highest precedence. Higher prec will be parsed first.
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
                   return $ Binary s

unary :: Parser Expression
unary =
  go <|> call
  where go = do spaces
                op <- string "-" <|> string "!"
                spaces
                right <- unary
                spaces
                return (Unary op right)

call :: Parser Expression
call = 
  do spaces
     t <- term
     spaces
     as <- args
     spaces 
     return $ foldl Call t as
     where 
      args :: Parser [[Maybe Expression]]
      args = many exprLists
      exprLists :: Parser [Maybe Expression]
      exprLists = do schar '('
                     es <- sepBy (optionMaybe expression) $ schar ','
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
     return $ (PNum . read) (case rest of
                              Just ns -> first ++ ns
                              Nothing -> first)

charConstant :: Parser Expression
charConstant = 
  do char '\''
     cs <- many1 (noneOf "\'")
     char '\''
     return $ PChar (escapeChar cs)

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
    "True"  -> return $ PBool True
    "False" -> return $ PBool False

stringConstant :: Parser Expression
stringConstant = do {char '"'; s <- many (noneOf "\""); char '"'; return $ PString s }

var :: Parser Expression
var = Var <$> identifier

strToType :: String -> PType
strToType "num" = NumT
strToType "char" = CharT
strToType "string" = StringT
strToType "bool" = BoolT
strToType s = CustomT s

getTypes :: Parser PType
getTypes =
  (do schar '('
      types <- sepBy identifier (schar ',')
      schar ')'
      return $ case types of
                  [t] -> strToType t
                  otherwise -> FunctionT tList typ where
                    tList = map strToType $ init types 
                    typ = strToType $ last types)
  <|> strToType <$> identifier

unbound :: Parser Expression
unbound = 
  do spaces
     varName <- identifier
     schar ':'
     typ <- getTypes
     spaces
     return $ Unbound varName typ

identifier :: Parser String
identifier = 
  many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))
  <|> many1 (char '%')

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