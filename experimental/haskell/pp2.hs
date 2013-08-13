module Main where

import Control.Monad
import Control.Applicative hiding (many, (<|>))
import System.Environment
import Text.ParserCombinators.Parsec

data Expression = 
  Call Call
  | Assign String Expression Expression
  | Conditional Expression Expression Expression deriving Show

data Call =
  Term Term
  | Binary String Expression Expression
  | Unary String Expression
  | FunctionCall Expression [Expression] deriving Show

data Term =
  Int Int
  | Float Double
  | String String
  | Char Char
  | Bool Bool
  | Var String
  | Unbound String String
  | Lambda Expression
  | Parens Expression deriving Show

run :: Show a => Parser a -> String -> IO ()
run parser input = 
  case parse (parser <* eof) "" input of
    Left err -> putStrLn $ "No match: " ++ show err
    Right val -> putStrLn $ "Found value: " ++ show val

pico :: Parser Expression
pico = expression <* eof

expression :: Parser Expression
expression = 
  logical
  <|> assign
  <|> conditional

assign :: Parser Expression -- assign a string to an expression
assign = 
  do
    varname <- identifier
    spaces >> char '=' >> spaces
    expr <- expression
    spaces >> char ',' >> spaces
    next <- expression
    spaces
    return (Assign varname expr next)

conditional :: Parser Expression -- if/then/else statement
conditional =
  do
    spaces >> string "if" >> spaces
    cond <- expression
    spaces >> string "then" >> spaces
    ifTrue <- expression
    spaces >> string "else" >> spaces
    ifFalse <- expression
    spaces
    return $ Conditional cond ifTrue ifFalse

--symbolic :: Parser Expression -- user-specified binary or unary symbols, like >>= or <*>
--symbolic =
--  logical
--  <|> do -- binary symbol
--    left <- logical
--    spaces
--    sym <- symbol
--    spaces
--    right <- symbolic
--    return $ FunctionCall (Call $ Term sym) [Call left, Call right]
--  <|> do
--    sym <- symbol
--    spaces
--    right <- symbolic
--    return $ FunctionCall (Call $ Term sym) [Call right]

logical :: Parser Expression -- and, or
logical = chainl1 (Call <$> comparison) getOp where
  getOp :: Parser (Expression -> Expression -> Call)
  getOp =
    do spaces
       op <- (string "&&" <|> string "||")
       spaces
       return (Binary op)

comparison :: Parser Expression -- less than, greater than, etc
comparison = chainl1 additive getOp where
  getOp :: Parser (Expression -> Expression -> Expression)
  getOp =
    do spaces
       op <- (string "<" <|> string ">" <|> string "<=" 
              <|> string ">=" <|> string "==" <|> string "!=")
       spaces
       return $ (Call . Binary) op

additive :: Parser Expression -- plus/minus
additive = chainl1 multiplicative getOp where
  getOp :: Parser (Expression -> Expression -> Expression)
  getOp =
    do spaces
       op <- (string "+" <|> string "-")
       spaces
       return $ (Call . Binary) op

multiplicative :: Parser Expression -- times/divide/mod
multiplicative = chainl1 unary getOp where
  getOp :: Parser (Expression -> Expression -> Expression)
  getOp =
    do spaces
       op <- (string "*" <|> string "/" <|> string "%")
       spaces
       return $ (Call . Binary) op


unary :: Parser Expression -- unary minus
unary =
  call
  <|> do
    spaces
    op <- Var <$> (string "-")
    spaces
    u <- unary
    spaces
    return $ Call (FunctionCall (Call $ Term op) [Call u])

call :: Parser Expression
call = 
  do
    spaces
    primary <- term -- primary term, e.g. 'f' in 'f(x)'
    spaces
    invs <- optionMaybe invocationList -- may not be there, if variable only
    spaces
    case invs of
      Nothing -> return (Term primary)
      Just invList -> return (genCalls primary invList) 
        where
          genCalls :: Term -> [[Expression]] -> Call -- grab all of the parentheses pairs
          genCalls t [] = Term t
          genCalls t es = FunctionCall (Call (genCalls t (init es))) (last es)
    where
      expressionList :: Parser [Expression] -- get a list of expressions separated by ','
      expressionList = sepBy expression (spaces >> char ',' >> spaces)

      invocationList :: Parser [[Expression]]
      invocationList =
        do
          spaces >> char '(' >> spaces -- we need at least one opening parenthesis
          invocations <- sepBy expressionList (spaces >> char ')' >> spaces >> char '(' >> spaces)
          spaces >> char ')' >> spaces
          return invocations

term :: Parser Term
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

floatConstant :: Parser Term
floatConstant = 
  do
    first <- many1 digit
    dot <- char '.'
    rest <- many1 digit
    return . Float . read $ first ++ (dot : rest)

intConstant :: Parser Term
intConstant = (Int . read) <$> (many1 digit)

charConstant :: Parser Term
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

boolConstant :: Parser Term
boolConstant = do  
  s <- (string "True" <|> string "False")
  case s of
    "True"  -> return $ Bool True
    "False" -> return $ Bool False

stringConstant :: Parser Term
stringConstant = char '"' >> many (noneOf "\"") >>= \s -> char '"' >> return (String s)

var :: Parser Term
var = Var <$> identifier

unbound :: Parser Term
unbound = 
  do
    spaces
    typeName <- identifier
    spaces >> char ':' >> spaces
    varName <- identifier
    spaces
    return $ Unbound typeName varName

identifier :: Parser String
identifier = many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))

symbol :: Parser Term
symbol =
  do
    first <- oneOf "!$%&|*+-/:<?>@^~#"
    rest <- many $ oneOf "!$%&|*+-/-><?>@^~#"
    return . Var $ first : rest

lambda :: Parser Term
lambda =
  do
    spaces >> char '{' >> spaces
    expr <- expression
    spaces >> char '}' >> spaces
    return $ Lambda expr

parens :: Parser Term
parens =
  do
    spaces >> char '(' >> spaces
    expr <- expression
    spaces >> char ')' >> spaces
    return $ Parens expr