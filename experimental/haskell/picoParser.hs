module Main where

import Control.Monad
import Control.Applicative hiding (many, (<|>))
import System.Environment
import Text.ParserCombinators.Parsec

data Expr = 
  Literal Literal
  | Var String
  | Symbol String
  | Unbound String String
  | If Expr Expr Expr
  | Lambda Expr
  | Assign String Expr Expr
  | Call Expr [Expr]

data Literal = 
  Int Int
  | Float Double
  | String String
  | Char Char
  | Bool Bool

main :: IO ()
main = 
  do
    args <- getArgs
    putStrLn (run parseExpr (args !! 0))

run :: Show a => Parser a -> String -> String
run parser input = 
  case parse parser "pico" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val


lcase = "abcdefghijklmnopqrstuvwxyz"
ucase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
dig = "0123456789"
validChars = many $ oneOf (ucase ++ lcase ++ dig ++ "_")

parseInt :: Parser Literal
parseInt = (Int . read) <$> (many1 digit)

parseFloat :: Parser Literal
parseFloat = 
  do
    first <- many1 digit
    dot <- char '.'
    rest <- many1 digit
    return . Float . read $ first ++ (dot : rest)

parseChar :: Parser Literal
parseChar = 
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

parseBool :: Parser Literal
parseBool = 
  do  s <- (string "True" <|> string "False")
      case s of
        "True"  -> return $ Bool True
        "False" -> return $ Bool False

parseString :: Parser Literal
parseString = char '"' >> many (noneOf "\"") >>= \s -> char '"' >> return (String s)

parseLiteral :: Parser Literal
parseLiteral = 
  try parseFloat 
  <|> parseInt 
  <|> parseString 
  <|> parseChar 
  <|> parseBool

parseID :: Parser String
parseID = 
  do
    first <- oneOf (lcase ++ "_")
    rest <- validChars
    return $ first : rest

parseTypeName :: Parser String
parseTypeName = 
  do
    first <- oneOf ucase
    rest <- validChars
    return $ first : rest

parseVar :: Parser Expr
parseVar = Var <$> parseID

parseUnbound :: Parser Expr
parseUnbound = 
  do
    spaces
    typeName <- parseTypeName
    spaces
    varName <- parseID
    spaces
    return $ Unbound typeName varName

--parseSymbol = 
--  do
   
--    return $ Symbol $ first : rest


parseSymbol :: Parser Expr
parseSymbol = Symbol <$> sym
  where
    sym = (:) <$> first <*> rest -- concatenation lifted into Parser
    first = oneOf "!$%&|*+-/:<?>@^~#"
    rest = many $ oneOf "!$%&|*+-/:=<?>@^~#"

parseSingleExpr :: Parser Expr
parseSingleExpr = 
  parseVar 
  <|> parseUnbound 
  <|> (Literal <$> parseLiteral)
  <|> parseSymbol
  <|> parseLambda
  <|> (char '(' >> parseExpr >>= \e -> char ')' >> return e)

parseBinSymbolCall = do
  spaces
  p1 <- parseSingleExpr
  spaces
  sym <- parseSymbol
  spaces
  p2 <- parseExpr
  spaces
  return $ Call sym [p1, p2]

parseUnSymbolCall = 
  do
    spaces
    s <- parseSymbol
    spaces
    e <- parseSingleExpr
    spaces
    return (Call s [e])

parseLambda = 
  do
    spaces >> char '{' >> spaces
    p <- parseExpr
    spaces >> char '}' >> spaces
    return (Lambda p)

parseAssign = 
  do
    var <- parseID
    spaces >> char '=' >> spaces
    expr <- parseExpr
    spaces >> char ',' >> spaces
    next <- parseExpr
    spaces
    return (Assign var expr next)


parseCall :: Parser Expr
parseCall = 
  do
    spaces  
    term <- parseSingleExpr
    spaces >> char '(' >> spaces
    exprList <- sepBy parseExpr (spaces >> char ',' >> spaces) -- get a list of expressions separated by ','
    spaces >> char ')' >> spaces
    return $ Call term exprList

parseExpr :: Parser Expr
parseExpr = 
  try parseCall
  <|> try parseBinSymbolCall
  <|> try parseUnSymbolCall
  <|> try parseAssign
  <|> parseSingleExpr

parsePico :: Parser Expr
parsePico = parseExpr <* eof

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

instance Show Literal where
  show (Int i) = show i
  show (Float f) = show f
  show (String s) = show s
  show (Char c) = show c
  show (Bool b) = show b

instance Show Expr where
  show (Literal l) = show l
  show (Var v) = v
  show (Symbol s) = s
  show (Unbound t v) = t ++ " " ++ v
  show (If cond ifTrue ifFalse) = "if " ++ show cond ++ " then " ++ 
                                  show ifTrue ++ ", else " ++ show ifFalse
  show (Lambda e) = "{" ++ show e ++ "}"
  show (Assign s e next) = s ++ " = " ++ show e ++ ", " ++ show next
  show (Call e es) = 
    case e of
      Symbol s -> 
        case es of 
          [ex] -> s ++ show ex
          (x:y:_) -> show x ++ " " ++ s ++ " " ++ show y
      otherwise ->
        show e ++ showExprList es True
      where 
        showExprList [] True = ""
        showExprList [] False = ")"
        showExprList (e:es) True = "(" ++ show e ++ showExprList es False
        showExprList (e:es) False = ", " ++ show e ++ showExprList es False
