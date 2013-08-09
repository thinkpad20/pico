module Main where

import Control.Monad
import Control.Applicative hiding (many, (<|>))
import System.Environment
import Text.ParserCombinators.Parsec

data Pico = Literal Literal
          | Var String
          | Symbol String
          | Unbound String String
          | If Pico Pico Pico
          | Lambda Pico
          | Assign String Pico
          | Call Pico [Pico] 
          deriving Show

data Literal = Int Int
             | Float Double
             | String String
             | Char Char
             | Bool Bool 
             deriving Show

main :: IO ()
main = do
   args <- getArgs
   putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse parseExpr "pico" input of
   Left err -> "No match: " ++ show err
   Right val -> "Found value: " ++ show val

--sp :: Parser ()
--sp = skipMany1 space

lcase = "abcdefghijklmnopqrstuvwxyz"
ucase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
dig = "0123456789"
validChars = many $ oneOf (ucase ++ lcase ++ dig ++ "_")

------------------------------------------------------------
------------------- LITERAL PARSERS-------------------------
------------------------------------------------------------

parseInt :: Parser Literal
parseInt = liftM (Int . read) (many1 digit)

parseFloat :: Parser Literal
parseFloat = do
   first <- many1 digit
   dot <- char '.'
   rest <- many1 digit
   return . Float . read $ first ++ (dot : rest)

parseChar :: Parser Literal
parseChar = do
   char '\''
   cs <- many1 (noneOf "\'")
   char '\''
   return $ case cs of
      '\\' : c : _ -> case c of
         'n' -> Char '\n'
         '\\' -> Char '\\'
         'r' -> Char '\r'
         't' -> Char '\t'
         'b' -> Char '\b'
         'a' -> Char '\a'
         '0' -> Char '\0'
         otherwise -> Char c
      c' : rest -> Char c'

parseBool :: Parser Literal
parseBool = (string "True" <|> string "False") >>= \s -> case s of
            "True" -> return $ Bool True
            "False" -> return $ Bool False

parseString :: Parser Literal
parseString = char '"' >> many (noneOf "\"") >>= \s -> char '"' >> return (String s)

parseLiteral :: Parser Literal
parseLiteral = try parseFloat <|> parseInt <|> parseString <|> parseChar <|> parseBool

parseID :: Parser String
parseID = do
   first <- oneOf (lcase ++ "_")
   rest <- validChars
   return $ first : rest

parseTypeName :: Parser String
parseTypeName = do
   first <- oneOf ucase
   rest <- validChars
   return $ first : rest

parseVar :: Parser Pico
parseVar = Var <$> parseID


parseUnbound :: Parser Pico
parseUnbound = do
   typeName <- parseTypeName
   spaces
   varName <- parseID
   return $ Unbound typeName varName

parseSymbol :: Parser Pico
parseSymbol = Symbol <$> (many1 $ oneOf "!$%&|*+-/:=<?>@^~#")

parseSingleExpr :: Parser Pico
parseSingleExpr = parseVar 
                  <|> parseUnbound 
                  <|> (Literal <$> parseLiteral) 
                  <|> parseSymbol
                  <|> parseLambda
                  <|> do 
                     char '('
                     pico <- parseExpr
                     char ')'
                     return pico
                     
parseBinSymbolCall = do
   p1 <- parseSingleExpr
   spaces
   sym <- parseSymbol
   spaces
   p2 <- parseSingleExpr
   return $ Call sym [p1, p2]

parseUnSymbolCall = parseSymbol >>= \s -> spaces >> parseSingleExpr >>= \e -> return (Call s [e])
parseLambda =  char '{' >> parseExpr >>= \p -> char '}' >> return (Lambda p)
parseAssign = parseID  >>= \var -> spaces >> char '=' >> spaces >> parseExpr >>= \expr -> return (Assign var expr)


parseCall :: Parser Pico
parseCall = do
   term <- parseSingleExpr
   spaces
   char '('
   spaces
   exprList <- sepBy parseExpr (spaces >> char ',' >> spaces) 
   spaces
   char ')'
   return $ Call term exprList

parseExpr :: Parser Pico
parseExpr = try parseAssign
            <|> try parseBinSymbolCall 
            <|> try parseUnSymbolCall
            <|> try parseCall
            <|> parseSingleExpr

re = putStrLn.show.readExpr