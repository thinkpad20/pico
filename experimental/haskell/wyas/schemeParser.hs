module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | String String
             | Bool Bool deriving Show

main :: IO ()
main = do
   args <- getArgs
   putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse (sp >> parseExpr) "lisp" input of
   Left err -> "No match: " ++ show err
   Right val -> "Found value: " ++ show val

parseSym :: Parser Char
parseSym = oneOf "!$%&|*+-/:<=?>@^_~#"

sp :: Parser ()
sp = skipMany1 space

parseStr :: Parser LispVal
parseStr = do
   char '"'
   s <- many (noneOf "\"")
   char '"'
   return $ String s

parseAtom :: Parser LispVal
parseAtom = do
   first <- letter <|> parseSym
   rest <- many $ letter <|> digit <|> parseSym
   let atom = [first] ++ rest
   return $ case atom of 
      "#t" -> Bool True
      "#f" -> Bool False
      otherwise -> Atom atom

parseInt :: Parser LispVal
parseInt = liftM (Integer . read) $ many1 digit


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseStr <|> parseInt
