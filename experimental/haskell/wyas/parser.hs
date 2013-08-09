module Main where

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | String String
             | Bool Bool deriving Show

main :: IO ()
main = do
   args <- getArgs
   putStrLn $ readExpr $ args !! 0

readExpr :: String -> String
readExpr input = case parse parseStr "foobar" input of
   Left err -> "No match " ++ show err
   Right val -> "Match! " ++ show val

parseStr :: Parser LispVal
parseStr = char '"' >> many (noneOf "\"") >>= \s -> char '"' >> return (String s)