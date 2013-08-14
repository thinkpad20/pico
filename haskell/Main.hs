module Main where

import Control.Applicative
import PicoParser
import PicoAST
import PicoEval (reduce, getType, getNumUnbound)
import System.Environment

main :: IO ()
main = 
  do input <- (!! 0) <$> getArgs
     parseReduce input
        

parseReduce :: String -> IO ()
parseReduce input = 
   case run input of
      Left err -> putStrLn $ "Ruh roh! Parse error: " ++ (show err)
      Right expr -> do
         putStrLn "Successful parse: "
         print expr
         --putStrLn "This is of type: "
         --print $ getType expr []
         putStrLn ("This has " ++ (show $ getNumUnbound expr) ++ " unbound variables.")
         putStrLn "Reduces to:"
         print $ reduce expr []