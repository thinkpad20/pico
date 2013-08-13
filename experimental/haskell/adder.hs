import Text.ParserCombinators.Parsec
import Control.Applicative hiding (spaces, (<|>), Const)

data IntExp = Const Int
            | BinaryCall String IntExp IntExp
            | UnaryCall String IntExp

instance Show IntExp where
  show (Const n) = show n
  show (BinaryCall s l r) = "(" ++ show l ++ s ++ show r ++ ")"
  show (UnaryCall s e) = s ++ "(" ++ show e ++ ")"

run :: Show a => Parser a -> String -> IO ()
run p input =
  case parse p "" input of
    Left err ->
      do putStr "parse error at "
         print err
    Right x -> print x

parseIntExp :: Parser IntExp
parseIntExp = chainl1 parseConstant parseOperation

parseOperation :: Parser (IntExp -> IntExp -> IntExp)
parseOperation =
  do spaces
     symbol <- many1 (oneOf "+-*/><=!&|")
     spaces
     return $ BinaryCall symbol

parseConstant :: Parser IntExp
parseConstant = Const . read <$> (many1 digit)