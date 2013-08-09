-- Rewrite parseNumber (parseInt = liftM (Integer . read) $ many1 digit) using
-- (a) do-notation
parseInt' :: Parser LispVal
parseInt' = do
   num <- many1 digit
   let n = read num
   return $ Integer n

-- (b) explicit sequencing with >>=
parseInt'' :: Parser LispVal
parseInt'' = many1 digit >>= (return . Integer . read)

--Our strings aren't quite R5RS compliant, because they don't support escaping of internal quotes within the string. Change parseString so that \" gives a literal quote character instead of terminating the string. You may want to replace noneOf "\"" with a new parser action that accepts either a non-quote character or a backslash followed by a quote mark.
--Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters
--Change parseNumber to support the Scheme standard for different bases. You may find the readOct and readHex functions useful.
--Add a Character constructor to LispVal, and create a parser for character literals as described in R5RS.
--Add a Float constructor to LispVal, and support R5RS syntax for decimals. The Haskell function readFloat may be useful.
--Add data types and parsers to support the full numeric tower of Scheme numeric types. Haskell has built-in types to represent many of these; check the Prelude. For the others, you can define compound types that represent eg. a Rational as a numerator and denominator, or a Complex as a real and imaginary part (each itself a Real number).