printParens :: Show a => [a] -> String
printParens as = p' as True where
   p' [] True = ""
   p' [] False = ")"
   p' (a:as) True = "(" ++ show a ++ p' as False
   p' (a:as) False = ", " ++ show a ++ p' as False