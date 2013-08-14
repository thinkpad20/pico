-- Implementing an LL(1) parser in Haskell. The pseudocode for the parse
-- as given by Alex Aiken in the Coursera compilers course is:

-- initialize stack = <S $> and next = first token
-- repeat:
--     case stack of:
--         <X, rest>: if T[X, *next] = [Y1..YN]
--         <t, rest>: if t == *next++