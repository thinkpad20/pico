module Trie where

import qualified Data.Map as M

data Trie a b = End
              | Node (M.Map a (Trie a b)) (Maybe b)
              deriving (Show)

insert :: (Ord a) => [a] -> b -> Trie a b -> Trie a b
insert [] val End = Node M.empty (Just val)
insert [] val (Node ts _) = Node ts (Just val)
insert (a:as) val End = Node (M.singleton a (insert as val End)) Nothing
insert (a:as) val (Node ts m) = Node (M.insert a (insert as val t') ts) m 
  where t' = case M.lookup a ts of
              Just t -> t
              Nothing -> End

find :: (Ord a) => [a] -> Trie a b -> Maybe b
find _ End = Nothing
find [] (Node _ (Just b)) = Just b
find (a:as) (Node ts _) = case M.lookup a ts of
                            Just t -> find as t
                            Nothing -> Nothing

--keys :: Trie a b -> [[a]]
--keys t = keys' t [] [] where
--  keys' End acc _ = acc
--  keys' (Node ts (Just _)) acc prefix = recurse (M.toList ts) []
--    where recurse [] acc' = acc'
--          recurse ((c,t) : vals) acc' = 
--            recurse vals $ (keys' t (acc ++ prefix) (prefix ++ c)) : acc' 

instance Functor (Trie a) where
   fmap _ End = End
   fmap f (Node ts Nothing) = Node (fmap (fmap f) ts) Nothing
   fmap f (Node ts (Just b)) = Node (fmap (fmap f) ts) (Just (f b))
