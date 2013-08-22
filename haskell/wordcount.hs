module WordCount where

import Trie

countWords = (foldr step End) . words
  where step w t = case find w t of
                    Just c -> insert w (c + 1) t
                    Nothing -> insert w 1 t