file2list :: [Char] -> [[Char]]
file2list pg = mydbsplit ' ' '\n' pg
  where mydbsplit :: (Eq a) => a -> a -> [a] -> [[a]]
        mydbsplit x y [] = []
        mydbsplit x y (c:cs)
          | c == x = mydbsplit x y cs
          | c == y = mydbsplit x y cs
          | otherwise = [res] ++ mydbsplit x y (drop (length res) cs)
          where res = read x y (c:cs)
                read x y [] = []
                read x y (c:cs) 
                  | c == x = []
                  | c == y = []
                  | otherwise = c:read x y cs

parseA :: [[Char]] -> [[[Char]]]
parseA tokens = oneSplit "let" tokens
  where oneSplit str [] = []
        oneSplit str (s:ns)
          | s == str = oneSplit s ns
          | otherwise = [res] ++ oneSplit str (drop (length res) ns)
          where res = read str (s:ns)
                read str [] = []
                read str (s:ns)
                  | s == str = []
                  | otherwise = s:read str ns
