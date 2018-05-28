{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module : Utils
Copyright : (c) 2018 Nicolas Osborne
Licence : BSD 3-clauses

-}
module Utils
  ( oneSplit
  , stripPar
  ) where
         
-- | Split a string into two substrings, the prefix up to the first occurrence
-- of the given Char, and the suffix beginning after this occurrence.
--
-- [@input@]: the separator
-- [@input@]: the string
-- [@output@]: the pair of the two substrings
oneSplit :: Char -> [Char] -> ([Char], [Char])
oneSplit c str = (fst res, drop (1+(snd res)) str)
  where res = substring 0 c [] str
        substring :: Int -> Char -> [Char] -> [Char] -> ([Char], Int)
        substring n c w [] = (w, n)
        substring n sep w (c:cs)
          | c == sep = (w, n)
          | otherwise = substring (n+1) sep (w++[c]) cs
               
-- | strip a string from its outer parenthesis if there is any
stripPar :: [Char] -> [Char]
stripPar (c:cs)
  | c == '(' = take ((length cs) - 1) cs
  | otherwise = c:cs
