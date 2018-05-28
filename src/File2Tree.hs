{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module : Main
Copyright = (c) 2018 Nicolas Osborne
Licence : BSD 3-clauses
-}
module Main where

import System.Environment
import System.IO
import Ast2Tree
import Lambda2Ast

-- | Write in a file which name is given in first argument the tree description
-- corresponding to the lambda expression described in the file given in second
-- argument.
main = do
  args <- getArgs
  if (length args) /= 2
    then putStrLn "Error"
    else do file <- readFile (head (drop 1 args))
            writeFile (head args) (file2tree file)
            where
              file2tree :: [Char] -> [Char]
              file2tree file = ast2tree (file2ast file) 
