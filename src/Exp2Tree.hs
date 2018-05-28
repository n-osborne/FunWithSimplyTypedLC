module Exp2Tree
  (exp2tree)
  where

import System.Environment
import System.IO
import Ast2Tree
import Lambda2Ast

 
exp2tree = do
  args <- getArgs
  if (length args) /= 2
    then putStrLn "Error"
    else do writeFile (head args) (exp2tree' (head (drop 1 args)))
              where 
                exp2tree' :: [Char] -> [Char]
                exp2tree' exp = ast2tree (exp2ast exp)
