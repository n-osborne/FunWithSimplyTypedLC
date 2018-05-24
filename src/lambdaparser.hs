{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module : LambdaParser
Copyrigth : (c) 2018 Nicolas Osborne
Licence : MIT

Provide the function pg2ast which turn a program written in Simply Typed Lambda
Calculus into an Abstract Syntax Tree.

A program in Simply Typed Lambda Calculus is divided into two main parts:

* An @Assignations@ part containing assignations instruction of the form:

    @
    let id Exp
    @

    Where @id@ is an identificator and @Exp@ is a simply styped lambda
    expression.

* A @Program@ part containing the actual computation which may contain @id@
  previously defined in the @Assignations@ part.
-}
module LambdaParser
  ( ) where

-- Type declaration and main program:

-- | Abstract Syntax Tree type declaration.
data AST =
  -- | Correspond to the constant of the language. 
  Constant { name :: String -- ^ name of the constant
           , ltype :: String -- ^ type of the constant
           } |
  -- | Correspond to the unary operators of the language.
  Unary { name :: String -- ^ name of the operator
        , ltype :: String -- ^ type of the operator
        , below :: AST -- ^ argument of the operator
        } |
  -- | Correspond to the binary operators of the language.
  Binary { name :: String -- ^ name of the operator
         , ltype :: String -- ^ type of the operator
         , left :: AST -- ^ first argument of the operator
         , rigth :: AST -- ^second argument of the operator
         } deriving (Show)

-- | Turn a source file into the corresponding AST.
--
-- A source file must contain two main parts deliminated by the three following
--key words in that order: @Assignations@, @Program@, @End@. The assignation part
--(between the two first key words) is a series of assignation instruction (with
--the @let@ key word) and the program part (between the two lasts key words) is
--the actual computation of which the source file is a description. The program
--part may contain the identificators declared in the assignation part.
--
-- [@input@]: The content of the source file.
-- [@output@]: the corresponding @AST@.
pg2ast :: [Char] -> AST
pg2ast pg = Constant "true" "bool"


-- Helper functions

-- TODO avoid use of length by computing length res when building res
-- in order to do that: read :: x -> y -> [] -> (Int, [])
-- | Turn a String into the corresponding list of tokens.
--
-- Split the string and drop the separators.
-- Separators are whitespace and end of line.
file2list :: [Char] -> [[Char]]
file2list pg = mydbsplit ' ' '\n' pg
  where 
    mydbsplit :: (Eq a) => a -> a -> [a] -> [[a]]
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

{--
would be version with import Data.List.Split
file2list pg = split (condens . dropDelims oneOf "\n ") pg
-}
  
-- TODO handle commentary section before Assignations
-- | Return the sub-list corresponding to the list of tokens corresponding to
-- the Assignation part of the source file.
extractAssignations :: [[Char]] -> [[Char]]
extractAssignations [] = []
extractAssignations (s:ns)
  | s == "Assignations" = extractAssignations ns
  | s == "Program" = []
  | otherwise = s:extractAssignations ns

-- | Return the sub-list corresponding to the list of tokens corresponding to
-- the Program part of the source file
extractPg :: [[Char]] -> [[Char]]
extractPg [] = []
extractPg (s:ns)
  | s == "Program" = stripEnd ns
  | otherwise = extractPg ns
  where stripEnd (w:ws)
          | w == "End" = []
          | otherwise = w:stripEnd ws
          
  -- TODO use Data.HashMap.Strict for better time complexity
-- | Parse the Assignation part of the source file.
--
-- [@Input@]: The list of tokens corresponding to the Assignation part of the
--source file.
-- [@Output@]: A list of assignations. The @head@ of each item is an @id@ and
--the @tail@ is the @expression@
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

-- | Substitute the Id in the list of tokens corresponding to the program part
-- of the source file by the corresponding list of tokens.
preparePg :: [[[Char]]] -> [[Char]] -> [[Char]]
preparePg _ [] = []
preparePg list1 (x:xs) = (check list1 x) ++ (preparePg list1 xs)
  where check :: [[[Char]]] -> [Char] -> [[Char]]
        check [] x = [x]
        check (y:ys) x
          | x == head y = tail y
          | otherwise = check ys x
         

-- | Create the AST corresponding to the given list of tokens.
createAST :: [[Char]] -> AST
createAST list = Constant "true" "bool"



